CXX := g++
CXXFLAGS := \
    -std=c++20 \
    -Wall \
    -Wextra \
    -Wpedantic \
    -g

CXXFLAGS += -fsanitize=address -fno-omit-frame-pointer
LDFLAGS += -fsanitize=address

TARGET := vm
CODEGEN := codegen
BUILD_DIR := _build
GEN_DIR := gen

SRCS := $(filter-out src/codegen.cpp, $(wildcard src/*.cpp))
HDRS := $(wildcard src/*.h) $(wildcard src/*.hpp)
OBJS := $(SRCS:src/%.cpp=$(BUILD_DIR)/%.o)

CODEGEN_SRCS := src/codegen.cpp $(filter-out src/main.cpp, $(SRCS))
CODEGEN_OBJS := $(BUILD_DIR)/codegen.o $(filter-out $(BUILD_DIR)/main.o, $(OBJS))

all: $(TARGET) $(GEN_DIR)/gen.pl

$(TARGET): $(OBJS)
	$(CXX) $(LDFLAGS) $(OBJS) -o $@

$(CODEGEN): $(CODEGEN_OBJS)
	$(CXX) $(LDFLAGS) $(CODEGEN_OBJS) -o $@

$(GEN_DIR)/gen.pl: $(CODEGEN) | $(GEN_DIR)
	./$(CODEGEN) > $@

$(BUILD_DIR)/%.o: src/%.cpp $(HDRS) | $(BUILD_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(BUILD_DIR):
	mkdir -p $@

$(GEN_DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR) $(GEN_DIR) $(TARGET) $(CODEGEN)

.PHONY: format
format:
	clang-format -i $(wildcard src/*.cpp) $(HDRS)

.PHONY: format-check
format-check:
	clang-format --dry-run --Werror $(wildcard src/*.cpp) $(HDRS)

.PHONY: test
test:
	swipl -g "['assembler/tests/assemble.plt'], ['assembler/tests/macro.plt'], run_tests." -t halt && \
	bats tests/integ.sh tests/stdlib.sh