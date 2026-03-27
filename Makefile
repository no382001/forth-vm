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

CODEGEN_OBJS := $(BUILD_DIR)/codegen.o $(BUILD_DIR)/vm.o

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
test: quad

.PHONY: quad
quad:
	@cd compiler && for mod in parser typecheck codegen; do \
		scryer-prolog -f -g "use_module(library('numerics/quadtests')), check_module_quads($$mod, _), halt." < /dev/null; \
	done

# Compile .lisp programs to .bin
programs/%.bin: programs/%.lisp $(GEN_DIR)/gen.pl
	@cd compiler && scryer-prolog -f -g "use_module(compiler), compile_file('../$<', '../$@'), halt." < /dev/null