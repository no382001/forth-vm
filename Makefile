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
BUILD_DIR := _build

SRCS := $(wildcard src/*.cpp)
HDRS := $(wildcard src/*.h) $(wildcard src/*.hpp)
OBJS := $(SRCS:src/%.cpp=$(BUILD_DIR)/%.o)

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(LDFLAGS) $(OBJS) -o $@

$(BUILD_DIR)/%.o: src/%.cpp $(HDRS) | $(BUILD_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(BUILD_DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR) $(TARGET)

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