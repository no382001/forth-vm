#include "vm.h"
#include <fstream>
#include <iostream>
#include <span>
#include <string_view>
#include <vector>

struct config {
  std::string_view filename;
  uint16_t load_addr = 0;
  uint16_t run_addr = 0;
  bool debug = false;
  bool step = false;
};

auto load_from_file(vm& v, std::string_view path, uint16_t addr = 0) -> size_t {
  auto file = std::ifstream{std::string{path}, std::ios::binary};
  if (!file) throw std::runtime_error{"failed to open: " + std::string{path}};
  
  auto bytes = std::vector<uint8_t>{
    std::istreambuf_iterator<char>{file},
    std::istreambuf_iterator<char>{}
  };
  
  std::copy(bytes.begin(), bytes.end(), v.mem.begin() + addr);
  return bytes.size();
}

auto load_from_stdin(vm& v, uint16_t addr = 0) -> size_t {
  auto bytes = std::vector<uint8_t>{
    std::istreambuf_iterator<char>{std::cin},
    std::istreambuf_iterator<char>{}
  };
  
  std::copy(bytes.begin(), bytes.end(), v.mem.begin() + addr);
  return bytes.size();
}

auto main(int argc, char** argv) -> int {
  auto v = vm{};
  init(v);
  
  auto args = std::span{argv, static_cast<size_t>(argc)};
  
  if (args.size() > 1) {
    load_from_file(v, args[1]);
  } else {
    load_from_stdin(v);
  }
  
  run(v);
}