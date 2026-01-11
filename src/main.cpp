#include "vm.h"
#include <fstream>
#include <getopt.h>
#include <iostream>
#include <span>
#include <string_view>
#include <vector>

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
  
  if (!freopen("/dev/tty", "r", stdin)) {
    std::cerr << "warning: could not reopen stdin from /dev/tty\n";
  }

  return bytes.size();
}

auto usage(const char* prog) -> void {
  std::cerr << "usage: " << prog << " [-t|--trace] [file]\n";
  std::cerr << "\n";
  std::cerr << "options:\n";
  std::cerr << "  -t, --trace   trace execution\n";
  std::cerr << "  -h, --help    show this help\n";
}

auto main(int argc, char** argv) -> int {
  
  static struct option long_options[] = {
    {"trace", no_argument, nullptr, 't'},
    {"help",  no_argument, nullptr, 'h'},
    {nullptr, 0, nullptr, 0}
  };
  
  auto v = vm{};
  int opt;
  while ((opt = getopt_long(argc, argv, "th", long_options, nullptr)) != -1) {
    switch (opt) {
      case 't':
        v.debug = true;
        break;
      case 'h':
        usage(argv[0]);
        return 0;
      default:
        usage(argv[0]);
        return 1;
    }
  }
  
  init(v);
  
  if (optind < argc) {
    load_from_file(v, argv[optind]);
  } else {
    load_from_stdin(v);
  }
  
  run(v);
}