#include "vm.h"
#include <iostream>
#include <string>

auto escape_prolog(std::string_view s) -> std::string {
  std::string out;
  for (char c : s) {
    switch (c) {
    case '\'':
      out += "\\'";
      break;
    case '\\':
      out += "\\\\";
      break;
    default:
      out += c;
    }
  }
  return out;
}

auto main() -> int {
  std::cout << "% auto-generated from dispatch table\n";
  std::cout << ":- module(gen, [op/6, cell_size/1]).\n\n";
  std::cout << "% op(Name, Opcode, StackIn, StackOut, RStackIn, RStackOut).\n";

  for (const auto &info : dispatch) {
    std::cout << "op('" << escape_prolog(info.name) << "', "
              << static_cast<int>(info.code) << ", "
              << static_cast<int>(info.in) << ", " << static_cast<int>(info.out)
              << ", " << static_cast<int>(info.rin) << ", "
              << static_cast<int>(info.rout) << ").\n";
  }

  std::cout << "op(X, X, 0, 0, 0, 0).\n\n";

  std::cout << "% cell_size(Size).\n";
  std::cout << "cell_size(" << CELL_SIZE << ").\n";

  return 0;
}