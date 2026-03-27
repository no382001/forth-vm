#include "vm.h"
#include <cstdio>

auto main() -> int {
  std::printf("%% auto-generated from dispatch table\n");
  std::printf(":- module(gen, [op/6, cell_size/1, trap_type/4]).\n\n");

  // opcodes
  std::printf("%% op(Name, Opcode, StackIn, StackOut, RStackIn, RStackOut).\n");
  for (const auto &info : dispatch) {
    std::printf("op('%s', %d, %d, %d, %d, %d).\n", info.name.data(), info.code,
                info.in, info.out, info.rin, info.rout);
  }

  // catch-all for numeric literals
  std::printf("op(X, X, 0, 0, 0, 0).\n\n");

  // cell size
  std::printf("%% cell_size(Size).\n");
  std::printf("cell_size(%zu).\n\n", CELL_SIZE);

  // trap types
  std::printf("%% trap_type(Name, Code, ParamTypes, ReturnType).\n");
  for (const auto &t : traps) {
    std::printf("trap_type('%s', %d, [", t.name.data(), t.code);
    for (uint8_t i = 0; i < t.arity; ++i) {
      if (i > 0)
        std::printf(",");
      std::printf("%s", vtype_str(t.params[i]).data());
    }
    std::printf("], %s).\n", vtype_str(t.ret).data());
  }

  return 0;
}
