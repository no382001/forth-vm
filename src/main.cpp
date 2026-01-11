#include "vm.h"

auto main() -> int {
  auto v = vm{};
  init(v);

  v.mem[0] = LIT;
  write_cell(v, 1, 'A');
  v.mem[5] = TRAP;
  write_cell(v, 6, TRAP_EMIT);
  v.mem[10] = TRAP;
  write_cell(v, 11, TRAP_BYE);

  run(v);

  return 0;
}