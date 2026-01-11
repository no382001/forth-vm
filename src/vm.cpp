#include "vm.h"
#include <array>
#include <cassert>
#include <cstdint>
#include <string_view>

inline auto push(vm &v, cell_t x) -> void { v.ds(v.sp()++) = x; }

inline auto pop(vm &v) -> cell_t { return v.ds(--v.sp()); }

inline auto top(vm &v) -> cell_t & { return v.ds(v.sp() - 1); }

inline auto rpush(vm &v, cell_t x) -> void { v.rs(v.rp()++) = x; }

inline auto rpop(vm &v) -> cell_t { return v.rs(--v.rp()); }

auto fetch_cell(vm &v) -> cell_t {
  auto x = *reinterpret_cast<cell_t *>(&v.mem[v.ip()]);
  v.ip() += CELL_SIZE;
  return x;
}

auto read_cell(vm &v, ucell_t a) -> cell_t {
  return *reinterpret_cast<cell_t *>(&v.mem[a]);
}

auto write_cell(vm &v, ucell_t a, cell_t x) -> void {
  *reinterpret_cast<cell_t *>(&v.mem[a]) = x;
}

inline auto h_nop(vm &) -> void {}

inline auto h_lit(vm &v) -> void { push(v, fetch_cell(v)); }

inline auto h_load(vm &v) -> void {
  top(v) = read_cell(v, static_cast<ucell_t>(top(v)));
}

inline auto h_store(vm &v) -> void {
  auto a = pop(v);
  write_cell(v, static_cast<ucell_t>(a), pop(v));
}

inline auto h_loadb(vm &v) -> void {
  top(v) = v.mem[static_cast<ucell_t>(top(v))];
}

inline auto h_storeb(vm &v) -> void {
  auto a = pop(v);
  v.mem[static_cast<ucell_t>(a)] = static_cast<uint8_t>(pop(v));
}

inline auto h_drop(vm &v) -> void { pop(v); }

inline auto h_dup(vm &v) -> void { push(v, top(v)); }

inline auto h_swap(vm &v) -> void {
  auto a = pop(v);
  auto b = pop(v);
  push(v, a);
  push(v, b);
}

inline auto h_over(vm &v) -> void { push(v, v.ds(v.sp() - 2)); }

inline auto h_tor(vm &v) -> void { rpush(v, pop(v)); }

inline auto h_fromr(vm &v) -> void { push(v, rpop(v)); }

inline auto h_rfetch(vm &v) -> void { push(v, v.rs(v.rp() - 1)); }

inline auto h_add(vm &v) -> void {
  auto b = pop(v);
  top(v) += b;
}

inline auto h_sub(vm &v) -> void {
  auto b = pop(v);
  top(v) -= b;
}

inline auto h_and(vm &v) -> void {
  auto b = pop(v);
  top(v) &= b;
}

inline auto h_or(vm &v) -> void {
  auto b = pop(v);
  top(v) |= b;
}

inline auto h_xor(vm &v) -> void {
  auto b = pop(v);
  top(v) ^= b;
}

inline auto h_eq(vm &v) -> void {
  auto b = pop(v);
  top(v) = (top(v) == b) ? -1 : 0;
}

inline auto h_lt(vm &v) -> void {
  auto b = pop(v);
  top(v) = (top(v) < b) ? -1 : 0;
}

inline auto h_branch(vm &v) -> void {
  v.ip() = static_cast<ucell_t>(fetch_cell(v));
}

inline auto h_zbranch(vm &v) -> void {
  auto a = fetch_cell(v);
  if (pop(v) == 0)
    v.ip() = static_cast<ucell_t>(a);
}

inline auto h_call(vm &v) -> void {
  auto a = fetch_cell(v);
  rpush(v, static_cast<cell_t>(v.ip()));
  v.ip() = static_cast<ucell_t>(a);
}

inline auto h_ret(vm &v) -> void { v.ip() = static_cast<ucell_t>(rpop(v)); }

inline auto h_execute(vm &v) -> void {
  rpush(v, static_cast<cell_t>(v.ip()));
  v.ip() = static_cast<ucell_t>(pop(v));
}

inline auto h_trap(vm &v) -> void {
  auto n = fetch_cell(v);
  switch (n) {
  case TRAP_EMIT:
    std::putchar(static_cast<char>(pop(v)));
    break;
  case TRAP_KEY:
    push(v, std::getchar());
    break;
  case TRAP_BYE:
    v.running = false;
    break;
  default:
    assert(false && "unknown trap");
    break;
  }
}

const std::array<op_info, OP_COUNT> dispatch = {{
    {NOP, "nop", 0, 0, 0, 0, h_nop},
    {LIT, "lit", 0, 1, 0, 0, h_lit},
    {LOAD, "@", 1, 1, 0, 0, h_load},
    {STORE, "!", 2, 0, 0, 0, h_store},
    {LOADB, "c@", 1, 1, 0, 0, h_loadb},
    {STOREB, "c!", 2, 0, 0, 0, h_storeb},
    {DROP, "drop", 1, 0, 0, 0, h_drop},
    {DUP, "dup", 1, 2, 0, 0, h_dup},
    {SWAP, "swap", 2, 2, 0, 0, h_swap},
    {OVER, "over", 2, 3, 0, 0, h_over},
    {TOR, ">r", 1, 0, 0, 1, h_tor},
    {FROMR, "r>", 0, 1, 1, 0, h_fromr},
    {RFETCH, "r@", 0, 1, 1, 1, h_rfetch},
    {ADD, "+", 2, 1, 0, 0, h_add},
    {SUB, "-", 2, 1, 0, 0, h_sub},
    {AND, "and", 2, 1, 0, 0, h_and},
    {OR, "or", 2, 1, 0, 0, h_or},
    {XOR, "xor", 2, 1, 0, 0, h_xor},
    {EQ, "=", 2, 1, 0, 0, h_eq},
    {LT, "<", 2, 1, 0, 0, h_lt},
    {BRANCH, "branch", 0, 0, 0, 0, h_branch},
    {ZBRANCH, "0branch", 1, 0, 0, 0, h_zbranch},
    {CALL, "call", 0, 0, 0, 1, h_call},
    {RET, "ret", 0, 0, 1, 0, h_ret},
    {EXECUTE, "execute", 1, 0, 0, 1, h_execute},
    {TRAP, "trap", 0, 0, 0, 0, h_trap}, // stack effect varies by trap
}};

auto step(vm &v) -> void {
  if (v.ip() >= MEMORY_SIZE) {
    std::fprintf(stderr, "error: ip out of bounds (ip=%u, max=%zu)\n", 
                 v.ip(), MEMORY_SIZE - 1);
    v.running = false;
    return;
  }

  auto prev_ip = v.ip();
  auto opcode = static_cast<uint8_t>(fetch_cell(v));

  if (opcode >= OP_COUNT) {
    std::fprintf(stderr, "error: invalid opcode %d at ip=%u\n", opcode, prev_ip);
    v.running = false;
    return;
  }

  const auto &info = dispatch[opcode];

  if (v.sp() < info.in) {
    std::fprintf(stderr, "error: stack underflow at ip=%u (%s needs %d, sp=%u)\n",
                 prev_ip, info.name.data(), info.in, v.sp());
    v.running = false;
    return;
  }

  if (v.rp() < info.rin) {
    std::fprintf(stderr, "error: return stack underflow at ip=%u (%s needs %d, rp=%u)\n",
                 prev_ip, info.name.data(), info.rin, v.rp());
    v.running = false;
    return;
  }

  info.fn(v);
}

auto run(vm &v) -> void {
  while (v.running) {
    step(v);
  }
}

auto init(vm &v) -> void {
  v.ip() = 0;
  v.sp() = 0;
  v.rp() = 0;
  v.running = true;
}