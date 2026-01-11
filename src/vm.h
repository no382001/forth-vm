#pragma once

#include <array>
#include <cassert>
#include <cstdint>
#include <cstdio>
#include <string_view>

using cell_t = int32_t;
using ucell_t = uint32_t;
constexpr size_t CELL_SIZE = sizeof(cell_t);
constexpr size_t MEMORY_SIZE = 65536;
constexpr size_t STACK_SIZE = 256;

constexpr ucell_t DS_START = MEMORY_SIZE - (STACK_SIZE * CELL_SIZE * 2);
constexpr ucell_t RS_START = DS_START + (STACK_SIZE * CELL_SIZE);
constexpr ucell_t SP_ADDR = DS_START - CELL_SIZE * 3;
constexpr ucell_t RP_ADDR = DS_START - CELL_SIZE * 2;
constexpr ucell_t IP_ADDR = DS_START - CELL_SIZE * 1;

enum trap : uint8_t {
  TRAP_EMIT = 0, // ( char -- )
  TRAP_KEY = 1,  // ( -- char )
  TRAP_BYE = 2,  // ( -- ) exit
};

enum op : uint8_t {
  // core
  NOP,
  LIT,
  // memory
  LOAD,
  STORE,
  LOADB,
  STOREB,
  // stack
  DROP,
  DUP,
  SWAP,
  OVER,
  // rack
  TOR,
  FROMR,
  RFETCH,
  // alu
  ADD,
  SUB,
  AND,
  OR,
  XOR,
  EQ,
  LT,
  // control
  BRANCH,
  ZBRANCH,
  CALL,
  RET,
  EXECUTE,
  // system
  TRAP,
  OP_COUNT
};

struct vm;
using handler_fn = void (*)(vm &);

struct op_info {
  op code;
  std::string_view name;
  uint8_t in;
  uint8_t out;
  uint8_t rin;
  uint8_t rout;
  handler_fn fn;
};

extern const std::array<op_info, OP_COUNT> dispatch;

struct vm {
  std::array<uint8_t, MEMORY_SIZE> mem{};
  bool running{true};

  auto ip() -> ucell_t & { return *reinterpret_cast<ucell_t *>(&mem[IP_ADDR]); }
  auto sp() -> ucell_t & { return *reinterpret_cast<ucell_t *>(&mem[SP_ADDR]); }
  auto rp() -> ucell_t & { return *reinterpret_cast<ucell_t *>(&mem[RP_ADDR]); }
  auto ds(ucell_t i) -> cell_t & {
    return *reinterpret_cast<cell_t *>(&mem[DS_START + i * CELL_SIZE]);
  }
  auto rs(ucell_t i) -> cell_t & {
    return *reinterpret_cast<cell_t *>(&mem[RS_START + i * CELL_SIZE]);
  }
};

auto fetch_cell(vm &v) -> cell_t;
auto read_cell(vm &v, ucell_t a) -> cell_t;
auto write_cell(vm &v, ucell_t a, cell_t x) -> void;

auto step(vm &v) -> void;
auto run(vm &v) -> void;
auto init(vm &v) -> void;