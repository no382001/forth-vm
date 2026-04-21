#pragma once
#include "../../src/vm.h"

enum chip8_trap : uint8_t {
  TRAP_FB_REFRESH  = 4, // ( fb_addr -- bool )
  TRAP_KEY_IS_DOWN = 5, // ( chip8_key -- bool )
  TRAP_MILLIS      = 6, // ( -- int )
  TRAP_SOUND       = 7, // ( bool -- )
};

void chip8_trap_handler(vm &v, uint8_t code);
