#include "chip8_ext.h"
#include "display.h"
#include "sound.h"
#include <cassert>

inline auto push(vm &v, cell_t x) -> void { v.ds(v.sp()++) = x; }
inline auto pop(vm &v) -> cell_t { return v.ds(--v.sp()); }
inline auto top(vm &v) -> cell_t & { return v.ds(v.sp() - 1); }

void chip8_trap_handler(vm &v, uint8_t code) {
  switch (code) {
  case TRAP_FB_REFRESH: {
    auto addr = static_cast<ucell_t>(pop(v));
    push(v, display_refresh(&v.mem[addr], 64, 32) ? -1 : 0);
    break;
  }
  case TRAP_KEY_IS_DOWN:
    top(v) = display_key_is_down(top(v)) ? -1 : 0;
    break;
  case TRAP_MILLIS:
    push(v, static_cast<cell_t>(display_millis()));
    break;
  case TRAP_SOUND:
    sound_beep(pop(v) != 0);
    break;
  default:
    assert(false && "unknown chip8 trap");
    break;
  }
}
