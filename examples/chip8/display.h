#pragma once
#include <cstdint>

// Blit a 1bpp framebuffer (w*h bytes, 0=off / nonzero=on) to the window.
// Opens the window on first call. Returns true if the window is still open.
bool display_refresh(uint8_t *fb, int w, int h);

// Returns true if CHIP-8 key (0x0-0xF) is currently held.
// Key map:  1 2 3 4 -> 1 2 3 C
//           Q W E R -> 4 5 6 D
//           A S D F -> 7 8 9 E
//           Z X C V -> A 0 B F
bool display_key_is_down(int chip8_key);

// Milliseconds since program start. Wraps at ~32s due to int16_t cell size — use deltas only.
int32_t display_millis();
