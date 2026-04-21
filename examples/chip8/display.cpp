#include "display.h"
#include <SDL2/SDL.h>
#include <cstdint>

static SDL_Window *g_win = nullptr;
static SDL_Renderer *g_ren = nullptr;
static SDL_Texture *g_tex = nullptr;
static uint32_t g_argb[64 * 32];
static uint64_t g_last_frame = 0;

// CHIP-8 key index (0x0-0xF) -> SDL scancode
static const SDL_Scancode chip8_to_sdl[16] = {
    SDL_SCANCODE_X, // 0x0
    SDL_SCANCODE_1, // 0x1
    SDL_SCANCODE_2, // 0x2
    SDL_SCANCODE_3, // 0x3
    SDL_SCANCODE_Q, // 0x4
    SDL_SCANCODE_W, // 0x5
    SDL_SCANCODE_E, // 0x6
    SDL_SCANCODE_A, // 0x7
    SDL_SCANCODE_S, // 0x8
    SDL_SCANCODE_D, // 0x9
    SDL_SCANCODE_Z, // 0xA
    SDL_SCANCODE_C, // 0xB
    SDL_SCANCODE_4, // 0xC
    SDL_SCANCODE_R, // 0xD
    SDL_SCANCODE_F, // 0xE
    SDL_SCANCODE_V, // 0xF
};

static bool ensure_init() {
  if (g_win)
    return true;
  if (SDL_Init(SDL_INIT_VIDEO) < 0)
    return false;
  g_win = SDL_CreateWindow("chip8", SDL_WINDOWPOS_CENTERED,
                           SDL_WINDOWPOS_CENTERED, 640, 320, 0);
  if (!g_win)
    return false;
  g_ren = SDL_CreateRenderer(g_win, -1, SDL_RENDERER_ACCELERATED);
  if (!g_ren)
    return false;
  g_tex = SDL_CreateTexture(g_ren, SDL_PIXELFORMAT_ARGB8888,
                            SDL_TEXTUREACCESS_STREAMING, 64, 32);
  return g_tex != nullptr;
}

bool display_refresh(uint8_t *fb, int w, int h) {
  if (!ensure_init())
    return false;

  // pace to 60fps
  uint64_t now = SDL_GetTicks64();
  uint64_t elapsed = now - g_last_frame;
  if (elapsed < 16)
    SDL_Delay((uint32_t)(16 - elapsed));
  g_last_frame = SDL_GetTicks64();

  for (int i = 0; i < w * h; i++)
    g_argb[i] = fb[i] ? 0xFFFFFFFF : 0xFF000000;

  SDL_UpdateTexture(g_tex, nullptr, g_argb, w * sizeof(uint32_t));
  SDL_RenderClear(g_ren);
  SDL_RenderCopy(g_ren, g_tex, nullptr, nullptr);
  SDL_RenderPresent(g_ren);

  SDL_Event e;
  while (SDL_PollEvent(&e))
    if (e.type == SDL_QUIT)
      return false;

  return true;
}

bool display_key_is_down(int chip8_key) {
  if (!g_win || chip8_key < 0 || chip8_key > 0xF)
    return false;
  const uint8_t *keys = SDL_GetKeyboardState(nullptr);
  return keys[chip8_to_sdl[chip8_key]];
}

int32_t display_millis() { return (int32_t)SDL_GetTicks(); }
