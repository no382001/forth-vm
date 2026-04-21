#include "sound.h"
#include <SDL2/SDL.h>
#include <atomic>
#include <cstdint>

static std::atomic<bool> g_beeping{false};
static SDL_AudioDeviceID g_dev = 0;

static void audio_callback(void *, uint8_t *stream, int len) {
  static int phase = 0;
  const int period = 44100 / 440; // samples per cycle at 440Hz
  int16_t *out = reinterpret_cast<int16_t *>(stream);
  int samples = len / sizeof(int16_t);

  if (!g_beeping.load(std::memory_order_relaxed)) {
    SDL_memset(stream, 0, len);
    phase = 0;
    return;
  }

  for (int i = 0; i < samples; i++) {
    out[i] = (phase < period / 2) ? 8000 : -8000;
    if (++phase >= period)
      phase = 0;
  }
}

void sound_beep(bool on) {
  if (!g_dev) {
    SDL_InitSubSystem(SDL_INIT_AUDIO);
    SDL_AudioSpec want{};
    want.freq = 44100;
    want.format = AUDIO_S16SYS;
    want.channels = 1;
    want.samples = 512;
    want.callback = audio_callback;
    g_dev = SDL_OpenAudioDevice(nullptr, 0, &want, nullptr, 0);
    if (g_dev)
      SDL_PauseAudioDevice(g_dev, 0);
  }
  g_beeping.store(on, std::memory_order_relaxed);
}
