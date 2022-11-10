/*
 * Following tutorial from https://lazyfoo.net/tutorials/SDL
 *
 * I also just finished reading https://www.learncpp.com/, but didn't do many of
 * the "assignments".  Please note, I am COMPLETELY new to C++ although I have a
 * lot of experience with other languages which drew from or inspired various
 * C++ features (C, Java and Rust to name the main ones). So this code might
 * seem rather odd in some places -- I'm still learning!
 *
 * Since the code from the SDL tutorial is VERY messy, it provides a perfect
 * opportunity to hone my skills.  I'm applying C++ idioms including wrapper
 * classes for the C resource (i.e.  `Resource` class) and just common
 * programming best practices like no globals, etc.
 *
 * This file will grow to become a "single file" project. I don't plan on
 * splitting it up very much to make development easier. When I start making an
 * actual game I will do that kind of cleanup.
 *
 * ## Notes
 * https://lazyfoo.net/tutorials/SDL/index.php
 * - Current place Lesson 05
 *
 */

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <iostream>
#include <cstdio>
#include <cassert>
#include <compare>
#include <set>
#include <vector>
#include <array>

// There is no good debugging of events in SDL2, so we import this library.
#include "libs/evt2str/sdl_event_to_string.h"
#include "resource.h" // Resource and DEFER for wrapping C resources.

using namespace std;
using TimeMs = Uint32;

// *****************
// * SDL wrapper types and helper functions
using RWindow  = Resource<SDL_Window, SDL_DestroyWindow>;
using RRenderer = Resource<SDL_Renderer, SDL_DestroyRenderer>;
using RSurface = Resource<SDL_Surface, SDL_FreeSurface>;
using RTexture = Resource<SDL_Texture, SDL_DestroyTexture>;

// *****************
// * Game Objects

//Screen dimension constants
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;
const TimeMs FRAME_LENGTH = 33; // 33ms, 30 fps

struct Loc;

struct Size {
  int w{}, h{};

  Size operator/ (const int r) const { return Size { w / r, h / r }; }
  bool operator==(Size r)  const { return w == r.w and w == r.w; }
};

class Timer {
public:
  string_view  m_name;
  TimeMs m_start;
  Timer(string_view name) : m_name{name} {
    m_start = SDL_GetTicks();
  }

  ~Timer() {
    TimeMs end = SDL_GetTicks();
    cout << "Timer " << m_name << " total=" << (end - m_start) << "ms\n";
  }
};

class Display {
  NO_COPY(Display);

public:
  Display() = default;

  Size        sz{SCREEN_WIDTH, SCREEN_HEIGHT};
  RWindow     window{};
  // RSurface    screen{};
  RRenderer   rend{};

  RTexture    i_hello{};
  RTexture    i_xOut{};
  RTexture    i_png{};

  bool init();
  RSurface optimize(RSurface& s, const std::string& path);
  RTexture loadTexture(const std::string& path);
  bool loadMedia();
};

struct Color {
  Uint8 r{}; Uint8 g{}; Uint8 b{}; Uint8 a{};
  Color(Uint8 r=0, Uint8 g=0, Uint8 b=0, Uint8 a=SDL_ALPHA_OPAQUE)
    : r{r}, g{g}, b{b}, a{a}
  {}

  void render(RRenderer& rend) {
    SDL_SetRenderDrawColor(&*rend, r, g, b, a);
  }
};

struct Loc {
  int x{}, y{};

  bool operator==(Loc r)  const { return x == r.x and y == r.y; }
  Loc operator- ()         const { return Loc{-x, -y};           }
  Loc operator+ (Loc r)   const { return Loc{x+r.x, y+r.y};     }
  Loc operator- (Loc r)   const { return Loc{x-r.x, y-r.y};     }
};

class Game;

class Entity {
public:
  Color     color{0xFF};
  Size      sz{100, 50};
  Loc       loc{0, 0};

  void render(Display& d, Game& g) {
    color.render(d.rend);
    SDL_Rect r = sdlRect(d, g);
    SDL_RenderFillRect(&*d.rend, &r);
  }

  // Get the SDL Rectangle for the entity
  SDL_Rect sdlRect(Display& d, Game& g);
};

// Helps record the total time a button was pressed.
class ButtonState {
public:
  ButtonState(TimeMs now): m_lastAction{now} {}

  bool   m_isPressed{};  // is currently pressed
  TimeMs m_lastAction;   // Start of press/unpress
  TimeMs m_pressed{0};   // total duration pressed
  TimeMs m_unpressed{0}; // total duration unpressed


  void event(TimeMs now, bool pressed) {
    if(pressed) press(now);
    else        unpress(now);
  }

  void press(TimeMs now) {
    m_unpressed += now - m_lastAction;
    m_isPressed = true;
    m_lastAction = now;
  }

  void unpress(TimeMs now) {
    if(m_isPressed) {
      m_pressed += now - m_lastAction;
    }
    m_lastAction = now;
    m_isPressed = false;
  }
};

class Controller {
public:
  Controller(TimeMs n)
    :         w{n},  a{n},    s{n},    d{n},
              up{n}, left{n}, down{n}, right{n},
              mLeft{n},       mRight{n}
  {}

  ButtonState w,     a,       s,       d;
  ButtonState up,    left,    down,    right;
  ButtonState mLeft,          mRight;
  Loc mouse{};
};

const int MAX_EVENTS = 256;

// Game State
class Game {
public:
  Controller controller;
  TimeMs frame{0};  // ticks at start of frame
  TimeMs loop {0};
  array<SDL_Event, 256> events;
  int numEvents;

  Loc       center{0, 0};

  bool quit{};
  bool ctrl{};

  Entity e1{};
  Entity e2{.loc{-100, -100}};

  Game(TimeMs now) : controller{now} {}
};

SDL_Rect Entity::sdlRect(Display& d, Game& g) {
  Loc rel = loc - g.center; // relative location to center
  rel.y = -rel.y;           // reverse so that +y goes down (SDL coordinates)
  rel = rel - Loc{sz.w / 2, sz.h / 2};     // get "top-left" position
  rel = rel + Loc{d.sz.w / 2, d.sz.h / 2}; // Update coordinates for SDL_Rect
  return (SDL_Rect) {rel.x, rel.y, sz.w, sz.h};
};

// Initialize SDL
bool Display::init() {
  // m_init = SDL_Init(SDL_INIT_VIDEO) >= 0;

  window = SDL_CreateWindow(
    "SDL Tutorial",
    SDL_WINDOWPOS_UNDEFINED,
    SDL_WINDOWPOS_UNDEFINED,
    SCREEN_WIDTH,
    SCREEN_HEIGHT,
    SDL_WINDOW_SHOWN);

  if(window.isNull()) {
    printf("Window could not be created! Error: %s\n", SDL_GetError());
    return false;
  }

  rend = SDL_CreateRenderer(&*window, -1, SDL_RENDERER_ACCELERATED);
  if(rend.isNull()) {
    printf("Renderer could not be created! Error: %s\n", SDL_GetError());
    return false;
  }

  SDL_SetRenderDrawColor(&*rend, 0xFF, 0xFF, 0xFF, 0xFF);
  return true;
}

RTexture Display::loadTexture(const std::string& path) {
  RTexture out{IMG_LoadTexture(&*rend, path.c_str())};
  if(out.isNull()) {
    cout << "Unable to load " << path << "! Error: " << IMG_GetError() << '\n';
  }
  return out;
}

bool Display::loadMedia() {
  return not (
    (i_hello = loadTexture("data/02_img.bmp")).isNull()
    or (i_xOut  = loadTexture("data/x.bmp")).isNull()
    or (i_png   = loadTexture("data/png_loaded.png")).isNull()
  );
}

void mouseEvent(Game& g, SDL_MouseButtonEvent& e, TimeMs now, bool pressed) {
  Controller& c = g.controller;
  ButtonState* b = nullptr;
  switch (static_cast<int>(e.button)) {
    case SDL_BUTTON_LEFT:  b = &c.mLeft; break;
    case SDL_BUTTON_RIGHT: b = &c.mRight; break;
    default: return;
  }
  b->event(now, pressed);
}

void keyEvent(Game& g, SDL_KeyboardEvent& e, TimeMs now, bool pressed) {
  if(e.repeat) return;
  cout << "Keydown: " << sdlEventToString(SDL_Event{.key = e}) << '\n';
  Controller& c = g.controller;
  ButtonState* b = nullptr;
  switch(e.keysym.sym) {
    case SDLK_UP:    g.e1.loc.y += 5; return;
    case SDLK_DOWN:  g.e1.loc.y -= 5; return;
    case SDLK_LEFT:  g.e1.loc.x -= 5; return;
    case SDLK_RIGHT: g.e1.loc.x += 5; return;

    case SDLK_a:     b = &c.a; break;
    case SDLK_s:     b = &c.s; break;
    case SDLK_d:     b = &c.d; break;
    case SDLK_w:     b = &c.w; break;

    case SDLK_LCTRL:
    case SDLK_RCTRL: g.ctrl = pressed; return;
    case SDLK_c:
      if(g.ctrl) { cout << "Got Cntrl+C\n"; g.quit = true; }
      return;
    default: cout << "  ... Hit default\n";
  }
  b->event(now, pressed);
}

void consumeEvents(Game& g, TimeMs now) {
  for(int i = 0; i < g.numEvents; ++i) {
    SDL_Event& e = g.events[i];
    cout << "e1 x=" << g.e1.loc.x << " y=" << g.e1.loc.y << '\n';
    switch (e.type) {
      case SDL_MOUSEBUTTONDOWN: mouseEvent(g, e.button, now, true); break;
      case SDL_MOUSEBUTTONUP:   mouseEvent(g, e.button, now, false); break;
      case SDL_KEYDOWN: keyEvent(g, e.key, now, true); break;
      case SDL_KEYUP:   keyEvent(g, e.key, now, false); break;
      case SDL_QUIT:
        cout << "Got SDL_QUIT\n";
        g.quit = true;
        break;
    }
  }

  g.numEvents = 0;
}
// *****************
// * Event Loop


void paintScreen(Display& d, Game& g) {
  SDL_RenderClear(&*d.rend);
  SDL_RenderCopy(&*d.rend, &*d.i_png, NULL, NULL);
  g.e1.render(d, g);
  g.e2.render(d, g);
  SDL_RenderPresent(&*d.rend);
}

void frameDelay(Game& g) {
  TimeMs now = SDL_GetTicks();
  TimeMs end = g.frame + FRAME_LENGTH;
  if(end > now) {
    SDL_Delay(end - now);
  }
  g.frame = SDL_GetTicks();
}


void eventLoop(Display& d, Game& g) {
  g.frame = SDL_GetTicks();

  // Demonstrate stretching/shrinking an image
  SDL_Rect stretchRect {
    .x = 0, .y = 0,
    .w = SCREEN_WIDTH / 2, .h = SCREEN_HEIGHT / 2
  };

  SDL_Event e;
  while(not g.quit){
    cout << "WHILE LOOP " << g.loop << '\n';
    assert(0 == g.numEvents);
    while(SDL_PollEvent(&g.events[g.numEvents])) {
      e = g.events[g.numEvents];
      g.numEvents += 1;
      if(g.numEvents >= MAX_EVENTS) {
        break;
      }
    }
    consumeEvents(g, SDL_GetTicks());
    paintScreen(d, g);
    frameDelay(g);
    g.loop += 1;
  }
}

// *****************
// * Game
int game() {
  if(SDL_Init(SDL_INIT_VIDEO) < 0) {
    printf("SDL could not initialize! SDL_Error: %s\n", SDL_GetError());
    return 1;
  }
  DEFER( SDL_Quit(); cout << "SDL_Quit Successfully\n" );

  int imgFlags = IMG_INIT_PNG;
  if(not (IMG_Init(imgFlags) & imgFlags)) {
    printf("Could not initialize! Error: %s\n", IMG_GetError());
    return 1;
  }
  DEFER( IMG_Quit() );

  Display d{};
  d.init();

  if(not d.loadMedia()) {
    return 1;
  }

  Game g{SDL_GetTicks()};

  eventLoop(d, g);
  return 0;
}


int main(int argc, char* args[]) {
  int err = game();
  return err;
}
