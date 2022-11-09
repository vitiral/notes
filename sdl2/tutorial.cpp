/*This source code copyrighted by Lazy Foo' Productions (2004-2022)
and may not be redistributed without written permission.*/

//Using SDL and standard IO
#include <SDL2/SDL.h>
#include <iostream>
#include <cstdio>
#include <cassert>

#include "libs/evt2str/sdl_event_to_string.h"

using namespace std;

//Screen dimension constants
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

// no copy or copy assignment
#define NO_COPY(C) \
  C(const C& a)            = delete; \
  C& operator=(const C& a) = delete;

// Resource
// Basically a unique_ptr with customized destructor <D>
template<typename T, void D(T*)>
class Res {
  NO_COPY(Res);
  T* res;
public:

  Res(T* ptr = nullptr) : res(ptr) {}
  ~Res() { destroy(); }

  // Get the resource, it's your job to manage it.
  T* unwrap() noexcept {
    T* out = res;
    res = nullptr;
    return out;
  }

  void destroy() noexcept {
    if(not res) return;
    cout << "Destroying a resource...\n";
    D(unwrap());
    cout << "  Destroyed a resource\n";
  }


  // Move constructor
  Res& operator=(Res&& a) noexcept {
    if (&a == this) return *this;
    destroy(); // destroy our own resource
    res = a.unwrap();
    return *this;
  }

  // Direct initialization from a pointer.
  Res& operator=(T* res) noexcept {
    if(not res) { destroy(); return *this; }
    assert(not this->res);
    this->res = res;
  }

  T& operator*() const  { return *res; }
  T* operator->() const { return res; }
  bool isNull() const { return res == nullptr; }

};

using RWindow  = Res<SDL_Window, SDL_DestroyWindow>;
using RSurface = Res<SDL_Surface, SDL_FreeSurface>;


// Used so it is called last in game state
class Quitter {
public:
  NO_COPY(Quitter);
 ~Quitter() { SDL_Quit(); }
};

class Game {
  NO_COPY(Game);
  Quitter  quitter{};

public:
  Game() = default;

  bool state{false};
  RWindow  window{};
  RSurface screen{};
  RSurface i_hello{};
  RSurface i_X{};
};

// Initialize SDL
bool init(Game& g) {
  if( SDL_Init( SDL_INIT_VIDEO ) < 0 ) {
    printf( "SDL could not initialize! SDL_Error: %s\n", SDL_GetError() );
    return false;
  }

  g.window = SDL_CreateWindow(
    "SDL Tutorial",
    SDL_WINDOWPOS_UNDEFINED,
    SDL_WINDOWPOS_UNDEFINED,
    SCREEN_WIDTH,
    SCREEN_HEIGHT,
    SDL_WINDOW_SHOWN);
  if(g.window.isNull()) {
    printf( "Window could not be created! SDL_Error: %s\n", SDL_GetError() );
    return false;
  }
  // Get window surface
  g.screen = SDL_GetWindowSurface(&*g.window);

  // Fill the surface white
  SDL_FillRect(
    &*g.screen,
    nullptr,
    SDL_MapRGB(g.screen->format, 0xFF, 0xFF, 0xFF )
  );

  // Update the surface
  SDL_UpdateWindowSurface(&*g.window);
  return true;
}

bool loadSurface(RSurface& toSurface, const std::string& path) {
  toSurface = SDL_LoadBMP(path.c_str());
  if(toSurface.isNull()) {
    cout << "Unable to load " << path << "! Error: " << SDL_GetError() << '\n';
    return false;
  }
  return true;
}

bool loadMedia(Game& g) {
  return (
    loadSurface(g.i_hello, "data/02_img.bmp")
    and loadSurface(g.i_X, "data/x.bmp")
  );
}

void eventLoop(Game& g) {
  SDL_Event e;
  bool quit = false;
  while( quit == false ){
    while(SDL_PollEvent(&e)) {
      cout << "Event: " << sdlEventToString(e) << '\n';
      switch (e.type) {
        case SDL_MOUSEBUTTONDOWN: {
          SDL_Surface* img = g.state ? &*g.i_hello : &*g.i_X ;
          SDL_BlitSurface(img, nullptr, &*g.screen, nullptr);
          SDL_UpdateWindowSurface(&*g.window);
          g.state = not g.state;
          break;
        }
        case SDL_QUIT: quit = true; break;
      }
    }
  }
}


int main( int argc, char* args[] ) {
  Game g;
  if(init(g) and loadMedia(g)) {
    SDL_BlitSurface(&*g.i_hello, nullptr, &*g.screen, nullptr);
    SDL_UpdateWindowSurface(&*g.window);
    eventLoop(g);
  }
  return 0;
}
