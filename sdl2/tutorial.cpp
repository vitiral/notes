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

// Smart unique_ptr with a customized destructor (D)
template<typename T, void D(T*)>
class Res {
public:
  T* res;

  Res(T* ptr = nullptr) : res(ptr) {}
  ~Res() { destroy(); }

  void destroy() {
    if(not res) return;
    D(res);
    res = nullptr;
  }

  // no copy or copy assignment
  Res(const Res& a)            = delete;
  Res& operator=(const Res& a) = delete;

  // Move constructor
  Res& operator=(Res&& a) noexcept {
    if (&a == this) return *this;
    destroy(); // destroy our own resource
    res = a.res;
    a.res = nullptr;
    return *this;
  }

  T& operator*() const { return *res; }
  T* operator->() const { return res; }
  bool isNull() const { return res == nullptr; }
};

using RWindow  = Res<SDL_Window, SDL_DestroyWindow>;
using RSurface = Res<SDL_Surface, SDL_FreeSurface>;

class Game {
public:
  RWindow  window;
  RSurface screen;
  RSurface i_hello;
  RSurface i_X;
};


SDL_Window*  gWindow = nullptr;
SDL_Surface* gScreenSurface = nullptr; // The surface contained by the window
SDL_Surface* gHelloWorldImg = nullptr;
SDL_Surface* gXImg = nullptr;

bool gState = false;


// Initialize SDL
bool init(Game& g) {
  if( SDL_Init( SDL_INIT_VIDEO ) < 0 ) {
    printf( "SDL could not initialize! SDL_Error: %s\n", SDL_GetError() );
    return false;
  }

  gWindow = SDL_CreateWindow(
    "SDL Tutorial",
    SDL_WINDOWPOS_UNDEFINED,
    SDL_WINDOWPOS_UNDEFINED,
    SCREEN_WIDTH,
    SCREEN_HEIGHT,
    SDL_WINDOW_SHOWN);
  if(not gWindow) {
    printf( "Window could not be created! SDL_Error: %s\n", SDL_GetError() );
    return false;
  }
  // Get window surface
  gScreenSurface = SDL_GetWindowSurface(gWindow);

  // Fill the surface white
  SDL_FillRect(
    gScreenSurface,
    nullptr,
    SDL_MapRGB(gScreenSurface->format, 0xFF, 0xFF, 0xFF )
  );

  // Update the surface
  SDL_UpdateWindowSurface(gWindow);
  return true;
}

bool loadSurface(SDL_Surface*& toSurface, const std::string& path) {
  toSurface = SDL_LoadBMP(path.c_str());
  if(not toSurface) {
    printf("Unable to load %s! Error: %s\n", toSurface, SDL_GetError());
    return false;
  }
  return true;
}

bool loadMedia(Game& g) {
  return (
    loadSurface(gHelloWorldImg, "data/02_img.bmp")
    and loadSurface(gXImg, "data/x.bmp")
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
          SDL_Surface* img = gState ?  gHelloWorldImg : gXImg ;
          SDL_BlitSurface(img, nullptr, gScreenSurface, nullptr);
          SDL_UpdateWindowSurface(gWindow);
          gState = not gState;
          break;
        }
        case SDL_QUIT: quit = true; break;
      }
    }
  }
}

void close() {
  SDL_FreeSurface(gHelloWorldImg);
  gHelloWorldImg = nullptr;
  SDL_FreeSurface(gXImg);
  gXImg = nullptr;

  {
    RWindow w{gWindow};
  }

  SDL_Quit();
}


int main( int argc, char* args[] ) {
  Game g;
  if(init(g) and loadMedia(g)) {
    SDL_BlitSurface(gHelloWorldImg, nullptr, gScreenSurface, nullptr);
    SDL_UpdateWindowSurface(gWindow);
    eventLoop(g);
  }
  close();
  return 0;
}
