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
#include <iostream>
#include <cstdio>
#include <cassert>

// There is no good debugging of events in SDL2, so we import this library.
#include "libs/evt2str/sdl_event_to_string.h"
#include "resource.h" // Resource and DEFER for wrapping C resources.

using namespace std;

// *****************
// * SDL wrapper types and helper functions
using RWindow  = Resource<SDL_Window, SDL_DestroyWindow>;
using RSurface = Resource<SDL_Surface, SDL_FreeSurface>;

bool loadSurface(RSurface& toSurface, const std::string& path) {
  toSurface = SDL_LoadBMP(path.c_str());
  if(toSurface.isNull()) {
    cout << "Unable to load " << path << "! Error: " << SDL_GetError() << '\n';
    return false;
  }
  return true;
}

// *****************
// * Game Objects

//Screen dimension constants
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

class Display {
  NO_COPY(Display);

public:
  Display() = default;

  bool state{false};
  RWindow     window{};
  RSurface    screen{};
  RSurface    i_hello{};
  RSurface    i_X{};

  bool init();
  bool loadMedia();
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
    printf( "Window could not be created! SDL_Error: %s\n", SDL_GetError() );
    return false;
  }
  // Get window surface
  screen = SDL_GetWindowSurface(&*window);

  // Fill the surface white
  SDL_FillRect(
    &*screen,
    nullptr,
    SDL_MapRGB(screen->format, 0xFF, 0xFF, 0xFF )
  );

  // Update the surface
  SDL_UpdateWindowSurface(&*window);
  return true;
}

bool Display::loadMedia() {
  return (
    loadSurface(i_hello, "data/02_img.bmp")
    and loadSurface(i_X, "data/x.bmp")
  );
}

// *****************
// * Event Loop
void eventLoop(Display& g) {
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

// *****************
// * Game
int game() {
  if(SDL_Init(SDL_INIT_VIDEO) < 0) {
    printf("SDL could not initialize! SDL_Error: %s\n", SDL_GetError());
    return 1;
  }
  DEFER(
    SDL_Quit();
    cout << "SDL_Quit Successfully\n";
  );

  Display g{};
  g.init();

  if(not g.loadMedia()) {
    return 1;
  }
  SDL_BlitSurface(&*g.i_hello, nullptr, &*g.screen, nullptr);
  SDL_UpdateWindowSurface(&*g.window);
  eventLoop(g);
  return 0;
}


int main( int argc, char* args[] ) {
  int err = game();
  return err;
}
