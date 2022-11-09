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

SDL_Window*  gWindow = nullptr;        // The window we'll be rendering to
SDL_Surface* gScreenSurface = nullptr; // The surface contained by the window
SDL_Surface* gHelloWorldImg = nullptr;
SDL_Surface* gXImg = nullptr;


// Initialize SDL
bool init() {
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

bool loadImg(SDL_Surface** toSurface, char* path) {
  *toSurface = SDL_LoadBMP(path);
  if(not *toSurface) {
    printf("Unable to load %s! Error: %s\n", *toSurface, SDL_GetError());
    return false;
  }
  return true;
}

bool loadMedia() {
  return (
    loadImg(&gHelloWorldImg, "data/02_img.bmp")
    and loadImg(&gXImg, "data/x.bmp")
  );
}

void eventLoop() {
  SDL_Event e;
  bool quit = false;
  while( quit == false ){
    while( SDL_PollEvent( &e ) ) {
      cout << "Event: " << sdlEventToString(e) << '\n';

      if( e.type == SDL_QUIT ) quit = true;
      else {
        SDL_BlitSurface(gXImg, nullptr, gScreenSurface, nullptr);
        SDL_UpdateWindowSurface(gWindow);
      }
    }
  }
}

void close() {
  SDL_FreeSurface(gHelloWorldImg);
  gHelloWorldImg = nullptr;
  SDL_FreeSurface(gXImg);
  gXImg = nullptr;

  SDL_DestroyWindow(gWindow);
  gWindow = nullptr;

  SDL_Quit();
}


int main( int argc, char* args[] ) {
  if(init() and loadMedia()) {
    SDL_BlitSurface(gHelloWorldImg, nullptr, gScreenSurface, nullptr);
    SDL_UpdateWindowSurface(gWindow);
    eventLoop();
  }
  close();
  return 0;
}
