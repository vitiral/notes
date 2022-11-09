/*
  Simple DirectMedia Layer
  Copyright (C) 1997-2018 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*/

/*
Most code is from src\events\SDL_events.c
current version is from 2.0.20.
SDL 2.0.20: see: SDL_LogEvent() from original SDL source code
SDL 2.0.18: see: SDL_LogEvent() from original SDL source code
old version was from 2.0.9.
SDL 2.0.9: see: SDL_DebugPrintEvent() from original SDL source code
*/

#include "sdl_event_to_string.h"
#include <stdio.h>
#include <string.h>
#include <SDL2/SDL.h>

#define TMP_STR_LEN 256
#define DST_STR_LEN 256

/* this is to make printf() calls cleaner. */
#define uint unsigned int

#if defined(_MSC_VER)
#define snprintf(buf,len, format,...) _snprintf_s(buf, len,len, format, __VA_ARGS__)
#endif

#define printf(f_, ...) { \
	snprintf(tmpStr, TMP_STR_LEN, (f_), ##__VA_ARGS__); \
	strnncat(dst, n, tmpStr, TMP_STR_LEN); \
}

static void strnncat(char *dst, size_t dn, const char *src, size_t sn)
{
#if defined(_MSC_VER)
	strncat_s(dst, dn, src, _TRUNCATE);
#else
	size_t dlen = strlen(dst);
	size_t dleft = dn - dlen;
	if (dleft <= 1) {
		return;
	}
	--dleft; // because of null termination
	strncat(dst, src, dleft < sn ? dleft : sn);
#endif
}

char *sdlEventToCString(char *dst, size_t n, const SDL_Event *event)
{
    char tmpStr[TMP_STR_LEN + 1];
    tmpStr[TMP_STR_LEN] = '\0';

    if (!n || !dst) {
        return dst;
    }
    dst[0] = '\0';

	///////////////////////// BEGIN OF COPY ///////////////////////

    char name[32];
    char details[128];

#if 0 // set 0 --> always convert event to string
    /* sensor/mouse/finger motion are spammy, ignore these if they aren't demanded. */
    if ( (SDL_DoEventLogging < 2) &&
            ( (event->type == SDL_MOUSEMOTION) ||
              (event->type == SDL_FINGERMOTION) ||
              (event->type == SDL_CONTROLLERTOUCHPADMOTION) ||
              (event->type == SDL_CONTROLLERSENSORUPDATE) ||
              (event->type == SDL_SENSORUPDATE) ) ) {
        return;
    }
#endif

    /* this is to make SDL_snprintf() calls cleaner. */
    #define uint unsigned int

    name[0] = '\0';
    details[0] = '\0';

    /* !!! FIXME: This code is kinda ugly, sorry. */

    if ((event->type >= SDL_USEREVENT) && (event->type <= SDL_LASTEVENT)) {
        char plusstr[16];
        SDL_strlcpy(name, "SDL_USEREVENT", sizeof (name));
        if (event->type > SDL_USEREVENT) {
            SDL_snprintf(plusstr, sizeof (plusstr), "+%u", ((uint) event->type) - SDL_USEREVENT);
        } else {
            plusstr[0] = '\0';
        }
        SDL_snprintf(details, sizeof (details), "%s (timestamp=%u windowid=%u code=%d data1=%p data2=%p)",
                plusstr, (uint) event->user.timestamp, (uint) event->user.windowID,
                (int) event->user.code, event->user.data1, event->user.data2);
    }

    switch (event->type) {
        #define SDL_EVENT_CASE(x) case x: SDL_strlcpy(name, #x, sizeof (name));
        SDL_EVENT_CASE(SDL_FIRSTEVENT) SDL_strlcpy(details, " (THIS IS PROBABLY A BUG!)", sizeof (details)); break;
        SDL_EVENT_CASE(SDL_QUIT) SDL_snprintf(details, sizeof (details), " (timestamp=%u)", (uint) event->quit.timestamp); break;
        SDL_EVENT_CASE(SDL_APP_TERMINATING) break;
        SDL_EVENT_CASE(SDL_APP_LOWMEMORY) break;
        SDL_EVENT_CASE(SDL_APP_WILLENTERBACKGROUND) break;
        SDL_EVENT_CASE(SDL_APP_DIDENTERBACKGROUND) break;
        SDL_EVENT_CASE(SDL_APP_WILLENTERFOREGROUND) break;
        SDL_EVENT_CASE(SDL_APP_DIDENTERFOREGROUND) break;
        SDL_EVENT_CASE(SDL_KEYMAPCHANGED) break;
        SDL_EVENT_CASE(SDL_CLIPBOARDUPDATE) break;
        SDL_EVENT_CASE(SDL_RENDER_TARGETS_RESET) break;
        SDL_EVENT_CASE(SDL_RENDER_DEVICE_RESET) break;

        SDL_EVENT_CASE(SDL_WINDOWEVENT) {
            char name2[64];
            switch(event->window.event) {
                case SDL_WINDOWEVENT_NONE: SDL_strlcpy(name2, "SDL_WINDOWEVENT_NONE (THIS IS PROBABLY A BUG!)", sizeof (name2)); break;
                #define SDL_WINDOWEVENT_CASE(x) case x: SDL_strlcpy(name2, #x, sizeof (name2)); break
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_SHOWN);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_HIDDEN);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_EXPOSED);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_MOVED);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_RESIZED);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_SIZE_CHANGED);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_MINIMIZED);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_MAXIMIZED);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_RESTORED);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_ENTER);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_LEAVE);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_FOCUS_GAINED);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_FOCUS_LOST);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_CLOSE);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_TAKE_FOCUS);
                SDL_WINDOWEVENT_CASE(SDL_WINDOWEVENT_HIT_TEST);
                #undef SDL_WINDOWEVENT_CASE
                default: SDL_strlcpy(name2, "UNKNOWN (bug? fixme?)", sizeof (name2)); break;
            }
            SDL_snprintf(details, sizeof (details), " (timestamp=%u windowid=%u event=%s data1=%d data2=%d)",
                        (uint) event->window.timestamp, (uint) event->window.windowID, name2, (int) event->window.data1, (int) event->window.data2);
            break;
        }

        SDL_EVENT_CASE(SDL_SYSWMEVENT)
            /* !!! FIXME: we don't delve further at the moment. */
            SDL_snprintf(details, sizeof (details), " (timestamp=%u)", (uint) event->syswm.timestamp);
            break;

        #define PRINT_KEY_EVENT(event) \
            SDL_snprintf(details, sizeof (details), " (timestamp=%u windowid=%u state=%s repeat=%s scancode=%u keycode=%u mod=%u)", \
                (uint) event->key.timestamp, (uint) event->key.windowID, \
                event->key.state == SDL_PRESSED ? "pressed" : "released", \
                event->key.repeat ? "true" : "false", \
                (uint) event->key.keysym.scancode, \
                (uint) event->key.keysym.sym, \
                (uint) event->key.keysym.mod)
        SDL_EVENT_CASE(SDL_KEYDOWN) PRINT_KEY_EVENT(event); break;
        SDL_EVENT_CASE(SDL_KEYUP) PRINT_KEY_EVENT(event); break;
        #undef PRINT_KEY_EVENT

        SDL_EVENT_CASE(SDL_TEXTEDITING)
            SDL_snprintf(details, sizeof (details), " (timestamp=%u windowid=%u text='%s' start=%d length=%d)",
                (uint) event->edit.timestamp, (uint) event->edit.windowID,
                event->edit.text, (int) event->edit.start, (int) event->edit.length);
            break;

        SDL_EVENT_CASE(SDL_TEXTINPUT)
            SDL_snprintf(details, sizeof (details), " (timestamp=%u windowid=%u text='%s')", (uint) event->text.timestamp, (uint) event->text.windowID, event->text.text);
            break;


        SDL_EVENT_CASE(SDL_MOUSEMOTION)
            SDL_snprintf(details, sizeof (details), " (timestamp=%u windowid=%u which=%u state=%u x=%d y=%d xrel=%d yrel=%d)",
                    (uint) event->motion.timestamp, (uint) event->motion.windowID,
                    (uint) event->motion.which, (uint) event->motion.state,
                    (int) event->motion.x, (int) event->motion.y,
                    (int) event->motion.xrel, (int) event->motion.yrel);
            break;

        #define PRINT_MBUTTON_EVENT(event) \
            SDL_snprintf(details, sizeof (details), " (timestamp=%u windowid=%u which=%u button=%u state=%s clicks=%u x=%d y=%d)", \
                    (uint) event->button.timestamp, (uint) event->button.windowID, \
                    (uint) event->button.which, (uint) event->button.button, \
                    event->button.state == SDL_PRESSED ? "pressed" : "released", \
                    (uint) event->button.clicks, (int) event->button.x, (int) event->button.y)
        SDL_EVENT_CASE(SDL_MOUSEBUTTONDOWN) PRINT_MBUTTON_EVENT(event); break;
        SDL_EVENT_CASE(SDL_MOUSEBUTTONUP) PRINT_MBUTTON_EVENT(event); break;
        #undef PRINT_MBUTTON_EVENT


        SDL_EVENT_CASE(SDL_MOUSEWHEEL)
            SDL_snprintf(details, sizeof (details), " (timestamp=%u windowid=%u which=%u x=%d y=%d preciseX=%f preciseY=%f direction=%s)",
                    (uint) event->wheel.timestamp, (uint) event->wheel.windowID,
                    (uint) event->wheel.which, (int) event->wheel.x, (int) event->wheel.y,
                    event->wheel.preciseX, event->wheel.preciseY,
                    event->wheel.direction == SDL_MOUSEWHEEL_NORMAL ? "normal" : "flipped");
            break;

        SDL_EVENT_CASE(SDL_JOYAXISMOTION)
            SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%d axis=%u value=%d)",
                (uint) event->jaxis.timestamp, (int) event->jaxis.which,
                (uint) event->jaxis.axis, (int) event->jaxis.value);
            break;

        SDL_EVENT_CASE(SDL_JOYBALLMOTION)
            SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%d ball=%u xrel=%d yrel=%d)",
                (uint) event->jball.timestamp, (int) event->jball.which,
                (uint) event->jball.ball, (int) event->jball.xrel, (int) event->jball.yrel);
            break;

        SDL_EVENT_CASE(SDL_JOYHATMOTION)
            SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%d hat=%u value=%u)",
                (uint) event->jhat.timestamp, (int) event->jhat.which,
                (uint) event->jhat.hat, (uint) event->jhat.value);
            break;

        #define PRINT_JBUTTON_EVENT(event) \
            SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%d button=%u state=%s)", \
                (uint) event->jbutton.timestamp, (int) event->jbutton.which, \
                (uint) event->jbutton.button, event->jbutton.state == SDL_PRESSED ? "pressed" : "released")
        SDL_EVENT_CASE(SDL_JOYBUTTONDOWN) PRINT_JBUTTON_EVENT(event); break;
        SDL_EVENT_CASE(SDL_JOYBUTTONUP) PRINT_JBUTTON_EVENT(event); break;
        #undef PRINT_JBUTTON_EVENT

        #define PRINT_JOYDEV_EVENT(event) SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%d)", (uint) event->jdevice.timestamp, (int) event->jdevice.which)
        SDL_EVENT_CASE(SDL_JOYDEVICEADDED) PRINT_JOYDEV_EVENT(event); break;
        SDL_EVENT_CASE(SDL_JOYDEVICEREMOVED) PRINT_JOYDEV_EVENT(event); break;
        #undef PRINT_JOYDEV_EVENT

        SDL_EVENT_CASE(SDL_CONTROLLERAXISMOTION)
            SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%d axis=%u value=%d)",
                (uint) event->caxis.timestamp, (int) event->caxis.which,
                (uint) event->caxis.axis, (int) event->caxis.value);
            break;

        #define PRINT_CBUTTON_EVENT(event) \
            SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%d button=%u state=%s)", \
                (uint) event->cbutton.timestamp, (int) event->cbutton.which, \
                (uint) event->cbutton.button, event->cbutton.state == SDL_PRESSED ? "pressed" : "released")
        SDL_EVENT_CASE(SDL_CONTROLLERBUTTONDOWN) PRINT_CBUTTON_EVENT(event); break;
        SDL_EVENT_CASE(SDL_CONTROLLERBUTTONUP) PRINT_CBUTTON_EVENT(event); break;
        #undef PRINT_CBUTTON_EVENT

        #define PRINT_CONTROLLERDEV_EVENT(event) SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%d)", (uint) event->cdevice.timestamp, (int) event->cdevice.which)
        SDL_EVENT_CASE(SDL_CONTROLLERDEVICEADDED) PRINT_CONTROLLERDEV_EVENT(event); break;
        SDL_EVENT_CASE(SDL_CONTROLLERDEVICEREMOVED) PRINT_CONTROLLERDEV_EVENT(event); break;
        SDL_EVENT_CASE(SDL_CONTROLLERDEVICEREMAPPED) PRINT_CONTROLLERDEV_EVENT(event); break;
        #undef PRINT_CONTROLLERDEV_EVENT

        #define PRINT_CTOUCHPAD_EVENT(event) \
            SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%d touchpad=%d finger=%d x=%f y=%f pressure=%f)", \
                (uint) event->ctouchpad.timestamp, (int) event->ctouchpad.which, \
                (int) event->ctouchpad.touchpad, (int) event->ctouchpad.finger, \
                event->ctouchpad.x, event->ctouchpad.y, event->ctouchpad.pressure)
        SDL_EVENT_CASE(SDL_CONTROLLERTOUCHPADDOWN) PRINT_CTOUCHPAD_EVENT(event); break;
        SDL_EVENT_CASE(SDL_CONTROLLERTOUCHPADUP) PRINT_CTOUCHPAD_EVENT(event); break;
        SDL_EVENT_CASE(SDL_CONTROLLERTOUCHPADMOTION) PRINT_CTOUCHPAD_EVENT(event); break;
        #undef PRINT_CTOUCHPAD_EVENT

       SDL_EVENT_CASE(SDL_CONTROLLERSENSORUPDATE)
            SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%d sensor=%d data[0]=%f data[1]=%f data[2]=%f)", \
                (uint) event->csensor.timestamp, (int) event->csensor.which, (int) event->csensor.sensor, \
                event->csensor.data[0], event->csensor.data[1], event->csensor.data[2]);
            break;

        #define PRINT_FINGER_EVENT(event) \
            SDL_snprintf(details, sizeof (details), " (timestamp=%u touchid=%lld fingerid=%lld x=%f y=%f dx=%f dy=%f pressure=%f)", \
                (uint) event->tfinger.timestamp, (long long)event->tfinger.touchId, \
                (long long)event->tfinger.fingerId, event->tfinger.x, event->tfinger.y, \
                event->tfinger.dx, event->tfinger.dy, event->tfinger.pressure)
        SDL_EVENT_CASE(SDL_FINGERDOWN) PRINT_FINGER_EVENT(event); break;
        SDL_EVENT_CASE(SDL_FINGERUP) PRINT_FINGER_EVENT(event); break;
        SDL_EVENT_CASE(SDL_FINGERMOTION) PRINT_FINGER_EVENT(event); break;
        #undef PRINT_FINGER_EVENT

        #define PRINT_DOLLAR_EVENT(event) \
            SDL_snprintf(details, sizeof (details), " (timestamp=%u touchid=%lld gestureid=%lld numfingers=%u error=%f x=%f y=%f)", \
                (uint) event->dgesture.timestamp, (long long)event->dgesture.touchId, \
                (long long)event->dgesture.gestureId, (uint) event->dgesture.numFingers, \
                event->dgesture.error, event->dgesture.x, event->dgesture.y);
        SDL_EVENT_CASE(SDL_DOLLARGESTURE) PRINT_DOLLAR_EVENT(event); break;
        SDL_EVENT_CASE(SDL_DOLLARRECORD) PRINT_DOLLAR_EVENT(event); break;
        #undef PRINT_DOLLAR_EVENT

        SDL_EVENT_CASE(SDL_MULTIGESTURE)
            SDL_snprintf(details, sizeof (details), " (timestamp=%u touchid=%lld dtheta=%f ddist=%f x=%f y=%f numfingers=%u)",
                (uint) event->mgesture.timestamp, (long long)event->mgesture.touchId,
                event->mgesture.dTheta, event->mgesture.dDist,
                event->mgesture.x, event->mgesture.y, (uint) event->mgesture.numFingers);
            break;

        #define PRINT_DROP_EVENT(event) SDL_snprintf(details, sizeof (details), " (file='%s' timestamp=%u windowid=%u)", event->drop.file, (uint) event->drop.timestamp, (uint) event->drop.windowID)
        SDL_EVENT_CASE(SDL_DROPFILE) PRINT_DROP_EVENT(event); break;
        SDL_EVENT_CASE(SDL_DROPTEXT) PRINT_DROP_EVENT(event); break;
        SDL_EVENT_CASE(SDL_DROPBEGIN) PRINT_DROP_EVENT(event); break;
        SDL_EVENT_CASE(SDL_DROPCOMPLETE) PRINT_DROP_EVENT(event); break;
        #undef PRINT_DROP_EVENT

        #define PRINT_AUDIODEV_EVENT(event) SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%u iscapture=%s)", (uint) event->adevice.timestamp, (uint) event->adevice.which, event->adevice.iscapture ? "true" : "false");
        SDL_EVENT_CASE(SDL_AUDIODEVICEADDED) PRINT_AUDIODEV_EVENT(event); break;
        SDL_EVENT_CASE(SDL_AUDIODEVICEREMOVED) PRINT_AUDIODEV_EVENT(event); break;
        #undef PRINT_AUDIODEV_EVENT

        SDL_EVENT_CASE(SDL_SENSORUPDATE)
            SDL_snprintf(details, sizeof (details), " (timestamp=%u which=%d data[0]=%f data[1]=%f data[2]=%f data[3]=%f data[4]=%f data[5]=%f)", \
                (uint) event->sensor.timestamp, (int) event->sensor.which, \
                event->sensor.data[0], event->sensor.data[1], event->sensor.data[2], \
                event->sensor.data[3], event->sensor.data[4], event->sensor.data[5]);
            break;

        #undef SDL_EVENT_CASE

        case SDL_POLLSENTINEL:
            /* No logging necessary for this one */
            break;

        default:
            if (!name[0]) {
                SDL_strlcpy(name, "UNKNOWN", sizeof (name));
                SDL_snprintf(details, sizeof (details), " #%u! (Bug? FIXME?)", (uint) event->type);
            }
            break;
    }
#if 0 // set 0 --> no logging only store to string dst
    if (name[0]) {
        SDL_Log("SDL EVENT: %s%s", name, details);
    }
#endif

    //#undef uint // don't undefine because for using later

    ///////////////////////// END OF COPY ///////////////////////

    if (!name[0]) {
        #define SDL_EVENT_CASE(x) case x: SDL_strlcpy(name, #x, sizeof (name));
        switch (event->type) {
            SDL_EVENT_CASE(SDL_POLLSENTINEL) SDL_snprintf(details, sizeof (details), " (timestamp=%u)", (uint) event->common.timestamp); break;
        }
        #undef SDL_EVENT_CASE
    }

    if (!name[0]) {
		SDL_snprintf(name, sizeof(name), "unknown");
        SDL_snprintf(details, sizeof(details), " type  %x", (unsigned int)event->type);
    }

    if (name[0]) {
        int len = strlen(name);
        strncpy(dst, name, n);
        if (len >= 0 && (unsigned int)len < n) {
            strncpy(dst + len, details, n - len);
        }
        dst[n - 1] = '\0';
	 }
    return dst;
}

void sdlEventToSdlLog(const SDL_Event* event)
{
	char str[DST_STR_LEN + 1];
	str[DST_STR_LEN] = '\0';
	sdlEventToCString(str, DST_STR_LEN, event);
	SDL_Log("%s", str);
}

#ifdef __cplusplus
void sdlEventToSdlLog(const SDL_Event& event)
{
	sdlEventToSdlLog(&event);
}

std::string sdlEventToString(const SDL_Event& event)
{
	char str[DST_STR_LEN + 1];
	str[DST_STR_LEN] = '\0';
	sdlEventToCString(str, DST_STR_LEN, &event);
	return str;
}
#endif

