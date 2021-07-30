# Build my own editor!
> [tutorial](https://viewsourcecode.org/snaptoken/kilo/01.setup.html)

## Notes

`c_lflag`:
- ECHO: whether to echo characters back to screen
- ICANON: whether in canonical mode (read input line-by-line), false if
  byte-by-byte
- IEXTEN: whether Ctrl+V allows you to "force" a control character.
- ISIG: whether signals should be sent from Ctrl+C or Z

`c_iflag`:
- ICRNL: translate CR (13) into newlines (10).
- IXON: turns off Ctrl+S|Q to stop/resume transmission of data.

`c_oflag`:
- OPOST: output post processing


Escape Sequences:
Arrow keys, Page Up, Page Down, Home, and End all input 3 or 4 bytes to the
terminal: 27, '[', and then one or two other characters. This is known as an
escape sequence. All escape sequences start with a 27 byte. Pressing Escape
sends a single 27 byte as input.


