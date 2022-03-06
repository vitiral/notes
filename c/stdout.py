
import tty
import sys
import termios
import subprocess

def readStuff():
  while True:
    v = sys.stdin.read(1)
    print("Py stdin:", v)
    if not v: break

def sendToC():
  p = subprocess.Popen(
      args=['./a.out'],
      stdin=sys.stdin,
      stdout=subprocess.PIPE,
      stderr=sys.stderr,
      bufsize=0,
  )

  for line in p.stdout:
    print("Py subprocess:", line)


fd = sys.stdin.fileno()
old = termios.tcgetattr(fd)
new = termios.tcgetattr(fd)
new[3] = new[3] & ~termios.ICANON
termios.tcsetattr(fd, termios.TCSANOW, new)

try:
  # readStuff()
  sendToC()
finally:
  termios.tcsetattr(fd, termios.TCSANOW, old)



