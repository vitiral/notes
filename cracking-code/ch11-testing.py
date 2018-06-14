# PG-66 CH11 Testing
> Lol, they swapped ch11 and ch12 in the answers...

# 11.1
> Find the mistake(s) in the following code:

```
unsigned int i;
for (i = 100; i <= 0; --i)
   printf(“%d\n”, i);
```

- should use `brackets`
- `--i` is actually unspecified/non-portable behavior in C
- `i <= 0` should be `i > 0`
- `i <= 0` but it is `unsigned int` -- it will never be < 0.
-

# 11.2
> You are given the source to an application which crashes when it is run After
> running it ten times in a debugger, you find it never crashes in the same
> place.
>
> The application is single threaded, and uses only the C standard library.
> What programming errors could be causing this crash? How would you test each
> one?

Any non-determinism can cause this kind of crash in C. This especially includes:
- Use of random values (duh).
- Flakey dependency.
- Use after free bugs.
- Use of null pointers.
- Using an array/pointer out of bounds.
- Use of undefined behaviors (although they would _probably_ be deterministically
  undefined).
- Memory leak (program ran out of memory).
- Compiler bug (lol)
- Flakey hardware (unlikely).

The things I would want to do:
- Unit test, unit test, unit test.
- Run valgrind or other memory tests while unit testing.
- Force array indexing (debug mode) wherever they are being used in tests.
- Don't use raw pointers for array traversal (related to above).


# 11.3
> We have the following method used in a chess game:
> `boolean canMoveTo(int x, int y)`.
>
> x and y are the coordinates of the chess board and it returns whether or not
> the piece can move to that position. Explain how you would test this method.

- Boundary conditions, make sure values outside [0, 8] always return false.
- What piece is this method attached to? Some pieces (pawn, rook, king) have
  very complex rules based on the current state of the game.
  - Do basic movements.
  - Do state-based movement tests if necessary.
- Do exahustive "sunny day" tests
- Try in different board states.
  - Moving through another piece (only knight can do it).
  - Moving on top of own piece.
  - Valid/invalid moving during check.
  - Moving to put self into check.


# 11.4
> How would you load test a webpage without using any test tools?

This depends heavily on the application in question and how much state that
application has. The more state, the more difficult to test.

I assume by "no test tools" it is meant that I can't use a programming
language of any kind, i.e. manual testing.

In that case I would do:
- blackbox testing of the requirements: does the application do what it
  says? Follow the steps and make sure they work.
- User acceptance style tests: what happens if the workflow goes outside
  of sunny day? What if I abuse the application?
  - Click buttons multiple times
  - Reload the page randomly
  - Have typo in various fields
  - Navigate to a url myself
  - Copy/paste url into new tab
  - Close browser and reopen


# 11.5
> How would you test a pen?

What is the pen being used for?

Basic tests:
- Open/close pen multiple times.
- Write on normal paper with pen in circles/words/etc.
- Write on table then on paper
- Write on self
- Write using different firmness. Does a hard firmness break the pen?
- Verify properties:
  - strength: how much can the pen withstand in bending/pushing/opening/etc?
  - drop the pen
  - submerge in water
- Subject to shaking
- Turn pen different directions, make sure it doesn't leak
  (put paper somewhere to detect leakage)

