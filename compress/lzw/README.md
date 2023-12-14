Lzw implementation

To run the test get the file for https://www.mattmahoney.net/dc/textrules.html
from https://www.mattmahoney.net/dc/textdata.html

You want `enwik8.zip`

unzip it in ~/tmp/wik

## LZW encode
We start with a dict of all char -> byte (char(0-255) -> (0-255))

To encode, we build up a "word" as we iterate through the bytes.
1. When we know the word, we emit nothing and keep building
2. When we don't know the word, we emit the word we do know
   and set dict[word] = size++. We continue building a word starting
   at the last character.

Let's take a look at how this works in practice:
abbabaabcabac

codes[a=1, b=2, c=3]

1. a -> w = 'a'   IS known, skip
2. b -> w = 'ab'  is NOT known:  emit 1'a';  codes[ab]=4;  w='b'
3. b -> w = 'bb'  is NOT known:  emit 2'b';  codes[bb]=5;  w='b'
4. a -> w = 'ba'  is NOT known:  emit 1'a';  codes[ba]=6;  w='a'
5. b -> w = 'ab'  IS known, skip
6. a -> w = 'aba' is NOT known:  emit 4'ab'; codes[aba]=7; w='a'
7. a -> w = 'aa'  is NOT known:  emit 1'a';  codes[aa]=8;  w='a'

Let's look at what was emitted and what the decoder has to build:

  1'a'
  2'b'  codes[4] = ab
  1'a'  codes[5] = bb
  4'ab' codes[6] = ba
  5'a'  codes[7] = aba
  1'a'  codes[8] = aa


To decode we reverse this. The decoder will receive only:
1. known codes
2. maxCode++

LZW decode
1. create same dict but of code -> word
1. initialize the word as the char(first code)
2. loop through the rest of codes. For each item the next code
   is equal to word..dict[code]:sub(1,1)
3. set word=dict[code]
