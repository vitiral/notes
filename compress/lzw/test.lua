-- Data for testing:
-- https://www.mattmahoney.net/dc/textrules.html
-- https://www.mattmahoney.net/dc/textdata.html

local llp, lln, llv = {1}, {1}, {}
local ll = require'll'

ll.push(llp, lln, 1, 2)
assert(2 == llp[1]); assert(2 == lln[1])
assert(1 == llp[2]); assert(1 == lln[2])
ll.push(llp, lln, 1, 3)
assert(2 == llp[1]); assert(3 == lln[1])
assert(1 == llp[3]); assert(2 == lln[3])
ll.pop(llp, lln, 3)
assert(2 == llp[1]); assert(2 == lln[1])
assert(1 == llp[2]); assert(1 == lln[2])


lzw = require'lzw'

local sfmt = string.format

local function nextStr(s, i)
  i = i + 1
  if i <= #s then return i, s:sub(i, i) end
end

local function encodeStr(s)
  print(sfmt('!! ENCODE %s', s))
  return lzw.encode(nextStr, s, 0)
end

local function dbg(codes)
  for _, c in ipairs(codes) do io.stdout:write(tostring(c), ' ') end
  print()
end

local function assertFilesEq(p1, p2)
  local p = io.popen(sfmt('diff -q %s %s', p1, p2))
  local msg = p:read()
  assert(p:close(), msg)
end



-- local s = 'hi hi there bob. hi hi there jane.'
-- local enc = encodeStr(s)
-- print(string.format('!! len %s -> %s', #s, #enc))
-- dbg(enc)
-- 
-- local dec = lzw.decode(ipairs(enc))
-- print('Exp: ', s)
-- print('Dec: ', dec)
-- 
-- print('!! encoding enwik8')
-- local rawF = 'test.lua'
-- local encF = 'test.lua.lzw'
-- local decF = 'test.lua.decoded'
-- lzw.encodeFile(rawF, encF)
-- lzw.decodeFile(encF, decF)
-- assertFilesEq(rawF, decF)
-- 
-- -- local rawF = '/home/rett/tmp/wik/enwik8_1MiB'
-- -- local encF = '/tmp/enwik8_16.lzw'
-- -- local decF = '/tmp/enwik8.decoded'
-- -- lzw.encodeFile(rawF, encF)
-- -- lzw.decodeFile(encF, decF)
-- -- print('asserting files')
-- -- assertFilesEq(rawF, decF)
-- 
-- 
-- local rawF = '/home/rett/tmp/wik/enwik8_1MiB'
-- local encF = '/tmp/enwik8_16.llz'
-- local decF = '/tmp/enwik8.decoded'
-- lzw.encodeFile(rawF, encF, lzw.encodeLlzFull)
-- print('!! Encoded to', encF)
-- lzw.decodeFile(encF, decF, lzw.decodeLlzFull)
-- print('asserting files')
-- assertFilesEq(rawF, decF)

local lzw2 = require'lzw2'
local s = 'hi hi there bob. hi hi there jane.'
local enc = lzw2.encode(s, 0xFFFF)
print(string.format('!! len %s -> %s', #s, #enc))
dbg(enc)

local dec = lzw2.decode(enc, 0xFFFF)
print('Exp: ', s)
print('Dec: ', dec)
assert(s == dec)

-- LZW 16
-- local rawF = '/home/rett/tmp/wik/enwik8_1MiB'
-- local encF = '/tmp/enwik8_16.lzw'
-- local decF = '/tmp/enwik8.decoded'
-- lzw2.encodeFile(rawF, encF)
-- print('!! Encoded to', encF)
-- lzw2.decodeFile(encF, decF)
-- print('!! Decoded to', decF)
-- assertFilesEq(rawF, decF)

-- LLZ 16
-- local encF = '/tmp/enwik8_16.llz'
-- lzw2.encodeFile(rawF, encF, lzw2.encodeLL)
-- print('!! Encoded to', encF)
-- lzw2.decodeFile(encF, decF, lzw2.decodeLL)
-- print('!! Decoded to', decF)
-- assertFilesEq(rawF, decF)

-- LZW 12
-- local encF = '/tmp/enwik8_12.lzw'
-- lzw2.encodeFile(rawF, encF, lzw2.encode, 0xFFF, M.serialize12)
-- print('!! Encoded to', encF)
-- lzw2.decodeFile(encF, decF, lzw2.decode, 0xFFF, M.deserialize12)
-- print('!! Decoded to', decF)
-- assertFilesEq(rawF, decF)

-- local encF = '/tmp/enwik8_12.llz'
-- lzw2.encodeFile(rawF, encF, lzw2.encodeLL, 0xFFF, M.serialize12)
-- print('!! Encoded to', encF)
-- lzw2.decodeFile(encF, decF)
-- print('!! Decoded to', decF)
-- assertFilesEq(rawF, decF)

-- LZW Big 16
local rawF = '/home/rett/tmp/wik/enwik8'
local encF = '/tmp/enwik8big_16.lzw'
lzw2.encodeFile(rawF, encF)
print('!! Encoded to', encF)

-- -- LLZ Big 16
-- local rawF = '/home/rett/tmp/wik/enwik8'
-- local encF = '/tmp/enwik8big_16.llz'
-- lzw2.encodeFile(rawF, encF, lzw2.encodeLL)
-- print('!! Encoded to', encF)
