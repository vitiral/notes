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

local s = 'hi hi there bob. hi hi there jane.'
local enc = encodeStr(s)
print(string.format('!! len %s -> %s', #s, #enc))
dbg(enc)

local dec = lzw.decode(ipairs(enc))
print('Exp: ', s)
print('Dec: ', dec)

print('!! encoding enwik8')
local rawF = 'test.lua'
local encF = 'test.lua.lzw'
local decF = 'test.lua.decoded'
lzw.encodeFile(rawF, encF)
lzw.decodeFile(encF, decF)
assertFilesEq(rawF, decF)

local rawF = '/home/rett/tmp/wik/enwik8_1MiB'
local encF = '/tmp/enwik8_2.lzw'
local decF = '/tmp/enwik8.decoded'
lzw.encodeFile(rawF, encF)
lzw.decodeFile(encF, decF)
print('asserting files')
assertFilesEq(rawF, decF)
