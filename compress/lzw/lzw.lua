
local push, sfmt = table.insert, string.format

local function encode(...)
  local t = {}
  local dict = {}; for i=0,0xFF do dict[string.char(i)] = i end
  local next_code = 0x100
  local build = ""
  for c in ... do  assert(type(c) == 'string')
    io.stdout:write('!! ', c, ': ')
    local new = build..c
    if dict[new] then -- recognized code
      build = new
      print(sfmt('building %q', new))
    elseif next_code >= 4096 then
      if #build > 0 then
        push(t, assert(dict[build]))
        io.stdout:write(sfmt('code %s=%q and ', dict[build], build))
      end
      push(t, string.byte(c))
      build = ''
      print('%s=%q', string.byte(c), c)
    else -- new code
      assert(#build > 0)
      push(t, assert(dict[build]))
      push(t, string.byte(c))
      print(sfmt('new %s=%q', next_code, new))
      dict[new] = next_code; next_code = next_code + 1
      build = ''
    end
  end
  if #build > 0 then
      push(t, assert(dict[build]))
  end
  return t
end

local function decode(f, state, idx)
  print'!! DECODE'
  local t = {}
  local dict = {}; for i=0,0xFF do dict[i] = string.char(i) end
  local next_code = 0x100
  local code = nil
  for i, c in f, state, idx do
    assert(type(c) == 'number')
    io.stdout:write('!! ', tostring(c), ': ')

    if not code then
      code = c
      print(sfmt('code=%s%q', code, dict[code]))
    else
      assert(c <= 0xFF)
      local new = assert(dict[code])..string.char(c)
      print(sfmt('new and push %s%q', next_code, new))
      dict[next_code] = new; next_code = next_code + 1
      push(t, new); code = nil
      if next_code >= 4096 then idx = i;  break end
    end
  end

  if next_code >= 4096 then
    for _, c in f, state, idx do push(t, assert(dict[c])) end
  end
  return table.concat(t)
end


local function encodeStr(s)
  print(sfmt('!! ENCODE %s', s))
  return encode(s:gmatch'.')
end
local function dbg(codes)
  for _, c in ipairs(codes) do io.stdout:write(tostring(c), ' ') end
  print()
end

local s = 'hi hi there bob. hi hi there jane.'
local enc = encodeStr(s)
print(string.format('!! len %s -> %s', #s, #enc))
dbg(enc)

local dec = decode(ipairs(enc))
print('Exp: ', s)
print('Dec: ', dec)
