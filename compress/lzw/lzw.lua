
local push, sfmt = table.insert, string.format

local M = {}

local function dpnt(...)
  print(...)
end

local function dwrite(...)
  io.stdout:write(...)
end

function M.encode(...)
  local t = {}
  local dict = {}; for i=0,0xFF do dict[string.char(i)] = i end
  local next_code = 0x100
  local build = ""
  for c in ... do  assert(type(c) == 'string')
    dwrite('!! ', c, ': ')
    local new = build..c
    if dict[new] then -- recognized code
      build = new
      dpnt(sfmt('building %q', new))
    elseif next_code >= 4096 then
      if #build > 0 then
        push(t, assert(dict[build]))
        dwrite(sfmt('code %s=%q and ', dict[build], build))
      end
      push(t, string.byte(c))
      build = ''
      dpnt('%s=%q', string.byte(c), c)
    else -- new code
      assert(#build > 0)
      push(t, assert(dict[build]))
      push(t, string.byte(c))
      dpnt(sfmt('new %s=%q', next_code, new))
      dict[new] = next_code; next_code = next_code + 1
      build = ''
    end
  end
  if #build > 0 then
      push(t, assert(dict[build]))
  end
  return t
end

function M.decode(f, state, idx)
  print'!! DECODE'
  local t = {}
  local dict = {}; for i=0,0xFF do dict[i] = string.char(i) end
  local next_code = 0x100
  local code = nil
  for i, c in f, state, idx do
    assert(type(c) == 'number')
    dwrite('!! ', tostring(c), ': ')

    if not code then
      code = c
      dpnt(sfmt('code=%s%q', code, dict[code]))
    else
      assert(c <= 0xFF)
      local new = assert(dict[code])..string.char(c)
      dpnt(sfmt('new and push %s%q', next_code, new))
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



return M
