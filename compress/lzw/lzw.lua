
local push, sfmt = table.insert, string.format
local co = coroutine

local M = {}

local function dpnt(...)
  print(...)
end

local function dwrite(...)
  io.stdout:write(...)
end

local function nextTh(th)
  if co.status(th) ~= "dead" then return select(2, co.resume(th)) end
end

local function iterTh(fn, ...)
  local th = co.create(fn)
  co.resume(th, ...)
  return nextTh, th
end

local function nextFile(file, amount)
  local s = file:read(amount)
  if s then return amount, s end
end

local function tofile(f, mode)
  if type(f) == 'string' then return io.open(f, mode) end
  return f
end

function M.encodeTh(fn, state, index)
  dpnt('!! encodeTh')
  co.yield()
  local dict = {}; for i=0,0xFF do dict[string.char(i)] = i end
  local next_code = 0x100
  local build = ""
  local size = 0
  for i, c in fn, state, index do
    size = size + 1
    if size % 1024 == 0 then
      dpnt(sfmt('Encoded %sKiB', size // 1024))
    end
    assert(type(c) == 'string')
    index = i
    dwrite('!! ', c, ': ')
    local new = build..c
    if dict[new] then -- recognized code
      build = new
      dpnt(sfmt('building %q', new))
    else -- new code
      assert(#build > 0)
      dpnt(sfmt('new %s=%q', next_code, new))
      co.yield(assert(dict[build])); build = ''
      co.yield(string.byte(c))
      dict[new] = next_code; next_code = next_code + 1
      if next_code >= 4096 then break end
    end
  end

  if next_code >= 4096 then -- no new codes
    assert(#build == 0)
    while true do
      index, c = fn(state, index); if index == nil then break end
      size = size + 1
      if size % 1024 == 0 then
        dpnt(sfmt('Encoded %sKiB', size // 1024))
      end
      local new = build..c
      if dict[new] then build = new
      else
        if #build > 0 then
          co.yield(assert(dict[build])); build = ''
        end
        co.yield(string.byte(c))
      end
    end
  end
  if #build > 0 then
    co.yield(assert(dict[build]))
  end
  return t
end

function M.encode(...)
  local t = {}
  for c in iterTh(M.encodeTh, ...) do
    push(t, c)
  end
  return t
end

function M.decodeTh(f, state, idx)
  dpnt'!! DECODE'
  co.yield()
  local t = {}
  local dict = {}; for i=0,0xFF do dict[i] = string.char(i) end
  local next_code = 0x100
  local code = nil
  for _, c in f, state, idx do
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
      co.yield(new); code = nil
      if next_code >= 4096 then idx = i;  break end
    end
  end

  if next_code >= 4096 then
    for _, c in f, state, idx do co.yield(assert(dict[c])) end
  end
  return table.concat(t)
end

function M.decode(...)
  local t = {}
  for c in iterTh(M.decodeTh, ...) do
    push(t, c)
  end
  return table.concat(t, '')
end

function M.encodeFile(from, to)
  from, to = tofile(from, 'rb'), tofile(to, 'wb')
  local c0 = nil
  -- encode two codes (12*2 bits) as three bytes
  for c in iterTh(M.encodeTh, nextFile, from, 1) do
    if c0 then
      to:write(
        c0 >> 4,                       -- c0 upper 8 bits
        ((c0 & 0x0F) << 8) | (c >> 8), -- c0 lower 4 bits + c upper 4
        c & 0xFF                       -- c lower 8 bits
      )
      c0 = nil
    else c0 = c end
  end
  if c0 then
    to:write(
      c0 >> 4,            -- c0 upper 8 bits
      ((c0 & 0x0F) << 8), -- c0 lower 4 bits + 0
      0)                  -- zero for c
  end
  to:flush()
  from:close(); to:close()
end

function M.readCodes(file, data)
  if data then -- leftover data
    local _, b2, b3 = string.byte(data)
    return false, (((b2 & 0x0F) << 4) | b3)
  end
  data = file:read(3)
  if data then
    local b1, b2, _ = string.byte(data)
    return data, (b1 << 8) | (b2 >> 4)
  end
end

function M.decodeFile(from, to)
  from, to = tofile(from, 'rb'), tofile(to, 'wb')
  for s in iterTh(M.decodeTh, M.readCodes, file, false) do
    to:write(s)
  end
  to:flush()
  from:close() to:close()
end


return M
