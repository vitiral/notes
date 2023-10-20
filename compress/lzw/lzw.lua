
local push, sfmt = table.insert, string.format
local co = coroutine

local M = {}

local function dpnt(...)
  -- print(...)
end

local function dwrite(...)
  -- io.stdout:write(...)
end

local function nextTh(th)
  local stat = co.status(th)
  if stat ~= "dead" then
    local res = {co.resume(th)}
    if not res[1] then error(sfmt(
      'resume failed on %s thread. Now stat=%s\n  result: %s %s\n',
      stat, co.status(th), tostring(res[1]), tostring(res[2])
    ))end
    return select(2, table.unpack(res))
  end
end

local function iterTh(fn, ...)
  local th = co.create(fn)
  assert(co.resume(th, ...))
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

function M.encodeTh(fn, state, idx, max)
  dpnt('!! encodeTh')
  max = max or 0xFFFF
  local dict = {}; for i=0,0xFF do dict[string.char(i)] = i end
  local next_code = 0x100
  local build = ""
  local size = 0
  co.yield()
  local llv, llnext, llprev = {}, {}, {}
  for i, c in fn, state, idx do
    size = size + 1; if size % 1024 == 0 then
      dpnt(sfmt('Encoded %sKiB', size // 1024))
    end
    assert(type(c) == 'string')
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
      if next_code > max then idx = i;  break end
    end
  end

  if next_code > max then -- no new codes
    assert(#build == 0)
    for _, c in fn, state, idx do
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

function M.decodeTh(fn, state, idx, max)
  max = max or 0xFFFF
  dpnt('!! DECODE', fn, state, idx, max)
  local t = {}
  local dict = {}; for i=0,0xFF do dict[i] = string.char(i) end
  local next_code = 0x100
  local code = nil
  co.yield()
  dpnt('!! decode resumed', fn, state, idx)
  for i, c in fn, state, idx do
    assert(type(c) == 'number')
    dwrite('!! decode loop', tostring(i), tostring(c), ': ')
    if not code then
      code = c
      dpnt(sfmt('code=%s%q', code, dict[code]))
    else
      assert(c <= 0xFF)
      local new = assert(dict[code])..string.char(c)
      dpnt(sfmt('new and push %s%q', next_code, new))
      dict[next_code] = new; next_code = next_code + 1
      co.yield(new); code = nil
      if next_code > max then idx = i;  break end
    end
  end
  if code then co.yield(dict[code]) end

  if next_code > max then
    for _, c in fn, state, idx do co.yield(assert(dict[c])) end
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
  from, to = assert(tofile(from, 'rb')), assert(tofile(to, 'wb'))
  for c in iterTh(M.encodeTh, nextFile, from, 1) do
    to:write(string.char(c >> 8), string.char(c & 0xFF))
  end
  to:flush()
  from:close(); to:close()
end

function M.readCodes(file)
  assert(file)
  local data = file:read(2)
  if data then
    local b1, b2 = string.byte(data), string.byte(data:sub(2,2))
    return true, (b1 << 8) | b2
  end
end

function M.decodeFile(from, to)
  from, to = assert(tofile(from, 'rb')), assert(tofile(to, 'wb'))
  dpnt('Decoding ', from, M.readCodes, from)
  for s in iterTh(M.decodeTh, M.readCodes, from) do
    to:write(s)
  end
  to:flush()
  from:close() to:close()
end


return M
