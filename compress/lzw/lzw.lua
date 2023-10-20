
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

function M.encodeLzwFull(fn, state, idx, max, dict, size)
  local build = ''
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

  if #build > 0 then
    co.yield(assert(dict[build]))
  end
end

local function llpop(prev, nxt, i)
  local pi = prev[i]
  local ni = nxt[i]
  nxt[pi] = ni
  prev[ni] = pi
  return i
end

-- insert after
local function llPutNext(prev, nxt, at, i)
  local ni = nxt[at]
  nxt[at] = i
  prev[i] = at

  nxt[i]   = ni
  prev[ni] = i
end

local function llzLLs(max)
  local llprev, llnext = {0}, {0}

  for i=256,max do
    llprev[i]=i-1
    llnext[i]=i+1
  end
  llprev[0],   llnext[max] = max, 0
  llnext[0],   llprev[256] = 256, 0
  return llprev, llnext
end


function M.encodeLlzFull(fn, state, idx, max, dict, size)
  print('!! encoding FULL', max)
  local llprev, llnext = llzLLs(max)
  local root = 0 -- root index. Items after root stay, items before root are deleted

  local build = ''
  for i, c in fn, state, idx do
    size = size + 1; if size % 1024 == 0 then
      dpnt(sfmt('Encoded %sKiB', size // 1024))
    end
    assert(type(c) == 'string')
    dwrite('!! ', c, ': ')
    local new = build..c
    local code = dict[new]
    if code then -- recognized code
      if code > 255 then
        llpop(llprev, llnext, code)
        llPutNext(llprev, llnext, root, code)
        root = code -- additional recognized items go after this
      end
      build = new
      dpnt(sfmt('building %q', new))
    else -- Create new code using least-used code.
      assert(#build > 0)
      co.yield(assert(dict[build])); build = ''
      co.yield(string.byte(c))
      root = 0
      code = llprev[root] -- replace least-used code with new
      llpop(llprev, llnext, code)
      llPutNext(llprev, llnext, root, code)
      dpnt(sfmt('new %s=%q del %q', code, new, dict[code]))
      dict[new] = code
    end
  end

  if #build > 0 then
    co.yield(assert(dict[build]))
  end

end

function M.encodeTh(fullFn, fn, state, idx, max)
  dpnt('!! encodeTh')
  max = max or 0xFFFF
  fullFn = fullFn or M.encodeLzwFull
  local dict = {}; for i=0,0xFF do dict[string.char(i)] = i end
  local next_code = 0x100
  local build = ""
  local size = 0
  co.yield()
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
    fullFn(fn, state, idx, max, dict, size)
  end
  if #build > 0 then
    co.yield(assert(dict[build]))
  end
end

function M.encode(...)
  local t = {}
  for c in iterTh(M.encodeTh, nil, ...) do
    push(t, c)
  end
  return t
end


function M.decodeLzwFull(fn, state, idx, max, dict)
  for _, c in fn, state, idx do co.yield(assert(dict[c])) end
end

function M.decodeLlzFull(fn, state, idx, max, dict)
  local llprev, llnext = llzLLs(max)
  local root = 0 -- root index. Items after root stay, items before root are deleted
  local code = nil
  for _, c in fn, state, idx do
    assert(type(c) == 'number')
    if c <= 0xFF then -- Create new code using least-used code.
      co.yield(c)
      root = 0
      local new = dict[code]..string.char(c)
      code = llprev[root] -- remove least-used code
      llpop(llprev, llnext, code)
      llPutNext(llprev, llnext, root, code)
      dict[new] = code
    else
      co.yield(assert(dict[c]))
      code = c
      llpop(llprev, llnext, code)
      llPutNext(llprev, llnext, root, code)
      root = code
    end
  end
end

function M.decodeTh(fullFn, fn, state, idx, max)
  max = max or 0xFFFF
  fullFn = fullFn or M.decodeLzwFull
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
      co.yield(new); code = nil
      dict[next_code] = new; next_code = next_code + 1
      if next_code > max then idx = i;  break end
    end
  end
  if code then co.yield(dict[code]) end

  if next_code > max then
    fullFn(fn, state, idx, max, dict)
  end
  return table.concat(t)
end

function M.decode(...)
  local t = {}
  for c in iterTh(M.decodeTh, nil, ...) do
    push(t, c)
  end
  return table.concat(t, '')
end

function M.encodeFile(from, to, fullFn)
  from, to = assert(tofile(from, 'rb')), assert(tofile(to, 'wb'))
  for c in iterTh(M.encodeTh, fullFn, nextFile, from, 1) do
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

function M.decodeFile(from, to, fullFn)
  from, to = assert(tofile(from, 'rb')), assert(tofile(to, 'wb'))
  dpnt('Decoding ', from, M.readCodes, from)
  for s in iterTh(M.decodeTh, fullFn, M.readCodes, from) do
    to:write(s)
  end
  to:flush()
  from:close() to:close()
end


return M
