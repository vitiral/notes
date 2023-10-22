local ll = require'll'

local push = table.insert
local char, byte = string.char, string.byte
local sfmt = string.format

local function pntf(...) print(sfmt(...)) end
M = {}

-- Traditional LZW with max. Based on:
-- https://rosettacode.org/wiki/LZW_compression

function M.encode(data, max)
  pntf('Encode len=%s max=%s', #data, max)
  local dict, size, w = {}, 256, ''
  local t = {}
  for i=0,0xFF do dict[char(i)] = i end
  local idx, len = 1, #data
  while idx <= len do
    local c = data:sub(idx, idx); idx = idx + 1
    local wc = w..c
    if dict[wc] then w = wc
    else
      push(t, dict[w])
      dict[wc] = size
      size = size + 1
      w = c
      if size >= max then break end
    end
  end
  while idx <= len do
    local c = data:sub(idx, idx); idx = idx + 1
    local wc = w..c
    if dict[wc] then w = wc
    else
      push(t, dict[w])
      w = c
    end
  end
  if w then push(t, dict[w]) end
  assert(idx == len + 1)
  return t
end

function M.decode(encoded, max)
  pntf('Decode len=%s max=%s', #encoded, max)
  local w, code, entry = char(encoded[1]), nil, nil
  local t, dict, size  = {w},              {},  256
  for i=0,0xFF do dict[i] = char(i) end
  local idx, len = 2, #encoded
  while idx <= len do
    code = encoded[idx]; idx = idx + 1
    found = dict[code]
    if found then            entry = found
    elseif code == size then entry = w..w:sub(1,1)
    else error'invalid compressed code' end
    push(t, entry);
    dict[size] = w..entry:sub(1,1)
    size = size + 1
    w = entry
    if size > max then break end
  end
  while idx <= len do
    code = encoded[idx]; idx = idx + 1
    push(t, assert(dict[code]))
  end
  return table.concat(t)
end

-----------------------
-- LL variant

function M.encodeLL(data, max)
  pntf('EncodeLL len=%s max=%s', #data, max)
  -- hack: w/out [-1] "reuse" causes memory thrashing on po2 boundary
  local dict, size, w = {[-1]=0}, 256, ''
  local t = {}
  for i=0,0xFF do dict[char(i)] = i end
  local root, llp, lln, llv = 1, {1}, {1}, {}
  local idx, len = 1, #data

  while idx <= len do
    local c = data:sub(idx, idx); idx = idx + 1
    local wc = w..c
    local code = dict[wc]
    if code then
      w = wc
      if code <= 0xFF then root = 1
      else
        ll.pop(llp, lln, code)
        ll.push(llp, lln, root, code)
        root = code
      end
    else
      if size <= max then
        code = size; size = size + 1
      else -- reuse
        code = llp[1]
        assert(lln[code] == 1 and code > 0xFF)
        ll.pop(llp, lln, code)
        dict[llv[code]] = nil
      end
      llv[code] = wc
      ll.push(llp, lln, root, code)
      root      = 1
      push(t, assert(dict[w]))
      dict[wc] = code
      w = c
    end
  end
  if w then push(t, dict[w]) end
  assert(idx == len + 1)
  return t
end

function M.decodeLL(data, max)
  pntf('DecodeLL len=%s max=%s', #data, max)
  local w, code, entry = char(encoded[1]), nil, nil
  local t, dict, size  = {w},              {[-1]=0},  256
  local wdict = {}
  for i=0,0xFF do dict[i] = char(i) end
  local root, llp, lln, llv = 1, {1}, {1}, {}
  local idx, len = 2, #encoded
  while idx <= len do
    code = encoded[idx]; idx = idx + 1
    found = dict[code]
    if found then            entry = found
    elseif code == size then entry = w..w:sub(1,1)
    else error'invalid compressed code' end
    wdict[entry] = code
    root = 1
    for i=2,#entry-1 do -- mark precursors to entry
      local pre = wdict[entry:sub(1,i)]
      ll.pop(llp, lln, pre)
      ll.push(llp, lln, root, pre)
      root = pre
    end

    if size <= max then
      code = size; size = size + 1
    else
      code = llp[1]; assert(lln[code] == 1 and code > 0xFF)
      ll.pop(llp, lln, code)
      ll.push(llp, lln, root, code)
      wdict[dict[code]] = nil
    end
    llv[code] = entry
    ll.push(llp, lln, 1, code)

    push(t, entry);
    dict[code] = w..entry:sub(1,1)
    w = entry
    if size > max then break end
  end
  return table.concat(t)
end

-------------------------
-- Deal with files

local function readPath(path)
  local f,out = io.open(path, 'rb')
  out = f:read'a'; f:close(); return out
end

local function writePath(path, text)
  local f = io.open(path, 'wb'); f:write(text); f:flush(); f:close()
end

function M.serialize16(codes)
  local t = {}; for _, c in ipairs(codes) do
    push(t, char(c >> 8)..char(c & 0xFF))
  end
  return table.concat(t)
end

function M.deserialize16(text)
  local t = {}
  for i=1,#text,2 do
    local b1, b2 = byte(text:sub(i,i)), byte(text:sub(i+1,i+1))
    push(t, (b1 << 8) | b2)
  end
  return t
end

function M.serialize12(codes)
  local t = {}
  for i=1,#codes,2 do
    local c1, c2 = codes[i], codes[i+1]
    push(t, char(c1 >> 4)
            ..char(((c1 & 0x0F) << 4) | (c2 >> 8))
            ..char(c2 & 0xFF))

  end
  return table.concat(t)
end

function M.deserialize12(text)
  local t = {}
  for i=1,#text,3 do
    local b1, b2 = byte(text:sub(i,i)), byte(text:sub(i+1,i+1))
    local b3 = byte(text:sub(i+2,i+2))
    if i == 1 then
      pntf('%q  %X %X %X', text:sub(i,i+2), b1, b2, b3)
    end
    push(t, (b1          << 4) | (b2 >> 4))
    push(t, ((b2 & 0x0F) << 8) | b3)
  end
  return t

end

function M.encodeFile(from, to, encoder, max, serializer)
  if not max then max, serializer = 0xFFFF, M.serialize16 end
  local codes = (encoder or M.encode)(readPath(from), max)
  writePath(to, serializer(codes))
end

function M.decodeFile(from, to, decoder, max, deserializer)
  if not max then max, deserializer = 0xFFFF, M.deserialize16 end
  local codes = deserializer(readPath(from))
  writePath(to, (decoder or M.decode)(codes, max))
end

return M
