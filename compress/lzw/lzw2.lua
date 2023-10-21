-- LZW based on:
-- https://rosettacode.org/wiki/LZW_compression

local push = table.insert
local char, byte = string.char, string.byte
M = {}

function M.encode(data, max)
  local dict, size, w = {}, 256, ''
  local t = {}
  for i=0,255 do dict[char(i)] = i end
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
   local w, code, entry = char(encoded[1]), nil, nil
   local t, dict, size  = {w},              {},  256
   for i=0,255 do dict[i] = char(i) end
   local idx, len = 2, #encoded
   while idx <= len do
     code = encoded[idx]; idx = idx + 1
     found = dict[code]
     if found then            entry = found
     elseif code == size then entry = w..w[1]
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
    push(t, string.char(c >> 8), string.char(c & 0xFF))
  end
  return table.concat(t)
end

function M.deserialize16(text)
  local t = {}
  for i=1,#text,2 do
    local b1, b2 = string.byte(text:sub(i,i)), string.byte(text:sub(i+1,i+1))
    push(t, (b1 << 8) | b2)
  end
  return t
end

function M.encodeFile(from, to, encoder, max, serializer)
  if not max then max, serializer = 0xFFFF, M.serialize16 end
  local codes = (encoder or M.encode)(readPath(from))
  writePath(to, serializer(codes))
end

function M.decodeFile(from, to, decoder, max, deserializer)
  if not max then max, deserializer = 0xFFFF, M.deserialize16 end
  local codes = deserializer(readPath(from))
  writePath(to, (decoder or M.decode)(codes))
end

return M
