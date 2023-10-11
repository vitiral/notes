
local function linemap(lines)
  local m = {}
  for i, l in ipairs(lines) do
    local s = m[l] -- set of indexes
    if not v then v = {}; m[l] = v end
    s[i] = true
  end
  return m
end


local function diff(lo, ln)
  local mo, mn = linemap(lo), linemap(ln)
  for 
end
