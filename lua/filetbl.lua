local a2e = table.insert

local noop = function()end

FileTbl = setmetatable({
  __len = function(ft)
    print('!! __len', ft.len)
    return ft.len
  end,
  __index = function(ft, i)
    return getmetatable(ft)[i] or error('cannot get indexes from FileTbl')
  end,
  __newindex = function(ft, i, v)
    if i ~= ft.len + 1 then error(string.format(
      'can only set to len+1, len=%s i=%s', ft.len, i
    ))end
    ft.len = i
    print('!! __newindex', i, v)
  end,
}, {
  __call=function(ty_)
    local t = {len=0}
    return setmetatable(t, ty_)
  end,
})

f = FileTbl()
a2e(f, 'hello')
a2e(f, 'goodbye')

return f
