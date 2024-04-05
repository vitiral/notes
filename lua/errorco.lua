-- pcall, errors and coroutines

local C = coroutine

local function resume(co, ...)
  print('resuming:', ...)
  local t = {C.resume(co, ...)}
  if not t[1] then
    print('coroutine failed with traceback:', debug.traceback(co, 'some message'))
  end
  return table.unpack(t)
end

local function failMaybe(fail, message)
  if(fail) then error'failed' end
  C.yield('(inner)', message)
  return 'returned'
end

myc = C.create(function(fail, message)
  for _=1,2 do
    local ok, err, b = pcall(failMaybe, fail, message)
    if ok then fail, message = C.yield('(ok) ', ok, err, b)
    else       fail, message = C.yield('(err)', err, b) end
  end
  print('final failing...')
  failMaybe(true)
end)

print('  resumed 1', resume(myc, false, 'one'))
print('  resumed 2', resume(myc, true, 'fail 1')) -- note: inputs are swallowed by (inner)
print('  resumed 2', resume(myc, true, 'fail 2'))
print('  resumed 3', resume(myc, false, 'failing'))
print('  resumed 4', resume(myc, false, 'failing'))
assert(coroutine.close(myc))
