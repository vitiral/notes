local ioc = require'ioc'

print('io.close iscfunction:', ioc.iscfunction(io.close))
print('inline fn iscfunction:', ioc.iscfunction(function() end))
print('empty iscfunction:', ioc.iscfunction())

local r, w = ioc.pipe()
print('r isclosed', ioc.isclosed(r))

w:write'hi there\n'
w:write'goodbye then\n'
w:flush()

assert('hi there' == r:read'l')

print('r isclosed', ioc.isclosed(r))
ioc.nonblock(r)
local res = r:read'l'
print('res:', res)
assert('goodbye then' == res)
print('r isclosed', ioc.isclosed(r))

-- doesn't block
ioc.clearerrno()
print('errno pre-read: ', ioc.errno())
assert(nil == r:read(42))
print('r isclosed after block', ioc.isclosed(r))
print('errno post-read: ', ioc.errno())
assert(11 == ioc.errno())

assert(nil == r:read(42))
assert(nil == r:read'l')
assert(nil == r:read'l')
assert(nil == r:read(42))
-- print'lines:'
-- for l in r:lines() do print(l) end
assert(nil == r:read(42))
w:write'finally writing again\n'; w:flush()

ioc.clearerrno()

res = r:read'l'
print('after:', res)
print('errno after', ioc.errno())
assert('finally writing again' == res)
print('r isclosed before close', ioc.isclosed(r))

print'filling pipe'
ioc.nonblock(w)
while w:write'hi there again\n' do end

while res do res = r:read'l'; print('read:', res) end

r:close()

