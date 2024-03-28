
local f = io.popen'uname'
local plat = f:read(); f:close()
local ext = '.so'; if plat == 'Darwin' then ext = '.dylib' end

-- include a single function from the C library at path
local mul, err = package.loadlib('./pear'..ext, 'l_mul')
assert(mul, err)

local v = math.floor(2 ^ 33) + 3
assert(mul(3, 3) == 9)
assert(mul(v, v) == 51539607561) -- overflow

-- requires: LUA_CPATH=./?.so
local pear = require'pear'
assert(pear.mul(v, v) == 51539607561) -- overflow

local pear2_func = package.loadlib('./pear'..ext, 'luaopen_pear')
print('pear2_func', pear2_func)
local pear2 = pear2_func()
print('pear2.mul', pear2.mul)
assert(pear2.mul(v, v) == 51539607561) -- overflow

print"test.lua done"
