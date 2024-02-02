
-- include a single function from the C library at path
local mul, err = package.loadlib("./pear.so", "l_mul")
assert(mul, err)

local v = math.floor(2 ^ 33) + 3
assert(mul(3, 3) == 9)
assert(mul(v, v) == 51539607561) -- overflow

-- requires: LUA_CPATH=./?.so
local pear = require'pear'
assert(pear.mul(v, v) == 51539607561) -- overflow

print"test.lua done"
