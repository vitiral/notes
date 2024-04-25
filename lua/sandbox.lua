local env = {}

local user_script

-- do
--   print('before _G', _G)          -- table 0x....
--   local _ENV = {print=print}
--   user_script = function()
--     print'hello from user script' -- only works because of above
--     print('_G', _G)               -- nil
--   end
-- end

local e = {print=print}
f = loadstring("print'hello from loaded string'")
f()

user_script()
