-- Module + Record template

-- a module is just a table that is returned (at the end of file)
local M = {}

-- Record / Class / whatever template
M.Example = setmetatable({
  __name='mymod.Example', -- used for debugging and some libraries
  __doc=[[
  (optional) Documentation for your type. This is used by some libraries to
  auto-provide help on types.
  ]],

  -- Example metamethod. This will be called by tostring(example)
  -- or 'something '..example
  __tostring=function(self)
    return string.format('Example(%s, %s)', self.first, self.last)
  end,

  __index = { -- Methods
    fullname = function(self)
      return string.format('%s %s', self.first, self.last)
    end,
    hello = function(self)
      print('Hello '..self:fullname()..'!')
    end,
  },
}, { -- Example's metatable: __call is Example's constructor
    __call=function(ty_, first, last)
      -- Note: ty_ == M.Example
      return setmetatable({first=first, last=last}, ty_)
    end
})

local example = M.Example('Vitiral', 'Example')
assert(example:fullname() == 'Vitiral Example')
example:hello() -- prints: Hello Vitiral Example!

return M
