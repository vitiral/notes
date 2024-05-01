mod = {insert = table.insert}

local function run()
  local t={}
  for i=1,50000,1 do
    mod.insert(t,1,i)
  end
end

run()

