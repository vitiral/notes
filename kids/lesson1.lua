
print("Hello kids, my name is Rett!")
print("4 + 7 =", (4 + 7))
print('setting x to 5')
x = 5
print("4 + x =", (4 + x))
print('setting x to 12')
x = 12
print("4 + x =", (4 + x))
efefrve = hybtgte
print("x * 100 = " .. (x * 100))
print("x * 1234 = " .. (x * 1234))
print('efefrve =', efefrve)
print'my mom'

t = {}
t.name = 'Joe'
t.job = "Freddy's Burger Griller"
print("t's name:", t.name, "t's job:", t.job)

for k, v in pairs(t) do
  print('k', k, 'v', v)
end

k = 'name'
print("t's name", t[k], "salary", t['salary'])
t['salary'] = 'a billion $$$$'
print("t's name", t[k], "salary", t['salary'])

Foods = {
  cheeseburger  = 'yumm',
  mud           = 'super yuck',
  taco          = 'meh',
  ['ice cream'] = 'super yum',
  ['candy rocks'] = 'super duper yum',
  ['eesjjt6ufe4yf'] = 'randomhouse',
}
function flavor(food)
  local f = Foods[food]
  if not f then f = "I don't know" end
  return f
end
function tellFlavor(food)
  print('the flavor of ' .. food .. ' is: ', flavor(food))
end
tellFlavor('ice cream')
tellFlavor('mud')
tellFlavor('bacon')
tellFlavor('candy rocks')
tellFlavor('eesjjt6ufe4yf')


