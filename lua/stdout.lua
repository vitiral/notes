print('Start: normal print')

o = io.stdout
io.stdout = io.open('STDOUT', 'w')
print('After redirect: normal print')
o:write('Write directly to original stdout\n')
io.stdout:write('Write directly to new file stdout\n')
io.stdout:flush()
