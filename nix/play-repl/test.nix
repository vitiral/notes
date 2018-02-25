{ a, b ? 3 }:
if a > b
  then builtins.trace "yes" true
  else builtins.trace "no" false
