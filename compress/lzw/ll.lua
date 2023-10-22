
M = {}

-- a -> node -> b  ==> a -> b
function M.pop(prev, nxt, node)
  local a, b = prev[node], nxt[node]
  nxt[a], prev[b] = b, a
end

-- node -> b  ==>  node -> a -> b
function M.push(prev, nxt, node, a)
  local b = nxt[node]
  nxt[node],  nxt[a]  = assert(a), b
  prev[b],    prev[a] = a, node
end

return M
