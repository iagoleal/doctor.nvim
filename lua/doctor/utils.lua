local M = {}
M["nil?"] = function(x)
  return (x == nil)
end
M["same-word?"] = function(a, b)
  return (a:lower() == b:lower())
end
M.split = function(str, sep)
  return string.gmatch(str, ("([^" .. sep .. "]+)"))
end
M.words = function(str)
  return string.gmatch(str, "%S+")
end
M.unwords = function(words)
  return table.concat(words, " ")
end
M["nonempty?"] = function(t)
  return (#t > 0)
end
M["has-elem?"] = function(t, x)
  for _, v in pairs(t) do
    if (x == v) then
      return true
    end
  end
  return false
end
M.slice = function(t, i, _3fj)
  if (_3fj == nil) then
    _3fj = #t
  else
    _3fj = _3fj
  end
  local out = {}
  for key = i, _3fj do
    table.insert(out, t[key])
  end
  return out
end
M.reverse = function(t)
  local out = {}
  for i = #t, 1, -1 do
    table.insert(out, t[i])
  end
  return out
end
M["random-element"] = function(t)
  local index = math.random(1, #t)
  return t[index]
end
M["random-remove!"] = function(t)
  local index = math.random(1, #t)
  return table.remove(t, index)
end
return M
