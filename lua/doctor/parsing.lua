local function ppf(...)
  local pp = require("fennelview")
  local ss
  do
    local tbl_15_auto = {}
    local i_16_auto = #tbl_15_auto
    for k, v in pairs({...}) do
      local val_17_auto = pp(v)
      if (nil ~= val_17_auto) then
        i_16_auto = (i_16_auto + 1)
        do end (tbl_15_auto)[i_16_auto] = val_17_auto
      else
      end
    end
    ss = tbl_15_auto
  end
  return print("&&&", unpack(ss))
end
local smatch = string.match
local _local_2_ = require("doctor.utils")
local same_word_3f = _local_2_["same-word?"]
local nil_3f = _local_2_["nil?"]
local has_elem_3f = _local_2_["has-elem?"]
local split = _local_2_["split"]
local words = _local_2_["words"]
local slice = _local_2_["slice"]
local random_element = _local_2_["random-element"]
local parsers
local function _3_(_241)
  return tonumber(smatch(_241, "^%#(%d+)$"))
end
local function _4_(_241)
  return smatch(_241, "^(%*)$")
end
local function _5_(_241)
  return smatch(_241, "^(%S+)$")
end
local function _6_(_241)
  return smatch(_241, "^%@(%w+)$")
end
local function _7_(_241)
  local content = smatch(_241, "^%[(.*)%]$")
  if content then
    local tbl_15_auto = {}
    local i_16_auto = #tbl_15_auto
    for keyword in split(content, "%,") do
      local val_17_auto = keyword
      if (nil ~= val_17_auto) then
        i_16_auto = (i_16_auto + 1)
        do end (tbl_15_auto)[i_16_auto] = val_17_auto
      else
      end
    end
    return tbl_15_auto
  else
    return nil
  end
end
parsers = {["match-n"] = _3_, ["match-many"] = _4_, ["match-word"] = _5_, ["match-group"] = _6_, ["match-choice"] = _7_}
local reassembly_parsers
local function _10_(_241)
  return smatch(_241, "^%:newkey$")
end
local function _11_(_241)
  return smatch(_241, "^%=(%S+)$")
end
local function reassemble_rule(rule)
  local index = 1
  local out = {}
  while (index < #rule) do
    local i, j, to_look = string.find(rule, "%$(%d+)", index)
    if to_look then
      table.insert(out, {type = "verbatim", content = string.sub(rule, index, (i - 1))})
      table.insert(out, {type = "lookup", content = tonumber(to_look)})
      index = (j + 1)
    else
      table.insert(out, {type = "verbatim", content = string.sub(rule, index)})
      index = #rule
    end
  end
  return out
end
reassembly_parsers = {newkey = _10_, ["try-keyword"] = _11_, reassemble = reassemble_rule}
local function parse_matching_rule(token)
  local result = nil
  local chosen_parser = nil
  local parsing_order = {"match-many", "match-n", "match-group", "match-choice", "match-word"}
  for _, parser_name in ipairs(parsing_order) do
    if result then break end
    local parser = parsers[parser_name]
    local content = parser(token)
    if content then
      chosen_parser = parser_name
      result = content
    else
    end
  end
  return chosen_parser, result
end
local function make_matching_rules(recipe)
  local tbl_15_auto = {}
  local i_16_auto = #tbl_15_auto
  for token in words(recipe) do
    local val_17_auto
    do
      local type, content = parse_matching_rule(token)
      val_17_auto = {type = type, content = content}
    end
    if (nil ~= val_17_auto) then
      i_16_auto = (i_16_auto + 1)
      do end (tbl_15_auto)[i_16_auto] = val_17_auto
    else
    end
  end
  return tbl_15_auto
end
local function parse_reassembly_rule(recipe)
  local result = nil
  local chosen_parser = nil
  local parsing_order = {"newkey", "try-keyword", "reassemble"}
  for _, parser_name in ipairs(parsing_order) do
    if result then break end
    local parser = reassembly_parsers[parser_name]
    local content = parser(recipe)
    if content then
      chosen_parser = parser_name
      result = content
    else
    end
  end
  return chosen_parser, result
end
local function pop_rule(rule)
  return rule.type, rule.content
end
local function match_here(script, rules, tokens, rule_index, token_index)
  local token = tokens[token_index]
  local rule = rules[rule_index]
  if (token and rule) then
    local _16_, _17_ = pop_rule(rule)
    if ((_16_ == "match-word") and (nil ~= _17_)) then
      local word = _17_
      if same_word_3f(word, token) then
        return 1
      else
        return nil
      end
    elseif ((_16_ == "match-choice") and (nil ~= _17_)) then
      local options = _17_
      if has_elem_3f(options, token) then
        return 1
      else
        return nil
      end
    elseif ((_16_ == "match-group") and (nil ~= _17_)) then
      local group = _17_
      local keywords = script.groups[group]
      if has_elem_3f(keywords, token) then
        return 1
      else
        return nil
      end
    elseif ((_16_ == "match-n") and (nil ~= _17_)) then
      local n = _17_
      if ((#tokens - token_index) >= n) then
        return n
      else
        return nil
      end
    elseif (_16_ == "match-many") then
      local next_rule = rules[(rule_index + 1)]
      local tindex_gap = (#tokens - token_index)
      if nil_3f(next_rule) then
        return (tindex_gap + 1)
      else
        local n = 0
        while (nil_3f(match_here(script, rules, tokens, (rule_index + 1), (token_index + n))) and ((token_index + n) <= #tokens)) do
          n = (n + 1)
        end
        if (n <= tindex_gap) then
          return n
        else
          return nil
        end
      end
    else
      return nil
    end
  else
    return nil
  end
end
local function disassemble(script, bot_rule, tokens)
  local fail = false
  local token_index = 1
  local matching_rules = make_matching_rules(bot_rule.decomposition)
  local pieces = {}
  for i, mrule in ipairs(matching_rules) do
    if (fail or (token_index > #tokens)) then break end
    local advance = match_here(script, matching_rules, tokens, i, token_index)
    if advance then
      local next_index = (token_index + advance)
      local found = slice(tokens, token_index, (next_index - 1))
      table.insert(pieces, table.concat(found, " "))
      token_index = next_index
    else
      fail = true
    end
  end
  if (fail or (token_index < #tokens)) then
    return nil
  else
    return pieces
  end
end
local function reassemble(rrules, pieces)
  local function _28_()
    local tbl_15_auto = {}
    local i_16_auto = #tbl_15_auto
    for _, rule in ipairs(rrules) do
      local val_17_auto
      do
        local _29_ = rule
        if ((_G.type(_29_) == "table") and ((_29_).type == "verbatim") and (nil ~= (_29_).content)) then
          local text = (_29_).content
          val_17_auto = text
        elseif ((_G.type(_29_) == "table") and ((_29_).type == "lookup") and (nil ~= (_29_).content)) then
          local n = (_29_).content
          val_17_auto = pieces[n]
        else
          val_17_auto = nil
        end
      end
      if (nil ~= val_17_auto) then
        i_16_auto = (i_16_auto + 1)
        do end (tbl_15_auto)[i_16_auto] = val_17_auto
      else
      end
    end
    return tbl_15_auto
  end
  return table.concat(_28_())
end
local function random_reassembly_rule(rule)
  local recipe = random_element(rule.reassembly)
  return parse_reassembly_rule(recipe)
end
local function try_random_reassemble(rule, pieces)
  local _32_, _33_ = random_reassembly_rule(rule)
  if ((_32_ == "reassemble") and (nil ~= _33_)) then
    local rrules = _33_
    return "done", reassemble(rrules, pieces)
  elseif ((nil ~= _32_) and (nil ~= _33_)) then
    local rtype = _32_
    local rcontent = _33_
    return rtype, rcontent
  else
    return nil
  end
end
return {disassemble = disassemble, reassemble = reassemble, ["try-random-reassemble"] = try_random_reassemble}
