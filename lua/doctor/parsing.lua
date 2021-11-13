local function ppf(...)
  local pp = require("fennelview")
  local ss
  do
    local tbl_12_auto = {}
    for k, v in pairs({...}) do
      tbl_12_auto[(#tbl_12_auto + 1)] = pp(v)
    end
    ss = tbl_12_auto
  end
  return print("&&&", unpack(ss))
end
local smatch = string.match
local _local_1_ = require("doctor.utils")
local has_elem_3f = _local_1_["has-elem?"]
local nil_3f = _local_1_["nil?"]
local random_element = _local_1_["random-element"]
local same_word_3f = _local_1_["same-word?"]
local slice = _local_1_["slice"]
local split = _local_1_["split"]
local words = _local_1_["words"]
local parsers
local function _2_(_241)
  local content = smatch(_241, "^%[(.*)%]$")
  if content then
    local tbl_12_auto = {}
    for keyword in split(content, "%,") do
      tbl_12_auto[(#tbl_12_auto + 1)] = keyword
    end
    return tbl_12_auto
  end
end
local function _4_(_241)
  return smatch(_241, "^%@(%w+)$")
end
local function _5_(_241)
  return smatch(_241, "^(%*)$")
end
local function _6_(_241)
  return tonumber(smatch(_241, "^%#(%d+)$"))
end
local function _7_(_241)
  return smatch(_241, "^(%S+)$")
end
parsers = {["match-choice"] = _2_, ["match-group"] = _4_, ["match-many"] = _5_, ["match-n"] = _6_, ["match-word"] = _7_}
local reassembly_parsers
local function _8_(_241)
  return smatch(_241, "^%=(%S+)$")
end
local function _9_(_241)
  return smatch(_241, "^%:newkey$")
end
local function reassemble_rule(rule)
  local index = 1
  local out = {}
  while (index < #rule) do
    local i, j, to_look = string.find(rule, "%$(%d+)", index)
    if to_look then
      table.insert(out, {content = string.sub(rule, index, (i - 1)), type = "verbatim"})
      table.insert(out, {content = tonumber(to_look), type = "lookup"})
      index = (j + 1)
    else
      table.insert(out, {content = string.sub(rule, index), type = "verbatim"})
      index = #rule
    end
  end
  return out
end
reassembly_parsers = {["try-keyword"] = _8_, newkey = _9_, reassemble = reassemble_rule}
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
    end
  end
  return chosen_parser, result
end
local function make_matching_rules(recipe)
  local tbl_12_auto = {}
  for token in words(recipe) do
    local _12_
    do
      local type, content = parse_matching_rule(token)
      _12_ = {content = content, type = type}
    end
    tbl_12_auto[(#tbl_12_auto + 1)] = _12_
  end
  return tbl_12_auto
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
    local _14_, _15_ = pop_rule(rule)
    if ((_14_ == "match-word") and (nil ~= _15_)) then
      local word = _15_
      if same_word_3f(word, token) then
        return 1
      end
    elseif ((_14_ == "match-choice") and (nil ~= _15_)) then
      local options = _15_
      if has_elem_3f(options, token) then
        return 1
      end
    elseif ((_14_ == "match-group") and (nil ~= _15_)) then
      local group = _15_
      local keywords = script.groups[group]
      if has_elem_3f(keywords, token) then
        return 1
      end
    elseif ((_14_ == "match-n") and (nil ~= _15_)) then
      local n = _15_
      if ((#tokens - token_index) >= n) then
        return n
      end
    elseif (_14_ == "match-many") then
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
        end
      end
    end
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
  local function _26_()
    local tbl_12_auto = {}
    for _, rule in ipairs(rrules) do
      local _28_
      do
        local _27_ = rule
        if ((type(_27_) == "table") and (nil ~= (_27_).content) and ((_27_).type == "verbatim")) then
          local text = (_27_).content
          _28_ = text
        elseif ((type(_27_) == "table") and (nil ~= (_27_).content) and ((_27_).type == "lookup")) then
          local n = (_27_).content
          _28_ = pieces[n]
        else
        _28_ = nil
        end
      end
      tbl_12_auto[(#tbl_12_auto + 1)] = _28_
    end
    return tbl_12_auto
  end
  return table.concat(_26_())
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
  end
end
return {["try-random-reassemble"] = try_random_reassemble, disassemble = disassemble, reassemble = reassemble}
