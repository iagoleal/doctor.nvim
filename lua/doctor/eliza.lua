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
  print(">>>", unpack(ss))
  return ...
end
local _local_1_ = require("doctor.utils")
local has_elem_3f = _local_1_["has-elem?"]
local nonempty_3f = _local_1_["nonempty?"]
local random_element = _local_1_["random-element"]
local random_remove_21 = _local_1_["random-remove!"]
local reverse = _local_1_["reverse"]
local unwords = _local_1_["unwords"]
local _local_2_ = require("doctor.parsing")
local disassemble = _local_2_["disassemble"]
local try_random_reassemble = _local_2_["try-random-reassemble"]
local function lookup_keyword(script, kw)
  local key = string.lower(kw)
  local keywords = script.keywords
  for _, obj in pairs(keywords) do
    if has_elem_3f(obj.keyword, kw) then
      return obj
    end
  end
  return nil
end
local function lookup_reflection(script, keyword)
  local key = string.lower(keyword)
  return script.reflections[key]
end
local function keyword_precedence(kw)
  return kw.precedence
end
local function keyword_rules(kw)
  return kw.rules
end
local function keyword_memory(keyword)
  return keyword.memory
end
local function pick_default_say(script)
  local default_says = script.default
  return random_element(default_says)
end
local function pick_greeting(script)
  local greetings = script.greetings
  return random_element(greetings)
end
local function split_into_words(text)
  local tbl_12_auto = {}
  for phrase in string.gmatch(text, "[^.,!?;:]+") do
    local _4_
    do
      local tbl_12_auto0 = {}
      for word in string.gmatch(phrase, "%S+") do
        tbl_12_auto0[(#tbl_12_auto0 + 1)] = word
      end
      _4_ = tbl_12_auto0
    end
    tbl_12_auto[(#tbl_12_auto + 1)] = _4_
  end
  return tbl_12_auto
end
local function scan_keywords_chunk(script, words)
  local stack = {}
  local current_precedence = ( - math.huge)
  for _, word in ipairs(words) do
    local kw = lookup_keyword(script, string.lower(word))
    if (kw ~= nil) then
      if (keyword_precedence(kw) > current_precedence) then
        current_precedence = keyword_precedence(kw)
        table.insert(stack, kw)
      else
        table.insert(stack, 1, kw)
      end
    end
  end
  return stack
end
local function scan_keywords(script, input)
  local keywords_2a = {}
  local chunked_text_2a = {}
  local slices = split_into_words(input)
  for i = #slices, 1, -1 do
    local chunked_text = slices[i]
    local keywords = scan_keywords_chunk(script, chunked_text)
    if nonempty_3f(keywords) then
      chunked_text_2a = chunked_text
      keywords_2a = keywords
    end
  end
  return chunked_text_2a, keywords_2a
end
local function reflect(script, chunked_text)
  local tbl_12_auto = {}
  for _, word in ipairs(chunked_text) do
    local _9_
    do
      local _8_ = lookup_reflection(script, word)
      if (_8_ == nil) then
        _9_ = word
      elseif (nil ~= _8_) then
        local refl = _8_
        _9_ = refl
      else
      _9_ = nil
      end
    end
    tbl_12_auto[(#tbl_12_auto + 1)] = _9_
  end
  return tbl_12_auto
end
local match_keyword = nil
local function try_decomposition_rule(script, rule, phrase)
  local pieces = disassemble(script, rule, phrase)
  if pieces then
    local _13_, _14_ = try_random_reassemble(rule, pieces)
    if ((_13_ == "done") and (nil ~= _14_)) then
      local response = _14_
      return response
    elseif ((_13_ == "try-keyword") and (nil ~= _14_)) then
      local t = _14_
      local kw = lookup_keyword(script, t)
      return match_keyword(script, kw, phrase)
    elseif (_13_ == "newkey") then
      return nil
    end
  end
end
local function try_decomposition_rules(script, rules, phrase)
  local _17_ = nil
  for _, rule in ipairs(rules) do
    if (_17_ ~= nil) then break end
    local this_stage_2_auto
    do
      this_stage_2_auto = try_decomposition_rule(script, rule, phrase)
    end
    if (this_stage_2_auto ~= nil) then
      _17_ = this_stage_2_auto
    end
  end
  return _17_
end
local function _19_(script, keyword, phrase)
  local rules = keyword_rules(keyword)
  return try_decomposition_rules(script, rules, phrase)
end
match_keyword = _19_
local function keywords_matcher(script, keywords, phrase)
  local it_2_auto
  do
    local _20_ = nil
    for _, keyword in ipairs(keywords) do
      if (_20_ ~= nil) then break end
      local this_stage_2_auto
      do
        this_stage_2_auto = match_keyword(script, keyword, phrase)
      end
      if (this_stage_2_auto ~= nil) then
        _20_ = this_stage_2_auto
      end
    end
    it_2_auto = _20_
  end
  if (it_2_auto ~= nil) then
    return it_2_auto
  else
    return pick_default_say(script)
  end
end
local function make_bot(script)
  local memory = {}
  local remembering_probability = 0.4
  local function maybe_remember_21()
    local p = math.random()
    if ((p > remembering_probability) and nonempty_3f(memory)) then
      return random_remove_21(memory)
    else
      return nil
    end
  end
  local function memorize_21(phrase)
    return table.insert(memory, phrase)
  end
  local function commit_to_memory(keyword, phrase)
    local rules = keyword_memory(keyword)
    if rules then
      local output = try_decomposition_rules(script, rules, phrase)
      if (output ~= nil) then
        return memorize_21(output)
      end
    end
  end
  local function greet()
    return pick_greeting(script)
  end
  local function answer(input)
    local chunked_text, keywords = scan_keywords(script, input)
    local phrase = reflect(script, chunked_text)
    local kw_stack = reverse(keywords)
    local _26_ = #kw_stack
    if (_26_ == 0) then
      local it_2_auto = maybe_remember_21()
      if (it_2_auto ~= nil) then
        return it_2_auto
      else
        return pick_default_say(script)
      end
    else
      local _ = _26_
      commit_to_memory(kw_stack[1], phrase)
      return keywords_matcher(script, kw_stack, phrase)
    end
  end
  local function _29_(_241, _242)
    return answer(_242)
  end
  return setmetatable({answer = answer, greet = greet}, {__call = _29_})
end
return make_bot
