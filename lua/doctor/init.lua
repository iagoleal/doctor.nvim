local make_bot = require("doctor.eliza")
local script = require("doctor.script")
local M = {}
local fmt = string.format
local theme = {["bot-prompt"] = "doctor> ", prompt = "talk>   "}
local header = {"  __ \\                |", "  |   |   _ \\    __|  __|   _ \\    __|", "  |   |  (   |  (     |    (   |  |", " ____/  \\___/  \\___| \\__| \\___/  _|"}
local function split_buffer(bufnr)
  return vim.cmd((bufnr .. "sbuffer"))
end
local function append_line(str)
  return vim.fn.append((vim.fn.line("$") - 1), str)
end
local function append_lines(...)
  for _, l in ipairs({...}) do
    append_line(l)
  end
  return nil
end
local bots = {}
local function prompt_callback(text)
  local bot = bots[vim.fn.bufnr()]
  local response = bot.answer(text)
  return append_lines("", (theme["bot-prompt"] .. response), "")
end
local function create_prompt_buffer()
  local buffer = vim.api.nvim_create_buf(false, true)
  local bot = make_bot(script)
  do end (bots)[buffer] = bot
  vim.bo[buffer]["buftype"] = "prompt"
  vim.bo[buffer]["bufhidden"] = "delete"
  vim.bo[buffer]["swapfile"] = false
  vim.fn.prompt_setprompt(buffer, theme.prompt)
  split_buffer(buffer)
  append_lines(unpack(header))
  append_lines("", "Open up about your feelings.", "When you're done, press <Return> to get your answer.", "Repeat.", "", "", (theme["bot-prompt"] .. bot.greet()), "")
  vim.cmd("startinsert")
  return vim.cmd(fmt("call prompt_setcallback(%d, v:lua.require('doctor').prompt_callback)", buffer))
end
M["go"] = create_prompt_buffer
M["prompt_callback"] = prompt_callback
return M
