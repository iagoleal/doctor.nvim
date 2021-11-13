(local make-bot (require :doctor.eliza))
(local script   (require :doctor.script))

(local M {})
(local fmt string.format)

(local theme {:prompt "talk>   "
              :bot-prompt "doctor> "})

(local header
    ["  __ \\                |"
     "  |   |   _ \\    __|  __|   _ \\    __|"
     "  |   |  (   |  (     |    (   |  |"
     " ____/  \\___/  \\___| \\__| \\___/  _|"])

;;; Open a buffer in a new split
(fn split-buffer [bufnr]
  (vim.cmd (.. bufnr "sbuffer")))

(fn append-line [str]
  (vim.fn.append (- (vim.fn.line "$") 1)
                 str))

(fn append-lines [...]
  (each [_ l (ipairs [...])]
    (append-line l)))

(local bots {})

(fn prompt-callback [text]
  "The answering function called by the prompt on <Return>."
  (let [bot      (. bots (vim.fn.bufnr))
        response (bot.answer text)]
    (append-lines ""
                  (.. theme.bot-prompt response)
                  "")))

(fn create-prompt-buffer []
  (let [buffer (vim.api.nvim_create_buf false true) ; Create a new, empty buffer
        bot    (make-bot script)]
    ; Save the bot for this buffer
    (tset bots buffer bot)
    ; Turn it into a prompt
    (tset vim.bo buffer :buftype   :prompt)
    (tset vim.bo buffer :bufhidden :delete)
    (tset vim.bo buffer :swapfile false)
    (vim.fn.prompt_setprompt buffer theme.prompt)
    ;; Create the buffer and put the user on it
    (split-buffer buffer)
    (append-lines (unpack header))
    (append-lines ""
                  "Open up about your feelings."
                  "When you're done, press <Return> to get your answer."
                  "Repeat."
                  ""
                  ""
                  (.. theme.bot-prompt (bot.greet))
                  "")
    (vim.cmd :startinsert)
    ;; FIXME: Why in hell can't I access the lua function directly via vim.fn ???
    ;; FIXME: hide this behind a require
    ; (vim.cmd (fmt "call prompt_setcallback(%d, luaeval('require(\"doctor\")')" buffer))))
    (vim.cmd (fmt "call prompt_setcallback(%d, v:lua.require('doctor').prompt_callback)" buffer))))

(tset M :go create-prompt-buffer)
(tset M :prompt_callback prompt-callback)

; export the module
M
