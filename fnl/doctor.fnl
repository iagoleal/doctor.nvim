(local M {})
(local fmt string.format)

(local theme {:prompt "> "})

;;; Open a buffer in a new split
(fn split-buffer [bufnr]
  (vim.cmd (.. bufnr "sbuffer")))


(fn process-text [text]
  "abcde")

(fn prompt-callback [text]
  (let [response (process-text text)]
    (vim.fn.append (- (vim.fn.line "$") 1)
                   response)))

(set _G.prompt_cbfnl prompt-callback)

; Create a new, empty buffer
(let [buffer (vim.api.nvim_create_buf false true)]
  ; Turn it into a prompt
  (tset vim.bo buffer :buftype  :prompt)
  (tset vim.bo buffer :swapfile false)
  (vim.fn.prompt_setprompt buffer theme.prompt)
  ;; Create the buffer and put the user on it
  (split-buffer buffer)
  (vim.cmd :startinsert)
  ;; FIXME: Why in hell can't I access the lua function directly via vim.fn ???
  (vim.cmd (fmt "call prompt_setcallback(%d, luaeval('prompt_cbfnl'))" buffer)))

; export the module
M
