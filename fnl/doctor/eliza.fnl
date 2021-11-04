;;;; Here lives all the parsing.

(macro with-default [value default]
  "Return a computation or a default in case of nil."
  `(let [it# ,value]
     (if (~= it# nil) it# ,default)))

;;;; General utility functions

(fn unwords [words]
  "Join a string with spaces."
  (table.concat words " "))

(fn has-elem? [t x]
  "Check whether a table contains a certain element."
  (each [_ v (pairs t)]
    (when (= x v)
      (lua "return true")))
  false)

(fn nonempty? [t]
  (> (length t) 0))

(fn random-element [t]
  "Pick a random element from a sequence."
  (let [index (math.random 1 (length t))]
    (. t index)))

(fn random-remove! [t]
  "Pick and remove a random element from a sequence."
  (let [index (math.random 1 (length t))]
    (table.remove t index)))


;;;; Script accessors

(fn lookup-keyword [script kw]
  "Look into a script for a keyword object."
  (let [key (string.lower kw)
        keywords (. script :keywords)]
    (each [_ obj (pairs keywords)]
      (when (has-elem? (. obj :keyword) kw)
        (lua "return obj")))))

(fn keyword-precedence [kw]
  "Look into a keyword and return its precedence."
  (. kw :precedence))

(fn keyword-rules [kw]
  "Look into a keyword and return its decomposition/reassembly rules."
  (. kw :rules))

(fn lookup-reflection [script keyword]
  "Look into a script for a keyword's reflection string."
  (let [key (string.lower keyword)]
    (. script :reflections key)))

(fn keyword-memory [keyword]
  "Look into a keyword for its memorization rules."
  (. keyword :memory))

(fn pick-default-say [script]
  "Pick a random say from the script.
   Used when no possible answer matches the Bot's input."
  (let [default-says (. script :default)]
    (random-element default-says)))

(fn pick-greeting [script]
  "Pick a random greeting from the script."
  (let [greetings (. script :greetings)]
    (random-element greetings)))

;;;; Scan for keywords

(fn split-into-words [text]  ; String -> List (List String)
  "Break a string into phrases by matching common punctuation marks.
   Then break each phrase into words."
  (icollect [phrase (string.gmatch text "[^.,!?;:]+")]
    (icollect [word (string.gmatch phrase "%S+")]
      word)))

;; This is the main loop for `scan-keywords`.
;; I just broke this apart for readability.
(fn scan-keywords-chunk [script words]
  (local stack [])
  (var current-precedence (- math.huge))
  (each [_ word (ipairs words)]
    (let [kw (lookup-keyword script word)]
      (when (~= kw nil)
        (if (> (keyword-precedence kw) current-precedence)
            (do (set current-precedence (keyword-precedence kw)) ; update precedence
                (table.insert stack kw))   ; Insert keyword at the top the stack
            (table.insert stack 1 kw)))))  ; Insert keyword at the end of stack
  stack)

(fn scan-keywords [script input]
  "Scan an user input for the keywords defined on a Bot script."
  (var keywords* [])
  (var chunked-text* [])
  (let [slices (split-into-words input)]
    (for [i (length slices) 1 -1]
      (let [chunked-text (. slices i)
            keywords (scan-keywords-chunk script chunked-text)]
        (when (nonempty? keywords)
          (set chunked-text* chunked-text)
          (set keywords* keywords))))
    (values chunked-text* keywords*)))

;;;; Basic word refleciton

(fn reflect [script chunked-text]
  "Apply all reflections on the script to the words."
  (icollect [_ word (ipairs chunked-text)]
    (match (lookup-reflection script word)
      nil  word
      refl refl)))

;;;; Matching keywords

;; TODO:
(fn try-decomposition-rule [script rule phrase])

;; TODO:
(fn try-decomposition-rules [script rules phrase]
  "Try to apply a series of decomposition rules to a string."
  (var result nil)
  (each [i rule (ipairs rules) :until result]
    (let [response (try-decomposition-rule script rule phrase)]
      (when response
        (set result response))))
  result)

(fn keywords-matcher [script keywords phrase]
  "Apply a sequence of keywords to an input string until a match is found."
  (var result nil)
  (each [i keyword (ipairs keywords) :until result]
    (let [rules    (keyword-rules keyword)
          response (try-decomposition-rules rules phrase)]
      (when response
        (set result response))))
  ;; Returned the result of applying the highest matching keyword
  ;; or, in case nothing matches, return a default response.
  (with-default result
                (pick-default-say script)))


;;;; Constructor

; Create a new bot from a script
(fn make-bot [script]
  "Turn a script into an answering function."
  (local memory {})                   ; The bot's memory
  (local remembering-probability 0.4) ; The probability that a Bot will answer from memory
  ;; Create methods local to the bot
  (fn maybe-remember! []
    (let [p (math.random)]
      (if (and (> p remembering-probability)
               (nonempty? memory))
          (random-remove! memory)
          nil)))

  (fn memorize! [phrase]
    "Insert a phrase inputed by the user into the Bot's memory.
     Adds extra wow factor."
    (table.insert memory phrase))

  (fn commit-to-memory [keyword phrase]
    (let [rules  (keyword-memory keyword)
          output (try-decomposition-rules script memo phrase)]
      (when (~= output nil)
        (memorize! output))))

  (fn greet []
    "Say a random hello phrase."
    (pick-greeting script))

  (fn answer [input]
    "The answering function is a script-aware closure.
     Receive some input and answer accordingly to the state of the bot.
     The process follows these steps:
       - Scan input looking for keywords
       - Keep only the first part of input (between punctuations) where we find any keyword
       - Apply reflections to each (possible) word according to script
       - If we find no keyword, answer with a phrase from memory or pick a default answer
       - Else, try to commit the phrase to memory
         and try to match the keywords to it.
       - Answer accordingly or with a default answer."
    (let [(keywords kw-stack) (scan-keywords script input)
          phrase              (unwords (reflect script keywords))]
      (match (length kw-stack)
        0  (with-default (maybe-remember!)
                         (pick-default-say script))
        _  (do (commit-to-memory (. kw-stack 1) phrase)
               (keywords-matcher script kw-stack phrase)))))
  ;; Make the bot itself
  (setmetatable {: greet : answer}
                {:__call #(answer $2)}))



;; export
{: make-bot}
