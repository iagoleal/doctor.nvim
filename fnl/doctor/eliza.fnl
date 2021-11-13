;;;; The Bot itself
(fn ppf [...]
  (let [pp (require :fennelview)
         ss (icollect [k v (pairs [...])] (pp v))]
     (print :>>> (unpack ss))
     ...))

(local {: disassemble
        : try-random-reassemble}
       (require :doctor.parsing))

(macro with-default [value default]
  "Return a computation or a default in case of nil."
  `(let [it# ,value]
     (if (~= it# nil) it# ,default)))

(macro loop-until [loop-args body]
  (assert-compile (sequence? loop-args)
                  "loop arguments should be inside square brackets"
                  loop-args)
  (local result# (gensym))
  (table.insert loop-args :until)
  (table.insert loop-args `(~= ,result# nil))
  `(do
     (var ,result# nil)
     (each ,loop-args
       (let [this-stage# (do ,body)]
         (when (~= this-stage# nil)
           (set ,result# this-stage#))))
     ,result#))

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

(fn reverse [t]
  "Reverse the elements of a sequence."
  (let [out {}]
    (for [i (length t) 1 -1]
      (table.insert out (. t i)))
    out))

;;;; Script accessors

(fn lookup-keyword [script kw]
  "Look into a script for a keyword object."
  (let [key (string.lower kw)
        keywords (. script :keywords)]
    (each [_ obj (pairs keywords)]
      (when (has-elem? (. obj :keyword) kw)
        (lua "return obj")))))

(fn lookup-reflection [script keyword]
  "Look into a script for a keyword's reflection string."
  (let [key (string.lower keyword)]
    (. script :reflections key)))


(fn keyword-precedence [kw]
  "Look into a keyword and return its precedence."
  (. kw :precedence))

(fn keyword-rules [kw]
  "Look into a keyword and return its decomposition/reassembly rules."
  (. kw :rules))

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
    (let [kw (lookup-keyword script (string.lower word))]
      (when (~= kw nil)
        (if (> (keyword-precedence kw) current-precedence)
            (do (set current-precedence (keyword-precedence kw)) ; update precedence
                (table.insert stack kw))   ; Insert keyword at the top the stack
            (table.insert stack 1 kw)))))  ; Insert keyword at the end of stack
  stack)

(fn scan-keywords [script input]
  "Scan an user input for the keywords defined in a Bot script."
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

(var match-keyword nil)

(fn try-decomposition-rule [script rule phrase]
  (let [pieces (disassemble script rule phrase)] ; Disassemble may fail
    ; (ppf "DISSASEMBLED PIECES" pieces)
    (when pieces
      (match (try-random-reassemble rule pieces)
        (:done response) response
        (:try-keyword t) (let [kw (lookup-keyword script t)]
                           (match-keyword script kw phrase))
        :newkey          nil))))

(fn try-decomposition-rules [script rules phrase]
  "Try to apply a series of decomposition rules to a string."
  (loop-until [_ rule (ipairs rules)]
    (try-decomposition-rule script rule phrase)))

(set match-keyword (fn [script keyword phrase]
                    (let [rules (keyword-rules keyword)]
                      (try-decomposition-rules script rules phrase))))

(fn keywords-matcher [script keywords phrase]
  "Apply a sequence of keywords to an input string until a match is found."
  ;; Return the result of applying the highest matching keyword
  ;; or, in case nothing matches, return a default response.
  (with-default (loop-until [_ keyword (ipairs keywords)]
                  (match-keyword script keyword phrase))
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
    (let [rules (keyword-memory keyword)]
      (when rules
        (let [output (try-decomposition-rules script rules phrase)]
          (when (~= output nil)
            (memorize! output))))))

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
    (let [(chunked-text keywords) (scan-keywords script input)
          phrase                  (reflect script chunked-text)
          kw-stack                (reverse keywords)]
      (match (length kw-stack)
        0  (with-default (maybe-remember!)
                         (pick-default-say script))
        _  (do
              ; (ppf "Keyword" kw-stack phrase)
              (commit-to-memory (. kw-stack 1) phrase)
              (keywords-matcher script kw-stack phrase)))))
  ;; Make the bot itself
  (setmetatable {: greet : answer}
                {:__call #(answer $2)}))


;;; Tests
;;; TODO: Remove this after it's done
; (local script (require :doctor.script))
; ((. (make-bot script) :greet))
; (local script ((loadfile "../lua/doctor/script.lua")))
; (local bot (make-bot script))
; (bot "I'm sorry")
; (bot.answer "Are you happy?")
; (bot "Hello, my name is Iago. How are you?")
; (bot "Do you want to be my friend?")

;; export
make-bot
