;;;; Here lives all the parsing.
;;;; Parse user input

(local smatch string.match)

(fn split [str sep]
  (string.gmatch str (.. "([^" sep "]+)")))

(fn words [str]
  "Iterator over the space-separated words of a string."
  (string.gmatch str "%S+"))

(fn has-elem? [t x]
  "Check whether a table contains a certain element."
  (each [_ v (pairs t)]
    (when (= x v)
      (lua "return true")))
  false)

;;;; Convert Text to Rules

(local parsers
  {:match-n      #(tonumber (smatch $1 "^%#(%d+)$"))    ; Match N words;                Pattern: #<number>
   :match-many   #(smatch $1 "^(%*)$")                  ; Match 0 or more words;        Pattern: *
   :match-word   #(smatch $1 "^(%S+)$")                 ; Match a word verbatim;        Pattern: <word>
   :match-group  #(smatch $1 "^%@(%w+)$")               ; Match any word on group;      Pattern: @<word>
   :match-choice #(let [content (smatch $1 "^%[(.*)%]$")] ; Match a choice between words; Pattern: [<word> ... <word>]
                    (when content
                      (icollect [keyword (split content "%,")]
                        keyword)))})

(local reassembly-parsers
  {:newkey      #(smatch $1 "^%:newkey$")
   :try-keyword #(smatch $1 "^%=(%S+)$")
   :reassemble (fn reassemble-rule [rule]
                 (var index 1)
                 (local out {})
                 (while (< index (length rule))
                   (local (i j to-look) (string.find rule "%$(%d+)" index))
                   (if to-look
                       (do
                         (table.insert out
                                       {:type "verbatim"
                                        :content (string.sub rule index (- i 1))})
                         (table.insert out
                                       {:type "lookup"
                                        :content (tonumber to-look)})
                         (set index (+ j 1)))
                       (do
                         (table.insert out
                                       {:type :verbatim
                                        :content (string.sub rule index)})
                         (set index (length rule)))))
                 out)})

(fn parse-matching-rule [token]
  (var result nil)
  (var chosen-parser nil)
  (local parsing-order [:match-many :match-n :match-group :match-choice :match-word])
  (each [_ parser-name (ipairs parsing-order) :until result]
    (let [parser (. parsers parser-name)
          content (parser token)]
      (when content
        (set chosen-parser parser-name)
        (set result content))))
  (values chosen-parser result))

(fn make-matching-rules [recipe]
  (icollect [token (words recipe)]
    (let [(type content) (parse-matching-rule token)]
      {: type : content})))

(fn parse-reassembly-rule [recipe]
  (var result nil)
  (var chosen-parser nil)
  (local parsing-order [:newkey :try-keyword :reassemble])
  (each [_ parser-name (ipairs parsing-order) :until result]
    (let [parser (. reassembly-parsers parser-name)
          content (parser recipe)]
      (when content
        (set chosen-parser parser-name)
        (set result content))))
  (values chosen-parser result))


(fn pop-rule [rule]
  (values rule.type rule.content))

(fn compare-words [a b]
  (= (a:lower) (b:lower)))

;; TODO:
(fn disassemble [script bot-rule tokens]
  (var fail false) ; The parsing failed
  (var token-index 1)
  (let [matching-rules (make-matching-rules (. bot-rule :decomposition))
        pieces {}]
    (each [i mrule (ipairs matching-rules) :until (or fail
                                                      (> token-index (length tokens)))]
      (local token (. tokens token-index))
      (match (pop-rule mrule)
        (:match-word   word) (if (compare-words word token)
                                 (do
                                   (table.insert pieces token)
                                   (set token-index (+ token-index 1)))
                                 (set fail true))
        (:match-n      n)    (if (>= (- (length tokens) token-index) n)
                                 (for [_ 1 n]
                                   (table.insert pieces (. tokens token-index))
                                   (set token-index (+ token-index 1)))
                                 (set fail true))

        (:match-choice options) (if (has-elem? options token)
                                    (do
                                      (table.insert pieces token)
                                      (set token-index (+ token-index 1)))
                                    (set fail true))
        (:match-group  group) (let [keywords (. script :groups group)]
                                (if (has-elem? keywords token)
                                    (do
                                      (table.insert pieces token)
                                      (set token-index (+ token-index 1)))
                                    (set fail true)))
        :match-many (if (= i (length matching-rules))
                        (for [_ token-index (length tokens)]
                          (table.insert pieces (. tokens token-index))
                          (set token-index (+ token-index 1))))))
    (if fail nil pieces)))


(fn reassemble [rrules pieces]
  "Reassemble the answer following a set of rules."
  (table.concat (icollect [_ rule (ipairs rrules)]
                  (match rule
                    {:type :verbatim
                     :content text} text
                    {:type :lookup
                     :content n}    (. pieces n)))))

(fn random-element [t]
  "Pick a random element from a sequence."
  (let [index (math.random 1 (length t))]
    (. t index)))

(fn random-reassembly-rule [rule]
  "Pick a reassembly recipe at random and return its rule."
  (let [recipe (random-element (. rule :reassembly))]
    (parse-reassembly-rule recipe)))

(fn try-random-reassemble [rule pieces]
  (match (random-reassembly-rule rule)
    (:reassemble rrules) (values :done (reassemble rrules pieces))
    (rtype rcontent)     (values rtype rcontent)))

;; Export
{: disassemble
 : reassemble
 : try-random-reassemble}
