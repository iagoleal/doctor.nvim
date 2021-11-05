;;;; Parse user input

(local smatch string.match)

(fn split [str sep]
  (string.gmatch str (.. "([^" sep "]+)")))

(fn words [str]
  "Iterator over the space-separated words of a string."
  (string.gmatch str "%S+"))

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

(fn make-matching-rules [str]
  (icollect [token (words str)]
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

(fn make-reassembly-rule [recipe]
  (let [(type content) (parse-reassembly-rule recipe)]
    {: type : content}))

;; TODO:
(fn disassemble [script rule phrase]
  (let [mrules (make-matching-rules (. rule :decomposition))]
    nil))

(fn reassemble [rrules pieces]
  (table.concat (icollect [_ rule (ipairs rrules)]
                  (match rule
                    {:type :verbatim
                     :content text} text
                    {:type :lookup
                     :content n}    (. pieces n)))))

(fn random-reassembly-rule [rule]
  (let [recipe (random-element (. rule :reassembly))]
    (parse-reassembly-rule recipe)))

(fn try-random-reassemble [rule phrase]
  (match (random-reassembly-rule rule)
    (:reassemble rrules) (values :done (reassemble rrules pieces))
    (rtype content)      (values rtype rcontent)))

;; Export
{: disassemble
 : reassemble
 : try-random-reassemble}
