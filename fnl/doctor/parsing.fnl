;;;; Here lives all the parsing.
;;;; Parse user input

(fn ppf [...]
  (let [pp (require :fennelview)
         ss (icollect [k v (pairs [...])] (pp v))]
     (print "&&&" (unpack ss))))

(local smatch string.match)

(local {: same-word?
        : nil?
        : has-elem?
        : split
        : words
        : slice
        : random-element}
       (require :doctor.utils))

;;; Convert Text to Rules

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


(fn match-here [script rules tokens rule-index token-index]
  (local token (. tokens token-index)) ; The current token we're looking at
  (local rule  (. rules  rule-index))
  (when (and token rule)
    (match (pop-rule rule)
      (:match-word  word) (if (same-word? word token)
                              1)
      (:match-choice options) (if (has-elem? options token)
                                  1)
      (:match-group group) (let [keywords (. script :groups group)]
                             (if (has-elem? keywords token)
                                 1))
      (:match-n n) (if (>= (- (length tokens) token-index) n)
                       n)
      :match-many (let [next-rule (. rules (+ rule-index 1))
                        tindex-gap (- (length tokens) token-index)]
                    ; (ppf "NEXT RULE" (type next-rule) next-rule)
                    ; (ppf "GAP" tindex-gap)
                    (if (nil? next-rule)
                        (+ tindex-gap 1)
                        (do
                          (var n 0)
                          (while (and (nil? (match-here script rules tokens (+ rule-index 1) (+ token-index n)))
                                      (<= (+ token-index n) (length tokens)))
                            (set n (+ n 1)))
                          (if (<= n tindex-gap)
                              n)))))))

(fn disassemble [script bot-rule tokens]
  (var fail false) ; The parsing failed
  (var token-index 1)
  (let [matching-rules (make-matching-rules (. bot-rule :decomposition))
        pieces {}]
    ; (ppf :Tokens? tokens)
    ; (ppf "Rules?" matching-rules)
    (each [i mrule (ipairs matching-rules) :until (or fail
                                                      (> token-index
                                                         (length tokens)))]
      (let [advance (match-here script matching-rules tokens i token-index)]
        ; (ppf "ADVANCE" advance)
        (if advance
            (let [next-index (+ token-index advance)
                  found      (slice tokens token-index (- next-index 1))]
              (table.insert pieces (table.concat found " "))
              (set token-index next-index))
            (set fail true))))
      ; (ppf "C-RULE" i (.. "#tokens = " (length tokens)) (.. "tidx = " token-index))
      ; (ppf "PIECES" pieces))
    (if (or fail (< token-index (length tokens)))
        nil
        pieces)))

(fn reassemble [rrules pieces]
  "Reassemble the answer following a set of rules."
  (table.concat (icollect [_ rule (ipairs rrules)]
                  (match rule
                    {:type :verbatim
                     :content text} text
                    {:type :lookup
                     :content n}    (. pieces n)))))

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
