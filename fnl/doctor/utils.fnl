(local M {})

;;; Set a default value to a variable
(macro default-value! [variable default]
  `(set-forcibly! ,variable
                  (if (= ,variable nil)
                      ,default
                      ,variable)))

(fn M.nil? [x] (= x nil))

;; String manipulation

(fn M.same-word? [a b]
  "Check if two strings are equal case insensitively."
  (= (a:lower) (b:lower)))

(fn M.split [str sep]
  "Iterator over substrings broken on a given separator."
  (string.gmatch str (.. "([^" sep "]+)")))

(fn M.words [str]
  "Iterator over the space-separated words of a string."
  (string.gmatch str "%S+"))

(fn M.unwords [words]
  "Join a string with spaces."
  (table.concat words " "))

;; Sequence manipulation

(fn M.nonempty? [t]
  (> (length t) 0))

(fn M.has-elem? [t x]
  "Check whether a table contains a certain element."
  (each [_ v (pairs t)]
    (when (= x v)
      (lua "return true")))
  false)

(fn M.slice [t i ?j]
  "Return a new table going from i to j.
  The last index defaults to the end of the table."
  (default-value! ?j (length t))
  (local out {})
  (for [key i ?j]
    (table.insert out (. t key)))
  out)

(fn M.reverse [t]
  "Reverse the elements of a sequence."
  (let [out {}]
    (for [i (length t) 1 -1]
      (table.insert out (. t i)))
    out))

(fn M.random-element [t]
  "Pick a random element from a sequence."
  (let [index (math.random 1 (length t))]
    (. t index)))

(fn M.random-remove! [t]
  "Pick and remove a random element from a sequence."
  (let [index (math.random 1 (length t))]
    (table.remove t index)))

;; export
M
