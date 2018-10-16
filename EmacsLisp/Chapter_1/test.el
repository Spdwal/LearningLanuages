;; Happy hacking, ayakawashinji - Emacs ♥ you!

(concat "abv" "edf")

(substring "helloworld" 1 3)

(concat "The " (number-to-string (+ 2 fill-column)))



(message "This message appears in the echo area")

(message "There are %d %s in the office!" (- fill-column 14) "pink elephants")

(message "He saw %d %s"
         (- fill-column 34)
         (concat "red "
                 (substring
                  "The quick brown foxes jumped." 16 21)
                 "leaping."))



(set 'flowers '(rose violet daisy buttercup))

flowers

'flowers

(setq carnivores '(lion tiger leopard))

(setq count 1)

(setq count (+ count 1))

count

(provide 'test)
