;; --- Day 5: Alchemical Reduction ---

;; You've managed to sneak in to the prototype suit manufacturing lab.
;; The Elves are making decent progress, but are still struggling with
;; the suit's size reduction capabilities.

;; While the very latest in 1518 alchemical technology might have solved
;; their problem eventually, you can do better. You scan the chemical
;; composition of the suit's material and discover that it is formed by
;; extremely long polymers (one of which is available as your puzzle input).

;; The polymer is formed by smaller units which, when triggered, react with
;; each other such that two adjacent units of the same type and opposite polarity
;; are destroyed. Units' types are represented by letters; units' polarity is
;; represented by capitalization. For instance, r and R are units with the
;; same type but opposite polarity, whereas r and s are entirely different
;; types and do not react.

;; For example:

;; In aA, a and A react, leaving nothing behind.
;; In abBA, bB destroys itself, leaving aA. As above, this then destroys itself,
;; leaving nothing.
;; In abAB, no two adjacent units are of the same type, and so nothing happens.
;; In aabAAB, even though aa and AA are of the same type, their polarities
;; match, and so nothing happens.
;; Now, consider a larger example, dabAcCaCBAcCcaDA:

;; dabAcCaCBAcCcaDA  The first 'cC' is removed.
;; dabAaCBAcCcaDA    This creates 'Aa', which is removed.
;; dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
;; dabCBAcaDA        No further actions can be taken.
;; After all possible reactions, the resulting polymer contains 10 units.

;; How many units remain after fully reacting the polymer you scanned?
;; (Note: in this puzzle and others, the input is large;
;; if you copy/paste your input, make sure you get the whole thing.)

;; --- Part Two ---
;; Time to improve the polymer.

;; One of the unit types is causing problems; it's preventing the polymer from
;; collapsing as much as it should. Your goal is to figure out which unit type
;; is causing the most problems, remove all instances of it (regardless of
;; polarity), fully react the remaining polymer, and measure its length.

;; For example, again using the polymer dabAcCaCBAcCcaDA from above:

;; Removing all A/a units produces dbcCCBcCcD.
;; Fully reacting this polymer produces dbCBcD, which has length 6.
;; Removing all B/b units produces daAcCaCAcCcaDA.
;; Fully reacting this polymer produces daCAcaDA, which has length 8.
;; Removing all C/c units produces dabAaBAaDA.
;; Fully reacting this polymer produces daDA, which has length 4.
;; Removing all D/d units produces abAcCaCBAcCcaA.
;; Fully reacting this polymer produces abCBAc, which has length 6.

;; In this example, removing all C/c units was best, producing the answer 4.

;; What is the length of the shortest polymer you can produce by removing all
;; units of exactly one type and fully reacting the result?

(defun trigger (polymer)
  "Returns the polymer after the reaction has been triggered"
  (interactive)
  (--trigger polymer 0))

(defun --trigger (polymer index)
  ""
  ()
  (while (< (+ index 1) (length polymer))
      polymer
    (let ((char (substring polymer index (+ index 1)))
           (next-char (substring polymer (+ index 1) (+ index 2))))
      (if (and (not (string-equal char next-char))
               (or (string-equal char (upcase next-char))
                   (string-equal (upcase char) next-char)))
          (setq polymer (concat (substring polymer 0 index) (substring polymer (+ index 2)))
                index (max 0 (- index 1)))
        (setq index (+ index 1)))))
  polymer)

(string-equal (trigger "aA") "")
(string-equal (trigger "abBA") "")
(string-equal (trigger "abAB") "abAB")
(string-equal (trigger "aabAAB") "aabAAB")
(string-equal (trigger "dabAcCaCBAcCcaDA") "dabCBAcaDA")

(defun puzzle-input ()
  "Returns the input of the puzzle"
  (with-temp-buffer
    (insert-file-contents "05_input")
    (s-trim (buffer-string))))

(length (trigger (puzzle-input)))

(defun optimize (polymer)
  "Optimize polymer by removing a single unit (e.g a/A, b/B, etc),
removing it from the polymer, and recording its triggered size.

This return polyer that reduces the most."
  (interactive)
  (let* ((units (s-split "" "abcdefghijklmnopqrstuvwxyz" t))
         (polymer-reductions (--map (s-replace (upcase it) "" (s-replace it "" polymer)) units))
         (triggered-reductions (--map (cons (length (trigger it)) it) polymer-reductions))
         (shortest-reduced (--min-by (> (car it) (car other)) triggered-reductions)))
    (car shortest-reduced)))

(eq (optimize polymer) 4)

(message "Part two has minimal reduction of %d" (optimize (puzzle-input)))
