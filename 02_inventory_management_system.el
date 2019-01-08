; --- Day 2: Inventory Management System ---
; You stop falling through time, catch your breath, and check the screen on the device. "Destination reached. Current
; Year: 1518. Current Location: North Pole Utility Closet 83N10." You made it! Now, to find those anomalies.

; Outside the utility closet, you hear footsteps and a voice. "...I'm not sure either. But now that so many people have
; chimneys, maybe he could sneak in that way?" Another voice responds, "Actually, we've been working on a new kind of
; suit that would let him fit through tight spaces like that. But, I heard that a few days ago, they lost the prototype
; fabric, the design plans, everything! Nobody on the team can even seem to remember important details of the project!"

; "Wouldn't they have had enough fabric to fill several boxes in the warehouse? They'd be stored together, so the box
; IDs should be similar. Too bad it would take forever to search the warehouse for two similar box IDs..." They walk too
; far away to hear any more.

; Late at night, you sneak to the warehouse - who knows what kinds of paradoxes you could cause if you were discovered -
; and use your fancy wrist device to quickly scan every box and produce a list of the likely candidates (your puzzle
; input).

; To make sure you didn't miss any, you scan the likely candidate boxes again, counting the number that have an ID
; containing exactly two of any letter and then separately counting those with exactly three of any letter. You can
; multiply those two counts together to get a rudimentary checksum and compare it to what your device predicts.

; For example, if you see the following box IDs:

; abcdef contains no letters that appear exactly two or three times.
; bababc contains two a and three b, so it counts for both.
; abbcde contains two b, but no letter appears exactly three times.
; abcccd contains three c, but no letter appears exactly two times.
; aabcdd contains two a and two d, but it only counts once.
; abcdee contains two e.
; ababab contains three a and three b, but it only counts once.
; Of these box IDs, four of them contain a letter which appears exactly twice, and three of them contain a letter which
; appears exactly three times. Multiplying these together produces a checksum of 4 * 3 = 12.

; What is the checksum for your list of box IDs?

(defun read-puzzle ()
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents "02_input")
    (split-string (buffer-string) "\n" t)))

(defun letter-frequency (word)
  "Returns list of (character . count) for the characters in WORD"
  (let ((chars (sort (split-string word "" t) 'string<))
        (reducer (lambda (acc x)
                   (if (assoc x acc)
                       (progn
                         (setcdr (assoc x acc) (+ (cdr (assoc x acc)) 1))
                         acc)
                     (cons (cons x 1) acc)))))
    (reduce reducer chars :initial-value '())))

(defun frequency-with-value (frequencies count)
  "Filters an association list by those that match the VALUE"
  (reduce (lambda (acc frequency)
            (let ((letter (car frequency))
                  (freq (cdr frequency)))
              (if (= count freq)
                  (cons letter acc)
                acc))
            ) frequencies :initial-value '()))

(defun duplicate-and-triplicate-counts (word)
  "Calculates a tuple (duplicate-count . triplicate-count) for the given WORD"
  (let ((freqs (letter-frequency word)))
    (cons (frequency-with-value freqs 2) (frequency-with-value freqs 3))))

(defun checksum (box-ids)
  "Returns the checksum for the given BOX-IDS"
  (let ((counts (reduce (lambda (acc count)
                              (cons (+ (car acc) (if (not (null (car count))) 1 0)) (+ (cdr acc) (if (not (null (cdr count))) 1 0))))
                      (seq-map 'duplicate-and-triplicate-counts box-ids)
                      :initial-value (cons 0 0))))
    (* (car counts) (cdr counts))))

(= 12 (checksum '("abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab")))

(ert-deftest duplicate-and-triplicate-counts-test ()
  "Tests duplicate-and-triplicate-counts"
  (should (equal (duplicate-and-triplicate-counts "abcdef") '(() . ())))
  (should (equal (duplicate-and-triplicate-counts "bababc") '(("a") . ("b"))))
  (should (equal (duplicate-and-triplicate-counts "abbcde") '(("b") . ())))
  (should (equal (duplicate-and-triplicate-counts "abcccd") '(() . ("c"))))
  (should (equal (duplicate-and-triplicate-counts "aabcdd") '(("a" "d") . ())))
  (should (equal (duplicate-and-triplicate-counts "abcdee") '(("e") . ())))
  (should (equal (duplicate-and-triplicate-counts "ababab") '(() . ("a" "b")))))

(checksum (read-puzzle))

; --- Part Two ---
; Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.

; The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given
; the following box IDs:

; abcde
; fghij
; klmno
; pqrst
; fguij
; axcye
; wvxyz

; The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij
; and fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.

; What letters are common between the two correct box IDs? (In the example above, this is found by removing the
; differing character from either ID, producing fgij.)

(defun string-distance (string-1 string-2)
  "Calculates the number of differences between the two strings"
  (reduce (lambda (differences letters)
            (if (equal (car letters) (cdr letters))
                differences
              (+ differences 1)))
          (zip (split-string string-1 "" t) (split-string string-2 "" t))
          :initial-value 0))

(ert-deftest string-distance-tests ()
  (should (equal 2 (string-distance "abcde" "axcye")))
  (should (equal 1 (string-distance "fghij" "fguij"))))

(defun common-letters-between (word other)
  (reverse (reduce (lambda (common letters)
                     (if (equal (car letters) (cdr letters))
                         (cons (car letters) common)
                       common))
                   (zip (split-string word "" t) (split-string other "" t))
                   :initial-value '())))

(defun common-letters (box-ids)
  "Returns the common letters between the two box IDs that differ by 1 character"
  (let ((id-pairs (list/all-pairs box-ids)))
    (catch 'return
      (while id-pairs
        (let ((pair (car id-pairs)))
          (if (= 1 (string-distance (car pair) (cdr pair)))
              (throw 'return (common-letters-between (car pair) (cdr pair)))
            (setq id-pairs (cdr id-pairs))))))))

(ert-deftest common-letters-test ()
  "Tests common-letters"
  (should (equal "fgij" (common-letters '("abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz")))))

(common-letters (read-puzzle))
