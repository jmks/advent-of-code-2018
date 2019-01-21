; --- Day 3: No Matter How You Slice It ---

; The Elves managed to locate the chimney-squeeze prototype fabric for Santa's suit (thanks to someone who helpfully
; wrote its box IDs on the wall of the warehouse in the middle of the night). Unfortunately, anomalies are still
; affecting them - nobody can even agree on how to cut the fabric.

; The whole piece of fabric they're working on is a very large square - at least 1000 inches on each side.

; Each Elf has made a claim about which area of fabric would be ideal for Santa's suit. All claims have an ID and
; consist of a single rectangle with edges parallel to the edges of the fabric. Each claim's rectangle is defined as
; follows:

; The number of inches between the left edge of the fabric and the left edge of the rectangle.
; The number of inches between the top edge of the fabric and the top edge of the rectangle.

; The width of the rectangle in inches.
; The height of the rectangle in inches.

; A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from
; the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square inches of fabric represented by # (and
; ignores the square inches of fabric represented by .) in the diagram below:

; ...........
; ...........
; ...#####...
; ...#####...
; ...#####...
; ...#####...
; ...........
; ...........
; ...........

; The problem is that many of the claims overlap, causing two or more claims to cover part of the same areas. For
; example, consider the following claims:

; #1 @ 1,3: 4x4
; #2 @ 3,1: 4x4
; #3 @ 5,5: 2x2
; Visually, these claim the following areas:

; ........
; ...2222.
; ...2222.
; .11XX22.
; .11XX22.
; .111133.
; .111133.
; ........

; The four square inches marked with X are claimed by both 1 and 2. (Claim 3, while adjacent to the others, does not
; overlap either of them.)

; If the Elves all proceed with their own plans, none of them will have enough fabric. How many square inches of fabric
; are within two or more claims?

;; how to make a record?
(defun claim-parse (claim)
  (let ((result (string-match "#\\(.*\\) @ \\(.*\\)\,\\(.*\\): \\(.*\\)x\\(.*\\)" claim 0))
        (number (string-to-int (match-string 1 claim)))
        (left-edge (string-to-int (match-string 2 claim)))
        (top-edge (string-to-int (match-string 3 claim)))
        (width (string-to-int (match-string 4 claim)))
        (height (string-to-int (match-string 5 claim))))
    (list (cons 'number number) (cons 'left-edge left-edge) (cons 'top-edge top-edge) (cons 'width width) (cons 'height height))))

(ert-deftest claim-parse-test ()
  ""
  (should (equal '((number . 2) (left-edge . 3) (top-edge . 1) (width . 4) (height . 4)) (claim-parse "#2 @ 3,1: 4x4")))
  (should (equal '((number . 123) (left-edge . 3) (top-edge . 2) (width . 5) (height . 4)) (claim-parse "#123 @ 3,2: 5x4"))))

(defun claim-squares (claim)
  "Returns the squares covered by a claim"
  (let* ((claim-alist (claim-parse claim))
         (left (alist-get 'left-edge claim-alist))
         (top (alist-get 'top-edge claim-alist))
         (width (alist-get 'width claim-alist))
         (height (alist-get 'height claim-alist))
         (width-range (list/range left width))
         (height-range (list/range top height)))
    (mapcan (lambda (height) (list/zip width-range (list/replicate height (length width-range)))) height-range)))

(ert-deftest claim-squares-test ()
  ""
  (should (equal '((1 . 3)) (claim-squares "#1 @ 1,3: 1x1")))
  (should (equal '((1 . 1) (1 . 2) (1 . 3)) (claim-squares "#1 @ 1,1: 1x3")))
  (should (equal '((3 . 2) (4 . 2) (5 . 2) (6 . 2) (7 . 2) (3 . 3) (4 . 3) (5 . 3) (6 . 3) (7 . 3) (3 . 4) (4 . 4) (5 . 4) (6 . 4) (7 . 4) (3 . 5) (4 . 5) (5 . 5) (6 . 5) (7 . 5)) (claim-squares "#2 @ 3,2: 5x4"))))

(defun squares-within-at-least-two-claims (claims)
  (let ((squares (mapcan 'claim-squares claims))
        (counts (make-hash-table :test 'equal))
        (more-than-two 0))
    (dolist (square squares)
      (puthash square (+ 1 (gethash square counts 0)) counts))
    (maphash (lambda (key count)
               (if (>= count 2)
                   (setq more-than-two (+ 1 more-than-two))))
             counts)
    more-than-two))

(ert-deftest overlapping-squares-test ()
  ""
  (should (equal (squares-within-at-least-two-claims '("#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2")) 4)))

(defun read-puzzle ()
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents "03_input")
    (split-string (buffer-string) "\n" t)))

(squares-within-at-least-two-claims (read-puzzle)) ; 110546
