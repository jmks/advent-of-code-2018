;; --- Day 6: Chronal Coordinates ---

;; The device on your wrist beeps several times, and once again you feel like
;; you're falling.

;; "Situation critical," the device announces. "Destination indeterminate.
;; Chronal interference detected. Please specify new target coordinates."

;; The device then produces a list of coordinates (your puzzle input). Are they
;; places it thinks are safe or dangerous? It recommends you check manual
;; page 729. The Elves did not give you a manual.

;; If they're dangerous, maybe you can minimize the danger by finding the
;; coordinate that gives the largest distance from the other points.

;; Using only the Manhattan distance, determine the area around each coordinate
;; by counting the number of integer X,Y locations that are closest to that
;; coordinate (and aren't tied in distance to any other coordinate).

;; Your goal is to find the size of the largest area that isn't infinite.
;; For example, consider the following list of coordinates:

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; If we name these coordinates A through F, we can draw them on a grid,
;; putting 0,0 at the top left:

;; ..........
;; .A........
;; ..........
;; ........C.
;; ...D......
;; .....E....
;; .B........
;; ..........
;; ..........
;; ........F.

;; This view is partial - the actual grid extends infinitely in all directions.
;; Using the Manhattan distance, each location's closest coordinate can be
;; determined, shown here in lowercase:

;; aaaaa.cccc
;; aAaaa.cccc
;; aaaddecccc
;; aadddeccCc
;; ..dDdeeccc
;; bb.deEeecc
;; bBb.eeee..
;; bbb.eeefff
;; bbb.eeffff
;; bbb.ffffFf

;; Locations shown as . are equally far from two or more coordinates,
;; and so they don't count as being closest to any.

;; In this example, the areas of coordinates A, B, C, and F are infinite -
;; while not shown here, their areas extend forever outside the visible grid.
;; However, the areas of coordinates D and E are finite: D is closest to 9
;; locations, and E is closest to 17 (both including the coordinate's location
;; itself). Therefore, in this example, the size of the largest area is 17.

;; What is the size of the largest area that isn't infinite?

(defun 06-example-input ()
  "Example input"
  (interactive)
  "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")

(defun parse-input (input)
  "Parse input from text to tuples (X . Y)"
  (interactive)
  (let* ((lines (s-split "\n" input 't))
         (string-lists (--map (s-split ", " it 't) lines))
         (tuples (--map (cons (string-to-number (car it)) (string-to-number (cadr it))) string-lists)))
    tuples))

(equal (parse-input (06-example-input)) '((1 . 1) (1 . 6) (8 . 3) (3 . 4) (5 . 5) (8 . 9)))

(defun horiztonal-range (points)
  "Returns the min and max of the range of tuples POINTS as (MIN . MAX)"
  (interactive)
  (cons (-min-by '> (-map 'car points)) (-max-by '> (-map 'car points))))

(defun vertical-range (points)
  "Returns the min and max of the range of tuples POINTS as (MIN . MAX)"
  (interactive)
  (cons (-min-by '> (-map 'cdr points)) (-max-by '> (-map 'cdr points))))

(equal (horiztonal-range (parse-input (06-example-input))) '(1 . 8))
(equal (vertical-range (parse-input (06-example-input))) '(1 . 9))

(defun manhattan-distance (h1 v1 h2 v2)
  "Manhattan distance between two points"
  (interactive)
  (+ (abs (- h1 h2)) (abs (- v1 v2))))

(equal (manhattan-distance 0 0 1 1) 2)
(equal (manhattan-distance 1 1 1 9) 8)

(defun make-grid (h-range v-range)
  "Produces a grid that is H-RANGE * V-RANGE.

Remembers the point (h . v) that was closet to it via grid/distance-to"
  (interactive)
  (let ((h-els (+ 1 (- (cdr h-range) (car h-range))))
        (v-els (+ 1 (- (cdr v-range) (car v-range)))))
    (list h-els h-range v-els v-range (make-vector (* h-els v-els) '()))))

(defun grid/length (grid)
  "Number of cells in the grid"
  (interactive)
  (length (car (cddddr grid))))

(eq (grid/length (make-grid (cons 1 3) (cons 1 5))) 15)

(defun grid/at (grid h v)
  "Get element at coordinates (H, V)."
  (interactive)
  (let* ((h-elements (car grid))
         (h-range (cadr grid))
         (v-range (cadddr grid))
         (vec (car (cddddr grid)))
         (index (+ (- h (car h-range)) (* h-elements (- v (car v-range))))))
    (aref vec index)))

(defun grid/update (grid h v identifier ih iv)
  "Update grid at point (H, V) with an identifer located at (IH, IV)

An identifer gets the position (H, V) if it is closer than any previous identifier.
If an identifer ties with the current position, no identifier gets the position and it is marked (TIED . DISTANCE)."
  (interactive)
  (let* ((h-elements (car grid))
        (h-range (cadr grid))
        (v-range (cadddr grid))
        (vec (car (cddddr grid)))
        (index (+ (- h (car h-range)) (* h-elements (- v (car v-range)))))
        (current-closest (aref vec index))
        (dist (manhattan-distance h v ih iv)))
    (if (null current-closest)
        (aset vec index (cons identifier dist))
      (let ((current-id (car current-closest))
            (current-dist (cdr current-closest)))
        (cond ((< dist current-dist)
               (aset vec index (cons identifier dist)))
              ((equal dist current-dist)
                (aset vec index (cons 'tied dist))))))))

(equal (let ((grid (make-grid (cons 1 3) (cons 1 5))))
         (grid/update grid 1 1 'apple 1 3)
         (grid/at grid 1 1)
         (grid/update grid 1 1 'banana 1 2)
         (grid/at grid 1 1))
       '(banana . 1))

(equal (let ((grid (make-grid (cons 1 3) (cons 1 5))))
         (grid/update grid 1 1 'apple 2 1)
         (grid/at grid 1 1)
         (grid/update grid 1 1 'banana 1 2)
         (grid/at grid 1 1))
       '(tied . 1))

(defun cartesian-product (it other)
  "Cartesian product of elemnts of IT x OTHER"
  (interactive)
  (let ((result '()))
    (loop for x in it do
          (loop for y in other do
                (setq result (cons (cons x y) result))))
    (reverse result)))

(equal (cartesian-product '(a b) '(1 2 3))
       '((a . 1) (a . 2) (a . 3) (b . 1) (b . 2) (b . 3)))

(defun grid/largest-identifier (grid ignored-identifiers)
  "Returns the identifier with the most positions where it is the closest"
  (interactive)
  (let* ((vec (car (cddddr grid)))
         (identifiers (--map (car it) vec))
         (grouped (-group-by 'identity identifiers))
         (filtered (--filter (and (not (null (car it))) (not (equal 'tied (car it)))) grouped))
         (non-ignored (--filter (not (-elem-index (car it) ignored-identifiers)) filtered))
         (max (--max-by (> (length it) (length other)) non-ignored)))
    (cons (car max) (length (cdr max)))))

(let* ((input (parse-input (06-example-input)))
      (h-range (horiztonal-range input))
      (v-range (vertical-range input))
      (grid (make-grid h-range v-range))
      (hrange (number-sequence (car h-range) (cdr h-range)))
      (vrange (number-sequence (car v-range) (cdr v-range)))
      (coordinates (cartesian-product hrange vrange)))
  (dolist (i input)
    (dolist (coord coordinates)
      (grid/update grid (car coord) (cdr coord) i (car i) (cdr i))))
  (grid/largest-identifier grid (processed-grid/infinite-identifiers grid))) ; (5 . 5) has 17 locations

(defun processed-grid/infinite-identifiers (grid)
  "Returns the identifiers that have infinite size.

If an identifier is the closest to a location on the boundary of the grid,
then it will be closest in that direction forever"
  (interactive)
  (let* ((h-range (cadr grid))
         (v-range (cadddr grid))
         (vec (car (cddddr grid)))
         (top (cartesian-product (list (car h-range)) (number-sequence (car v-range) (cdr v-range))))
         (bottom (cartesian-product (list (cdr h-range)) (number-sequence (car v-range) (cdr v-range))))
         (left (cartesian-product (number-sequence (car h-range) (cdr h-range)) (list (car v-range))))
         (right (cartesian-product (number-sequence (car h-range) (cdr h-range)) (list (cdr v-range))))
         (identifiers '()))
    (dolist (edge (-concat top bottom left right))
      (let ((identifier (grid/at grid (car edge) (cdr edge))))
        (if (not (equal 'tied (car identifier)))
            (setq identifiers (cons (car identifier) identifiers)))))
    (-uniq identifiers)))

(let* ((input (parse-input (06-example-input)))
       (h-range (horiztonal-range input))
       (v-range (vertical-range input))
       (grid (make-grid h-range v-range))
       (hrange (number-sequence (car h-range) (cdr h-range)))
       (vrange (number-sequence (car v-range) (cdr v-range)))
       (coordinates (cartesian-product hrange vrange)))
  (dolist (i input)
    (dolist (coord coordinates)
      (grid/update grid (car coord) (cdr coord) i (car i) (cdr i))))
  (processed-grid/infinite-identifiers grid))

(defun puzzle-input ()
  "Returns the input of the puzzle"
  (with-temp-buffer
    (insert-file-contents "06_input")
    (s-trim (buffer-string))))

(defun solve-part-one ()
  "Solve part one"
  (interactive)
  (let* ((input (parse-input (puzzle-input)))
         (h-range (horiztonal-range input))
         (v-range (vertical-range input))
         (grid (make-grid h-range v-range))
         (hrange (number-sequence (car h-range) (cdr h-range)))
         (vrange (number-sequence (car v-range) (cdr v-range)))
         (coordinates (cartesian-product hrange vrange)))
    (dolist (i input)
      (dolist (coord coordinates)
        (grid/update grid (car coord) (cdr coord) i (car i) (cdr i))))
    (let* ((inf-ids (processed-grid/infinite-identifiers grid))
           (largest-finite-identifer (grid/largest-identifier grid inf-ids))
           (identifier (car largest-finite-identifer))
           (size (cdr identifier)))
      size)))

(solve-part-one)
