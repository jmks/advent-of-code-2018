;; --- Day 10: The Stars Align ---

;; It's no use; your navigation system simply isn't capable of providing walking directions in the arctic circle, and certainly not in 1018.

;; The Elves suggest an alternative. In times like these, North Pole rescue operations will arrange points of light in the sky to guide missing Elves back to base.
;; Unfortunately, the message is easy to miss: the points move slowly enough that it takes hours to align them, but have so much momentum that they only stay aligned for a second.
;; If you blink at the wrong time, it might be hours before another message appears.

;; You can see these points of light floating in the distance, and record their position in the sky and their velocity, the relative change in position per second (your puzzle input).
;; The coordinates are all given from your perspective; given enough time, those positions and velocities will move the points into a cohesive message!

;; Rather than wait, you decide to fast-forward the process and calculate what the points will eventually spell.

;; For example, suppose you note the following points:

;; position=< 9,  1> velocity=< 0,  2>
;; position=< 7,  0> velocity=<-1,  0>
;; position=< 3, -2> velocity=<-1,  1>
;; position=< 6, 10> velocity=<-2, -1>
;; position=< 2, -4> velocity=< 2,  2>
;; position=<-6, 10> velocity=< 2, -2>
;; position=< 1,  8> velocity=< 1, -1>
;; position=< 1,  7> velocity=< 1,  0>
;; position=<-3, 11> velocity=< 1, -2>
;; position=< 7,  6> velocity=<-1, -1>
;; position=<-2,  3> velocity=< 1,  0>
;; position=<-4,  3> velocity=< 2,  0>
;; position=<10, -3> velocity=<-1,  1>
;; position=< 5, 11> velocity=< 1, -2>
;; position=< 4,  7> velocity=< 0, -1>
;; position=< 8, -2> velocity=< 0,  1>
;; position=<15,  0> velocity=<-2,  0>
;; position=< 1,  6> velocity=< 1,  0>
;; position=< 8,  9> velocity=< 0, -1>
;; position=< 3,  3> velocity=<-1,  1>
;; position=< 0,  5> velocity=< 0, -1>
;; position=<-2,  2> velocity=< 2,  0>
;; position=< 5, -2> velocity=< 1,  2>
;; position=< 1,  4> velocity=< 2,  1>
;; position=<-2,  7> velocity=< 2, -2>
;; position=< 3,  6> velocity=<-1, -1>
;; position=< 5,  0> velocity=< 1,  0>
;; position=<-6,  0> velocity=< 2,  0>
;; position=< 5,  9> velocity=< 1, -2>
;; position=<14,  7> velocity=<-2,  0>
;; position=<-3,  6> velocity=< 2, -1>

;; Each line represents one point.
;; Positions are given as <X, Y> pairs: X represents how far left (negative) or right (positive) the point appears, while Y represents how far up (negative) or down (positive) the point appears.

;; At 0 seconds, each point has the position given. Each second, each point's velocity is added to its position.
;; So, a point with velocity <1, -2> is moving to the right, but is moving upward twice as quickly. If this point's initial position were <3, 9>, after 3 seconds, its position would become <6, 3>.

;; Over time, the points listed above would move like this:

;; Initially:
;; ........#.............
;; ................#.....
;; .........#.#..#.......
;; ......................
;; #..........#.#.......#
;; ...............#......
;; ....#.................
;; ..#.#....#............
;; .......#..............
;; ......#...............
;; ...#...#.#...#........
;; ....#..#..#.........#.
;; .......#..............
;; ...........#..#.......
;; #...........#.........
;; ...#.......#..........

;; After 1 second:
;; ......................
;; ......................
;; ..........#....#......
;; ........#.....#.......
;; ..#.........#......#..
;; ......................
;; ......#...............
;; ....##.........#......
;; ......#.#.............
;; .....##.##..#.........
;; ........#.#...........
;; ........#...#.....#...
;; ..#...........#.......
;; ....#.....#.#.........
;; ......................
;; ......................

;; After 2 seconds:
;; ......................
;; ......................
;; ......................
;; ..............#.......
;; ....#..#...####..#....
;; ......................
;; ........#....#........
;; ......#.#.............
;; .......#...#..........
;; .......#..#..#.#......
;; ....#....#.#..........
;; .....#...#...##.#.....
;; ........#.............
;; ......................
;; ......................
;; ......................

;; After 3 seconds:
;; ......................
;; ......................
;; ......................
;; ......................
;; ......#...#..###......
;; ......#...#...#.......
;; ......#...#...#.......
;; ......#####...#.......
;; ......#...#...#.......
;; ......#...#...#.......
;; ......#...#...#.......
;; ......#...#..###......
;; ......................
;; ......................
;; ......................
;; ......................

;; After 4 seconds:
;; ......................
;; ......................
;; ......................
;; ............#.........
;; ........##...#.#......
;; ......#.....#..#......
;; .....#..##.##.#.......
;; .......##.#....#......
;; ...........#....#.....
;; ..............#.......
;; ....#......#...#......
;; .....#.....##.........
;; ...............#......
;; ...............#......
;; ......................
;; ......................

;; After 3 seconds, the message appeared briefly: HI. Of course, your message will be much longer and will take many more seconds to appear.

;; What message will eventually appear in the sky?

(require 'rx)

(cl-defstruct coord x y)
(cl-defstruct point position velocity)

(cl-defun input/parse-point (line)
  (let ((position-rx (rx "position=<" (zero-or-more space) (group (one-or-more (any digit "-"))) "," (zero-or-more space) (group (one-or-more (any "-" digit))) ">"))
        (velocity-rx (rx "velocity=<" (zero-or-more space) (group (one-or-more (any digit "-"))) "," (zero-or-more space) (group (one-or-more (any digit "-"))) ">")))
    (string-match (concat position-rx " " velocity-rx) line)
    (make-point :position (make-coord :x (string-to-number (match-string 1 line))
                                      :y (string-to-number (match-string 2 line)))
                :velocity (make-coord :x (string-to-number (match-string 3 line))
                                      :y (string-to-number (match-string 4 line))))))

(input/parse-point "position=< 9,  1> velocity=< 0,  2>")
(input/parse-point "position=< 7,  0> velocity=<-1,  0>")
(input/parse-point "position=< 3, -2> velocity=<-1,  1>")
(input/parse-point "position=< 6, 10> velocity=<-2, -1>")

(cl-defun puzzle/example ()
  "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>
")

(cl-defun example/points ()
  (-map 'input/parse-point (s-split "\n" (puzzle/example) 't)))

(example/points)

(cl-defun points/print (points)
  (let* ((coords (-map 'point-position points))
         (xpos (-map 'coord-x coords))
         (ypos (-map 'coord-y coords))
         (xmin (-min xpos))
         (xmax (-max xpos))
         (ymin (-min ypos))
         (ymax (-max ypos))
         (y (- ymin 2))
         (rows '()))
    (while (<= y (+ ymax 2))
      (let ((x (- xmin 2))
            (row '()))
        (while (<= x (+ xmax 2))
          (if (-elem-index (make-coord :x x :y y) coords)
              (setq row (cons "#" row))
            (setq row (cons "." row)))
          (setq x (+ x 1)))
        (let ((str (string-join (reverse row) "")))
          (message "%s" str)
          (setq rows (cons (reverse row) rows))))
      (setq y (+ y 1)))
    (message "%s" (reverse rows))))

(points/print (example/points))

(cl-defun step (points)
  (--map (make-point :position (make-coord :x (+ (coord-x (point-position it)) (coord-x (point-velocity it)))
                                           :y (+ (coord-y (point-position it)) (coord-y (point-velocity it))))
                     :velocity (point-velocity it))
         points))

(cl-defun points/area (points)
  (let* ((positions (-map 'point-position points))
         (xpos (-map 'coord-x positions))
         (ypos (-map 'coord-y positions))
         (xmin (-min xpos))
         (xmax (-max xpos))
         (ymin (-min ypos))
         (ymax (-max ypos)))
    (* (- xmax xmin) (- ymax ymin))))

(cl-defun print-when-smallet (points)
  (let ((last-points points)
        (last-area (points/area points)))
    (while (<= (points/area points) last-area)
      (setq last-points points
            last-area (points/area points)
            points (step points)))
    (points/print last-points)
    last-points))

(print-when-smallet (example/points))

(cl-defun puzzle-input ()
  (with-temp-buffer
    (insert-file-contents "10_input")
    (-map 'input/parse-point (s-split "\n" (s-trim (buffer-string)) 't))))

(print-when-smallet (puzzle-input))
