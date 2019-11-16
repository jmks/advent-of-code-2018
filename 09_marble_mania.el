;; --- Day 9: Marble Mania ---

;; You talk to the Elves while you wait for your navigation system to initialize.
;; To pass the time, they introduce you to their favorite marble game.

;; The Elves play this game by taking turns arranging the marbles in a circle according to very particular rules.
;; The marbles are numbered starting with 0 and increasing by 1 until every marble has a number.

;; First, the marble numbered 0 is placed in the circle. At this point, while it contains only a single marble,
;; it is still a circle: the marble is both clockwise from itself and counter-clockwise from itself.
;; This marble is designated the current marble.

;; Then, each Elf takes a turn placing the lowest-numbered remaining marble into the circle between the marbles
;; that are 1 and 2 marbles clockwise of the current marble.
;; (When the circle is large enough, this means that there is one marble between the marble that was just placed and the current marble.)
;; The marble that was just placed then becomes the current marble.

;; However, if the marble that is about to be placed has a number which is a multiple of 23, something entirely different happens.
;; First, the current player keeps the marble they would have placed, adding it to their score.
;; In addition, the marble 7 marbles counter-clockwise from the current marble is removed from the circle
;; and also added to the current player's score. The marble located immediately clockwise of the marble
;; that was removed becomes the new current marble.

;; For example, suppose there are 9 players. After the marble with value 0 is placed in the middle,
;; each player (shown in square brackets) takes a turn. The result of each of those turns would produce
;; circles of marbles like this, where clockwise is to the right and the resulting current marble is in parentheses:

;; [-] (0)
;; [1]  0 (1)
;; [2]  0 (2) 1
;; [3]  0  2  1 (3)
;; [4]  0 (4) 2  1  3
;; [5]  0  4  2 (5) 1  3
;; [6]  0  4  2  5  1 (6) 3
;; [7]  0  4  2  5  1  6  3 (7)
;; [8]  0 (8) 4  2  5  1  6  3  7
;; [9]  0  8  4 (9) 2  5  1  6  3  7
;; [1]  0  8  4  9  2(10) 5  1  6  3  7
;; [2]  0  8  4  9  2 10  5(11) 1  6  3  7
;; [3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
;; [4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
;; [5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
;; [6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
;; [7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
;; [8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
;; [9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
;; [1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
;; [2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
;; [3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
;; [4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
;; [5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
;; [6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
;; [7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

;; The goal is to be the player with the highest score after the last marble is used up.
;; Assuming the example above ends after the marble numbered 25, the winning score is 23+9=32
;; (because player 5 kept marble 23 and removed marble 9, while no other player got any points in this very short example game).

;; Here are a few more examples:

;; 10 players; last marble is worth 1618 points: high score is 8317
;; 13 players; last marble is worth 7999 points: high score is 146373
;; 17 players; last marble is worth 1104 points: high score is 2764
;; 21 players; last marble is worth 6111 points: high score is 54718
;; 30 players; last marble is worth 5807 points: high score is 37305

;; What is the winning Elf's score?

(require 'cl-lib)
(require 'dash)

(cl-defun puzzle/input ()
  "424 players; last marble is worth 71144 points"
  '(424 . 71144))

(cl-defun puzzle/example ()
  '(9 . 25))

(cl-defstruct (game (:copier nil))
  final-marble player-scores next-player root-marble next-marble current-marble)

(cl-defstruct marble-node
  marble next prev)

(cl-defun marble-node/clockwise (node elements)
  (if (equal elements 0)
      node
    (marble-node/clockwise (marble-node-next node) (- elements 1))))

(cl-defun marble-node/counter-clockwise (node elements)
  (if (equal elements 0)
      node
    (marble-node/counter-clockwise (marble-node-prev node) (- elements 1))))

(let* ((root (make-marble-node :marble 0))
       (one (make-marble-node :marble 1 :prev root))
       (two (make-marble-node :marble 2 :prev one :next root)))
  (setf (marble-node-next root) one
        (marble-node-prev root) two
        (marble-node-next one) two)
  (marble-node/counter-clockwise root 2))

(cl-defun game/new (players-count final-marble)
  (let ((root (make-marble-node :marble 0)))
    (setf (marble-node-next root) root
          (marble-node-prev root) root)
    (make-game :final-marble final-marble
               :player-scores (make-vector (+ players-count 1) 0)
               :next-player 1
               :next-marble 1
               :root-marble root
               :current-marble root)))

(game/new 9 25)

(cl-defun game/place-marble (game)
  (if (equal (mod (game-next-marble game) 23) 0)
      (game/scoring-move game)
    (game/placing-move game)))

(cl-defun game/next-player (current-player max-players)
  (if (equal current-player max-players)
      1
    (+ current-player 1)))

(game/next-player 1 2)
(game/next-player 2 2)

(cl-defun game/current-player (next-player max-players)
  (if (equal next-player 1)
      max-players
    (- next-player 1)))

(game/current-player 2 9)
(game/current-player 1 9)

(cl-defun game/players (game)
  (- (length (game-player-scores game)) 1))

(cl-defun game/placing-move (game)
  ;; insert new marble between 1 and 2 marbles clockwise from current marble
  (let* ((one-marble (marble-node/clockwise (game-current-marble game) 1))
         (two-marble (marble-node/clockwise (game-current-marble game) 2))
         (new-marble (make-marble-node :marble (game-next-marble game)
                                       :prev one-marble
                                       :next two-marble)))
    (setf (marble-node-next one-marble) new-marble
          (marble-node-prev two-marble) new-marble)

    (make-game :final-marble (game-final-marble game)
               :player-scores (game-player-scores game)
               :next-player (game/next-player (game-next-player game) (game/players game))
               :next-marble (+ (game-next-marble game) 1)
               :root-marble (game-root-marble game)
               :current-marble new-marble)))

(cl-defun game/scoring-move (game)
  (let* ((removed-marble (marble-node/counter-clockwise (game-current-marble game) 7))
         (prev (marble-node-prev removed-marble))
         (next (marble-node-next removed-marble)))
    (setf (marble-node-next prev) next
          (marble-node-prev next) prev)
    (aset (game-player-scores game)
          (game-next-player game)
          (+ (game-next-marble game)
             (marble-node-marble removed-marble)
             (aref (game-player-scores game) (game-next-player game))))

    (make-game :final-marble (game-final-marble game)
               :player-scores (game-player-scores game)
               :next-player (game/next-player (game-next-player game) (game/players game))
               :next-marble (+ (game-next-marble game) 1)
               :root-marble (game-root-marble game)
               :current-marble next)))

(game/place-marble (game/new 1 0))
(game/place-marble (game/new 1 1))

(cl-defun marble-node/print-node (node current)
  (if (equal node current)
      (format "(%s)" (marble-node-marble node))
    (number-to-string (marble-node-marble node))))

(marble-node/print-node (make-marble-node :marble 0) (make-marble-node :marble 0))
(marble-node/print-node (make-marble-node :marble 0) (make-marble-node :marble 1))

(cl-defun marble-node/print (node current)
  (let ((nodes '())
        (printing node))
    ;; should be able to eliminate this special case?
    (setq nodes (cons (marble-node/print-node printing current) nodes)
          printing (marble-node-next printing))

    (while (not (equal printing node))
      (setq nodes (cons (marble-node/print-node printing current) nodes)
            printing (marble-node-next printing)))
    (string-join (reverse nodes) " ")))

(let* ((root (make-marble-node :marble 0))
       (one (make-marble-node :marble 1 :prev root))
       (two (make-marble-node :marble 2 :prev one :next root)))
  (setf (marble-node-next root) one)
  (setf (marble-node-next one) two)
  (marble-node/print root two))

(cl-defun game/play (game)
  (let ((move 0))
    (while (< move (game-final-marble game))
      (setq game (game/place-marble game)
            move (+ move 1)))
    game))

(cl-defun game/print (game)
  (if (equal (game-next-marble game) 1)
      (string-join (list "[-]"
                         (marble-node/print (game-root-marble game) (game-current-marble game)))
                   " ")
    (string-join (list (format "[%s]" (game/current-player (game-next-player game) (game/players game)))
                       (marble-node/print (game-root-marble game) (game-current-marble game)))
                 " ")))

(cl-defun game/winning-score (game)
  (-max (mapcar (lambda (x) x) (game-player-scores game))))

(game/winning-score (game/play (game/new 9 25)))

(cl-defun examples ()
  (list '(10 1618 8317)
        '(10 1618 8317)
        '(13 7999 146373)
        '(17 1104 2764)
        '(21 6111 54718)
        '(30 5807 37305)))

(--map (equal (game/winning-score (game/play (game/new (car it) (cadr it))))
              (caddr it))
       (examples))

(game/winning-score (game/play (game/new (car (puzzle/input)) (cdr (puzzle/input)))))

;; --- Part Two ---

;; Amused by the speed of your answer, the Elves are curious:

;; What would the new winning Elf's score be if the number of the last marble were 100 times larger?

(game/winning-score (game/play (game/new (car (puzzle/input)) (* 100 (cdr (puzzle/input))))))
