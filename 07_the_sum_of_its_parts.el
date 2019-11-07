;; --- Day 7: The Sum of Its Parts ---

;; You find yourself standing on a snow-covered coastline; apparently, you landed a little off course.
;; The region is too hilly to see the North Pole from here, but you do spot some Elves
;; that seem to be trying to unpack something that washed ashore.
;; It's quite cold out, so you decide to risk creating a paradox by asking them for directions.

;; "Oh, are you the search party?" Somehow, you can understand whatever Elves
;; from the year 1018 speak; you assume it's Ancient Nordic Elvish. Could the
;; device on your wrist also be a translator? "Those clothes don't look very
;; warm; take this." They hand you a heavy coat.

;; "We do need to find our way back to the North Pole, but we have higher priorities at the moment.
;; You see, believe it or not, this box contains something that will solve all of Santa's transportation problems -
;; at least, that's what it looks like from the pictures in the instructions."
;; It doesn't seem like they can read whatever language it's in, but you can: "Sleigh kit. Some assembly required."

;; "'Sleigh'? What a wonderful name! You must help us assemble this 'sleigh' at once!"
;; They start excitedly pulling more parts out of the box.

;; The instructions specify a series of steps and requirements about which steps must be
;; finished before others can begin (your puzzle input).
;; Each step is designated by a single letter.
;; For example, suppose you have the following instructions:

;; Step C must be finished before step A can begin.
;; Step C must be finished before step F can begin.
;; Step A must be finished before step B can begin.
;; Step A must be finished before step D can begin.
;; Step B must be finished before step E can begin.
;; Step D must be finished before step E can begin.
;; Step F must be finished before step E can begin.

;; Visually, these requirements look like this:

;;   -->A--->B--
;;  /    \      \
;; C      -->D----->E
;;  \           /
;;   ---->F-----

;; Your first goal is to determine the order in which the steps should be completed.
;; If more than one step is ready, choose the step which is first alphabetically.

;; In this example, the steps would be completed as follows:

;; Only C is available, and so it is done first.
;; Next, both A and F are available. A is first alphabetically, so it is done next.
;; Then, even though F was available earlier, steps B and D are now also available, and B is the first alphabetically of the three.
;; After that, only D and F are available. E is not available because only some of its prerequisites are complete. Therefore, D is completed next.
;; F is the only choice, so it is done next.
;; Finally, E is completed.

;; So, in this example, the correct order is CABDFE.

;; In what order should the steps in your instructions be completed?

(require 'rx)
(require 'dash)

(defun example-input ()
  ""
  (interactive)
  "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(defun puzzle-input ()
  "Returns the input of the puzzle"
  (with-temp-buffer
    (insert-file-contents "07_input")
    (s-trim (buffer-string))))

(defun input/parse (line)
  "
Parse line into (ID . ID)

e.g. Step X must be finished before step Q can begin.
(Q '(X))
"
  (interactive)
  (when (string-match
         (rx "Step " (group (any "A-Z")) " must be finished before step " (group (any "A-Z")) " can begin.")
         line)
    (cons (match-string 2 line) (match-string 1 line))))

(equal (input/parse "Step C must be finished before step A can begin.") (cons "A" "C"))

(defun input/parse-dependencies (text)
  "Parses text into alist of dependencies
((ID . (ID-DEPS)) ...)
"
  (interactive)
  (let ((lines (s-split "\n" text 't)))
    (->> lines
         (-map 'input/parse)
         (-group-by 'car)
         (--map (cons (car it) (-map 'cdr (cdr it)))))))

(input/parse-dependencies (example-input))
(input/parse-dependencies (puzzle-input))

(defun input/step (deps)
  "Return the highest step (A-Z) in the deps"
  (interactive)
  (car (last (sort (-flatten deps) 'string-lessp))))

(defun input/build (text)
  "Parses text into dependency alist"
  (interactive)
  (let* ((deps (input/parse-dependencies text))
         (max-step (input/step deps))
         (steps (--take-while (string-lessp it max-step)
                              (-map 'char-to-string (number-sequence 65 90)))))
    (loop for step in steps do
          (unless (assoc step deps)
            (setq deps (cons (cons step '()) deps))))
    deps))

(input/build (example-input))
(input/build (puzzle-input))

(defun deps/next-step (deps)
  "Selects the next step to perform.
The next step has no dependencies and is the lowest alphabetical character"
  (interactive)
  (let* ((no-deps (--filter (null (cdr it)) deps))
         (steps (-map 'car no-deps))
         (sorted (cl-sort steps 'string-lessp)))
    (car sorted)))

(deps/next-step '(("B" "C") ("A")))
(deps/next-step '(("C") ("A")))

(defun deps/apply (deps step)
  "Return new list of dependencies after STEP is done"
  (interactive)
  (-reduce-from (lambda (acc dep)
                  (if (equal (car dep) step)
                      acc
                    (cons (cons (car dep)
                                (-remove-item step (cdr dep)))
                          acc)))
                '()
                deps))

(deps/apply '(("B" "D" "C") ("A" "C") ("C")) "C")

(defun solve (text)
  "Returns the order of the steps performed"
  (interactive)
  (let ((deps (input/build text))
        (steps '()))
    (while deps
      (let* ((next-step (deps/next-step deps))
             (new-deps (deps/apply deps next-step)))
        (setq steps (cons next-step steps)
              deps new-deps)))
    (s-join "" (reverse steps))))

(equal (solve (example-input)) "CABDFE")

;; --- Part Two ---

;; As you're about to begin construction, four of the Elves offer to help.
;; "The sun will set soon; it'll go faster if we work together."
;; Now, you need to account for multiple people working on steps simultaneously.
;; If multiple steps are available, workers should still begin them in alphabetical order.

;; Each step takes 60 seconds plus an amount corresponding to its letter: A=1, B=2, C=3, and so on.
;; So, step A takes 60+1=61 seconds, while step Z takes 60+26=86 seconds.
;; No time is required between steps.

;; To simplify things for the example, however, suppose you only have help from one Elf (a total of two workers)
;; and that each step takes 60 fewer seconds (so that step A takes 1 second and step Z takes 26 seconds).
;; Then, using the same instructions as above, this is how each second would be spent:

;; Second   Worker 1   Worker 2   Done
;;    0        C          .
;;    1        C          .
;;    2        C          .
;;    3        A          F       C
;;    4        B          F       CA
;;    5        B          F       CA
;;    6        D          F       CAB
;;    7        D          F       CAB
;;    8        D          F       CAB
;;    9        D          .       CABF
;;   10        E          .       CABFD
;;   11        E          .       CABFD
;;   12        E          .       CABFD
;;   13        E          .       CABFD
;;   14        E          .       CABFD
;;   15        .          .       CABFDE

;; Each row represents one second of time.
;; The Second column identifies how many seconds have passed as of the beginning of that second.
;; Each worker column shows the step that worker is currently doing (or . if they are idle).
;; The Done column shows completed steps.

;; Note that the order of the steps has changed; this is because steps now take time to finish
;; and multiple workers can begin multiple steps simultaneously.

;; In this example, it would take 15 seconds for two workers to complete these steps.

;; With 5 workers and the 60+ second step durations described above, how long will it take to complete all of the steps?

(require 'cl-lib)

(cl-defun plan/step-time (step base-time)
  (let ((steps (-map 'char-to-string (number-sequence 65 90))))
    (+ (-elem-index step steps)
       1
       base-time)))

(plan/step-time "A" 60)
(plan/step-time "A" 0)
(plan/step-time "Z" 60)

(cl-defun deps/ready-steps (deps)
  "Returns the steps available to be worked on from DEPS.
The steps have no dependencies and are sorted alphabetically"
  (interactive)
  (let* ((no-deps (--filter (null (cdr it)) deps))
         (steps (-map 'car no-deps))
         (sorted (cl-sort steps 'string-lessp)))
    sorted))

(deps/ready-steps (input/build (example-input)))

(cl-defun deps/time (deps num-workers &optional (step-base-time 60))
  (let ((second 0)
        (work '())
        (done '()))
    (while deps
      ;; record completed work
      (--each (--filter (equal second (car it)) work)
        (setq done (cons (cdr it) done)
              deps (deps/apply deps (cdr it))))

      ;; free workers that are done
      (setq work (--reject (equal second (car it)) work))

      ;; assign work to available workers
      (let* ((ready-steps (deps/ready-steps deps))
             (steps-in-progress (-map 'cdr work))
             (unclaimed-steps (cl-set-difference ready-steps steps-in-progress)))
        (when (and unclaimed-steps
                   (< (length work) num-workers))
          (let* ((next-steps (-take (- num-workers (length work)) unclaimed-steps))
                 (new-work (--map (cons (+ second (plan/step-time it step-base-time)) it) next-steps)))
            (setq work (-concat work new-work)))))

      ;; tick
      (setq second (+ second 1)))
    (- second 1)))

(deps/time (input/build (example-input)) 2 0) ;; 15

(deps/time (input/build (puzzle-input)) 5 60)
