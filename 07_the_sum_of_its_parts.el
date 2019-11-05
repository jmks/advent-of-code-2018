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
The next step has no dependencies is is the lowest alphabetical character"
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
