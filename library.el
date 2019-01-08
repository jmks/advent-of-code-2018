(defun list/replicate (element times)
  "Returns a list of ELEMENT with TIMES elements"
  (interactive)
  (let ((result '()))
    (while (> times 0)
      (setq result (cons element result)
            times (- times 1)))
    result))

(defun list/all-pairs (list)
  "Returns a list of all pairs of elements in LIST"
  (let ((pairs '()))
    (while list
      (setq pairs (append pairs (mapcar (lambda (el) (cons (car list) el)) (cdr list))))
      (setq list (cdr list)))
    pairs))

(defun list/zip (list-1 list-2)
  (if (or (null list-1) (null list-2))
      '()
    (cons (cons (car list-1) (car list-2)) (zip (cdr list-1) (cdr list-2)))))
