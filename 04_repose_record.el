;; --- Day 4: Repose Record ---

;; You've sneaked into another supply closet - this time, it's across from the prototype suit manufacturing lab.
;; You need to sneak inside and fix the issues with the suit, but there's a guard stationed outside the lab,
;; so this is as close as you can safely get.

;; As you search the closet for anything that might help, you discover that you're not the first person to want to
;; sneak in. Covering the walls, someone has spent an hour starting every midnight for the past few months secretly
;; observing this guard post! They've been writing down the ID of the one guard on duty that night -
;; the Elves seem to have decided that one guard was enough for the overnight shift -
;; as well as when they fall asleep or wake up while at their post (your puzzle input).

;; For example, consider the following records, which have already been organized into chronological order:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; Timestamps are written using year-month-day hour:minute format. The guard falling asleep or waking up is always
;; the one whose shift most recently started. Because all asleep/awake times are during the midnight hour
;; (00:00 - 00:59), only the minute portion (00 - 59) is relevant for those events.

;; Visually, these records show that the guards are asleep at these times:

;; Date   ID   Minute
;;             000000000011111111112222222222333333333344444444445555555555
;;             012345678901234567890123456789012345678901234567890123456789
;; 11-01  #10  .....####################.....#########################.....
;; 11-02  #99  ........................................##########..........
;; 11-03  #10  ........................#####...............................
;; 11-04  #99  ....................................##########..............
;; 11-05  #99  .............................................##########.....

;; The columns are:
;; Date, which shows the month-day portion of the relevant day;
;; ID, which shows the guard on duty that day; and
;; Minute, which shows the minutes during which the guard was asleep within the midnight hour.
;; (The Minute column's header shows the minute's ten's digit in the first row and the one's digit in the second row.)
;; Awake is shown as ., and asleep is shown as #.

;; Note that guards count as asleep on the minute they fall asleep, and they count as awake on the minute they wake up.
;; For example, because Guard #10 wakes up at 00:25 on 1518-11-01, minute 25 is marked as awake.

;; If you can figure out the guard most likely to be asleep at a specific time, you might be able to trick that guard
;; into working tonight so you can have the best chance of sneaking in. You have two strategies for choosing the best
;; guard/minute combination.

;; Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?

;; In the example above, Guard #10 spent the most minutes asleep, a total of 50 minutes (20+25+5),
;; while Guard #99 only slept for a total of 30 minutes (10+10+10). Guard #10 was asleep most during minute 24
;; (on two days, whereas any other minute the guard was asleep was only seen on one day).

;; While this example listed the entries in chronological order, your entries are in the order you found them.
;; You'll need to organize them before they can be analyzed.

;; What is the ID of the guard you chose multiplied by the minute you chose?
;; (In the above example, the answer would be 10 * 24 = 240.)

(require 'rx)
(require 's)
(require 'parse-time)
;; (require 'ht)

(defun records/parse (data)
  "Parses Guard records into record data structures"
  (interactive)
  (let* ((lines (s-split "\n" data 't))
         (sorted (cl-sort lines 'string-lessp)))
    (mapcar 'records/parse-record sorted)))

;; (records/parse "[1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up")

(defun records/parse-record (record)
  "Parses a single record into a record data structure.

The data structure is a list of '(EVENT-TYPE DATE ...META-DATA)

EVENT-TYPE is one of 'shift-began 'fell-asleep 'woke-up
DATE is the list (SEC MIN HOUR DAY MON YEAR DOW DST TZ)"
  (interactive)
  (cond ((s-index-of "begins shift" record)
         (list 'shift-began (records/extract-date record) (records/extract-guard record)))
        ((s-index-of "falls asleep" record)
         (list 'fell-asleep (records/extract-date record)))
        ((s-index-of "wakes up" record)
         (list 'woke-up (records/extract-date record)))))

;; (records/parse-record "[1518-11-01 00:00] Guard #10 begins shift")
;; (records/parse-record "[1518-11-01 00:05] falls asleep")
;; (records/parse-record "[1518-11-01 00:25] wakes up")

(defun records/extract-guard (record)
  "Returns the Guard's number for the record"
  (interactive)
  (when (string-match
         (rx space
             "#"
             (group (one-or-more digit))
             space)
         record)
    (string-to-number (match-string 1 record))))

;; (records/extract-guard "[1518-11-05 00:03] Guard #103 begins shift")

(defun records/extract-date (record)
  "Returns a Date list of the record"
  (interactive)
  (when (string-match
         (rx bol
             "[" (group (one-or-more (any alnum space "-" ":"))) "]")
         record)
    (parse-time-string (match-string 1 record))))

;; (records/extract-date "[1518-11-05 00:03] Guard #103 begins shift")

(defun records/input ()
  "Returns the input of the puzzle, parsed as records"
  (with-temp-buffer
    (insert-file-contents "04_input")
    (records/parse (buffer-string))))

(defun record/shift-began-p (record)
  "Returns t if record is a 'shift-began type"
  (interactive)
  (eq (car record) 'shift-began))

(defun record/fell-asleep-p (record)
  "Returns t if record is a 'fell-asleep type"
  (interactive)
  (eq (car record) 'fell-asleep))

(defun record/woke-up-p (record)
  "Returns t if record is a 'woke-up type"
  (interactive)
  (eq (car record) 'woke-up))

(defun record/minute (record)
  "Returns minute of the record"
  (cadadr record))

;; (record/minute (records/parse-record "[1518-11-01 00:55] falls asleep"))

(defun record/guard (record)
  "Returns the Guard ID for 'shift-began record types"
  (interactive)
  (when (record/shift-began-p record)
    (caddr record)))

;; (record/guard (records/parse-record "[1518-11-01 00:55] falls asleep"))
;; (record/guard (records/parse-record "[1518-03-30 00:02] Guard #1933 begins shift"))

;; TODO: zero-pad month/day?
(defun record/date (record)
  "Returns the date of the record as YYY-MM-DD"
  (interactive)
  (let* ((date (cadr record))
         (year (cadr (cddddr date)))
         (month (car (cddddr date)))
         (day (cadddr date)))
    (concat (int-to-string year) "-" (int-to-string month) "-" (int-to-string day))))

;; (record/date (records/parse-record "[1518-03-30 00:02] Guard #1933 begins shift"))

(defun records/group-by-shift (records)
  "Returns a list of lists, where each sublist are the records for a single guard for a particular shift.

Note: some Guards may start their shift BEFORE midnight, i.e. on the previous day.
This is considered the start of the shift"
  (interactive)
  (let ((groups '()))
    (while records
      (let* ((begins-shift (car records))
            (rest (cdr records))
            (in-shift (--take-while (not (record/shift-began-p it)) rest))
            (next-shift (--drop-while (not (record/shift-began-p it)) rest)))
        (setq groups (cons (cons begins-shift in-shift) groups)
              records next-shift)))
    (reverse groups)))

;; (setq aoc-test-record "[1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up")
;; (setq aoc-test-records (records/parse aoc-test-record))
;; (records/group-by-shift aoc-test-records)

(defun shifts/from-records (shift-records)
  "Aggregate the records from a shift into a list (DATE GUARD MINUTES-ASLEEP)"
  (interactive)
  (let* ((shift-begin-record (car shift-records))
         (date (record/date shift-begin-record))
         (guard (record/guard shift-begin-record))
         (minutes-asleep '()))

    (when (not (record/shift-began-p shift-begin-record))
      (error "Expected record to be a shift starting" shift-begin-record))

    (setq shift-records (cdr shift-records))
    (while shift-records
      (let ((asleep-record (car shift-records))
            (wakes-record (cadr shift-records)))

        (when (not (record/fell-asleep-p asleep-record))
          (error "Expected record to be falling asleep" asleep-record))
        (when (not (record/woke-up-p wakes-record))
          (error "Expected record to be falling asleep" wakes-record))

        (setq minutes-asleep (-concat minutes-asleep
                                      (number-sequence (record/minute asleep-record) (- (record/minute wakes-record) 1))))

        (setq shift-records (cddr shift-records))))
    (list date guard minutes-asleep)))

;; (shifts/from-records (car (records/group-by-shift aoc-test-records)))

(defun shifts/merge (shift other-shift)
  "Merges the results into a SINGLE shift

It uses the DATE from the first argument's DATE"
  (interactive)
  (when (not (eq (cadr shift) (cadr other-shift)))
    (error "Can not merge shifts from different guards!"))

  (list (car shift) (cadr shift) (-concat (caddr shift) (caddr other-shift))))

;; (shifts/merge (list "FIRST" 123 '(1 2 3)) (list "SECOND" 123 '(4 5 6)))

(defun most-frequent (list)
  "Most frequent element in the list"
  (car (--max-by (> (length it) (length other)) (-group-by 'identity list))))

;; (most-frequent '(1 2 3 2))

(defun puzzle/strategy-1 (shifts)
  "Returns the tuple (GUARD . MINUTE-MOST-FREQUENTLY-ASLEEP)"
  (interactive)
  (let* ((by-guards (--group-by (cadr it) shifts))
         (aggregated (--map (-reduce-from 'shifts/merge (car it) (cdr it)) (--map (cdr it) by-guards)))
         (sleepy (--max-by (> (length (caddr it)) (length (caddr other))) aggregated)))
    (cons (cadr sleepy) (most-frequent (caddr sleepy)))))

;; (puzzle/strategy-1 (-map 'shifts/from-records (records/group-by-shift aoc-test-records)))
