(require 'uiop)

(defun contentful-file-lines (filename)
  (let* (
	 (content (uiop:read-file-lines filename))
	 (lines-without-empties
	   (remove-if
	     (lambda (str) (string= "" str))
	     content)))
    lines-without-empties))

(defun space-split (str)
  (uiop:split-string str :separator " "))

(defun file-lists (filename)
  (let* (
	 (lines (contentful-file-lines filename))
	 (split-lines (mapcar #'space-split lines))
	 (lists (apply #'mapcar #'list split-lines)))
    lists))

(defun lr-lists (filename)
  (let ((lists (file-lists filename)))
    (list (first lists) (first (last lists)))))

(defun to-integers (strings)
  (mapcar #'parse-integer strings))

(defun lists-to-integers (lists)
  (mapcar #'to-integers lists))

(defun numerical-lr-lists (filename)
  (lists-to-integers (lr-lists filename)))

(defun sort-lists (lists)
  (mapcar
    (lambda (l) (sort l #'<))
    lists))

(defun sorted-lr-lists (filename)
  (sort-lists (numerical-lr-lists filename)))

(defun list-differences (lists)
  (apply
    #'mapcar
    (lambda (i1 i2) (abs (- i1 i2)))
    lists))

(defun sum (numbers)
  (reduce #'+ numbers))

(defun part1 (filename)
  (let* (
	 (lr (sorted-lr-lists filename))
	 (differences (list-differences lr))
	 (total (sum differences)))
    total))

(defun digit-counts (integers)
  (let ((counts (make-hash-table)))
    (progn
      (loop for digit in integers
	    do (if (gethash digit counts)
		 (setf (gethash digit counts) (+ (gethash digit counts) 1))
		 (setf (gethash digit counts) 1))))
    counts))

(defun score-list (lr)
  (let* (
	 (counts (digit-counts (second lr)))
	 (scores (mapcar
		   (lambda
		     (num) (* (gethash num counts 0) num))
		   (first lr))))
    scores))

(defun part2 (filename)
  (let* (
	 (lr (numerical-lr-lists filename))
	 (scores (score-list lr))
	 (total (sum scores)))
    total))
