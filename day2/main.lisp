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

(defun to-integers (strings)
  (mapcar #'parse-integer strings))

(defun lists-to-integers (lists)
  (mapcar #'to-integers lists))

(defun reports (filename)
  (let* (
	 (lines (contentful-file-lines filename))
	 (digits (mapcar #'space-split lines))
	 (reports (lists-to-integers digits)))
    reports))

(defun difference-safe (cur prev increasing)
    (list
      (and
	(and
	  (> (abs (- prev cur)) 0)
	  (< (abs (- prev cur)) 4))
	(equal increasing (> 0 (- cur prev))))
      cur
      increasing))

; I tried something clever here to automatically do the problem damper
; I faced trouble with
(defun safe (report)
  (first
    (reduce
      (lambda (prev digit)
        (if (first prev)
	  (difference-safe digit (second prev) (third prev))
	  (list nil)))
      (cdr report)
      :initial-value (list
		       t
		       (first report)
		       (> 0 (- (second report) (first report)))))))

(defun part1 (filename)
  (let* (
	 (reports (reports filename))
	 (report-safeties (mapcar
			    (lambda (report) (safe report))
			    reports))
	 (total (reduce
		  (lambda (c safe)
		    (+ c (if safe 1 0)))
		  report-safeties
		  :initial-value 0)))
    total))

(defun any (booleans)
  (reduce (lambda (i1 i2) (or i1 i2)) booleans :initial-value nil))

(defun sublists (items)
  (let (
	(before '()))
    (loop :for i :on items
	  :collect (concatenate 'list (reverse before) (cdr i))
	  :do (push (first i) before))))

(defun safe-with-dampener (report)
  (any
    (mapcar #'safe (append (list report) (sublists report)))))

(defun part2 (filename)
  (let* (
	 (reports (reports filename))
	 (report-safeties (mapcar
			    (lambda (report) (safe-with-dampener report))
			    reports))
	 (total (reduce
		  (lambda (c safe)
		    (+ c (if safe 1 0)))
		  report-safeties
		  :initial-value 0)))
    total))
