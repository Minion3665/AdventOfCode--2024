(require 'uiop)

; Please install quicklisp on your own time from https://common-lisp-libraries.readthedocs.io/quicklisp/
(ql:quickload "cl-ppcre")

(defun mul-instructions (str)
  (ppcre:all-matches-as-strings "mul\\(([0-9]+),([0-9]+)\\)" str))

(defun get-enabled-portion (str)
  (let (
	(disables (ppcre:all-matches "don't\\(\\)" str))
	(enables (ppcre:all-matches "do\\(\\)" str))
	(result "")
	(enabled t))
    (progn
      (loop
	for index from 0
        for c in (coerce str 'list)
        do (progn
	     (when (equal (first disables) index)
	       (setq enabled nil)
	       (pop disables))
	     (when (equal (first enables) index)
	       (setq enabled t)
	       (pop enables))
	     (when enabled
	       (setq result (concatenate 'string result (list c))))))
      result)))

(defun numbers-to-multiply (instruction)
  (ppcre:register-groups-bind
    ((#'parse-integer num1 num2))
    ("mul\\(([0-9]+),([0-9]+)\\)" instruction :sharedp t)
    (list num1 num2)))

(defun all-multiplications (str)
  (mapcar #'numbers-to-multiply (mul-instructions str)))

(defun part1 (filename)
  (let* (
	 (str (uiop:read-file-string filename))
	 (multiplications (all-multiplications str))
	 (multiplied (mapcar
		       (lambda (nums)
			 (reduce #'* nums))
		       multiplications))
	 (total (reduce #'+ multiplied)))
    total))

(defun part2 (filename)
  (let* (
	 (str (uiop:read-file-string filename))
	 (active (get-enabled-portion str))
	 (multiplications (all-multiplications active))
	 (multiplied (mapcar
		       (lambda (nums)
			 (reduce #'* nums))
		       multiplications))
	 (total (reduce #'+ multiplied)))
    total))
