(require 'uiop)
(ql:quickload "cl-ppcre")

(defun get-forwards-lines (strs)
  strs)

(defun get-backwards-lines (strs)
  (mapcar #'reverse strs))

(defun get-downwards-lines (strs)
  (apply #'mapcar
	 (lambda (&rest chars) (coerce chars 'string))
	 (mapcar (lambda (s) (coerce s 'list)) strs)))

(defun get-upwards-lines (strs)
  (get-backwards-lines (get-downwards-lines strs)))

(defun get-diagonal-count (side-length)
  (- (* side-length 2) 1))

(defun get-diagonal-1 (strs)
  (let (
	(result (make-list
		  (get-diagonal-count (length strs))
		  :initial-element '())))
    (progn
      (loop
        for str in strs
        for startline from 0
        do (loop
	     for chr across str
  	     for charindex from 0
	     do (setf
		  (nth (+ startline charindex) result)
		  (append
		    (nth (+ startline charindex) result)
		    (list chr)))))
      (mapcar (lambda (s) (coerce s 'string)) result))))

(defun get-diagonal-2 (strs)
  (get-backwards-lines (get-diagonal-1 strs)))

(defun get-diagonal-3 (strs)
  (get-diagonal-1 (get-upwards-lines strs)))

(defun get-diagonal-4 (strs)
  (get-backwards-lines (get-diagonal-3 strs)))

(defun get-xmas-count-in-line (str)
  (ppcre:count-matches "XMAS" str))

(defun get-xmas-count-in-lines (strs)
  (reduce #'+ (mapcar #'get-xmas-count-in-line strs)))

(defun get-xmas-count-in-grids (&rest grids)
  (reduce #'+ (mapcar #'get-xmas-count-in-lines grids)))

(defun part1 (filename)
  (let (
	(wordsearch (uiop:read-file-lines filename)))
    (get-xmas-count-in-grids
      (get-forwards-lines wordsearch)
      (get-backwards-lines wordsearch)
      (get-downwards-lines wordsearch)
      (get-upwards-lines wordsearch)
      (get-diagonal-1 wordsearch)
      (get-diagonal-2 wordsearch)
      (get-diagonal-3 wordsearch)
      (get-diagonal-4 wordsearch))))
