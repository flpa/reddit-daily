(in-package :cl-user)

(defpackage :com.github.flpa.daily-reddit.ascii-game-of-life
  (:use :cl :split-sequence))

(in-package :com.github.flpa.daily-reddit.ascii-game-of-life)

(defun play (filename)
  (with-open-file (s filename)
    (let ((infoline (read-line s)))
      (destructuring-bind (

(parse-integer 			   

;; not only cube			   
(defparameter *n* 10)

(defun coord+ (coord)
  (mod (1+ coord) *n*))
(defun coord- (coord)
  (mod (1- coord) *n*))


; norvig from SO

(defun cross-product (fn list-1 list-2)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x)
                           (funcall fn y x))
                       list-2))
           list-1))

(defun mappend (fn the-list)
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))

;norvig out

(defun neighbours (x y)
  (remove
   (cons x y)
   (cross-product #'cons
		  (list (coord- x) x (coord+ x))
		  (list (coord- y) y (coord+ y)))
   :test #'equal))

(defconstant +on+ #\#)
(defconstant +off+ #\.)

(defun play (filename)
  (with-open-file (s filename)
    (let ((infoline (read-line s)))
      (destructuring-bind (n width height) (mapcar #'parse-integer
					  (split-sequence #\Space infoline))
	(format t "n is ~d, width is ~d, height is ~d~%" n width height)
	(let ((field (loop for i from 0 below height collecting (read-line s))))
	  (print-field field)
	  (dotimes (round n)
	    (loop for index from 0 below (* width height)
	       for x = (mod index height)
	       for y = (floor index height)
	       for neighbour-count = (count +on+ (neighbours x y)
					    :key #'(lambda (l)
						     (get-cell field (first l) (rest l))))
	       for cell-value = (get-cell field x y)
	       do
		  (setf (elt (elt field y) x) (new-value cell-value neighbour-count)))
	    (print-field field)))))))

(defun print-field (field)
    (format t "~{~a~%~}~%" field))


(defun new-value (current-value neighbour-count)
  (if (equal +off+ current-value)
      (if (eql 3 neighbour-count)
	  +on+ +off+)
      (if (or (< neighbour-count 2)
	      (> neighbour-count 3))
	  +off+ +on+)))


(defun get-cell (field x y)
  (elt (elt field y) x))
