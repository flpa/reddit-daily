(in-package :cl-user)

(defpackage :com.github.flpa.daily-reddit.ascii-game-of-life
  (:use :cl :split-sequence :lisp-unit))

(in-package :com.github.flpa.daily-reddit.ascii-game-of-life)

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
  "Lists the 8 neighbours' coordinates of the point x/y as cons cells."
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
  "Determines the new value of a cell based on the current value and the number of neighbours"
;;  (assertor (eql current-value +on+)
  (if (equal +off+ current-value)
      (if (eql 3 neighbour-count)
	  +on+ +off+)
      (if (or (< neighbour-count 2)
	      (> neighbour-count 3))
	  +off+ +on+)))


(define-test test-new-value
	     ;;If a cell is 'off' but exactly 3 of its neighbours are on, that
	     ;;cell will also turn on - like reproduction.
	     (assert-equal +on+ (new-value +off+ 3))
	     (assert-equal +off+ (new-value +off+ 2))
	     (assert-equal +off+ (new-value +off+ 4))
	     ;;If a cell is 'on' but less than two of its neighbours are on, it
	     ;;will die out - like underpopulation.
	     (assert-equal +off+ (new-value +on+ 1))
	     (assert-equal +on+ (new-value +on+ 2))
	     ;;If a cell is 'on' but more than three of its neighbours are on,
	     ;;it will die out - like overcrowding.
	     (assert-equal +off+ (new-value +on+ 4))
	     (assert-equal +on+ (new-value +on+ 3))
	     )



(defun get-cell (field x y)
  (elt (elt field y) x))
