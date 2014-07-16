(in-package :cl-user)

(defpackage :com.github.flpa.daily-reddit.ascii-game-of-life
  (:use :cl :split-sequence :lisp-unit))

(in-package :com.github.flpa.daily-reddit.ascii-game-of-life)

(defun coord+ (coord limit)
  (mod (1+ coord) limit))
(defun coord- (coord limit)
  (mod (1- coord) limit))

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

(defun neighbours (x y width height)
  "Lists the 8 neighbours' coordinates of the point x/y as cons cells."
  (remove
   (cons x y)
   (cross-product #'cons
		  (list (coord- x width) x (coord+ x width))
		  (list (coord- y height) y (coord+ y height)))
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
	       for neighbour-count = (count-living-neighbours field x y)
	       for cell-value = (get-cell field x y)
	       collecting (get-update x y cell-value neighbour-count) into updates
	       finally (apply-updates field updates))
	    (print-field field)))))))

(defun get-update (x y cell-value neighbour-count)
  (let ((value (new-value cell-value neighbour-count)))
    (unless (eql value cell-value)
      (list x y value))))

(defun apply-updates (field updates)
  (loop for (x y val) in (remove-if #'null updates)
       do (set-value field x y val)))

(defun set-value (field x y value)
  (setf (elt (elt field y) x)  value))

(defun field-width (field)
  (length (elt field 0)))

(defun field-height (field)
  (length field))

(defun count-living-neighbours (field x y)
  (count +on+ (neighbours x y (field-width field) (field-height field))
	 :key #'(lambda (l)
		  (get-cell field (first l) (rest l)))))

(define-test test-count-living-neighbours
  (Let* ((field `("...."
		  ".#.."
		  ".#.#"
		  "#..."
		  "...."
		  "...."
		  "...."
		  "####"
		  "...."
		 )))
  ;;upper hash
  (assert-equal 1 (count-living-neighbours field 1 1))
  ;;middle hash
  (assert-equal 2 (count-living-neighbours field 1 2))
  ;;lower hash
  (assert-equal 2 (count-living-neighbours field 0 3))
  ;;lonely hash
  (assert-equal 1 (count-living-neighbours field 3 2))
  ;;lonely dot
  (assert-equal 0 (count-living-neighbours field 2 4))
  ;;bottom dot
  (assert-equal 3 (count-living-neighbours field 2 8))
  ))

(setf *print-errors* t)
(setf *print-failures* t)

(defun print-field (field)
    (format t "~{~a~%~}~%" field))


(defun new-value (current-value neighbour-count)
  "Determines the new value of a cell based on the current value and the number of neighbours"
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

(define-test test-get-cell
  (assert-equal #\# (get-cell `("..#", "...") 2 0)))

(defun play-sample ()
  (play "/home/flo/code/misc/reddit-daily/ascii-game-of-life/sample.txt"))

(defun play-challenge ()
  (play "/home/flo/code/misc/reddit-daily/ascii-game-of-life/challenge.txt"))



;; animate test


(defclass bb (glut:window)
  ((cells :accessor cells-of :initarg :cells))
  (:default-initargs
   :title "Brian's Brain in CL"
   :mode '(:double :rgb)))

(defmethod glut:display-window :before ((w bb))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((cells (cells-of w)))
   (gl:ortho 0 (field-height cells)  0 (field-width cells) -1 1)))


(defun render-cell (x y cell)
  (flet ((draw-cell (x y)
           (gl:with-pushed-matrix
               (gl:translate x y 0)
             (gl:with-primitive :polygon
               (gl:vertex 0.1 0.1 0)
               (gl:vertex 0.9 0.1 0)
               (gl:vertex 0.9 0.9 0)
               (gl:vertex 0.1 0.9 0)))))
    (case cell
      (+on+ (gl:color 1 1 1)
           (draw-cell x y))
      (+off+ (gl:color 0.5 0.5 0.5)
              (draw-cell x y)))))


(defmethod glut:display ((w bb))
  (gl:clear :color-buffer)
  (let* ((cells (cells-of w))
         (w (field-width cells))
         (h (field-height cells)))
    (loop for index from 0 below (* w h)
       for x = (mod index h)
       for y = (floor index h)
	 do (render-cell x y (get-cell cells x y))))
  (glut:swap-buffers))


(defmethod glut:idle ((w bb))
  ;;(format t "flo")
  ;;(setf (cells-of w) (evolve (cells-of w)))
  (glut:post-redisplay))
  )


(defun play-graphic (filename)
  (with-open-file (s filename)
    (let ((infoline (read-line s)))
      (destructuring-bind (n width height) (mapcar #'parse-integer
					  (split-sequence #\Space infoline))
	(let ((field (loop for i from 0 below height collecting (read-line s))))
	  (format t "letse go")
	  (glut:display-window (make-instance 'bb
					      :cells field
					      :width 512
					      :height 512)))))))


