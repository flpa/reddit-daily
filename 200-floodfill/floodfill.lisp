(defpackage #:200-floodfill
  (:use #:cl))

(in-package #:200-floodfill)

(labels ((neighbours (x y width height)
           (remove-if-not #'(lambda (candidate)
                              (and (<= 0 (car candidate) (1- width)) 
                                   (<= 0 (cdr candidate) (1- height))))
                          (list (cons (1- x) y)
                                (cons (1+ x) y)
                                (cons x (1- y))
                                (cons x (1+ y)))))
         (floodfill (frontier symbol lines width height)
           (if frontier
             (let ((x (caar frontier))
                   (y (cdar frontier)))
               (if (char-equal #\. (elt (elt lines y) x))
                 (progn
                   (setf (elt (elt lines y) x) symbol)
                   (floodfill (append (rest frontier) (neighbours x y width height)) symbol lines width height))
                 (floodfill (rest frontier) symbol lines width height)))
             (format t "~{~a~%~}" lines))))
  (with-open-file (s  "~/code/misc/reddit-daily/200-floodfill/res/01.in" :direction :input)
    (loop with width = (read s) and height = (read s)
      for i from 1 to height collecting (read-line s) into lines
      finally (floodfill (list (cons (read s) (read s))) (read-char s) lines width height))))
