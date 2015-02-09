(defpackage #:198-words-with-enemies
  (:use #:cl))
(in-package #:198-words-with-enemies)

(loop for w1 = (coerce (string (read)) 'list) then (remove (first in) w1 :count 1)
      for w2 = (coerce (string (read)) 'list) then (remove (first in) w2 :count 1)
      for in = (intersection w1 w2)
      while in 
      finally (format t "~a wins! Remaining: ~{~a ~}~%" 
                      (cond ((< (length w1) (length w2)) "Right")
                            ((> (length w1) (length w2)) "Left")
                            (t "Nobody")) 
                      (append w1 w2)))

