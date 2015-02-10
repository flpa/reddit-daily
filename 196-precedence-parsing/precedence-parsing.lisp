(in-package #:precedence-parsing)

(defun read-it (stream symbols)
  (let ((in (read-char stream nil #\))))
    (if (char-equal in #\))
      (reverse symbols)
      (read-it stream (cons 
                        (if (char-equal in #\()
                          (read-it stream nil)
                          in) 
                        symbols)))))

(with-open-file (s "~/code/misc/reddit-daily/196-precedence-parsing/res/1.in")
           (read-line s)
           (aref (read-line s) 0) 
           (aref (read-line s) 0) 
           (aref (read-line s) 0)
           (read-it s nil)
           )


; sublists > 3
; find strongest
; find associative
; wrap in new list



