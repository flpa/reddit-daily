(in-package #:precedence-parsing)

(defun read-it (stream symbols)
  "Read from stream recursively resolving lists."
  (let ((in (read-char stream nil #\))))
    (if (or (eql in #\)) (eql in #\Newline))
      (reverse symbols)
      (read-it stream (cons 
                        (if (eql in #\()
                          (read-it stream nil)
                          in) 
                        symbols)))))

(let ((operators nil))
  (with-open-file (s "~/code/misc/reddit-daily/196-precedence-parsing/res/1.in")
    (dotimes (i (parse-integer (read-line s)))
      (let ((line (read-line s)))
        (setf operators (acons (elt line 0) (eql #\r (elt line 2)) operators)))
      )
    ;; now we have an alist symbol->right-associative-p
    ;; next: read input and start checking
    ))


; sublists > 3
; find strongest
; find associative
; wrap in new list



