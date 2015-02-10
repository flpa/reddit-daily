(in-package #:precedence-parsing)

;; assume: only perfect input
;; assume: only single-digit numbers (...)

(defun read-term (symbols)
  "Read from standard-input, recursively resolving lists."
  (let ((in (read-char *standard-input* nil #\))))
    (if (or (eql in #\)) (eql in #\Newline))
      (reverse symbols)
      (read-term (cons 
                   (if (eql in #\()
                     ;; read sub-list
                     (read-term '())
                     in) 
                   symbols)))))

(with-open-file (*standard-input* "~/code/misc/reddit-daily/196-precedence-parsing/res/5.in")
  (main))

(defun main ()
  (let ((operators nil))
    (flet ((parse-operator (line)
             (acons (elt line 0) (eql #\r (elt line 2)) operators)))
      (dotimes (i (parse-integer (read-line)))
        (setf operators (parse-operator (read-line))))
      ;; now we have an alist symbol->right-associative-p
      (format t "~a~%" (disambiguate (reverse operators) (read-term '()))))))

;;e.g. (wrap-at '(1 + 2 + 3) 3)  => (1 + (2 + 3))
(defun wrap-at (term op-pos)
  (append (subseq term 0 (1- op-pos))
          (list (subseq term (1- op-pos) (+ op-pos 2)))
          (subseq term (+ op-pos 2))))

(defun pos-of-strongest-operator (operators term)
  (operator-pos (car (remove-if #'(lambda (op)
                                    (not (find op term))) 
                                operators 
                                :key #'car )) 
                term))

(defun operator-pos (op term)
  (position (car op) term :from-end (cdr op)))

(defun disambiguate (operators term)
  (if (listp term)
    (if (> (length term) 3) ; assume there's an operator
      (disambiguate operators (wrap-at term (pos-of-strongest-operator operators term)))
      ; fix nested lists
      (mapcar #'(lambda (symbol)
                  (if (listp symbol)
                    (disambiguate operators symbol)
                    symbol))
              term))
    (list term)))
