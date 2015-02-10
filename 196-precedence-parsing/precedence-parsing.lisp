(defpackage #:precedence-parsing
  (:use #:cl))
(in-package #:precedence-parsing)

(loop for input in (uiop/filesystem:directory-files "~/code/misc/reddit-daily/196-precedence-parsing/res/" "*.in")
      do (with-open-file (*standard-input* input)
           (format t "Input ~a~%" (pathname-name input))
           (main)))

;; Assumptions:
;;      - no broken input
;;      - only single-character operators
;;      - only single-digit numbers (...)
;;
(defun main ()
  (format t "~a~%" (disambiguate (read-operators) (read-term))))

(defun read-operators ()
  ;; now we have an alist symbol->right-associative-p
  (labels ((parse-add-operator (operators line)
             (acons (elt line 0) (eql #\r (elt line 2)) operators))
           (recurse (i n operators)
             (if (eql i n)
               (reverse operators)
               (recurse (1+ i) n (parse-add-operator operators (read-line)))
               )))
    (recurse 0 (parse-integer (read-line)) '())))

(defun read-term ()
  "Read from standard-input, recursively resolving lists."
  (labels ((recurse (symbols)
             (let ((in (read-char *standard-input* nil #\))))
               (if (or (eql in #\)) (eql in #\Newline))
                 (reverse symbols)
                 (recurse (cons 
                            (if (eql in #\()
                              (read-term) ; read sublist
                              in) 
                            symbols))))))
    (recurse '())))

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

(defun pos-of-strongest-operator (operators term)
  (operator-pos (car (remove-if #'(lambda (op)
                                    (not (find op term))) 
                                operators 
                                :key #'first)) 
                term))

(defun operator-pos (op term)
  (position (first op) term :from-end (rest op)))

(defun wrap-at (term op-pos)
  "e.g. (wrap-at '(1 + 2 + 3) 3)  => (1 + (2 + 3))"
  (append (subseq term 0 (1- op-pos))
          (list (subseq term (1- op-pos) (+ op-pos 2)))
          (subseq term (+ op-pos 2))))





