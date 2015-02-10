(defpackage #:precedence-parsing
  (:use #:cl))
(in-package #:precedence-parsing)

(defun letse-go () 
  ;; Adapt path to run main function with all sample inputs
  (loop for input in (uiop/filesystem:directory-files 
                       "~/code/misc/reddit-daily/196-precedence-parsing/res/" 
                       "*.in")
        do (with-open-file (*standard-input* input)
             (format t "Input ~a~%" (pathname-name input))
             (main))))

;;; We're relying on some assumptions here:
;;;      - no broken input
;;;      - only single-character operators
;;;      - only single-digit numbers (...)

(defun main ()
  (format t "~a~%" (disambiguate (read-operators) (read-term))))

(defun read-operators ()
  "Reads the operator definitions. Operators are represented by an alist, which
   provides a mapping from operator symbol to a flag whether the operator is
   right associative. The order of the alist expresses operator precedence."
  (labels ((parse-add-operator (operators line)
             (acons (elt line 0) (eql #\r (elt line 2)) operators))
           (recurse (i n operators)
             (if (= i n)
               (reverse operators)
               (recurse (1+ i) n (parse-add-operator operators (read-line))))))
    (recurse 0 (parse-integer (read-line)) '())))

(defun read-term ()
  "Reads a term, which is represented by a list of characters and lists, 
   recursively resolving sub-terms. Terms are terminated by a closing paren, 
   newline or EOF."
  (labels ((recurse (symbols)
             (let ((in (read-char *standard-input* nil :eof)))
               (case in 
                 ((#\) #\Newline :eof) (reverse symbols))
                 (t (recurse (cons 
                               (if (eql in #\()
                                 (read-term) ; read sub-term
                                 in) 
                               symbols)))))))
    (recurse '())))

(defun disambiguate (operators term)
  "Resolves ambiguous parts of TERM by applying the operator precedence and 
   associativity defined in OPERATORS."
  (if (listp term) ; during recursion we'll eventually encounter single symbols
    (if (> (length term) 3) 
      ;; Terms with more than three symbols are considered ambiguous. 
      (disambiguate operators (wrap-at term 
                                       (pos-of-strongest-op operators term)))
      ;; Disambiguate sub-terms
      (mapcar #'(lambda (subterm) (disambiguate operators subterm)) term))
    term))

(defun pos-of-strongest-op (operators term)
  "Returns the position of the strongest operator in a term, i.e. the operator
   with the highest precedence."
  (operator-pos (first (remove-if #'(lambda (op)
                                      (not (find op term))) 
                                  operators 
                                  :key #'first)) 
                term))

(defun operator-pos (op term)
  "Returns the position of the first or last occurence of opreator OP in TERM,
   depending on on whether the operator is left- or right-associative."
  (position (first op) term :from-end (rest op)))

(defun wrap-at (term op-pos)
  "Wraps the operator at position OP-POS in TERM and its operands into a new
   pair of parens, e.g. 
   (wrap-at '(1 + 2 + 3) 3)  
   => (1 + (2 + 3))"
  (append (subseq term 0 (1- op-pos))
          (list (subseq term (1- op-pos) (+ op-pos 2)))
          (subseq term (+ op-pos 2))))
