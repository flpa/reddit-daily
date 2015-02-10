(in-package #:precedence-parsing)

;; assume: only perfect input
;; assume: only single-digit numbers (...)

(defun read-it (symbols)
  "Read from standard in recursively resolving lists."
  (let ((in (read-char *standard-input* nil #\))))
    (if (or (eql in #\)) (eql in #\Newline))
      (reverse symbols)
      (read-it (cons 
                 (if (eql in #\()
                   (read-it nil)
                   in) 
                 symbols)))))

(with-open-file (*standard-input* "~/code/misc/reddit-daily/196-precedence-parsing/res/1.in")
  (make-clear))

(defun make-clear ()
  (let ((operators nil))
    (flet ((parse-operator (line)
             (acons (elt line 0) (eql #\r (elt line 2)) operators)))
      (dotimes (i (parse-integer (read-line)))
        (setf operators (parse-operator (read-line))))
      ;; now we have an alist symbol->right-associative-p
      ;; next: read input and start checking
      (pos-of-strongest-operator (reverse operators) (read-it nil)))))

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

(defun fix-it (operators term)
  (if (listp term)
    (if (> (length term) 3)
      ;; > 3: there gotta be an operator
      (fix-it operators (wrap-at term ))



      ;;fixmagic
      (mapcar #'(lambda (symbol)
                  (if (listp symbol)
                    (fix-it operators symbol)
                    (symbol)))))
    (list term))
  ;; if length > 3: need fix
  ;;  fix = append sublist before, list wrapping sublist, sublist after
  )


; find strongest
; find associative
; wrap in new list
