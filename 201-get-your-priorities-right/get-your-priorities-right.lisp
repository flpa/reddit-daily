(in-package :cl-user)
(defpackage #:get-your-priorities-right
  (:use #:cl :5am))

(in-package #:get-your-priorities-right)

(defparameter *queue* '()
  "A plain list holding the items of the two-priority queue. 
   Items are appended to the end, so that the items in front are the oldest
   ones.")

(defstruct item value prio-a prio-b 
  "A simple structure holding the properties of an item in the two-priority 
   queue. 
   VALUE is not limited to strings, it can really be anything.
   PRIO-A and PRIO-B need to be numbers, or anything comparable by #'>")

(defun enqueue (value prio-a prio-b)
  "Enqueues a new item."
  (setf *queue* (append *queue* (list (make-item :value value 
                                                 :prio-a prio-a 
                                                 :prio-b prio-b)))))

(test enqueue-single
  (setf *queue* '())
  (enqueue "reddit" 1 2.2)
  (is (equal "reddit" (item-value (first *queue*))))
  (is (equal 1 (item-prio-a (first *queue*))))
  (is (equal 2.2 (item-prio-b (first *queue*)))))

(test enqueues-at-end
  (setf *queue* '())
  (enqueue "reddit" 1 2.2)
  (enqueue "daily" 3 400)
  (is (equal "reddit" (item-value (first *queue*))))
  (is (equal 1 (item-prio-a (first *queue*))))
  (is (equal 2.2 (item-prio-b (first *queue*)))))

(defun dequeue-a ()
  "Applies DEQUEUE for priority value A."
  (dequeue #'item-prio-a))

(defun dequeue-b ()
  "Applies DEQUEUE for priority value B."
  (dequeue #'item-prio-b))

(defun dequeue (key-fn)
  "Removes and returns the item with the highest priority value, determined by 
   the function KEY-FN, from the queue. If there are multiple entries with the 
   same value, the oldest entry is returned."
  (when *queue* 
    (let ((dequeued-item 
            (reduce #'(lambda (a b) 
                        ;; by only picking B if it's > A, the oldest entry wins
                        ;; in case of equal values
                        (if (> (funcall key-fn b)
                               (funcall key-fn a)) 
                          b a))
                    *queue*)))
      (setf *queue* (remove dequeued-item *queue* :count 1))
      dequeued-item)))

(test dequeue-a-uses-prio
  (clear)
  (enqueue "reddit" 1 0)
  (enqueue "daily" 3 0)
  (enqueue "programmer" 2 0)
  (is (equal "daily" (item-value (dequeue-a)))))

(test dequeue-b-uses-prio
  (setf *queue* '())
  (enqueue "reddit" 1 10)
  (enqueue "daily" 3 0)
  (enqueue "programmer" 2 20)
  (is (equal "programmer" (item-value (dequeue-b)))))

(test dequeue-returns-oldest-on-conflict
  (clear)
  (enqueue "reddit" 1 0)
  (enqueue "daily" 3 0)
  (enqueue "programmer" 3 0)
  (is (equal "daily" (item-value (dequeue-a)))))

(test dequeue-removes-item-from-queue
  (clear)
  (enqueue "reddit" 1 0)
  (enqueue "daily" 3 0)
  (enqueue "programmer" 3 0)
  (dequeue-a)
  (is (equal 2 (length *queue*))))

(test dequeue-handles-floating-point
  (clear)
  (enqueue "reddit" 1 0)
  (enqueue "daily" 1.5 0)
  (enqueue "programmer" 1.06 0)
  (is (equal "daily" (item-value (dequeue-a)))))

(test dequeue-empty
  (clear)
  (is-false (dequeue-a)))

(defun clear ()
  "Clears the two-priority queue."
  (setf *queue* '()))

(test clear
  (enqueue "reddit" 1 0)
  (clear)
  (is (endp *queue*)))
