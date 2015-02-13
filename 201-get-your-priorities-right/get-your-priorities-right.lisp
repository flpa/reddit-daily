(in-package :cl-user)
(defpackage #:get-your-priorities-right
  (:use #:cl :5am))

(in-package #:get-your-priorities-right)

(defparameter *queue* '()
  "A plain list holding the items of the two-priority queue. 
   Items are appended to the end, so that the items in front are the oldest
   ones.")

(defstruct item 
  "A simple structure holding the properties of an item in the two-priority 
   queue. 
   VALUE is not limited to strings, it can really be anything.
   PRIO-A and PRIO-B need to be numbers, or anything comparable by #'>" 
  value prio-a prio-b)

(defun enqueue (value prio-a prio-b)
  "Enqueues a new item."
  (setf *queue* (append *queue* (list (make-item :value value 
                                                 :prio-a prio-a 
                                                 :prio-b prio-b)))))

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
    (flet ((pick-higher-older (a b)
             "Picks the ITEM with higher value, prefering A on tie."
             (if (> (funcall key-fn b) (funcall key-fn a)) 
               b a))
           (remove-first (item)
             (setf *queue* (remove item *queue* :count 1))   
             item)) 
      (remove-first (reduce #'pick-higher-older *queue*)))))

(defun clear ()
  "Clears the two-priority queue."
  (setf *queue* '()))
