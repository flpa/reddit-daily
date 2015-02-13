(in-package #:get-your-priorities-right)

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

(test clear
  (enqueue "reddit" 1 0)
  (clear)
  (is (endp *queue*)))
