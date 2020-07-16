(in-package :wfc)

(defun set/size (set)
  (length set))

(defun set/singleton-el (set)
  (car set))

(defun set/random-elt (set)
  (alx:random-elt set))

(defun set/add (set el)
  (cond ((null set) (list el))
        ((< (car set) el) (cons (car set) (set/add (cdr set) el)))
        ((= (car set) el) set)
        (t (cons el set))))

(defun set/inter (s1 s2)
  (cond ((null s1) nil)
        ((null s2) nil)
        ((= (car s1) (car s2)) (cons (car s1) (set/inter (cdr s1) (cdr s2))))
        ((< (car s1) (car s2)) (set/inter (cdr s1) s2))
        (t (set/inter s1 (cdr s2)))))

(defun set/union (s1 s2)
  (cond ((null s1) s2)
        ((null s2) s1)
        ((= (car s1) (car s2)) (cons (car s1) (set/union (cdr s1) (cdr s2))))
        ((< (car s1) (car s2)) (cons (car s1) (set/union (cdr s1) s2)))
        (t (cons (car s2) (set/union s1 (cdr s2))))))

(defun set/singleton (item n-slices)
  (declare (ignore n-slices))
  (list item))

(defun set/empty (n-slices)
  (declare (ignore n-slices))
  nil)

(defun set/full (n-slices)
  (range 0 n-slices))

(defun set/finalized (set)
  (and set (null (cdr set))))
