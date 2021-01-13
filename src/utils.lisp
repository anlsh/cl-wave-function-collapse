
(uiop:define-package :wfc/src/utils
  (:use :cl :arrow-macros)
  (:documentation
   "Internal functionality for super-basic stuff")
  (:export #:loc-subtract
           #:lift-set
           #:random-from-set
           #:range-set))

(in-package :wfc/src/utils)

(defun loc-subtract (a b)
  (map 'vector #'- a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions that don't exist or that I can't find in FSet

(defun lift-set (fn set)
  "'Lifts' a set to a map given a function"
  (fset:reduce (lambda (map el) (fset:with map el (funcall fn el)))
               set :initial-value (fset:empty-map)))

(defun random-from-set (set)
  (fset:@ (fset:convert 'fset:seq set)
          (random (fset:size set))))

(defun range-set (n)
  ;; Returns [0, n) as a set
  (-<> n (picl:range <>) (picl:iter-to-list <>) (fset:convert 'fset:set <>)))
