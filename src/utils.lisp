
(uiop:define-package :wfc/src/utils
  (:use :cl)
  (:documentation
   "Internal functionality for super-basic stuff")
  (:export #:loc-subtract))

(in-package :wfc/src/utils)

(defun loc-subtract (a b)
  (map 'vector #'- a b))
