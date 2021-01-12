;;;; package.lisp

(uiop:define-package :wfc/package
  (:use #:cl)
  (:nicknames #:wfc)
  (:use-reexport :wfc/src/package))
