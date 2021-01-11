;;;; wfc-cl.asd

(asdf:defsystem #:wfc
  :description "Wavefunction Collapse in Common Lisp"
  :author "Anish Moorthy <anlsh@protonmail.com>"
  :license  "MIT"
  :version "0.0.2"
  :class :package-inferred-system
  :depends-on (
               :alexandria
               :arrow-macros
               :picl
               ;; :cl-opengl
               ;; :sdl2
               :pngload-fast
               :uiop
               :fset

               :wfc/package))
