;;;; wfc-cl.asd

(asdf:defsystem #:wfc
  :description "Describe wfc-cl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:volt :alexandria :clunit :generic-cl :named-readtables
                     :picl
                     :cl-opengl :sdl2 :pngload-fast)
  :components ((:file "package")
               (:file "list-set" :depends-on ("package"))
               (:file "wfc" :depends-on ("package" "list-set"))
               (:file "tests" :depends-on ("wfc"))))
