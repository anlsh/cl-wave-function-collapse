;;;; wfc-cl.asd

(asdf:defsystem #:wfc
  :description "Describe wfc-cl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:volt :alexandria :clunit :generic-cl :named-readtables)
  :components ((:file "package")
               (:file "main" :depends-on ("package"))
               (:file "tests" :depends-on ("main"))))
