;;;; wfc-cl.asd

(asdf:defsystem #:wfc-cl
  :description "Describe wfc-cl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:volt :alexandria :numcl :clunit :generic-cl)
  :components ((:file "package")
               (:file "wfc-cl" :depends-on ("package"))
               (:file "tests" :depends-on ("wfc-cl"))))
