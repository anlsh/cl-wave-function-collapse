;;;; package.lisp

(defpackage #:wfc
  (:use #:generic-cl #:volt)
  (:local-nicknames (#:nrt #:named-readtables) (#:alx #:alexandria-2)
                    (#:gl #:cl-opengl) (#:sd #:sdl2) (#:png #:pngload-fast)))
