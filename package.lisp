;;;; package.lisp

(defpackage #:wfc
  (:use #:cl)
  (:import-from #:picl #:product #:range #:always)
  (:local-nicknames (#:nrt #:named-readtables) (#:alx #:alexandria-2)
                    (#:gcl #:generic-cl)
                    (#:gl #:cl-opengl) (#:sd #:sdl2) (#:png #:pngload-fast)))
