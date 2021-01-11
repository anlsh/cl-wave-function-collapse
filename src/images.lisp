(uiop:define-package :wfc/src/image
  (:use :cl)
  (:use-reexport :wfc/src/wfc))

(let* ((pixel-size 10)
       (source-path #P"~/Downloads/flowers.png")
       (source-png (png:load-file source-path))
       (source-data (png:data source-png))

       (out-dims '(128 64)))

  (defun draw-image (source-data)
    (sd:with-init (:everything)
      (sd:with-window (win :w (* (ncols source-data) pixel-size)
                           :h (* (nrows source-data) pixel-size)
                           :flags '(:shown :opengl))
        (sd:with-gl-context (ctx win)
          (sd:with-renderer (rend win)
            (sdl2:with-event-loop (:method :poll)
              (:quit () t)
              (:idle ()
                     (loop for i below (array-total-size source-data)
                           for (row col) = (alx:rmajor-to-indices (array-dimensions source-data) i)
                           for color = (elt source-data (list row col))
                           do
                              (sdl2:set-render-draw-color rend
                                                          (aref color 0)
                                                          (aref color 1)
                                                          (aref color 2) 255)
                              (sd:render-fill-rect rend
                                                   (sd:make-rect (* col pixel-size)
                                                                 (* row pixel-size)
                                                                 pixel-size pixel-size)))

                     ;; (gl:flush)
                     ;; (sdl2:gl-swap-window win)
                     (sdl2:render-present rend))))))))

  (defun draw-src ()
    (draw-image source-data))

  (defun draw-wfc ()
    (draw-image (wave-function-collapse source-data 4 4 out-dims))))
