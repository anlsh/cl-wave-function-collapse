(in-package :wfc-cl)

(defun 2d-window (arr row col height width)
  (let* ((slice (numcl:zeros (list height width) :type 'integer)))
    (loop for srow from 0 to (1- height)
          do (loop for el across (subseq (numcl:aref arr (+ row srow)) col (+ col width))
                   for scol from 0
                   do (setf (numcl:aref slice srow scol) el)))
    slice))

;; (defun wave-function-collapse (image filter-width filter-height output-width output-height)
;;   (destructuring-bind (image-height image-width) (array-dimensions image)
;;     ))
