
(uiop:define-package :wfc/src/images
  (:use :cl :arrow-macros
        :wfc/src/utils
        :wfc/src/core
        :wfc/src/pipeline)
  (:local-nicknames (:img :opticl)))

(in-package :wfc/src/images)

(defun slice-idx-fn-from-dims (slice-dims)
  (let ((offsets (-<> slice-dims
                   (picl:map #'picl:range <>)
                   (picl:apply #'picl:product <>)
                   (picl:iter-to-list <>)
                   (fset:convert 'fset:set <>))))
    (lambda (loc)
      (-<> offsets
        (fset:image (lambda (off) (map 'vector #'+ loc off)) <>)))))

(defun domain-for-dims (dims)
  (-<> dims
    (picl:map #'picl:range <>)
    (picl:apply #'picl:product <>)
    (picl:iter-to-list <>)
    (fset:convert 'fset:set <>)))

(defun wfc-from-image (source-img-path output-file-path slice-dims out-dims)
  "Samples from source-png and writes to an output image

1, `source-png`: Path to the image to sample from
2. `output-file-path`: Path to the output files
3. `slice-dims` a vector representing the slice dimensions
4. `out-dims`: a vector giving the dimensions of the ouput image"
  (let* ((source-png (opticl:read-image-file source-img-path))
         (src-dims (array-dimensions source-png))
         (slice-idx-fn (slice-idx-fn-from-dims slice-dims))
         (src-domain (domain-for-dims (subseq src-dims 0 2)))
         (out-domain (domain-for-dims out-dims))
         (nbor-fn (construct-nbor-fn out-domain slice-idx-fn))
         (state-map (lift-set (lambda (pos) (vector (aref source-png (aref pos 0) (aref pos 1) 0)
                                                    (aref source-png (aref pos 0) (aref pos 1) 1)
                                                    (aref source-png (aref pos 0) (aref pos 1) 2)))
                              src-domain)))
    (labels ((slice-loc? (pos) (every #'<= (map 'vector #'+ pos slice-dims src-dims))))
      (let* ((slice-seq (fset:convert 'fset:seq
                                      (construct-slices state-map slice-idx-fn #'slice-loc?)))
             (offslice-decider (offslice-decider-coords slice-seq slice-idx-fn #(0 0) #'equalp))
             (pinned-slices (wfc-parametrized (domain-for-dims out-dims)
                                              (range-set (fset:size slice-seq))
                                              nbor-fn offslice-decider)))
        pinned-slices))))
