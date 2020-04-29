(in-package :wfc-cl)

(defun ncols (im) (second (array-dimensions im)))
(defun nrows (im) (first (array-dimensions im)))

(defun enumerate-range (start end)
  ;; Returns a list [start, end>
  (if (>= start end)
      nil
      (cons start (enumerate-range (1+ start) end))))

(defun combinations (seq1 seq2)
  (reduce #'append (mapcar (lambda (e2) (mapcar (lambda (e1) (list e1 e2)) seq1)) seq2)))

(defun 2d-window (arr row col width height)
  (let* ((slice (numcl:zeros (list height width) :type 'integer)))
    (mapcar (lambda (offsets)
              (destructuring-bind (srow scol) offsets
                (setf (numcl:aref slice srow scol) (numcl:aref arr (+ row srow) (+ col scol)))))
            (combinations (enumerate-range 0 height) (enumerate-range 0 width)))
    slice))

(defun make-slices (image filter-ncols filter-nrows)
  ;; Just collect the set of (filter-ncols, filter-nrows slices present in image)
  (let ((slices (make-hash-table :test #'numcl:equalp)))
    (mapcar (lambda (offs)
              (setf (gethash (2d-window image (first offs) (second offs)
                                        filter-ncols filter-nrows)
                             slices)
                    t))
            (combinations (enumerate-range 0 (1+ (- (nrows image) filter-nrows)))
                          (enumerate-range 0 (1+ (- (ncols image) filter-ncols)))))
    (alexandria:hash-table-keys slices)))

(defun allowable-offsets (root-img filter)
  ;; This would be clearer using loop honestly
  (let ((filter-nrows (nrows filter))
        (filter-ncols (ncols filter)))
    (remove-if-not
     (lambda (filter-offset)
       (destructuring-bind (row-off col-off) filter-offset
         (every (lambda (root-idxs)
                  (destructuring-bind (row col) root-idxs
                    (equalp (numcl:aref root-img row col)
                            (numcl:aref filter (- row row-off) (- col col-off)))))
                (combinations (enumerate-range (max 0 row-off)
                                               (min (nrows root-img) (+ filter-nrows row-off)))
                              (enumerate-range (max 0 col-off)
                                               (min (ncols root-img) (+ filter-ncols col-off)))))))
     (combinations (enumerate-range (- 1 (nrows root-img)) (nrows root-img))
                   (enumerate-range (- 1 (ncols root-img)) (ncols root-img))))))

;; (defun wave-function-collapse (image filter-ncols filter-nrows output-width output-height)
;;   (let ((slices (make-slices image filter-ncols filter-nrows)))))
