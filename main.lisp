(in-package :wfc-cl)

;; Abstraction functions
(defun ncols (im) (second (array-dimensions im)))
(defun nrows (im) (first (array-dimensions im)))

(defun blank-slice (nrows ncols)
  (make-array (list nrows ncols) :element-type 'integer))
(defvar slice-equality-test #'equalp)
(defvar element-equality-test #'equalp)

(defun range (start end)
  ;; Returns a list [start, end>
  ;; TODO Would be better with generators
  (if (>= start end)
      nil
      (cons start (range (1+ start) end))))

(defun product (seq1 seq2)
  (reduce #'append (mapcar (lambda (e2) (mapcar (lambda (e1) (list e1 e2)) seq1)) seq2)))

(defun 2d-window (arr row col nrows ncols)
  (let* ((slice (blank-slice nrows ncols)))
    (mapcar (lambda (offsets)
              (destructuring-bind (srow scol) offsets
                (setf (aref slice srow scol) (aref arr (+ row srow) (+ col scol)))))
            (product (range 0 nrows) (range 0 ncols)))
    slice))

(defun make-slices (image filter-ncols filter-nrows)
  ;; Just collect the set of (filter-ncols, filter-nrows slices present in image)
  ;; Returns a list of the [filter-nrows, filter-ncols] slices of the image with no duplicate
  (let ((slices (make-hash-table :test slice-equality-test)))
    (mapcar (lambda (offs)
              (setf (gethash (2d-window image (first offs) (second offs)
                                        filter-ncols filter-nrows)
                             slices)
                    t))
            (product (range 0 (1+ (- (nrows image) filter-nrows)))
                     (range 0 (1+ (- (ncols image) filter-ncols)))))
    (alexandria:hash-table-keys slices)))

(defun allowable-offsets (root-img filter)
  ;; Returns a list of allowable offsets ((r0, c0), (r1, c1), ...) such that when the
  ;; top-left corner of filter is placed at a row, col offset of (ri, ci), the overlapping
  ;; portions of root-img and filter coincide
  ;; TODO This function could use less memory if rewritten to use generators
  (let ((filter-nrows (nrows filter))
        (filter-ncols (ncols filter)))
    (remove-if-not
     (lambda (filter-offset)
       (destructuring-bind (row-off col-off) filter-offset
         (every (lambda (root-idxs)
                  (destructuring-bind (row col) root-idxs
                    (funcall element-equality-test
                             (aref root-img row col)
                             (aref filter (- row row-off) (- col col-off)))))
                (product (range (max 0 row-off)
                                (min (nrows root-img) (+ filter-nrows row-off)))
                         (range (max 0 col-off)
                                (min (ncols root-img) (+ filter-ncols col-off)))))))
     (product (range (- 1 (nrows root-img)) (nrows root-img))
              (range (- 1 (ncols root-img)) (ncols root-img))))))

(defun make-index (slices)
  ;; Given a list of slices of length n, generate a hash map "index" where
  ;; index[i][j] = (allowable-offsets slices[i] slices[j])
  ;; TODO This function doesn't take advantage of relationship between index[i][j] and index[j][i]
  (let ((valid-offsets (make-hash-table :test #'equal)))
    (loop for slice1 in slices
          for i0 from 0
          do (loop for slice2 in slices
                   for i1 from 0
                   do (setf (gethash (list i0 i1) valid-offsets) (allowable-offsets slice1 slice2))))
    valid-offsets))

(defun index-to-lookup (index num-slices)
  ;; Given an index of the sort described by make-index, construct a hash table "lookup"
  ;; where lookup[i][off] is the set of slice indexes {j0, ..., jf} such that "off" is in
  ;; (allowable-offsets i jf)
  ;; lookup[i][off] might not exist in the table, in which the key set is empty
  (let ((lookup {}))
    (loop for (i j) in (alexandria:hash-table-keys index)
          for offsets = (gethash (list i j) index)
          do (volt:put-if-absent i {} lookup)
             (loop for i-lookup = (gethash i lookup)
                   for off in offsets
                   do
                      (volt:put-if-absent off
                                          (make-array num-slices :element-type 'bit)
                                          i-lookup)
                      (setf (aref (gethash off i-lookup) j) 1)))
    lookup))

(defun wave-function-collapse (image filter-ncols filter-nrows output-width output-height)
  (let* ((slices (make-slices image filter-ncols filter-nrows))
         (num-slices (length slices))
         (lookup (index-to-lookup (make-index (make-index slices)) num-slices)))
    (print slices)
    (print num-slices)
    (print lookup)
    ))
