(in-package :wfc-cl)

;; Abstraction functions
(defun ncols (im) (second (array-dimensions im)))
(defun nrows (im) (first (array-dimensions im)))

(defun num-possibs (set)
  (reduce #'+ set :initial-value 0))
(defun set-inter (set1 set2)
  (bit-and set1 set2))
(defun random-from-set (set)
  (loop for i from 0
        with count = 0
        with rand = (1+ (random (num-possibs set)))
        do (progn (incf count (aref set i))
                  (when (<= rand count) (return-from random-from-set i)))))
(defun singleton-set (i n-slices)
  (let ((s (make-array n-slices :initial-element 0 :element-type 'bit)))
    (setf (aref s i) 1)
    s))
(defun empty-set (n-slices)
  (make-array n-slices :initial-element 0 :element-type 'bit))
(defun full-set (n-slices)
  (make-array n-slices :initial-element 1 :element-type 'bit))

(defun range (start end)
  ;; Returns a list [start, end>
  (if (>= start end)
      nil
      (cons start (range (1+ start) end))))

(defun product (seq1 seq2)
  (reduce #'append (mapcar (lambda (e2) (mapcar (lambda (e1) (list e1 e2)) seq1)) seq2)))

(defun make-empty-array (nrows ncols &key (value-thunk nil) (el-type t))
  (let ((wave (make-array (list nrows ncols) :element-type el-type)))
    (when value-thunk
      (loop for r from 0 to (1- nrows)
            do (loop for c from 0 to (1- ncols)
                     do (setf (aref wave r c) (funcall value-thunk)))))
    wave))

(defun 2d-window (arr row col nrows ncols)
  (let* ((slice (make-array (list nrows ncols))))
    (mapcar (lambda (offsets)
              (destructuring-bind (srow scol) offsets
                (setf (aref slice srow scol) (aref arr (+ row srow) (+ col scol)))))
            (product (range 0 nrows) (range 0 ncols)))
    slice))

(defun make-slices (image f-nrows f-ncols )
  ;; Just collect the set of (f-ncols, f-nrows slices present in image)
  ;; Returns a list of the [f-nrows, f-ncols] slices of the image with no duplicate
  (let ((slices (make-hash-table :test #'equalp)))
    (mapcar (lambda (offs)
              (setf (gethash (2d-window image (first offs) (second offs) f-nrows f-ncols)
                             slices)
                    t))
            (product (range 0 (1+ (- (nrows image) f-nrows)))
                     (range 0 (1+ (- (ncols image) f-ncols)))))
    (alexandria:hash-table-keys slices)))

(defun allowable-offsets (root filter)
  ;; Returns a list of allowable offsets ((r0, c0), (r1, c1), ...) such that when the
  ;; top-left corner of filter is placed at a row, col offset of (ri, ci), the overlapping
  ;; portions of root and filter coincide
  ;; TODO This function could use less memory if rewritten to use generators
  (let ((filter-nrows (nrows filter))
        (filter-ncols (ncols filter))
        (root-nrows (nrows root))
        (root-ncols (ncols root)))
    (remove-if-not
     (lambda (filter-offset)
       (destructuring-bind (row-off col-off) filter-offset
         (every (lambda (root-idxs)
                  (destructuring-bind (row col) root-idxs
                    (funcall #'equalp
                             (aref root row col)
                             (aref filter (- row row-off) (- col col-off)))))
                (product (range (max 0 row-off)
                                (min root-nrows (+ filter-nrows row-off)))
                         (range (max 0 col-off)
                                (min root-ncols (+ filter-ncols col-off)))))))
     (product (range (- 1 (nrows root)) (nrows root))
              (range (- 1 (ncols root)) (ncols root))))))

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

(defun wave-function-collapse (image filter-ncols filter-nrows out-nrows out-ncols)
  (let* ((mb-filter-offs (product (range 0 (1- filter-nrows)) (range 0 (1- filter-ncols))))
         (slices (make-slices image filter-ncols filter-nrows))
         (num-slices (length slices))
         (empty-set (empty-set num-slices))
         (lookup (index-to-lookup (make-index slices) num-slices))
         (wave (make-empty-array out-nrows out-ncols
                                 :value-thunk (lambda () (full-set num-slices)))))
    (labels ((entropy-fn (bitset)
               (1- (reduce #'+ bitset)))
             (min-ent-locs ()
               (let (min-locs)
                 (loop with min-ent = (1+ num-slices)
                       for r from 0 to (1- out-nrows)
                       do (loop for c from 0 to (1- out-ncols)
                                for cell-ent = (funcall #'entropy-fn (aref wave r c))
                                for loc = (list r c)
                                do (when (> cell-ent 0)
                                       (cond ((< cell-ent min-ent) (setf min-ent cell-ent
                                                                         min-locs (list loc)))
                                             ((= cell-ent min-ent) (push loc min-locs))))))
                 min-locs)))
      (loop for min-locs = (min-ent-locs)
            while min-locs
            for update-loc = (alexandria:random-elt min-locs)
            for (urow ucol) = update-loc
            for chosen-slice-idx = (random-from-set (aref wave urow ucol))
            for singleton = (singleton-set chosen-slice-idx num-slices)
            do (progn (setf (aref wave urow ucol) singleton)
                      (loop for (frow-off fcol-off) in mb-filter-offs
                            for lrow = (+ urow frow-off)
                            for lcol = (+ ucol fcol-off)
                            when (array-in-bounds-p wave lrow lcol)
                              do (setf (aref wave lrow lcol)
                                       (set-inter (or (gethash (list lrow lcol)
                                                             (gethash chosen-slice-idx lookup))
                                                    empty-set)
                                                (aref wave lrow lcol))))))
      wave)))
