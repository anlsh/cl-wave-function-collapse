(in-package :wfc-cl)

;; Abstraction functions
(defun ncols (im) (second (array-dimensions im)))
(defun nrows (im) (first (array-dimensions im)))

(defun blank-slice (nrows ncols)
  (make-array (list nrows ncols)))
(defvar slice-equality-test #'equalp)
(defvar element-equality-test #'equalp)

(defun num-possibs (bitset)
  (reduce #'+ bitset :initial-value 0))
(defun set-inter (set1 set2)
  (bit-and set1 set2))
(defun random-from-bitset (bitset)
  (loop for i from 0
        with count = 0
        with rand = (1+ (random (num-possibs bitset)))
        do (progn (incf count (aref bitset i))
                  (when (<= rand count) (return-from random-from-bitset i)))))
(defun singleton-bitset (i n-slices)
  (let ((s (make-array n-slices :initial-element 0 :element-type 'bit)))
    (setf (aref s i) 1)
    s))

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

(defun make-wave-fn (num-slices nrows ncols)
  (let ((wave (make-array (list nrows ncols))))
    (loop for r from 0 to (1- nrows)
          do (loop for c from 0 to (1- ncols)
                   do (setf (aref wave r c) (make-array num-slices
                                                        :initial-element 1 :element-type 'bit))))
    wave))

(defun wave-function-collapse (image filter-ncols filter-nrows out-nrows out-ncols)
  (let* ((mb-filter-offs (product (range 0 (1- filter-nrows)) (range 0 (1- filter-ncols))))
         (slices (make-slices image filter-ncols filter-nrows))
         (num-slices (length slices))
         (empty-bitset (make-array num-slices :initial-element 0 :element-type 'bit))
         (lookup (index-to-lookup (make-index slices) num-slices))
         (wave (make-wave-fn num-slices out-nrows out-ncols)))
    (labels ((entropy-fn (bitset)
               (1- (reduce #'+ bitset)))
             (min-ent-locs ()
               (let (min-locs)
                 (loop with min-ent = (1+ num-slices)
                       for r from 0 to (1- out-nrows)
                       do (loop for c from 0 to (1- out-ncols)
                                for cell-ent = (funcall #'entropy-fn (aref wave r c))
                                for loc = (list r c)
                                do (if (> cell-ent 0)
                                       (cond ((< cell-ent min-ent) (setf min-ent cell-ent
                                                                         min-locs (list loc)))
                                             ((= cell-ent min-ent) (push loc min-locs))))))
                 min-locs)))
      (loop for min-locs = (min-ent-locs)
            while min-locs
            for update-loc = (alexandria:random-elt min-locs)
            for (urow ucol) = update-loc
            for chosen-slice-idx = (random-from-bitset (aref wave urow ucol))
            for singleton = (singleton-bitset chosen-slice-idx num-slices)
            do (progn (setf (aref wave urow ucol) singleton)
                      (loop for (frow-off fcol-off) in mb-filter-offs
                            for lrow = (+ urow frow-off)
                            for lcol = (+ ucol fcol-off)
                            when (and (<= 0 lrow (1- out-nrows)) (<= 0 out-ncols))
                              do (setf (aref wave lrow lcol)
                                       (bit-and (or (gethash (list lrow lcol)
                                                             (gethash chosen-slice-idx lookup))
                                                    empty-bitset)
                                                (aref wave lrow lcol))))))
      wave)))
