(in-package :wfc)
(nrt:in-readtable volt:readtable)

;; Abstraction functions
(defun ncols (im) [(array-dimensions im) 1])
(defun nrows (im) [(array-dimensions im) 0])
(declaim (inline ncols) (inline nrows))

(defun num-possibs (set)
  (reduce #'+ set :initial-value 0))

(defun entropy-fn (bitset)
  (1- (reduce #'+ bitset)))

(defun set-to-index (bitset)
  (loop for i from 0 below (length bitset)
        when (not (zerop [bitset i])) do (return i)
          finally (return #(255 0 255))))

(defun set-inter (set1 set2)
  (bit-and set1 set2))

(defun range (start end)
  (labels ((rec (start end acc)
             (if (<= end start)
                 acc
                 (rec start (1- end) (cons (1- end) acc)))))
    (rec start end nil)))

(defun random-from-set (set)
  (loop for i from 0
        with count = 0
        with rand = (1+ (random (num-possibs set)))
        do (progn (incf count [set i])
                  (when (<= rand count) (return-from random-from-set i)))))

(defun singleton-set (i n-slices)
  (let ((s (make-array n-slices :initial-element 0 :element-type 'bit)))
    (setf [s i] 1)
    s))

(defun empty-set (n-slices)
  (make-array n-slices :initial-element 0 :element-type 'bit))

(defun full-set (n-slices)
  (make-array n-slices :initial-element 1 :element-type 'bit))

(defun product (seq1 seq2)
  (reduce #'append (mapcar (lambda (e2) (mapcar (lambda (e1) (list e1 e2)) seq1))
                           seq2)))

(defun array-from-thunk (dims &key (value-thunk nil) (el-type t))
  (let ((wave (make-array dims :element-type el-type)))
    (when value-thunk
      (loop for i below (array-total-size wave)
            do (setf [wave i] (funcall value-thunk))))
    wave))

(defun 2d-window (arr row col nrows ncols)
  (let ((slice (make-array (list nrows ncols))))
    (mapcar (lambda (offsets)
              (destructuring-bind (srow scol) offsets
                (setf [slice (list srow scol)] [arr (list (+ row srow) (+ col scol))])))
            (product (range 0 nrows) (range 0 ncols)))
    slice))

(defun make-slices (image f-nrows f-ncols )
  ;; Just collect the set of (f-ncols, f-nrows slices present in image)
  ;; Returns a list of the f-nrows, f-ncols slices of the image with no duplicate
  (let ((slices {}))
    (mapcar (lambda (offs)
              (setf [slices (2d-window image (first offs) (second offs) f-nrows f-ncols)] t))
            (product (range 0 (1+ (- (nrows image) f-nrows)))
                     (range 0 (1+ (- (ncols image) f-ncols)))))
    (map-keys slices)))

(defun allowable-offsets (root filter)
  ;; Returns a list of allowable offsets ((r0, c0), (r1, c1), ...) such that when the
  ;; top-left corner of filter is placed at a row, col offset of (ri, ci), the overlapping
  ;; portions of root and filter coincide
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
  ;; index[i,j] = (allowable-offsets slices[i] slices[j])
  (loop with valid-offsets = {}
        for slice-ls on slices
        for s0 = (car slice-ls)
        for i0 from 0
        do (loop for slice1 in slices
                 for i1 from 0
                 unless (find (list i0 i1) (map-keys valid-offsets))
                   do (let ((offs (allowable-offsets s0 slice1)))
                        (setf [valid-offsets (list i0 i1)] offs)
                        (setf [valid-offsets (list i1 i0)]
                              (loop for (roff coff) in offs
                                    collect (list (* -1 roff) (* -1 coff))))))
        finally (return valid-offsets)))

(defun index-to-lookup (index num-slices)
  ;; Given an index of the sort described by make-index, construct a hash table "lookup"
  ;; where lookup[i][off] is the set of slice indexes {j0, ..., jf} such that "off" is in
  ;; (allowable-offsets i jf)
  ;; lookup[i][off] might not exist in the table, in which the key set is empty
  (loop with lookup = {}
        for (i j) in (map-keys index)
        for offsets = [index (list i j)]
        do (ensure-get i lookup {})
           (loop for i-lookup = [lookup i]
                 for off in offsets
                 do
                    (ensure-get off i-lookup (make-array num-slices :element-type 'bit))
                    (setf [[i-lookup off] j] 1))
        finally (return lookup)))

(defun wave-function-collapse (image filter-ncols filter-nrows out-nrows out-ncols)
  (let* ((mb-filter-offs (product (range 0 (1- filter-nrows)) (range 0 (1- filter-ncols))))
         (slices (make-slices image filter-nrows filter-ncols))
         (slice-reprs (loop with h = {}
                            for s in slices
                            for i from 0
                            do (setf [h i] [s (list 0 0)])
                            finally (return h)))
         (num-slices (length slices))
         (empty-set (empty-set num-slices))
         (lookup (index-to-lookup (make-index slices) num-slices))
         (wave (array-from-thunk (list out-nrows out-ncols)
                                 :value-thunk (lambda () (full-set num-slices)))))
    (labels ((min-ent-locs ()
               (loop with min-locs = nil
                     with min-ent = (1+ num-slices)
                     for i below (array-total-size wave)
                     for cell-ent = (entropy-fn [wave i])
                     for (r c) = (alx:rmajor-to-indices (array-dimensions wave) i)
                     for loc = (list r c)
                     do (when (> cell-ent 0)
                          (cond ((< cell-ent min-ent) (setf min-ent cell-ent
                                                            min-locs (list loc)))
                                ((= cell-ent min-ent) (push loc min-locs))))
                     finally (return min-locs))))

      (loop for min-locs = (min-ent-locs)
            while min-locs
            for update-loc = (alexandria:random-elt min-locs)
            for (urow ucol) = update-loc
            for chosen-slice-idx = (random-from-set (aref wave urow ucol))
            for singleton = (singleton-set chosen-slice-idx num-slices)
            do (setf (aref wave urow ucol) singleton)
               (loop for (row-off col-off) in mb-filter-offs
                     for lrow = (+ urow row-off)
                     for lcol = (+ ucol col-off)
                     when (array-in-bounds-p wave lrow lcol)
                       do (setf (aref wave lrow lcol)
                                (set-inter (or [[lookup chosen-slice-idx] (list row-off col-off)]
                                               empty-set)
                                           (aref wave lrow lcol)))))
      (loop for i below (array-total-size wave)
            for loc = (alx:rmajor-to-indices (array-dimensions wave) i)
            with final-output = (make-array (list out-nrows out-ncols))
            do (setf [final-output loc] [slice-reprs (set-to-index [wave loc])])
            finally (return final-output)))))

(defun encode-sequence (seq)
  (let ((i -1)
        (hash {}))
    (loop for el in seq
          when (not (memberp el hash))
            do (setf [hash el] (incf i)))
    hash))

(nrt:in-readtable volt:readtable)
(let* ((pixel-size 10)
       (source-path #P"~/Downloads/flowers.png")
       (source-png (png:load-file source-path))
       (source-data (png:data source-png))

       (out-width 64)
       (out-height 128))

  (defun draw-src ()
    (let ((source-width (png:width source-png))
          (source-height (png:height source-png)))
      (sd:with-init (:everything)
        (sd:with-window (win :w (* source-width pixel-size)
                             :h (* source-height pixel-size)
                             :flags '(:shown :opengl))
          (sd:with-gl-context (ctx win)
            (sd:with-renderer (rend win)
              (sdl2:with-event-loop (:method :poll)
                (:idle ()
                       (loop for i below (array-total-size source-data)
                             for (row col) = (alx:rmajor-to-indices (array-dimensions source-data) i)
                             for color = (generic-cl:elt (generic-cl:elt source-data (list row)) (list col))
                             do (gl:color (aref color 0) (aref color 1) (aref color 2))
                                (sd:render-fill-rect rend
                                                     (sd:make-rect (* col pixel-size) (* row pixel-size)
                                                                   pixel-size pixel-size)))

                       (gl:flush)
                       (sdl2:gl-swap-window win))

                (:quit () t))))))))

  (defun draw-wfc ()
    (wave-function-collapse source-data 4 4 out-height out-width)))
