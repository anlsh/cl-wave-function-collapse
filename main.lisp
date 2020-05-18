(in-package :wfc)
(nrt:in-readtable volt:readtable)

;; Abstraction functions
(defun ncols (im) [(array-dimensions im) 1])
(defun nrows (im) [(array-dimensions im) 0])
(declaim (inline ncols) (inline nrows))

(defun set/size (set)
  (length set))

(defun set/to-index (set)
  (car set))

(defun set/random-elt (set)
  (alx:random-elt set))

(defun set/add (set el)
  (cond ((null set) (list el))
        ((< (car set) el) (cons (car set) (set/add (cdr set) el)))
        ((= (car set) el) set)
        (t (cons el set))))

(defun set/inter (s1 s2)
  (cond ((null s1) nil)
        ((null s2) nil)
        ((= (car s1) (car s2) (cons (car s1) (set/inter (cdr s1) (cdr s2)))))
        ((< (car s1) (car s2) (set/inter (cdr s1) s2)))
        (t (set/inter s1 (cdr s2)))))

(defun set/singleton (item n-slices)
  (declare (ignore n-slices))
  (list item))

(defun set/empty (n-slices)
  (declare (ignore n-slices))
  nil)

(defun set/full (n-slices)
  (range 0 n-slices))

(defun range (start end)
  (labels ((rec (start end acc)
             (if (<= end start)
                 acc
                 (rec start (1- end) (cons (1- end) acc)))))
    (rec start end nil)))

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
  ;; index(i,j} = (allowable-offsets slices(i) slices(j))
  (loop with valid-offsets = {}
        for slice-ls on slices
        for s0 = (car slice-ls)
        for i0 from 0
        do (loop for slice1 in slices
                 for i1 from 0
                 unless (memberp (list i0 i1) (map-keys valid-offsets))
                   do (let ((offs (allowable-offsets s0 slice1)))
                        (setf [valid-offsets (list i0 i1)] offs)
                        (setf [valid-offsets (list i1 i0)]
                              (loop for (roff coff) in offs
                                    collect (list (* -1 roff) (* -1 coff))))))
        finally (return valid-offsets)))

(defun index-to-lookup (index num-slices)
  ;; Given an index of the sort described by make-index, construct a hash table "lookup"
  ;; where lookup(i)(off) is the set of slice indexes {j0, ..., jf} such that "off" is in
  ;; (allowable-offsets i jf)
  ;; lookup(i)(off) might not exist in the table, in which the key set is empty
  (loop with lookup = {}
        for (i j) in (map-keys index)
        for offsets = [index (list i j)]
        do (ensure-get i lookup {})
           (loop for i-lookup = [lookup i]
                 for off in offsets
                 do
                    (ensure-get off i-lookup (set/empty num-slices))
                    (set/add [i-lookup off] j))
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
         (set/empty (set/empty num-slices))
         (lookup (index-to-lookup (make-index slices) num-slices))
         (wave (array-from-thunk (list out-nrows out-ncols)
                                 :value-thunk (lambda () (set/full num-slices)))))
    (labels ((min-ent-locs ()
               (loop with min-locs = nil
                     with min-ent = (1+ num-slices)
                     for i below (array-total-size wave)
                     for cell-ent = (1- (set/size [wave i]))
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
            for chosen-slice-idx = (set/random-elt (aref wave urow ucol))
            for singleton = (set/singleton chosen-slice-idx num-slices)
            do (setf (aref wave urow ucol) singleton)
               (loop for (row-off col-off) in mb-filter-offs
                     for lrow = (+ urow row-off)
                     for lcol = (+ ucol col-off)
                     when (array-in-bounds-p wave lrow lcol)
                       do (setf (aref wave lrow lcol)
                                (aref wave lrow lcol)
                                (set/inter (or [[lookup chosen-slice-idx] (list row-off col-off)]
                                               set/empty)))))
      (loop for i below (array-total-size wave)
            for loc = (alx:rmajor-to-indices (array-dimensions wave) i)
            with final-output = (make-array (list out-nrows out-ncols))
            do
               (break)
               (setf [final-output loc] [slice-reprs (set/to-index [wave loc])])
            finally (return final-output)))))

(let* ((pixel-size 10)
       (source-path #P"~/Downloads/flowers.png")
       (source-png (png:load-file source-path))
       (source-data (png:data source-png))

       (out-width 64)
       (out-height 128))

  (defun draw-image (source-data)
    (sd:with-init (:everything)
      (sd:with-window (win :w (* [(array-dimensions source-data) 1] pixel-size)
                           :h (* [(array-dimensions source-data) 0] pixel-size)
                           :flags '(:shown :opengl))
        (sd:with-gl-context (ctx win)
          (sd:with-renderer (rend win)
            (sdl2:with-event-loop (:method :poll)
              (:idle ()
                     (loop for i below (array-total-size source-data)
                           for (row col) = (alx:rmajor-to-indices (array-dimensions source-data) i)
                           for color = [source-data (list row col)]
                           do
                              (gl:color (aref color 0) (aref color 1) (aref color 2))
                              (sd:render-fill-rect rend
                                                   (sd:make-rect (* col pixel-size) (* row pixel-size)
                                                                 pixel-size pixel-size)))

                     (gl:flush)
                     (sdl2:gl-swap-window win))

              (:quit () t)))))))

  (defun draw-src ()
    (draw-image source-data))

  (defun draw-wfc ()
    (draw-image (wave-function-collapse source-data 2 2 out-height out-width))))
