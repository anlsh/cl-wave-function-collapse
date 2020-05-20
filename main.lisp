(in-package :wfc)
(nrt:in-readtable volt:readtable)

;; Abstraction functions
(defun ncols (im) (elt (array-dimensions im) 1))
(defun nrows (im) (elt (array-dimensions im) 0))
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
        ((= (car s1) (car s2)) (cons (car s1) (set/inter (cdr s1) (cdr s2))))
        ((< (car s1) (car s2)) (set/inter (cdr s1) s2))
        (t (set/inter s1 (cdr s2)))))

(defun set/union (s1 s2)
  (cond ((null s1) s2)
        ((null s2) s1)
        ((= (car s1) (car s2)) (cons (car s1) (set/union (cdr s1) (cdr s2))))
        ((< (car s1) (car s2)) (cons (car s1) (set/union (cdr s1) s2)))
        (t (cons (car s2) (set/union s1 (cdr s2))))))

(defun set/singleton (item n-slices)
  (declare (ignore n-slices))
  (list item))

(defun set/empty (n-slices)
  (declare (ignore n-slices))
  nil)

(defun set/full (n-slices)
  (range 0 n-slices))

(defun set/finalized (set)
  (and set (null (cdr set))))

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
            do (setf (elt wave i) (funcall value-thunk))))
    wave))

(defun 2d-window (arr row col nrows ncols)
  (let ((slice (make-array (list nrows ncols))))
    (mapcar (lambda (offsets)
              (destructuring-bind (srow scol) offsets
                (setf (elt slice (list srow scol)) (elt arr (list (+ row srow) (+ col scol))))))
            (product (range 0 nrows) (range 0 ncols)))
    slice))

(defun make-slices (image f-nrows f-ncols)
  ;; Just collect the set of (f-ncols, f-nrows slices present in image)
  ;; Returns a list of the f-nrows, f-ncols slices of the image with no duplicate
  (let ((slices {}))
    (mapcar (lambda (offs)
              (setf (elt slices (2d-window image (first offs) (second offs) f-nrows f-ncols)) t))
            (product (range 0 (- (nrows image) f-nrows))
                     (range 0 (- (ncols image) f-ncols))))
    (map-keys slices)))

(defun allowable-offsets (root filter)
  ;; Returns a list of allowable offsets ((r0, c0), (r1, c1), ...) such that when the
  ;; top-left corner of filter is placed at a row, col offset of (ri, ci), the overlapping
  ;; portions of root and filter coincide
  (let ((filter-nrows (nrows filter))
        (filter-ncols (ncols filter)))
    (remove-if-not (lambda (filter-offset)
                     (destructuring-bind (row-off col-off) filter-offset
                       (every (lambda (root-idxs)
                                (equalp (elt root root-idxs)
                                        (elt filter (mapcar #'- root-idxs filter-offset))))
                              (product (range (max 0 row-off)
                                              (min (nrows root) (+ filter-nrows row-off)))
                                       (range (max 0 col-off)
                                              (min (ncols root) (+ filter-ncols col-off)))))))
                   (product (range (- 1 filter-nrows) (nrows root))
                            (range (- 1 filter-ncols) (ncols root))))))

(defun make-index (slices)
  ;; Given a list of slices of length n, generate a hash map "index" where
  ;; index(i,j} = (allowable-offsets slices(i) slices(j))
  (loop with valid-offsets = {}
        for slice-ls on slices
        for s0 = (car slice-ls)
        for i0 from 0
        do (loop for s1 in slice-ls
                 for i1 from i0
                 do (let ((offs (allowable-offsets s0 s1)))
                      (setf (elt valid-offsets (list i0 i1)) offs)
                      (setf (elt valid-offsets (list i1 i0))
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
        for offsets = (elt index (list i j))
        do (loop for i-lookup = (ensure-get i lookup {})
                 for off in offsets
                 do
                    (setf (elt i-lookup off) (set/add (ensure-get off i-lookup
                                                        (set/empty num-slices))
                                                      j)))
        finally (return lookup)))

(defun wave-function-collapse (image filter-ncols filter-nrows out-dims)
  (let* ((mb-filter-offs (product (range (- 1 filter-nrows) filter-nrows)
                                  (range (- 1 filter-ncols) filter-ncols)))
         (slices (make-slices image filter-nrows filter-ncols))
         (slice-reprs (loop with h = {}
                            for s in slices
                            for i from 0
                            do (setf (elt h i)  (elt s '(0 0)))
                            finally (return h)))
         (num-slices (length slices))
         (lookup (index-to-lookup (make-index slices) num-slices))
         (wave (array-from-thunk out-dims
                                 :value-thunk (lambda () (set/full num-slices)))))

    (labels ((min-locs ()
               (loop with min-locs = nil
                     with min-ent = nil
                     for i below (array-total-size wave)
                     for cell-ent = (set/size (elt wave i))
                     for loc = (alx:rmajor-to-indices (array-dimensions wave) i)
                     when (not (set/finalized (elt wave i)))
                       do (cond ((or (null min-ent) (< cell-ent min-ent))
                                 (setf min-ent cell-ent
                                       min-locs (list loc)))
                                ((= cell-ent min-ent) (push loc min-locs)))
                     finally (return min-locs)))
             (propagate (loc)
               (declare (ignore loc))
               (loop for changed = nil
                     do (loop
                          for i below (array-total-size wave)
                          for loc = (alx:rmajor-to-indices (array-dimensions wave) i)
                          do (loop
                               for off in mb-filter-offs
                               for off-loc = (mapcar #'+ loc off)
                               when (apply #'array-in-bounds-p wave off-loc)
                                 do (loop with orig-off-loc-possibs = (elt wave off-loc)
                                          for idx in (elt wave loc)
                                          for possibs = (set/union possibs (elt (elt lookup idx) off))
                                          finally
                                             (setf (elt wave off-loc)
                                                   (set/inter possibs orig-off-loc-possibs))
                                             (setf changed (or changed (not (equalp orig-off-loc-possibs
                                                                                    possibs)))))))
                     while changed))
             ;; (propagate-recur (loc)
             ;;   (loop for offs in mb-filter-offs
             ;;         for off-loc = (mapcar #'+ loc offs)
             ;;         when (apply #'array-in-bounds-p wave off-loc)
             ;;           do
             ;;              (loop
             ;;                with orig-possibs = (elt wave off-loc)
             ;;                for idx in (elt wave loc)
             ;;                for possibs = (set/union possibs (elt (elt lookup idx) offs))
             ;;                finally
             ;;                   (setf (elt wave off-loc) (set/inter orig-possibs possibs))
             ;;                   (unless (equalp orig-possibs (elt wave off-loc))
             ;;                     (propagate off-loc)))))
             )

      (loop for min-locs = (min-locs)
            while min-locs
            for chosen-loc = (alx:random-elt min-locs)
            for chosen-idx = (set/random-elt (elt wave chosen-loc))
            do (setf (elt wave chosen-loc) (set/singleton chosen-idx num-slices))
               (propagate chosen-loc))

      (loop for i below (array-total-size wave)
            for loc = (alx:rmajor-to-indices (array-dimensions wave) i)
            with final-output = (make-array out-dims)
            do
               (setf (elt final-output loc) (elt slice-reprs (set/to-index (elt wave loc))))
            finally
               (return final-output)))))

(let* ((pixel-size 10)
       (source-path #P"~/Downloads/pixil-4.png")
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
    (draw-image (wave-function-collapse source-data 2 2 out-dims))))
