(in-package :wfc)
(nrt:in-readtable volt:readtable)

;; Abstraction functions
(defun ncols (im) [(array-dimensions im) 1])
(defun nrows (im) [(array-dimensions im) 0])
(declaim (inline ncols) (inline nrows))

(defun num-possibs (set)
  (reduce #'+ set :initial-value 0))

(defun set-inter (set1 set2)
  (bit-and set1 set2))

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
  (let* ((slice (make-array (list nrows ncols))))
    (mapcar (lambda (offsets)
              (destructuring-bind (srow scol) offsets
                (setf (aref slice srow scol) (aref arr (+ row srow) (+ col scol)))))
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
         (slices (make-slices image filter-ncols filter-nrows))
         (num-slices (length slices))
         (empty-set (empty-set num-slices))
         (lookup (index-to-lookup (make-index slices) num-slices))
         (wave (array-from-thunk (list out-nrows out-ncols)
                                 :value-thunk (lambda () (full-set num-slices)))))
    (labels ((entropy-fn (bitset)
               (1- (reduce #'+ bitset)))
             (min-ent-locs ()
               (loop with min-locs = nil
                      with min-ent = (1+ num-slices)
                      for i below (array-total-size wave)
                      for cell-ent = (funcall #'entropy-fn [wave i])
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
      wave)))

(nrt:in-readtable volt:readtable)
(let* ((pixel-size 20)
       (source-path #P"~/Downloads/flowers.png")
       (source-png (png:load-file source-path))
       (source-data (png:data source-png))
       (source-width (png:width source-png)) (source-height (png:height source-png)))

  (sd:with-init (:everything)
    (sd:with-window (win :w 800 :h 600 :flags '(:shown :opengl))
      (sd:with-gl-context (ctx win)
        (sdl2:gl-make-current win ctx)
        (gl:viewport 0 0 800 600)
        (gl:matrix-mode :projection)
        (gl:ortho -2 2 -2 2 -2 2)
        (gl:matrix-mode :modelview)
        (gl:load-identity)
        (gl:clear-color 0.0 0.0 1.0 1.0)
        (gl:clear :color-buffer)
        (sdl2:with-event-loop (:method :poll)
          (:keydown (:keysym keysym)
                    (let ((scancode (sdl2:scancode-value keysym))
                          (sym (sdl2:sym-value keysym))
                          (mod-value (sdl2:mod-value keysym)))
                      (cond
                        ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
                        ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
                        ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
                      (format t "Key sym: ~a, code: ~a, mod: ~a~%"
                              sym
                              scancode
                              mod-value)))

          (:idle ()
                 (gl:clear :color-buffer)
                 (gl:begin :triangles)
                 (gl:color 255 0 0)
                 (gl:vertex 0.0 1.0)
                 (gl:vertex -1.0 -1.0)
                 (gl:vertex 1.0 -1.0)
                 (gl:end)
                 (loop for i below (array-total-size source-data)
                       for (row col) = (alx:rmajor-to-indices (array-dimensions source-data) i)
                       for color = (generic-cl:elt (list col) (generic-cl:elt (list row) source-data))
                       do (+ 1 2))


                 ;; (loop
                 ;;   for i below (array-total-size source-data)
                 ;;   for (row col) = (alx:rmajor-to-indices (array-dimensions source-data) i)
                 ;;   for color = [[source-data row] col]
                 ;;   do
                 ;;      (+ 1 2)
                 ;;      (/ 1 0)
                 ;;      (sk:with-pen (sk:make-pen :fill (sk:rgb ;; (/ col source-height)
                 ;;                                       ;; 0 (/ row source-width)
                 ;;                                       (aref color 0) (aref color 1) (aref color 2)))
                 ;;        (sk:rect (* col pixel-size) (* row pixel-size) pixel-size pixel-size)))
                 ;;   )
                 (gl:flush)
                 (sdl2:gl-swap-window win))

          (:quit () t))))))
