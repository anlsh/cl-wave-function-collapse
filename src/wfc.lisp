(uiop:define-package :wfc/src/wfc
  (:use :cl :arrow-macros))

(in-package :wfc/src/wfc)

;; Abstraction functions
(defun ncols (im) (gcl:elt (array-dimensions im) 1))
(defun nrows (im) (gcl:elt (array-dimensions im) 0))
(declaim (inline ncols) (inline nrows))

(defun array-from-thunk (dims &key (value-thunk nil) (el-type t))
  (let ((wave (make-array dims :element-type el-type)))
    (when value-thunk
      (loop for i below (array-total-size wave)
            do (setf (elt wave i) (funcall value-thunk))))
    wave))

(defun 2d-window (arr row col nrows ncols)
  (let ((slice (make-array (list nrows ncols))))
    (picl:map (lambda (offsets)
                (destructuring-bind (srow scol) offsets
                  (setf (elt slice (list srow scol)) (elt arr (list (+ row srow) (+ col scol))))))
              (product (range nrows) (range ncols)))
    slice))

(defun make-slices (image f-nrows f-ncols)
  ;; Just collect the set of (f-ncols, f-nrows slices present in image)
  ;; Returns a list of the f-nrows, f-ncols slices of the image with no duplicate
  (let ((slices {}))
    (picl:map (lambda (offs)
                (setf (gcl:elt slices (2d-window image (gcl:elt offs 0) (gcl:elt offs 1)
                                                 f-nrows f-ncols)) t))
              (product (range (- (nrows image) f-nrows))
                       (range (- (ncols image) f-ncols))))
    (map-keys slices)))

(defun allowable-offsets (root mask)
  ;; Returns a list of allowable offsets ((r0, c0), (r1, c1), ...) such that when the
  ;; top-left corner of filter is placed at a row, col offset of (ri, ci), the overlapping
  ;; portions of root and filter coincide
  (let ((filter-nrows (nrows filter))
        (filter-ncols (ncols filter)))
    (picl:filter (lambda (mask-offset)
                   (destructuring-bind (row-off col-off) mask-offset
                     (always (picl:map (lambda (root-idxs)
                                         (equalp (gcl:elt root root-idxs)
                                                 (gcl:elt mask (picl:map #'- root-idxs mask-offset))))
                                       (product (range (max 0 row-off)
                                                       (min (nrows root) (+ mask-nrows row-off)))
                                                (range (max 0 col-off)
                                                       (min (ncols root) (+ mask-ncols col-off))))))))
                 (product (range (1- mask-nrows) (nrows root))
                          (range (1- mask-ncols) (ncols root))))))

(defun make-index (slices)
  ;; Given a list of slices of length n, generate a hash map "index" where
  ;; index(i,j} = (allowable-offsets slices(i) slices(j))
  (let ((valid-offsets {}))
    (picl:starmap (lambda (slice-spec0 slice-spec1)
                    (let ((i0 (car slice-spec-0))
                          (i1 (car slice-spec-1))
                          (s0 (cadr slice-spec-0))
                          (s1 (cadr slice-spec-1))))
                    (unless (gcl:elt (list i0 i1) valid-offsets)
                      (let ((offs (allowable-offsets s0 s1)))
                        (setf (elt valid-offsets (list i0 i1)) offs)
                        (setf (elt valid-offsets (list i1 i0))
                              (loop for (roff coff) in offs
                                    collect (list (* -1 roff) (* -1 coff)))))))
                  (picl:nfold-product 2 (picl:enumerate slices)))
    valid-offsets))

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
               (loop with changed = {(loc t)}
                     for new-changed = {}
                     do (loop
                          for loc in (map-keys changed)
                          do (loop
                               for off in mb-filter-offs
                               for off-loc = (mapcar #'+ loc off)
                               when (apply #'array-in-bounds-p wave off-loc)
                                 do (loop with orig-off-loc-possibs = (elt wave off-loc)
                                          for idx in (elt wave loc)
                                          for possibs = (set/union possibs (elt (elt lookup idx) off))
                                          finally
                                             (setf (elt wave off-loc)
                                                   (or (set/inter possibs orig-off-loc-possibs)
                                                       (error "Unresolvalble configuration")))
                                             (unless (equalp (elt wave off-loc) orig-off-loc-possibs)
                                               (setf (elt new-changed off-loc) t))))
                          finally (setf changed new-changed))
                     while (map-keys changed))))

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
               (setf (elt final-output loc) (elt slice-reprs (set/singleton-el (elt wave loc))))
            finally
               (return final-output)))))

(defun loc-subtract (a b)
  (map 'vector #'- a b))

(defun wfc (state-map slice-idx-fn loc-is-edge?)
  "Arguments are
1. A map from locations in the source space to values
2. The slice-idx-fn should map each location in state-map to the set of locations forming a
   slice 'rooted' at said location.
   Note: (slice-idx-fn loc) union (the set of locations with loc contained in their slice-idx-fn)
         forms the 'neighborhood' of loc, ie this function is 'asymmetric' in a sense
3, Argument 3 determines whether any given loc is near an edge (ie some of its slice-idx-fn is out
   of bounds), which determines whether the given loc is used to generate slices"

  (let* ((loc-nbor-map
           (let ((res (fset:empty-map (fset:empty-set))))
             ;; We use fset:image only for the side effects here, so return nil from the respective
             ;; lambdas to avoid any potentially expensive construction of result sets
             (fset:image (lambda (loc)
                           (let ((loc-nbors (funcall slice-idx-fn loc)))
                             (fset:unionf (fset:@ res loc) loc-nbors)
                             (fset:image (lambda (nbor)
                                           (fset:unionf (fset:@ res nbor)
                                                        (fset:set loc))
                                           nil)
                                         loc-nbors))
                           nil)
                         (fset:domain state-map))
             res))
         (slices (-<> state-map
                   (fset:domain <>)
                   (fset:filter (lambda (x) (not (funcall loc-is-edge? x)))
                                <>)
                   (fset:image (lambda (loc)
                                 (fset:reduce (lambda (nbor-map nbor-loc)
                                                (fset:with nbor-map
                                                           (loc-subtract nbor-loc loc)
                                                           (fset:@ state-map nbor-loc)))
                                              (funcall slice-idx-fn loc)))
                               <>))))))
