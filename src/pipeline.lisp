
(uiop:define-package :wfc/src/pipeline
  (:use :cl :arrow-macros
        :wfc/src/utils)
  (:export
   #:construct-nbor-map
   #:construct-nbor-fn
   #:construct-slices
   #:offslice-decider-raw
   #:offslice-decider-coords))

(in-package :wfc/src/pipeline)

(defun construct-nbor-map (domain slice-idx-fn)
  (let ((res (fset:empty-map (fset:empty-set))))
    (fset:do-set (loc domain)
      (let ((loc-nbors (fset:intersection (funcall slice-idx-fn loc)
                                          domain)))
        (fset:unionf (fset:@ res loc) loc-nbors)
        (fset:do-set (nbor loc-nbors)
          (fset:unionf (fset:@ res nbor)
                       (fset:set loc)))))
    res))

(defun construct-nbor-fn (domain slice-idx-fn)
  (let ((nbor-map (construct-nbor-map domain slice-idx-fn)))
    (lambda (loc) (fset:@ nbor-map loc))))

(defun construct-slices (state-map slice-idx-fn slice-loc?)
  "Arguments are
1. A map from locations in the source space to values
2. The slice-idx-fn should map each location in state-map to the set of locations forming a
   slice 'rooted' at said location.
   Note: (slice-idx-fn loc) union (the set of locations with loc contained in their slice-idx-fn)
         forms the 'neighborhood' of loc, ie this function is 'asymmetric' in a sense
3, Argument 3 determines whether any given loc will be used to generate a slice: useful for not
   slicing near boundaries, for instance"
  (-<> state-map
    (fset:domain <>)
    (fset:filter slice-loc? <>)
    (fset:image (lambda (loc)
                  (fset:reduce (lambda (slice-map slice-loc)
                                 (fset:with slice-map
                                            (loc-subtract slice-loc loc)
                                            (fset:@ state-map slice-loc)))
                               (funcall slice-idx-fn loc)
                               :initial-value (fset:empty-map)))
                <>)))

(defun offslice-decider-raw (slice-idx-fn origin val-eql?)
  "Returns a memoizing function which takes an offset and two slices and returns whether such
a placement is valid"
  (let ((cache (fset:empty-map)))
    (lambda (off s0 s1)
      (let ((key (list off s0 s1)))
        (multiple-value-bind (val done) (fset:@ cache key)
          (if done
              val
              (setf (fset:@ cache key)
                    (-<> (fset:set origin off)
                      (fset:image slice-idx-fn <>)
                      (fset:reduce #'fset:intersection <>)
                      ;; TODO I'd really like to be able to short-circuit the reduce function...
                      ;; Right now I it'll keep chugging even after the first mismatch is detected
                      (fset:reduce (lambda (res pos)
                                     (and res (funcall val-eql?
                                                       (fset:@ s0 pos)
                                                       (fset:@ s1 (loc-subtract pos off)))))
                                   <> :initial-value t)))))))))

(defun offslice-decider-coords (slices-seq slice-idx-fn origin val-eql?)
  (let ((raw-decider (offslice-decider-raw slice-idx-fn origin val-eql?)))
    (lambda (off s0coord s1coord)
      (funcall raw-decider off (fset:@ slices-seq s0coord) (fset:@ slices-seq s1coord)))))
