(uiop:define-package :wfc/src/wfc
  (:use :cl :arrow-macros))

(in-package :wfc/src/wfc)

;; Abstraction functions

(defun loc-subtract (a b)

  (map 'vector #'- a b))

(defun construct-nbor-map (domain slice-idx-fn)
  (let ((res (fset:empty-map (fset:empty-set))))
    ;; We use fset:image only for the side effects here, so return nil from the respective
    ;; lambdas to avoid any potentially expensive construction of result sets
    (fset:do-set (loc domain)
      (let ((loc-nbors (fset:intersection (funcall slice-idx-fn loc)
                                          domain)))
        (fset:unionf (fset:@ res loc) loc-nbors)
        (fset:do-set (nbor loc-nbors)
          (fset:unionf (fset:@ res nbor)
                       (fset:set loc)))))
    res))

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
                               (funcall slice-idx-fn loc)))
                <>)))

(defun updated-possibs (offslice-decider offset rootslices nborslices)
  ;; TODO This should really only neead the offsets
  (fset:reduce #'fset:union
               (fset:image (lambda (rootslice)
                             (fset:filter (lambda (nborslice)
                                            (funcall offslice-decider
                                                     offset rootslice nborslice))
                                          nborslices))
                           rootslices)))

(defun step-wfc (wave nbor-fn offslice-decider)
  "Takes a single step of the WFC algorithm, and returns the state which was just pinned and the
updated wave.

wave is a map from UNpinned locs to the respective set of remaining possible slices.
It must be 'consistent' in that any single choice of slice for any single state should not
conflict with any pinned states.

Of course, this does not imply that wave admits a consistent assignment. When this is the
case, this function will signal an error
"
  (labels ((random-from-set (set)
             ;; I can't hardcode the error because unreachabel code :/
             ;; (error "Function not implemented")
             (print "This really isn't implemented yet!")
             set)
           (propagate (pos)
             (fset:image (lambda (nbor)
                           (let* ((old-possibs (fset:@ wave nbor))
                                  (new-possibs (updated-possibs offslice-decider
                                                                (loc-subtract nbor pos)
                                                                (fset:@ wave pos)
                                                                (fset:@ wave nbor))))
                             (when (zerop (fset:size new-possibs))
                               (error "Inconsistent assignment"))
                             (setf (fset:@ wave nbor) new-possibs)
                             (unless (fset:equal? old-possibs new-possibs)
                               (propagate nbor))))
                         (fset:less (fset:intersection (fset:domain wave)
                                                       (funcall nbor-fn pos))
                                    pos))))

    (let* ((min-entropy-locs (-<> (fset:range wave)
                               (fset:image #'fset:size <>)
                               (fset:reduce #'min <>)
                               (fset:filter (lambda (loc) (= (fset:size (fset:@ wave loc))
                                                             <>))
                                            (fset:domain wave))))
           (pin-loc (random-from-set min-entropy-locs))
           (pin-loc-slice (random-from-set (fset:@ wave pin-loc))))
      (setf (fset:@ wave pin-loc) (fset:set pin-loc-slice))
      (propagate pin-loc)
      (values pin-loc pin-loc-slice
              (fset:less wave pin-loc)))))

(defun offslice-decider-raw (slice-idx-fn origin loc-eql?)
  "Returns a memoizing function which takes an offset and two slices and returns whether such
a placement is valid"
  (let ((cache (fset:empty-map)))
    (lambda (off s0 s1)
      (let ((key (list off s0 s1)))
        (if (fset:contains? cache key)
            (fset:@ cache key)
            (setf (fset:@ cache key)
                  (-<> (fset:set origin off)
                    (fset:image slice-idx-fn <>)
                    (fset:reduce #'intersection <>)
                    ;; TODO I'd really like to be able to short-circuit the reduce function...
                    ;; Right now I it'll keep chugging even after the first mismatch is detected
                    (fset:reduce (lambda (res pos)
                                   (and res (funcall loc-eql?
                                                     (fset:@ s0 pos)
                                                     (fset:@ s1 (loc-subtract pos off)))))
                                 <> :initial-value t))))))))

(defun offslice-decider-coords (slices slice-idx-fn origin loc-eql?)
  (let ((slices (fset:convert 'fset:seq slices))
        (raw-decider (offslice-decider-raw slice-idx-fn origin loc-eql?)))
    (lambda (off s0coord s1coord)
      (funcall raw-decider off (fset:@ slices s0coord) (fset:@ slices s1coord)))))

(defun lift-set (set fn)
  "'Lifts' a set to a map given a function"
  (fset:reduce (lambda (map el) (fset:with map el (funcall fn el)))
               set :initial-value (fset:empty-map)))

(defun wfc-parametrized (domain slices nbor-fn offslice-decider)
  "Return a map which assigns every element in domain a representative slice"
  (loop with pins = (fset:empty-map)
        with wave = (lift-set domain (lambda (loc) (declare (ignore loc)) slices))
        until (fset:equal? domain (fset:domain pins))
        do (multiple-value-bind
                 (pin-loc pin-loc-slice new-wave) (step-wfc wave nbor-fn offslice-decider)
             (fset:adjoinf pins pin-loc pin-loc-slice)
             (setf wave new-wave))))
