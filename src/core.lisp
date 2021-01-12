
(uiop:define-package :wfc/src/core
  (:use :cl :arrow-macros
        :wfc/src/utils)
  (:documentation
   "Functions forming the core of the Wave Function Collapse algorithm.
The pipeline is parametrized, so it can accomadate different
strategies for checking offsets & representing slices")
  (:export #:step-wfc
           #:wfc-parametrized))

(in-package :wfc/src/core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions that don't exist or that I can't find in FSet

(defun lift-set (set fn)
  "'Lifts' a set to a map given a function"
  (fset:reduce (lambda (map el) (fset:with map el (funcall fn el)))
               set :initial-value (fset:empty-map)))

(defun random-from-set (set)
  ;; I can't hardcode the error because unreachabel code :/
  ;; (error "Function not implemented")
  (print "This really isn't implemented yet!")
  set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functionality

(defun filter-possibs (offslice-decider offset rootslices nborslices)
  "Filter nborslices to the subset where each slice fits with the given offset for *some* choice
from rootslices"
  (fset:reduce #'fset:union
               (fset:image (lambda (rootslice)
                             (fset:filter (lambda (nborslice)
                                            (funcall offslice-decider
                                                     offset rootslice nborslice))
                                          nborslices))
                           rootslices)))

(defun step-wfc (wave nbor-fn offslice-decider)
  "Takes a single step of the WFC algorithm.

Returns the state which was just pinned, the slice it was pinned to, and the updated wave.

wave is a map from UNpinned locs to the respective set of remaining possible slices.
It must be 'consistent' in that any single choice of slice for any single state should not
conflict with any pinned states.

When this function makes a pin which leads to an inconsistent wave, it will signal an error"
  (labels ((propagate (pos)
             "This function's purpose is to ensure consistency (as defined by step-wfc) of the wave
function after changes in the set of allowable slices for pos."
             (fset:image (lambda (nbor)
                           (let* ((old-possibs (fset:@ wave nbor))
                                  (new-possibs (filter-possibs offslice-decider
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

(defun wfc-parametrized (domain slices nbor-fn offslice-decider)
  "Return a map which assigns every element in domain a representative slice"
  (loop with pins = (fset:empty-map)
        with wave = (lift-set domain (lambda (loc) (declare (ignore loc)) slices))
        until (fset:equal? domain (fset:domain pins))
        do (multiple-value-bind
                 (pin-loc pin-loc-slice new-wave) (step-wfc wave nbor-fn offslice-decider)
             (fset:adjoinf pins pin-loc pin-loc-slice)
             (setf wave new-wave))))
