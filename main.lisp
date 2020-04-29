(in-package :wfc-cl)

(defun im-width (im) (second (array-dimensions im)))
(defun im-height (im) (first (array-dimensions im)))

(defun enumerate-range (start end)
  ;; Returns a list [start, end>
  (if (>= start end)
      nil
      (cons start (enumerate-range (1+ start) end))))

(defun combinations (seq1 seq2)
  (reduce #'append (mapcar (lambda (e2) (mapcar (lambda (e1) (list e1 e2)) seq1)) seq2)))

(defun 2d-window (arr row col width height)
  (let* ((slice (numcl:zeros (list height width) :type 'integer)))
    (mapcar (lambda (offsets)
              (destructuring-bind (srow scol) offsets
                (setf (numcl:aref slice srow scol) (numcl:aref arr (+ row srow) (+ col scol)))))
            (combinations (enumerate-range 0 height) (enumerate-range 0 width)))
    slice))

(defun make-slices (image filter-width filter-height)
  ;; Just collect the set of (filter-width, filter-height slices present in image)
  (let ((slices (make-hash-table :test #'numcl:equalp)))
    (mapcar (lambda (offs)
              (setf (gethash (2d-window image (first offs) (second offs)
                                        filter-width filter-height)
                             slices)
                    t))
            (combinations (enumerate-range 0 (1+ (- (im-height image) filter-height)))
                          (enumerate-range 0 (1+ (- (im-width image) filter-width)))))
    (alexandria:hash-table-keys slices)))

(defun wave-function-collapse (image filter-width filter-height output-width output-height)
  (let ((slices (make-slices image filter-width filter-height)))))
