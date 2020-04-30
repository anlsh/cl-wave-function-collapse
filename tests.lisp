(in-package :wfc-cl)

(clunit:defsuite logic ())

(clunit:deftest test/make-slices (logic)
  (let ((img (make-array '(4 3) :initial-contents '((1 0 1) (0 1 0) (0 0 1) (0 0 0)))))
    (clunit:assert-eql 4 (generic-cl:length (make-slices img 2 2)))
    (clunit:assert-eql 1 (generic-cl:length (make-slices img (nrows img) (ncols img))))))

(clunit:deftest test/window (logic)
  (let ((img (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (7 8 9))))
        (slice (make-array '(2 2) :initial-contents '((2 3) (5 6)))))
    (clunit:assert-equalp slice (2d-window img 0 1 2 2))
    (clunit:assert-condition sb-int:invalid-array-index-error (2d-window img 0 2 2 2))))

(clunit:deftest test/get-offsets (logic)
  (let ((img1 (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (7 8 9))))
        (img2 (make-array '(2 2) :initial-contents '((1 0) (0 1)))))
    (clunit:assert-equal '((0 0)) (allowable-offsets img1 img1))
    (clunit:assert-equal 5 (generic-cl:length (allowable-offsets img2 img2)))))
