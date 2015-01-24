(in-package :transparent-wrap-test)

(in-root-suite)

(defsuite* test-all)

(deftest test-no-params ()
  (is
   (equal
    (transparent-wrap-test.original:no-params)
    (transparent-wrap-test.wrapping:no-params))))

(deftest test-required ()
  (is
   (equal
    (transparent-wrap-test.original:required 1 2)
    (transparent-wrap-test.wrapping:required 1 2))))

(deftest test-required-optional ()
  (is
   (equal
    (transparent-wrap-test.original:required-optional 1 2)
    (transparent-wrap-test.wrapping:required-optional 1 2)))
  (is
   (equal
    (transparent-wrap-test.original:required-optional 1 2 3)
    (transparent-wrap-test.wrapping:required-optional 1 2 3)))
  (is
   (equal
    (transparent-wrap-test.original:required-optional 1 2 3 4)
    (transparent-wrap-test.wrapping:required-optional 1 2 3 4)))
  (is
   (equal
    (transparent-wrap-test.original:required-optional 1 2 3 4 5)
    (transparent-wrap-test.wrapping:required-optional 1 2 3 4 5))))

(deftest test-required-optional-rest ()
  (is
   (equal
    (transparent-wrap-test.original:required-optional-rest 1 2)
    (transparent-wrap-test.wrapping:required-optional-rest 1 2)))
  (is
   (equal
    (transparent-wrap-test.original:required-optional-rest 1 2 3)
    (transparent-wrap-test.wrapping:required-optional-rest 1 2 3)))
  (is
   (equal
    (transparent-wrap-test.original:required-optional-rest 1 2 3 4)
    (transparent-wrap-test.wrapping:required-optional-rest 1 2 3 4)))
  (is
   (equal
    (transparent-wrap-test.original:required-optional-rest 1 2 3 4 5)
    (transparent-wrap-test.wrapping:required-optional-rest 1 2 3 4 5)))
  (is
   (equal
    (transparent-wrap-test.original:required-optional-rest 1 2 3 4 5 6)
    (transparent-wrap-test.wrapping:required-optional-rest 1 2 3 4 5 6))))

#.
`(deftest test-required-key ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:c 3) (:d 4) (:e 5) (:z 6) (:y 7) (:x 8))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (transparent-wrap-test.original:required-key 1 2 ,@combination)
             (transparent-wrap-test.wrapping:required-key 1 2 ,@combination)))))
