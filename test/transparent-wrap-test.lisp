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
