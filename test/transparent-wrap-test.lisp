(in-package :transparent-wrap-test)

(in-root-suite)

(defsuite* test-all)

(deftest test-no-params ()
  (is
   (equal
    (transparent-wrap-test.original:no-params)
    (transparent-wrap-test.wrapping:no-params))))

(deftest test-no-params-aux ()
  (is
   (equal
    (transparent-wrap-test.original:no-params-aux)
    (transparent-wrap-test.wrapping:no-params-aux))))

(deftest test-required ()
  (is
   (equal
    (transparent-wrap-test.original:required 1 2)
    (transparent-wrap-test.wrapping:required 1 2))))

(deftest test-required-aux ()
  (is
   (equal
    (transparent-wrap-test.original:required-aux 1 2)
    (transparent-wrap-test.wrapping:required-aux 1 2))))

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

(deftest test-required-optional-aux ()
  (is
   (equal
    (transparent-wrap-test.original:required-optional-aux 1 2)
    (transparent-wrap-test.wrapping:required-optional-aux 1 2)))
  (is
   (equal
    (transparent-wrap-test.original:required-optional-aux 1 2 3)
    (transparent-wrap-test.wrapping:required-optional-aux 1 2 3)))
  (is
   (equal
    (transparent-wrap-test.original:required-optional-aux 1 2 3 4)
    (transparent-wrap-test.wrapping:required-optional-aux 1 2 3 4)))
  (is
   (equal
    (transparent-wrap-test.original:required-optional-aux 1 2 3 4 5)
    (transparent-wrap-test.wrapping:required-optional-aux 1 2 3 4 5))))

#.
`(deftest test-required-optional-rest ()
   ,@(let ((arguments (list 1 2 3 4 5 6 7)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (transparent-wrap-test.original:required-optional-rest
                   ,@arglist)
                  (transparent-wrap-test.wrapping:required-optional-rest
                   ,@arglist))))))

#.
`(deftest test-required-optional-rest-aux ()
   ,@(let ((arguments (list 1 2 3 4 5 6 7)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (transparent-wrap-test.original:required-optional-rest-aux
                   ,@arglist)
                  (transparent-wrap-test.wrapping:required-optional-rest-aux
                   ,@arglist))))))

#.
`(deftest test-required-optional-rest-key ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (transparent-wrap-test.original:required-optional-rest-key
                   ,@arglist)
                  (transparent-wrap-test.wrapping:required-optional-rest-key
                   ,@arglist)))))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:f 6) (:g 7) (:h 8) (:z 9) (:y 10) (:x 11))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (transparent-wrap-test.original:required-optional-rest-key
              1 2 3 4 5 ,@combination)
             (transparent-wrap-test.wrapping:required-optional-rest-key
              1 2 3 4 5 ,@combination)))))

#.
`(deftest test-required-optional-rest-key-aux ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (transparent-wrap-test.original:required-optional-rest-key-aux
                   ,@arglist)
                  (transparent-wrap-test.wrapping:required-optional-rest-key-aux
                   ,@arglist)))))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:f 6) (:g 7) (:h 8) (:z 9) (:y 10) (:x 11))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (transparent-wrap-test.original:required-optional-rest-key-aux
              1 2 3 4 5 ,@combination)
             (transparent-wrap-test.wrapping:required-optional-rest-key-aux
              1 2 3 4 5 ,@combination)))))

#.
`(deftest test-required-optional-key ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (transparent-wrap-test.original:required-optional-key
                   ,@arglist)
                  (transparent-wrap-test.wrapping:required-optional-key
                   ,@arglist)))))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:f 6) (:g 7) (:h 8) (:z 9) (:y 10) (:x 11))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (transparent-wrap-test.original:required-optional-key
              1 2 3 4 5 ,@combination)
             (transparent-wrap-test.wrapping:required-optional-key
              1 2 3 4 5 ,@combination)))))

#.
`(deftest test-required-optional-key-aux ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (transparent-wrap-test.original:required-optional-key-aux
                   ,@arglist)
                  (transparent-wrap-test.wrapping:required-optional-key-aux
                   ,@arglist)))))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:f 6) (:g 7) (:h 8) (:z 9) (:y 10) (:x 11))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (transparent-wrap-test.original:required-optional-key-aux
              1 2 3 4 5 ,@combination)
             (transparent-wrap-test.wrapping:required-optional-key-aux
              1 2 3 4 5 ,@combination)))))

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
             (transparent-wrap-test.original:required-key
              1 2 ,@combination)
             (transparent-wrap-test.wrapping:required-key
              1 2 ,@combination)))))

#.
`(deftest test-required-key-aux ()
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
             (transparent-wrap-test.original:required-key-aux
              1 2 ,@combination)
             (transparent-wrap-test.wrapping:required-key-aux
              1 2 ,@combination)))))

#.
`(deftest test-required-rest-key ()
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
             (transparent-wrap-test.original:required-rest-key
              1 2 ,@combination)
             (transparent-wrap-test.wrapping:required-rest-key
              1 2 ,@combination)))))

#.
`(deftest test-required-rest-key-aux ()
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
             (transparent-wrap-test.original:required-rest-key-aux
              1 2 ,@combination)
             (transparent-wrap-test.wrapping:required-rest-key-aux
              1 2 ,@combination)))))
