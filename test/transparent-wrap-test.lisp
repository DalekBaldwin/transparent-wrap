(in-package :transparent-wrap-test)

(in-root-suite)

(defsuite* test-all)

(deftest test-no-params ()
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:no-params))
    (multiple-value-list (transparent-wrap-test.wrapping:no-params)))))

(deftest test-no-params-aux ()
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:no-params-aux))
    (multiple-value-list (transparent-wrap-test.wrapping:no-params-aux)))))

(deftest test-required ()
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:required 1 2))
    (multiple-value-list (transparent-wrap-test.wrapping:required 1 2)))))

(deftest test-required-aux ()
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:required-aux 1 2))
    (multiple-value-list (transparent-wrap-test.wrapping:required-aux 1 2)))))

(deftest test-required-optional ()
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:required-optional 1 2))
    (multiple-value-list (transparent-wrap-test.wrapping:required-optional 1 2))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:required-optional 1 2 3))
    (multiple-value-list (transparent-wrap-test.wrapping:required-optional 1 2 3))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:required-optional 1 2 3 4))
    (multiple-value-list (transparent-wrap-test.wrapping:required-optional 1 2 3 4))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:required-optional 1 2 3 4 5))
    (multiple-value-list (transparent-wrap-test.wrapping:required-optional 1 2 3 4 5)))))

(deftest test-required-optional-aux ()
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:required-optional-aux 1 2))
    (multiple-value-list (transparent-wrap-test.wrapping:required-optional-aux 1 2))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:required-optional-aux 1 2 3))
    (multiple-value-list (transparent-wrap-test.wrapping:required-optional-aux 1 2 3))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:required-optional-aux 1 2 3 4))
    (multiple-value-list (transparent-wrap-test.wrapping:required-optional-aux 1 2 3 4))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:required-optional-aux 1 2 3 4 5))
    (multiple-value-list (transparent-wrap-test.wrapping:required-optional-aux 1 2 3 4 5)))))

#.
`(deftest test-required-optional-rest ()
   ,@(let ((arguments (list 1 2 3 4 5 6 7)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:required-optional-rest
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:required-optional-rest
                                        ,@arglist)))))))

#.
`(deftest test-required-optional-rest-aux ()
   ,@(let ((arguments (list 1 2 3 4 5 6 7)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:required-optional-rest-aux
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:required-optional-rest-aux
                                        ,@arglist)))))))

#.
`(deftest test-required-optional-rest-key ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:required-optional-rest-key
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:required-optional-rest-key
                                        ,@arglist))))))
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
             (multiple-value-list (transparent-wrap-test.original:required-optional-rest-key
                                   1 2 3 4 5 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:required-optional-rest-key
                                   1 2 3 4 5 ,@combination))))))

#.
`(deftest test-required-optional-rest-key-aux ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:required-optional-rest-key-aux
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:required-optional-rest-key-aux
                                        ,@arglist))))))
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
             (multiple-value-list (transparent-wrap-test.original:required-optional-rest-key-aux
                                   1 2 3 4 5 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:required-optional-rest-key-aux
                                   1 2 3 4 5 ,@combination))))))

#.
`(deftest test-required-optional-key ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:required-optional-key
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:required-optional-key
                                        ,@arglist))))))
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
             (multiple-value-list (transparent-wrap-test.original:required-optional-key
                                   1 2 3 4 5 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:required-optional-key
                                   1 2 3 4 5 ,@combination))))))

#.
`(deftest test-required-optional-key-aux ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:required-optional-key-aux
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:required-optional-key-aux
                                        ,@arglist))))))
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
             (multiple-value-list (transparent-wrap-test.original:required-optional-key-aux
                                   1 2 3 4 5 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:required-optional-key-aux
                                   1 2 3 4 5 ,@combination))))))

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
             (multiple-value-list (transparent-wrap-test.original:required-key
                                   1 2 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:required-key
                                   1 2 ,@combination))))))

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
             (multiple-value-list (transparent-wrap-test.original:required-key-aux
                                   1 2 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:required-key-aux
                                   1 2 ,@combination))))))

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
             (multiple-value-list (transparent-wrap-test.original:required-rest-key
                                   1 2 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:required-rest-key
                                   1 2 ,@combination))))))

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
             (multiple-value-list (transparent-wrap-test.original:required-rest-key-aux
                                   1 2 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:required-rest-key-aux
                                   1 2 ,@combination))))))

(deftest test-optional ()
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:optional))
    (multiple-value-list (transparent-wrap-test.wrapping:optional))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:optional 1))
    (multiple-value-list (transparent-wrap-test.wrapping:optional 1))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:optional 1 2))
    (multiple-value-list (transparent-wrap-test.wrapping:optional 1 2))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:optional 1 2 3))
    (multiple-value-list (transparent-wrap-test.wrapping:optional 1 2 3)))))

(deftest test-optional-aux ()
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:optional-aux))
    (multiple-value-list (transparent-wrap-test.wrapping:optional-aux))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:optional-aux 1))
    (multiple-value-list (transparent-wrap-test.wrapping:optional-aux 1))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:optional-aux 1 2))
    (multiple-value-list (transparent-wrap-test.wrapping:optional-aux 1 2))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:optional-aux 1 2 3))
    (multiple-value-list (transparent-wrap-test.wrapping:optional-aux 1 2 3)))))

#.
`(deftest test-optional-rest ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:optional-rest
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:optional-rest
                                        ,@arglist)))))))

#.
`(deftest test-optional-rest-aux ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:optional-rest-aux
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:optional-rest-aux
                                        ,@arglist)))))))

#.
`(deftest test-optional-rest-key ()
   ,@(let ((arguments (list 1 2 3)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:optional-rest-key
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:optional-rest-key
                                        ,@arglist))))))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:d 4) (:e 5) (:f 6) (:z 7) (:y 8) (:x 9))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (multiple-value-list (transparent-wrap-test.original:optional-rest-key
                                   1 2 3 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:optional-rest-key
                                   1 2 3 ,@combination))))))

#.
`(deftest test-optional-rest-key-aux ()
   ,@(let ((arguments (list 1 2 3)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:optional-rest-key-aux
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:optional-rest-key-aux
                                        ,@arglist))))))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:d 4) (:e 5) (:f 6) (:z 7) (:y 8) (:x 9))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (multiple-value-list (transparent-wrap-test.original:optional-rest-key-aux
                                   1 2 3 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:optional-rest-key-aux
                                   1 2 3 ,@combination))))))

#.
`(deftest test-optional-key ()
   ,@(let ((arguments (list 1 2 3)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:optional-key
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:optional-key
                                        ,@arglist))))))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:d 4) (:e 5) (:f 6) (:z 7) (:y 8) (:x 9))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (multiple-value-list (transparent-wrap-test.original:optional-key
                                   1 2 3 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:optional-key
                                   1 2 3 ,@combination))))))

#.
`(deftest test-optional-key-aux ()
   ,@(let ((arguments (list 1 2 3)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect
               `(is
                 (equal
                  (multiple-value-list (transparent-wrap-test.original:optional-key-aux
                                        ,@arglist))
                  (multiple-value-list (transparent-wrap-test.wrapping:optional-key-aux
                                        ,@arglist))))))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:d 4) (:e 5) (:f 6) (:z 7) (:y 8) (:x 9))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (multiple-value-list (transparent-wrap-test.original:optional-key-aux
                                   1 2 3 ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:optional-key-aux
                                   1 2 3 ,@combination))))))

#.
`(deftest test-key ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:a 1) (:b 2) (:c 3) (:z 4) (:y 5) (:x 6))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (multiple-value-list (transparent-wrap-test.original:key
                                   ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:key
                                   ,@combination))))))

#.
`(deftest test-key-aux ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:a 1) (:b 2) (:c 3) (:z 4) (:y 5) (:x 6))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (multiple-value-list (transparent-wrap-test.original:key-aux
                                   ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:key-aux
                                   ,@combination))))))

(deftest test-rest_ ()
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:rest_))
    (multiple-value-list (transparent-wrap-test.wrapping:rest_))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:rest_ 1))
    (multiple-value-list (transparent-wrap-test.wrapping:rest_ 1))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:rest_ 1 2))
    (multiple-value-list (transparent-wrap-test.wrapping:rest_ 1 2)))))

(deftest test-rest-aux ()
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:rest-aux))
    (multiple-value-list (transparent-wrap-test.wrapping:rest-aux))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:rest-aux 1))
    (multiple-value-list (transparent-wrap-test.wrapping:rest-aux 1))))
  (is
   (equal
    (multiple-value-list (transparent-wrap-test.original:rest-aux 1 2))
    (multiple-value-list (transparent-wrap-test.wrapping:rest-aux 1 2)))))

#.
`(deftest test-rest-key ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:a 1) (:b 2) (:c 3) (:z 4) (:y 5) (:x 6))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (multiple-value-list (transparent-wrap-test.original:rest-key
                                   ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:rest-key
                                   ,@combination))))))

#.
`(deftest test-rest-key-aux ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:a 1) (:b 2) (:c 3) (:z 4) (:y 5) (:x 6))))
            (loop for i from 0 to (length arguments)
                 do
                 (map-combinations
                  (lambda (c) (push c combinations))
                  arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect
          `(is
            (equal
             (multiple-value-list (transparent-wrap-test.original:rest-key-aux
                                   ,@combination))
             (multiple-value-list (transparent-wrap-test.wrapping:rest-key-aux
                                   ,@combination))))))
