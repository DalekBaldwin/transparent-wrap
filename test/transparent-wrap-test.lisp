(in-package :transparent-wrap-test)

(in-root-suite)

(defsuite* test-all)

(defmacro do-test (function-name &rest args)
  `(is
    (equal
     (let ((transparent-wrap-test.original::*state* nil))
       (multiple-value-list (,(find-symbol
                               (symbol-name function-name)
                               :transparent-wrap-test.original)
                              ,@args)))
     (let ((transparent-wrap-test.original::*state* nil))
       (multiple-value-list (,(find-symbol
                               (symbol-name function-name)
                               :transparent-wrap-test.wrapping)
                              ,@args))))))

(deftest test-no-params ()
  (do-test :no-params))

(deftest test-no-params-aux ()
  (do-test :no-params-aux))

(deftest test-required ()
  (do-test :required 1 2))

(deftest test-required-aux ()
  (do-test :required-aux 1 2))

(deftest test-required-optional ()
  (do-test :required-optional 1 2)
  (do-test :required-optional 1 2 3)
  (do-test :required-optional 1 2 3 4)
  (do-test :required-optional 1 2 3 4 5))

(deftest test-required-optional-aux ()
  (do-test :required-optional-aux 1 2)
  (do-test :required-optional-aux 1 2 3)
  (do-test :required-optional-aux 1 2 3 4)
  (do-test :required-optional-aux 1 2 3 4 5))

#.
`(deftest test-required-optional-rest ()
   ,@(let ((arguments (list 1 2 3 4 5 6 7)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :required-optional-rest ,@arglist))))

#.
`(deftest test-required-optional-rest-aux ()
   ,@(let ((arguments (list 1 2 3 4 5 6 7)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :required-optional-rest-aux ,@arglist))))

#.
`(deftest test-required-optional-rest-key ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :required-optional-rest-key ,@arglist)))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:f 6) (:g 7) (:h 8) (:z 9) (:y 10) (:x 11))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :required-optional-rest-key 1 2 3 4 5 ,@combination)))

#.
`(deftest test-required-optional-rest-key-aux ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :required-optional-rest-key-aux ,@arglist)))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:f 6) (:g 7) (:h 8) (:z 9) (:y 10) (:x 11))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :required-optional-rest-key-aux 1 2 3 4 5 ,@combination)))

#.
`(deftest test-required-optional-key ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :required-optional-key ,@arglist)))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:f 6) (:g 7) (:h 8) (:z 9) (:y 10) (:x 11))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :required-optional-key 1 2 3 4 5 ,@combination)))

#.
`(deftest test-required-optional-key-aux ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 2 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :required-optional-key-aux ,@arglist)))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:f 6) (:g 7) (:h 8) (:z 9) (:y 10) (:x 11))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :required-optional-key-aux 1 2 3 4 5 ,@combination)))

#.
`(deftest test-required-key ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:c 3) (:d 4) (:e 5) (:z 6) (:y 7) (:x 8))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :required-key 1 2 ,@combination)))

#.
`(deftest test-required-key-aux ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:c 3) (:d 4) (:e 5) (:z 6) (:y 7) (:x 8))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :required-key-aux 1 2 ,@combination)))

#.
`(deftest test-required-rest-key ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:c 3) (:d 4) (:e 5) (:z 6) (:y 7) (:x 8))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :required-rest-key 1 2 ,@combination)))

#.
`(deftest test-required-rest-key-aux ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:c 3) (:d 4) (:e 5) (:z 6) (:y 7) (:x 8))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :required-rest-key-aux 1 2 ,@combination)))

(deftest test-optional ()
  (do-test :optional)
  (do-test :optional 1)
  (do-test :optional 1 2)
  (do-test :optional 1 2 3))

(deftest test-optional-aux ()
  (do-test :optional-aux)
  (do-test :optional-aux 1)
  (do-test :optional-aux 1 2)
  (do-test :optional-aux 1 2 3))

#.
`(deftest test-optional-rest ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :optional-rest ,@arglist))))

#.
`(deftest test-optional-rest-aux ()
   ,@(let ((arguments (list 1 2 3 4 5)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :optional-rest-aux ,@arglist))))

#.
`(deftest test-optional-rest-key ()
   ,@(let ((arguments (list 1 2 3)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :optional-rest-key ,@arglist)))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:d 4) (:e 5) (:f 6) (:z 7) (:y 8) (:x 9))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :optional-rest-key 1 2 3 ,@combination)))

#.
`(deftest test-optional-rest-key-aux ()
   ,@(let ((arguments (list 1 2 3)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :optional-rest-key-aux ,@arglist)))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:d 4) (:e 5) (:f 6) (:z 7) (:y 8) (:x 9))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :optional-rest-key-aux 1 2 3 ,@combination)))

#.
`(deftest test-optional-key ()
   ,@(let ((arguments (list 1 2 3)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :optional-key ,@arglist)))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:d 4) (:e 5) (:f 6) (:z 7) (:y 8) (:x 9))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :optional-key 1 2 3 ,@combination)))

#.
`(deftest test-optional-key-aux ()
   ,@(let ((arguments (list 1 2 3)))
          (loop for arglist in
                 (loop for i from 0 to (length arguments)
                    collect (subseq arguments 0 i))
             collect `(do-test :optional-key-aux ,@arglist)))
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:d 4) (:e 5) (:f 6) (:z 7) (:y 8) (:x 9))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :optional-key-aux 1 2 3 ,@combination)))

#.
`(deftest test-key ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:a 1) (:b 2) (:c 3) (:z 4) (:y 5) (:x 6))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :key ,@combination)))

#.
`(deftest test-key-aux ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:a 1) (:b 2) (:c 3) (:z 4) (:y 5) (:x 6))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :key-aux ,@combination)))

(deftest test-rest_ ()
  (do-test :rest_)
  (do-test :rest_ 1)
  (do-test :rest_ 1 2))

(deftest test-rest-aux ()
  (do-test :rest-aux)
  (do-test :rest-aux 1)
  (do-test :rest-aux 1 2))

#.
`(deftest test-rest-key ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:a 1) (:b 2) (:c 3) (:z 4) (:y 5) (:x 6))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :rest-key ,@combination)))

#.
`(deftest test-rest-key-aux ()
   ,@(loop for combination in
          (let ((combinations)
                (arguments '((:a 1) (:b 2) (:c 3) (:z 4) (:y 5) (:x 6))))
            (loop for i from 0 to (length arguments)
                 do (map-combinations
                     (lambda (c) (push c combinations))
                     arguments :length i))
            (reverse (mapcar #'flatten combinations)))
          collect `(do-test :rest-key-aux ,@combination)))

(deftest test-alternating-optional-supplied ()
  (do-test :alternating-optional-supplied)
  (do-test :alternating-optional-supplied 1)
  (do-test :alternating-optional-supplied 1 2)
  (do-test :alternating-optional-supplied 1 2 3))

#.
`(deftest test-optional-combinations ()
  ,@(loop for sym being the symbols of :transparent-wrap-test.wrapping
         when (starts-with-subseq "OPTIONAL-MATRIX" (symbol-name sym))
         appending
         `((do-test ,sym)
           (do-test ,sym 1)
           (do-test ,sym 1 2)
           (do-test ,sym 1 2 3))))
