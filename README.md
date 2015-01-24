[![Build Status](https://travis-ci.org/DalekBaldwin/transparent-wrap.svg?branch=master)](https://travis-ci.org/DalekBaldwin/transparent-wrap)

This is a small utility for writing wrapper functions with the same signature as the functions they wrap so that you can still interactively see the same function signature in the SLIME minibuffer.

So if lots of functionality in a package is breaking because of some trivial incompatibility with your Lisp environment, you can do something like this:

```lisp
(defpackage :wrapping-package
  (:use :cl)
  #.`(:export
      ,@(loop for symbol being the external-symbols of :some-package
             when (and (fboundp symbol)
                       (eql (symbol-package symbol) (find-package :some-package)))
             collect (intern (symbol-name symbol) :keyword))))

#.`(progn
     ,@(loop for symbol being the external-symbols of :some-package
          when (and (fboundp symbol)
                    (eql (symbol-package symbol) (find-package :some-package)))
          collect
            (transparent-wrap:create-transparent-defun
             symbol
             (lambda (real-function-call)
               `(let ((*broken-parameter* bug-fixing-value)) ,real-function-call))
             :wrapping-package)))
```

and fix it without having to massively edit the package's source code or your client code. Just import the wrapper package instead.

There is also a convenience macro so you don't have to quote or sharp-quote anything, as long as you define the wrapping code as a macro: `(transparent-defun old-function wrapper-macro :wrapper-package)`

For some argument lists, this imposes a considerable overhead, since we have to manually ensure that we only pass exactly the same subset of optional or keyword arguments that appeared in the outer call in case the wrapped function explicitly checks whether any of those arguments were supplied. Transparent-wrap offers two ways to mitigate this overhead:

1. Set the keyword argument `:force-rest` to `t` in `create-transparent-defun`. This adds a `&rest` parameter when wrapping functions that have `&key` arguments but no `&rest` argument. This way, the keyword arguments can be passed through with `apply` without checking which ones are present.

2. When turning a development build into a production build, you can swap out `create-transparent-defun` for `create-opaque-defun` to include the same wrapping logic but strip out all the infrastructure for imitating the function signature.
