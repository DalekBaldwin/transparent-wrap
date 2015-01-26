Transparent-wrap
================

[![Build Status](https://travis-ci.org/DalekBaldwin/transparent-wrap.svg?branch=master)](https://travis-ci.org/DalekBaldwin/transparent-wrap)

This is a small utility for generating wrapper functions that have the same signature as the functions they wrap so that you can still interactively see the same function signature in the SLIME minibuffer. (It also offers the same feature for macros, which is not quite as difficult but is included for completeness' sake.)

Example
-------

When using `lispbuilder-sdl` on SBCL, you may encounter all kinds of floating point errors, but you don't want to disable those errors globally - you want to do your own math before calling out to foreign functions. In that case you can do something like this:

```lisp
(defpackage :sdl-wrap
  (:use :cl)
  #.`(:export
      ,@(loop for symbol being the external-symbols of :sdl
           when (and (fboundp symbol)
                     (eql (symbol-package symbol) (find-package :sdl)))
           collect symbol)))

#.`(progn
     ,@(loop for symbol being the external-symbols of :sdl
          when (and (fboundp symbol)
                    (eql (symbol-package symbol) (find-package :sdl)))
          collect
            (transparent-wrap:create-transparent-defun
             symbol
             (lambda (real-function-call)
               `(sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero)
                  ,real-function-call))
             :sdl-wrap)))
```

and fix it without having to massively edit the package's source code or your client code. Just import the wrapper package instead. Now you can get the functionality you need and see that `sdl-wrap:draw-string-solid-*` has the signature `(string x y &key (justify :left) (surface lispbuilder-sdl:*default-surface*) (font lispbuilder-sdl:*default-font*) (color lispbuilder-sdl:*default-color*))` without manually searching for it!

Performance
-----------

For some argument lists, the wrapping layer imposes a considerable overhead, since we have to manually ensure that we only pass exactly the same optional and keyword arguments that appeared in the outer call in case the wrapped function explicitly checks whether any of those arguments were supplied. Transparent-wrap offers two ways to mitigate this overhead:

1. Set the keyword argument `:force-rest` to `t` in `create-transparent-defun`. This adds a `&rest` parameter when wrapping a function that has `&key` arguments but no `&rest` argument. This way, the keyword arguments can be passed through with `apply` without checking which ones are present, with minimal clutter added to the function signature.

2. When turning a development build into a production build, you can swap out `create-transparent-defun` for `create-opaque-defun` to include the same wrapping logic but strip out all the infrastructure for imitating the function signature.

Usage
-----

```lisp
;; function
(create-transparent-defun 'package:function
                          (lambda (code)
                            `(wrap ,code))
                          :wrapping-package)
```

```lisp
(defmacro wrapper (code)
  `(wrap ,code))

;; macro
(transparent-defun package:function wrapper :wrapping-package)
```

```lisp
;; function
(create-transparent-defmacro package:macro
                             (lambda (code)
                               ``(wrap ,,code))
                             :wrapping-package)
```

```lisp
(defmacro wrapper (code)
  `(wrap ,code))

;; macro
(transparent-defmacro package:macro wrapper :wrapping-package)
```

Limitations
-----------

This repo uses `trivial-arguments` to retrieve function and macro signatures. On some implementations, this will not retrieve default arguments for some parameters. When no signature can be found, `transparent-defun` falls back to basic `opaque-defun` functionality and creates a wrapper with a `&rest` parameter only.
