[![Build Status](https://travis-ci.org/DalekBaldwin/transparent-wrap.svg?branch=master)](https://travis-ci.org/DalekBaldwin/transparent-wrap)

This is a small utility for writing wrapper functions with the same signature as the functions they wrap so that you can create wrapper packages and still interactively see signature info in SLIME. For some argument lists this imposes a considerable overhead (since we have to manually ensure that we only pass exactly the same subset of optional or keyword arguments that appeared in the outer call) but for production purposes you can swap out `create-transparent-defun` for `create-basic-defun`.
