# cl-zipper

cl-zipper is a Common Lisp implementation of the Zipper data structure first
described by [Gerárd Huet](http://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf).

## Supported Implementations

The code was tested and runs successfuly on each of the following
Common Lisp platforms:

* [SBCL](http://www.sbcl.org/)
* [CLISP](http://www.gnu.org/software/clisp/)

## Runtime Dependencies

First, make sure that you have
[ASDF](http://common-lisp.net/project/asdf/) installed and loaded:

````common-lisp
> (asdf:asdf-version)
"2.017"
````

A simple way to get ASDF is via
[QuickLisp](http://www.quicklisp.org/beta/), which is a library
manager for Common Lisp.

## Installing cl-zipper

At this moment the package is not yet available for download through
QuickLisp.

However, it could be installed rather easily by cloning the project
inside `~/quicklisp/local-projects` directory and running
`(ql:quickload :cl-zipper)` in the REPL.

## Usage

First, start a REPL and load the system:

````common-lisp
> (asdf:load-system :cl-zipper)
T

> (use-package :cl-zipper)
T
````

Suppose we have the tree `(a + b) * (c - d)` to play with:

````common-lisp
> (defparameter *loc* (zipper '(* (+ a b) (- c d))))
*loc*
````

### Basic Navigation

Now, let's examine the first four zipper operations: `go-down`,
`go-right`, `go-left`, and `go-up`.

Every zipper operation returns what we call a _loc_, or location,
which consists in the current focus of attention within the tree.

Taking a closer look at what `(go-down loc)` does:

````common-lisp
> (documentation 'go-down 'function)
"Returns the loc of the leftmost child of the node at this loc, or
nil if no children."
````

Obtaining more information about the current location:

````common-lisp
> (defparameter *loc-down* (go-down *loc*))
*LOC-DOWN*

> (car *loc-down*)    ;; node at this loc
*

> (lefts *loc-down*)  ;; left siblings of the node at this loc
NIL

> (rights *loc-down*) ;; right siblings of the node at this loc
((+ A B) (- C D))
````

The nice thing about this kind of abstraction is that you can navigate
a tree by chaining calls:

````common-lisp
> (defparameter *loc-down-right* (go-right *loc-down*))
*LOC-DOWN-RIGHT*

> (car *loc-down-right*)
(+ A B)

> (lefts *loc-down-right*)
(*)

> (rights *loc-down-right*)
((- C D))
````

By now you probably have guessed what `(go-left loc)` and
`(go-right loc)` do:

````common-lisp
> (documentation 'go-left 'function)
"Returns the loc of the left sibling of the node at this loc,
or nil."
````

To zip up to the parent node of this loc:

````common-lisp
> (car (go-up *loc-down-right*))
(* (+ A B) (- C D))
````

### Navigation Shortcuts

TODO.

### Changes, Insertions and Deletions

TODO.

## Contributing

If you found bugs or want to add new features to cl-zipper, the first
step is to write tests that cover your changes.

As you'll see in a moment, [5am](http://www.cliki.net/FIVEAM) testing
framework is required in order to run the tests.

Now, clone this repository and open Lisp REPL at its root directory:

````common-lisp
> (ql:quickload :fiveam)
...
(:FIVEAM)

> (asdf:test-system :cl-zipper)
...
T
````

## License

Copyright (C) Destaquenet Technology Solutions

Distributed under the New BSD License. See COPYING for further details.
