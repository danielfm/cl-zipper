# cl-zipper

cl-zipper is a Common Lisp implementation of the Zipper data structure first
described by [Gerárd Huet](http://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf).

## Supported Implementations

The code was tested and runs successfuly on each of the following
Common Lisp platforms:

* [Clozure CL](http://ccl.clozure.com/)
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

## Getting Started

First, start a REPL and load the system:

````common-lisp
(asdf:load-system :cl-zipper)
(use-package :cl-zipper)
````

Suppose we have the tree `(a + b) * (c - d)` to play with:

````common-lisp
(defparameter *loc* (zipper '(* (+ a b) (- c d))))
````

### Navigation Primitives

Now, let's examine the four basic zipper operations: `(go-down loc)`,
`(go-right loc)`, `(go-left loc)`, and `(go-up loc)`.

Every zipper operation gets what we call a _loc_, or location, which
consists in the current focus of attention within the tree, and the
return value is a _loc_ that represents the new location after such
operation is performed.

For instance, let's take a look at what `(go-down loc)` does:

````common-lisp
> (documentation 'go-down 'function)
"Returns the loc of the leftmost child of the node at this loc, or
nil if no children."
````

Obtaining more information about the current _loc_ and its
surroundings:

````common-lisp
(defparameter *loc-down* (go-down *loc*))

(car *loc-down*)    ;; *
(lefts *loc-down*)  ;; NIL
(rights *loc-down*) ;; ((+ A B) (- C D))
````

The nice thing about this kind of abstraction is that you can navigate
a tree by chaining calls:

````common-lisp
(defparameter *loc-down-right* (go-right *loc-down*))

(car *loc-down-right*)    ;; (+ A B)
(lefts *loc-down-right*)  ;; (*)
(rights *loc-down-right*) ;; ((- C D))
````

By now you probably have guessed what the other basic navigation
primitives do:

````common-lisp
> (documentation 'go-left 'function)
"Returns the loc of the left sibling of the node at this loc,
or nil."
````

To zip up to the parent node of a nested _loc_:

````common-lisp
(car (go-up *loc-down-right*)) ;; (* (+ A B) (- C D))
````

### Navigation Shortcuts

Use `(go-next loc)` if you just want to visit the nodes of
the tree in depth-first order:

````common-lisp
(defparameter *loc-next-2* (go-next (go-next *loc*)))

(car *loc-next-2*)    ;; (+ A B)
(lefts *loc-next-2*)  ;; (*)
(rights *loc-next-2*) ;; (- C D)
````

Similarly, use `(go-prev loc)` to walk to the opposite direction:

````common-lisp
(defparameter *loc-next* (go-prev *loc-next-2*))

(car *loc-next*)    ;; *
(lefts *loc-next*)  ;; NIL
(rights *loc-next*) ;; ((+ A B) (- C D))
````

Now, suppose you have a _loc_ that points to `A`:

````common-lisp
(defparameter *loc-a* (go-right (go-down (go-right (go-down *loc*)))))

(car *loc-a*)    ;; A
(lefts *loc-a*)  ;; (+)
(rights *loc-a*) ;; (B)
`````

You can get the leftmost or rightmost _loc_ with a simple function
call:

````common-lisp
(car (leftmost *loc-a*))  ;; +
(car (rightmost *loc-a*)) ;; B
````

### Removing Nodes

Just call `(remove-node loc)` to remove the node at _loc_:

````common-lisp
(root-node (remove-node *loc-a*)) ;; (* (+ B) (- C D))
````

### Inserting Nodes

The first functions we'll see are `(insert-left loc node)` and
`(insert-right loc node)`:

````common-lisp
(root-node (insert-left *loc-a* 'x))  ;; (* (+ X A B) (- C D))
(root-node (insert-right *loc-a* 'x)) ;; (* (+ A X B) (- C D))
````

If the node at _loc_ is the root of a subtree, it's possible to
insert child nodes with `(append-down loc node)` and
`(insert-down loc node)`.

The `(append-down loc node)` function inserts a node as the rightmost
child of the node at _loc_:

````common-lisp
(defparameter *loc-subtree* (go-right (go-down *loc*)))
(root-node (append-down *loc-subtree* '(/ x y))) ;; (* (+ A B (/ X Y)) (- C D))
````

Use `(insert-down loc node)` to insert a node as the leftmost child:

````common-lisp
(root-node (insert-down *loc-subtree* '(/ x y))) ;; (* ((/ X Y) + A B) (- C D))
````

### Changing Nodes

Use `(change-node loc node)` in order to replace the node at _loc_:

````common-lisp
(root-node (change-node *loc-a* 'x)) ;; (* (+ X B) (- C D))
````

If the change is modeled by a function, the function
`(edit-node loc func &rest args)` replaces the node at _loc_ with the
result of applying `(func (car loc) arg1 arg2 ... argN)`:

````common-lisp
(defun crazy-fn (node n1 n2)
  (if (equal node 'A)
    n1
    n2))

(root-node (edit-node *loc-a* #'crazy-fn 1 2)) ;; (* (+ 1 B) (- C D))
````

### Zippers Are Functional

With zippers you can write code that looks like an imperative,
destructive walk through a tree, call `(root-node loc)` when you are
done and get a new tree reflecting all the changes, when in fact nothing
at all is mutated - it's all thread safe and shareable.

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

## Donate

If this project is useful for you, buy me a beer!

Bitcoin: `bc1qtwyfcj7pssk0krn5wyfaca47caar6nk9yyc4mu`

## License

Copyright (C) Daniel Fernandes Martins

Distributed under the New BSD License. See COPYING for further details.
