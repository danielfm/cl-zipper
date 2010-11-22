(defpackage :cl-zipper.tests
  (:documentation "Provides tests for the zipper library.")
  (:use :cl :cl-zipper :5am))

(in-package :cl-zipper.tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get-test :cl-zipper)
    (def-suite :cl-zipper)))