(defpackage :cl-zipper-system
  (:use :cl :asdf))

(in-package :cl-zipper-system)

(defsystem :cl-zipper
  :description "Common Lisp implementation of Gérard Huet's Zippers."
  :version "0.1"
  :author "Daniel Martins <daniel.tritone@gmail.com>"
  :license "BSD"
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "zipper")))
               (:static-file "README.rst")
               (:static-file "COPYING"))
  :in-order-to ((asdf:test-op (load-op :cl-zipper.tests))))

(defsystem :cl-zipper.tests
  :depends-on (:fiveam :cl-zipper)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "zipper-tests")))))

(defmethod asdf:perform :after ((op asdf:test-op) (c (eql (asdf:find-system :cl-zipper))))
  "Runs the tests when test-op is issued for :cl-zipper."
  (funcall (find-symbol (string :run!) :cl-zipper.tests)))