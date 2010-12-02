(defpackage :cl-zipper
  (:documentation "Provides the zipper library.")
  (:use :cl)
  (:export :append-down
           :change-node
           :edit-node
           :go-down
           :go-left
           :go-next
           :go-prev
           :go-right
           :go-up
           :insert-down
           :insert-left
           :insert-right
           :leftmost
           :lefts
           :path
           :remove-node
           :rightmost
           :rights
           :root-node
           :zipper))