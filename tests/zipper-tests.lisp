(in-package :cl-zipper.tests)

(in-suite :cl-zipper)

(defparameter *tree* '(/ (+ (* a 2) (- b 4))))

(defun -> (tree &rest path)
  "Navigates the given path on tree, where each element in path is a keyword
that represents the desired direction, i.e., :down, :up, :left, :right."
  (labels ((nav (loc rest)
                (if rest
                    (nav (case (car rest)
                           (:down (go-down loc))
                           (:up (go-up loc))
                           (:left (go-left loc))
                           (:right (go-right loc))
                           (otherwise loc))
                         (cdr rest))
                  loc)))
    (nav (zipper tree) path)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ZIPPER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test zipper
  (let ((loc (-> *tree*)))
    (is (equal 2 (length loc)))
    (is (equal *tree* (car loc)))
    (is (null (cadr loc)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ROOT-NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test root-node
  (let ((loc (root-node (-> *tree* :down :right :down :right :right))))
    (is (equal 2 (length loc)))
    (is (equal *tree* loc))))

(test root-node-at-root
  (let ((loc (root-node (-> *tree*))))
    (is (equal 2 (length loc)))
    (is (equal *tree* loc))))

(test root-node-at-nil
  (is (null (root-node nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-LEFT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test go-left
  (let* ((loc (-> *tree* :down :right :down :right :right))
         (new-loc-1 (go-left loc))
         (new-loc-2 (go-left new-loc-1)))
    (is (equal '(* a 2) (car new-loc-1)))
    (is (equal '(+) (lefts new-loc-1)))
    (is (equal '((- b 4)) (rights new-loc-1)))

    (is (equal '+ (car new-loc-2)))
    (is (null (lefts new-loc-2)))
    (is (equal '((* a 2) (- b 4)) (rights new-loc-2)))))

(test go-left-at-root
  (is (null (go-left (-> *tree*)))))

(test go-left-at-nil
  (is (null (go-left nil))))

(test go-left-at-end
  (let ((loc (-> *tree* :down)))
    (is (equal '/ (car loc)))
    (is (null (lefts loc)))
    (is (null (go-left loc)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-RIGHT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test go-right
  (let* ((loc (-> *tree* :down :right :down))
         (new-loc-1 (go-right loc))
         (new-loc-2 (go-right new-loc-1)))
    (is (equal '(* a 2) (car new-loc-1)))
    (is (equal '(+) (lefts new-loc-1)))
    (is (equal '((- b 4)) (rights new-loc-1)))

    (is (equal '(- b 4) (car new-loc-2)))
    (is (equal '((* a 2) +) (lefts new-loc-2)))
    (is (null (rights new-loc-2)))))

(test go-right-at-root
  (is (null (go-right (-> *tree*)))))

(test go-right-at-nil
  (is (null (go-right nil))))

(test go-right-at-end
  (let ((loc (rightmost (-> *tree* :down :right :down))))
    (is (equal '(- b 4) (car loc)))
    (is (null (rights loc)))
    (is (null (go-right loc)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test go-up
  (let* ((loc (-> *tree* :down :right :down :right :down))
         (new-loc-1 (go-up loc))
         (new-loc-2 (go-up new-loc-1)))
    (is (equal '(* a 2) (car new-loc-1)))
    (is (equal '(+) (lefts new-loc-1)))
    (is (equal '((- b 4)) (rights new-loc-1)))

    (is (equal '(+ (* a 2) (- b 4)) (car new-loc-2)))
    (is (equal '(/) (lefts new-loc-2)))
    (is (null (rights new-loc-2)))))

(test go-up-at-root
  (is (null (go-up (-> *tree*)))))

(test go-up-at-nil
  (is (null (go-up nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-DOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test go-down
  (let* ((loc (-> *tree* :down :right))
         (new-loc-1 (go-down loc))
         (new-loc-2 (go-down (go-right new-loc-1))))
    (is (equal '+ (car new-loc-1)))
    (is (null (lefts new-loc-1)))
    (is (equal '((* a 2) (- b 4)) (rights new-loc-1)))

    (is (equal '* (car new-loc-2)))
    (is (null (lefts new-loc-2)))
    (is (equal '(a 2) (rights new-loc-2)))))

(test go-down-at-end
  (is (null (go-down (-> *tree* :down :right :down :right :down)))))

(test go-down-at-nil
  (is (null (go-down nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LEFTMOST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test leftmost
  (let ((loc (leftmost (-> *tree* :down :right :down :right :right))))
    (is (equal '+ (car loc)))
    (is (null (lefts loc)))
    (is (equal '((* a 2) (- b 4)) (rights loc)))))

(test leftmost-at-nil
  (is (null (leftmost nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RIGHTMOST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test rightmost
  (let ((loc (rightmost (-> *tree* :down :right :down))))
    (is (equal '(- b 4) (car loc)))
    (is (null (rights loc)))
    (is (equal '((* a 2) +) (lefts loc)))))

(test rightmost-at-nil
 (is (null (rightmost nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-NEXT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test go-next
  (let* ((loc (-> *tree* :down :right :down :right :down))
         (next-loc (go-next loc)))
    (is (equal '* (car loc)))
    (is (equal 'a (car next-loc)))
    (is (equal '(*) (lefts next-loc)))
    (is (equal '(2) (rights next-loc)))))

(test go-next-at-root
  (let* ((loc (-> *tree*))
         (next-loc (go-next loc)))
    (is (equal *tree* (car loc)))
    (is (equal '/ (car next-loc)))
    (is (equal '() (lefts next-loc)))
    (is (equal '((+ (* a 2) (- b 4))) (rights next-loc)))))

(test go-next-at-rightmost
  (let* ((loc (-> *tree* :down :right :down :right :down :right :right))
         (next-loc (go-next loc)))
    (is (equal 2 (car loc)))
    (is (equal '(- b 4) (car next-loc)))
    (is (equal '((* a 2) +) (lefts next-loc)))
    (is (equal '() (rights next-loc)))))

(test go-next-at-nested-rightmost
  (let* ((loc (-> '(+ (- 1 (* 2 3)) 4) :down :right :down :right :right :down :right :right))
         (next-loc (go-next loc)))
    (is (equal 3 (car loc)))
    (is (equal 4 (car next-loc)))
    (is (equal '((- 1 (* 2 3)) +) (lefts next-loc)))
    (is (equal '() (rights next-loc)))))

(test go-next-at-last
  (let* ((loc (-> *tree* :down :right :down :right :right :down :right :right))
         (next-loc (go-next loc)))
    (is (equal 4 (car loc)))
    (is (null next-loc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-PREV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test go-prev
  (let* ((loc (-> *tree* :down :right :down :right :down :right))
         (prev-loc (go-prev loc)))
    (is (equal 'a (car loc)))
    (is (equal '* (car prev-loc)))
    (is (equal '() (lefts prev-loc)))
    (is (equal '(a 2) (rights prev-loc)))))

(test go-prev-at-root
  (let* ((loc (-> *tree*))
         (prev-loc (go-prev loc)))
    (is (null prev-loc))))

(test go-prev-at-leftmost
  (let* ((loc (-> *tree* :down :right :down :right :down))
         (prev-loc (go-prev loc)))
    (is (equal '* (car loc)))
    (is (equal '(* a 2) (car prev-loc)))))

(test go-prev-at-rightmost
  (let* ((loc (-> *tree* :down :right :down :right :right))
         (prev-loc (go-prev loc)))
    (is (equal '(- b 4) (car loc)))
    (is (equal 2 (car prev-loc)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test path
  (let ((loc (-> *tree* :down :right :down :right :down)))
    (is (equal '((/ (+ (* a 2) (- b 4)))
                 (+ (* a 2) (- b 4))
                 (* a 2)) (path loc)))))

(test path-at-root
  (is (null (path (-> *tree*)))))

(test path-at-nil
  (is (null (path nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHANGE-NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test change-node-atom
  (let* ((loc (-> *tree* :down :right :down :right :down :right))
         (new-loc (change-node loc 'x)))
    (is (equal 'a (car loc)))
    (is (equal 'x (car new-loc)))
    (is (equal '(*) (lefts new-loc)))
    (is (equal '(2) (rights new-loc)))
    (is (equal '(/ (+ (* x 2) (- b 4))) (root-node new-loc)))))

(test change-node-subtree
  (let* ((loc (-> *tree* :down :right :down :right))
         (new-loc (change-node loc '(+ a a))))
    (is (equal '(* a 2) (car loc)))
    (is (equal '(+ a a) (car new-loc)))
    (is (equal '(+) (lefts new-loc)))
    (is (equal '((- b 4)) (rights new-loc)))
    (is (equal '(/ (+ (+ a a) (- b 4))) (root-node new-loc)))))

(test change-node-null
  (is (null (change-node nil 'x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EDIT-NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test edit-node
  (labels ((fn (node arg-1 arg-2 &optional (arg-3 3))
               (is (equal '(* a 2) node))
               (is (equal 1 arg-1))
               (is (equal 2 arg-2))
               (is (equal 3 arg-3))
               '(- b)))
    (let* ((loc (-> *tree* :down :right :down :right))
           (new-loc (edit-node loc #'fn 1 2)))
      (is (equal '(* a 2) (car loc)))
      (is (equal '(- b) (car new-loc)))
      (is (equal '(+) (lefts new-loc)))
      (is (equal '((- b 4)) (rights new-loc)))
      (is (equal '(/ (+ (- b) (- b 4))) (root-node new-loc))))))

(test edit-node-null
  (is (null (edit-node nil #'identity))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INSERT-DOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test insert-down
  (let* ((loc (-> *tree* :down :right :down :right))
         (new-loc (insert-down loc '-)))
    (is (equal '(* a 2) (car loc)))
    (is (equal '(- * a 2) (car new-loc)))
    (is (equal '(+) (lefts new-loc)))
    (is (equal '((- b 4)) (rights new-loc)))
    (is (equal '(/ (+ (- * a 2) (- b 4))) (root-node new-loc)))))

(test insert-down-at-atom
  (let ((loc (-> *tree* :down :right :down :right :down)))
    (is (null (insert-down loc '-)))))

(test insert-down-at-nil
  (is (null (insert-down nil '-))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; APPEND-DOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test append-down
  (let* ((loc (-> *tree* :down :right :down :right))
         (new-loc (append-down loc 'b)))
    (is (equal '(* a 2) (car loc)))
    (is (equal '(* a 2 b) (car new-loc)))
    (is (equal '(+) (lefts new-loc)))
    (is (equal '((- b 4)) (rights new-loc)))
    (is (equal '(/ (+ (* a 2 b) (- b 4))) (root-node new-loc)))))

(test append-down-at-atom
  (let ((loc (-> *tree* :down :right :down :right :down)))
    (is (null (append-down loc 'b)))))

(test append-down-at-nil
  (is (null (append-down nil 'b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INSERT-LEFT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test insert-left
  (let* ((loc (-> *tree* :down :right :down :right :down :right))
         (new-loc (insert-left loc 'b)))
    (is (equal 'a (car new-loc)))
    (is (equal '(*) (lefts loc)))
    (is (equal '(b *) (lefts new-loc)))
    (is (equal '(/ (+ (* b a 2) (- b 4))) (root-node new-loc)))))

(test insert-left-at-nil
  (is (null (insert-left nil 'b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INSERT-RIGHT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test insert-right
  (let* ((loc (-> *tree* :down :right :down :right :down :right))
         (new-loc (insert-right loc 'b)))
    (is (equal 'a (car new-loc)))
    (is (equal '(2) (rights loc)))
    (is (equal '(b 2) (rights new-loc)))
    (is (equal '(/ (+ (* a b 2) (- b 4))) (root-node new-loc)))))

(test insert-right-at-nil
  (is (null (insert-right nil 'b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REMOVE-NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test remove-node
  (let* ((loc (-> *tree* :down :right :down :right :down :right))
         (new-loc (remove-node loc)))
    (is (equal 'a (car loc)))
    (is (equal '2 (car new-loc)))
    (is (equal '(*) (lefts new-loc)))
    (is (null (rights new-loc)))
    (is (equal '(/ (+ (* 2) (- b 4))) (root-node new-loc)))))

(test remove-node-at-leftmost-node
  (let* ((loc (-> *tree* :down :right :down :right :down))
         (new-loc (remove-node loc)))
    (is (equal '* (car loc)))
    (is (equal 'a (car new-loc)))
    (is (null (lefts new-loc)))
    (is (equal '(2) (rights new-loc)))
    (is (equal '(/ (+ (a 2) (- b 4))) (root-node new-loc)))))

(test remove-node-at-rightmost-node
  (let* ((loc (rightmost (-> *tree* :down :right :down :right :down)))
         (new-loc (remove-node loc)))
    (is (equal '2 (car loc)))
    (is (equal 'a (car new-loc)))
    (is (equal '(*) (lefts new-loc)))
    (is (null (rights new-loc)))
    (is (equal '(/ (+ (* a) (- b 4))) (root-node new-loc)))))

(test remove-node-at-nil
  (is (null (remove-node nil))))