(in-package :cl-zipper)

(defstruct node left ppath right)

(defun zipper (lst)
  "Creates a zipper for the list lst."
  (list lst nil))

(defmacro with-loc (loc &body body)
  "Binds the variables 'loc-tree' and 'loc-path' for the tree and path of this
loc, respectively."
  `(let ((loc-tree (car ,loc))
         (loc-path (cadr ,loc)))
     loc-tree loc-path
     ,@body))

(defun root-node (loc)
  "Zips all the way up and returns the root node, reflecting any changes."
  (with-loc loc
    (let ((loc-up (go-up loc)))
      (if loc-up
          (root-node loc-up)
        (car loc)))))

(defun go-down (loc)
  "Returns the loc of the leftmost child of the node at this loc, or nil if no
children."
  (with-loc loc
    (when (consp loc-tree)
      (list (car loc-tree)
            (make-node :left nil
                       :ppath loc-path
                       :right (cdr loc-tree))))))

(defun go-right (loc)
  "Returns the loc of the right sibling of the node at this loc, or nil."
  (with-loc loc
    (when loc-path
      (let ((left (node-left loc-path))
            (right (node-right loc-path)))
        (when right
          (list (car right)
                (make-node :left (cons loc-tree left)
                           :ppath (node-ppath loc-path)
                           :right (cdr right))))))))

(defun rights (loc)
  "Returns a list of the right siblings of this loc."
  (with-loc loc
    (when loc-path
      (node-right loc-path))))

(defun rightmost (loc)
  "Returns the loc of the rightmost sibling of the node at this loc, or self."
  (with-loc loc
    (if loc-path
        (let ((right (node-right loc-path)))
          (list (car (last right))
                (make-node :left (cons (car (butlast right)) (list loc-tree))
                           :ppath (node-ppath loc-path)
                           :right nil)))
      loc)))

(defun go-left (loc)
  "Returns the loc of the left sibling of the node at this loc, or nil."
  (with-loc loc
     (when loc-path
       (let ((left (node-left loc-path))
             (right (node-right loc-path)))
         (when left
           (list (car left)
                 (make-node :left (cdr left)
                            :ppath (node-ppath loc-path)
                            :right (cons loc-tree right))))))))

(defun lefts (loc)
  "Returns a list of the left siblings of this loc."
  (with-loc loc
    (when loc-path
      (node-left loc-path))))

(defun leftmost (loc)
  "Returns the loc of the leftmost sibling of the node at this loc, or self."
  (with-loc loc
    (if loc-path
        (let ((left (node-left loc-path)))
          (list (car (last left))
                (make-node :left nil
                           :ppath (node-ppath loc-path)
                           :right (cons (car (butlast left)) (list loc-tree)))))
      loc)))

(defun go-up (loc)
  "Returns the loc of the parent node of the node at this loc, or nil if at
the top."
  (with-loc loc
    (when loc-path
      (list (concatenate 'list (reverse (node-left loc-path))
                         (when loc-tree
                           (cons loc-tree (node-right loc-path))))
            (node-ppath loc-path)))))

(defun path (loc)
  "Returns the list of nodes leading to this loc."
  (let ((loc-up (go-up loc)))
    (when loc-up
      (append (path loc-up) (list (car loc-up))))))

(defun change-node (loc tree)
  "Replaces the node at this loc, whithout moving."
  (when loc
    (list tree (cadr loc))))

(defun edit-node (loc func &rest args)
  "Replaces the node at this loc with the value of (func node args)."
  (change-node loc (apply func (car loc) args)))

(defun insert-right (loc tree)
  "Inserts the item as the right sibling of the node at this loc, without
moving."
  (with-loc loc
    (when loc-path
      (list loc-tree
            (make-node :left (node-left loc-path)
                       :ppath (node-ppath loc-path)
                       :right (cons tree (node-right loc-path)))))))

(defun insert-left (loc tree)
  "Inserts the item as the left sibling of the node at this loc, without
moving."
  (with-loc loc
    (when loc-path
      (list loc-tree
            (make-node :left (cons tree (node-left loc-path))
                       :ppath (node-ppath loc-path)
                       :right (node-right loc-path))))))

(defun insert-down (loc tree)
  "Inserts the item as the leftmost child of the node at this loc, without
moving."
  (with-loc loc
    (when (consp loc-tree)
      (list (cons tree loc-tree)
            loc-path))))

(defun append-down (loc tree)
  "Inserts the item as the rightmost child of the node at this loc, without
moving."
  (with-loc loc
    (when (consp loc-tree)
      (list (concatenate 'list loc-tree (list tree))
            loc-path))))

(defun remove-node (loc)
  "Removes the node at loc, returning the loc that would have preceded it in a
depth-first walk."
  (with-loc loc
    (when (and loc loc-path)
      (cond ((node-right loc-path) (replace-by-right loc-path))
            ((node-left loc-path) (replace-by-left loc-path))))))

(defun replace-by-right (loc-path)
  "Replaces the current node at loc by the node at the right."
  (when loc-path
    (let ((right (node-right loc-path)))
      (list (car right)
            (make-node :left (node-left loc-path)
                       :ppath (node-ppath loc-path)
                       :right (cdr right))))))

(defun replace-by-left (loc-path)
  "Replaces the current node at loc by the node at the left."
  (when loc-path
    (let ((left (node-left loc-path)))
      (list (car left)
            (make-node :left (cdr left)
                       :ppath (node-ppath loc-path)
                       :right (node-right loc-path))))))