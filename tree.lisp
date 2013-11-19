;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: TREE; -*-
;;; Adapted from http://gajon.org/trees-linked-lists-common-lisp/. Originally written by Jorge Gajon
;;; From Jorge's original web page:
;;; "PLEASE NOTE, that if you need to represent trees in a production program you
;;;  should not use lists as described here unless you have a good reason.
;;;  This is only an exercise in understanding how cons cells work."
;;;
;;; I will replace with more efficient code once things settle down.

(in-package #:tree)

(declaim (inline #:first-child #:add-child #:next-sibling #:data))

(defun make-node (data)
  "Creates a new node with DATA as contents"
  (declare #.utils:*standard-optimize-settings*)
  (cons (cons data nil) nil))

(defun add-child (node child)
  "Takes two nodes created with MAKE-NODE and adds CHILD"
  (declare #.utils:*standard-optimize-settings*)
  (nconc (first node) child)
  node)

(defun first-child (node)
  "Returns a reference to the first child of NODE"
  (declare #.utils:*standard-optimize-settings*)
  (rest (first node)))

(defun next-sibling (node)
  "Returns next SIBLING of NODE"
  (declare #.utils:*standard-optimize-settings*)
  (rest node))

(defun data (node)
  "Returns the information in NODE"
  (declare #.utils:*standard-optimize-settings*)
  (first (first node)))

(defun traverse (tree func &optional (depth 0))
  "Depth-first traversal of TREE calling FUNC for each node"
  (declare #.utils:*standard-optimize-settings*)
  (when tree
    (funcall func tree depth)
    (traverse (first-child tree) func (+ 2 depth))
    (traverse (next-sibling tree) func depth)))

(defun print-tree (tree)
  "Print the nodes of TREE"
  (declare #.utils:*standard-optimize-settings*)
  (traverse tree (lambda (node depth) (format t "~v@tNode: ~a~%" depth (data node)))))

(defun find-tree (tree test)
  "Find all nodes in TREE where TEST returns T"
  (declare #.utils:*standard-optimize-settings*)
  (let ((results))
    (traverse tree (lambda (node depth)
                     (declare (ignore depth))
                     (when (funcall test node)
                       (push node results))))
    (nreverse results)))

(defun at-path (tree path cmp)
  "Return node from TREE located at PATH"
  (declare #.utils:*standard-optimize-settings*)

  (when (or (null tree) (null path))
    (return-from at-path nil))

  (when (funcall cmp tree (first path))
    (when (= 1 (length path))
      (return-from at-path tree))
    (loop for node = (first-child tree) then (next-sibling node)
          until (null node) do
            (utils:aif (at-path node (rest path) cmp)
                       (return-from at-path utils:it))))
  nil)

(let ((pkg (find-package :tree)))
  (do-all-symbols (sym pkg) (when (eql (symbol-package sym) pkg) (export sym pkg))))
