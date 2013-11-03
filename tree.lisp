;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: TREE; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:tree)

(defun make-node (data)
  "Creates a new node with DATA as contents"
  (cons (cons data nil) nil))

(defun add-child (node child)
  "Takes two nodes created with MAKE-NODE and adds CHILD"
  (nconc (first node) child)
  node)

(defun first-child (node)
  "Returns a reference to the first child of NODE"
  (rest (first node)))

(defun next-sibling (node)
  "Returns next SIBLING of NODE"
  (rest node))

(defun data (node)
  "Returns the information in NODE"
  (first (first node)))

(defun traverse (tree func &optional (depth 0))
  "Depth-first traversal of TREE calling FUNC for each node"
  (when tree
    (funcall func (data tree) depth)
    (traverse (first-child tree) func  (+ 2 depth))
    (traverse (next-sibling tree) func depth)))

(defun print-tree (tree)
  "Print the nodes of TREE"
  (traverse tree (lambda (node depth) (format t "~v@tNode: ~a~%" depth node))))

(defun find-tree (tree test)
  "Find all nodes in TREE where TEST returns T"
  (let ((results))
    (traverse tree (lambda (node depth)
                     (declare (ignore depth))
                     (when (funcall test node)
                       (push node results))))
    (nreverse results)))

(let ((pkg (find-package :tree)))
  (do-all-symbols (sym pkg) (when (eql (symbol-package sym) pkg) (export sym pkg))))
