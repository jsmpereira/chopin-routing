;;; -*- Mode: Lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load"/Users/josesantos/quicklisp/local-projects/matlisp-master/build/start.lisp"))

(in-package :chopin-sim)
;;;; Connectivity metric
;;;; http://www.sciencedirect.com/science/article/pii/S0921889012001753

;; Adjacency Matrix 
(defun build-matrix ()
  (let ((n-nodes (length (nodes))))
    (matlisp:make-float-matrix n-nodes n-nodes)))

(defparameter *adj-matrix* nil)
(defparameter *diag-matrix* nil)

(defmacro with-matrix (matrix &body body)
  `(loop for i below (first (matlisp:size ,matrix)) do 
	 (loop for j below (nth 1 (matlisp:size ,matrix))
	      ,@body)))

(defun adj-matrix (adj-matrix)
  (with-matrix adj-matrix do
    (let ((node (node-by-id i)))
      (loop for neighbour across (neighbours node)
	    when (= (address neighbour) j) do
	    (setf (matlisp:matrix-ref adj-matrix j i) 1.0f0)))))

(defun vector-sum (adj-matrix)
  (matlisp:sum adj-matrix))

(defun diag-from-vector-sum (diag-matrix vector-sum)
  (setf *diag-matrix* (matlisp:diag vector-sum)))

(defun laplacian-matrix (adj-matrix diag-matrix)
  (matlisp:m- diag-matrix adj-matrix))

(defun fiedler-value ()
  "We want the second (last?) eigen value."
  (setf *adj-matrix* (build-matrix))
  (adj-matrix *adj-matrix*)
  (diag-from-vector-sum *diag-matrix* (vector-sum *adj-matrix*))
  (let ((eigenvalues (matlisp:eig (laplacian-matrix *adj-matrix* *diag-matrix*))))
    (values (matlisp:mref eigenvalues 3) eigenvalues)))

(defun connectivity ()
  (/ (fiedler-value) (length (nodes))))
