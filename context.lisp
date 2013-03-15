;;; -*- Mode: Lisp -*-

(in-package :chopin-routing)
;;;; Connectivity metric
;;;; http://www.sciencedirect.com/science/article/pii/S0921889012001753

;; Adjacency Matrix 
(defun build-matrix ()
  (let* ((n-nodes (length (nodes))))
    (lisplab:dnew 0 n-nodes n-nodes)))

(defparameter *adj-matrix* nil)
(defparameter *diag-matrix* nil)

(defmacro with-matrix (matrix &body body)
  `(loop for i below (first (lisplab:dim ,matrix)) do 
	 (loop for j below (nth 1 (lisplab:dim ,matrix))
	      ,@body)))

(defun adj-matrix (adj-matrix)
  (with-matrix adj-matrix do
    (let ((node (node-by-id i)))
      (loop for neighbour across (neighbours node)
	    when (= (address neighbour) j) do
	    (setf (lisplab:mref adj-matrix j i) 1.0d0)))))

(defun vector-sum (adj-matrix)
  (loop for i below (first (lisplab:dim adj-matrix))
	collect (lisplab:msum (lisplab:view-col adj-matrix i))))

(defun diag-from-vector-sum (diag-matrix vector-sum)
  (with-matrix diag-matrix do
	       (when (= i j)
		 (setf (lisplab:mref diag-matrix i j) (nth i vector-sum)))))

(defun laplacian-matrix (adj-matrix diag-matrix)
  (lisplab:.- diag-matrix adj-matrix))

(defun fiedler-value ()
  "We want the second eigen value."
  (setf *adj-matrix* (build-matrix))
  (setf *diag-matrix* (build-matrix))
  (adj-matrix *adj-matrix*)
  (diag-from-vector-sum *diag-matrix* (vector-sum *adj-matrix*))
  (lisplab:mref (first (lisplab::dgeev (laplacian-matrix *adj-matrix* *diag-matrix*) nil nil)) 1 0))

(defun connectivity ()
  (/ (fiedler-value) (length (nodes))))
