;;; -*- Mode: Lisp -*-

(in-package #:chopin-routing)

(defvar *paused* nil)
(defvar *running* nil)

(defparameter *width* 1024)
(defparameter *height* 768)

(defparameter *scale* 20)
(defparameter *radius* 10)

(defun start-simulation ()
  (loop for node across (nodes) do
	(if (not (base-station-p node))
	    (setf (color node) sdl:*red*)))
  (render-nodes *nodes*)
  (sdl:update-display))

(defun render-node (node)
  (with-slots (signal-strength x y base-station-p address color refreshing-number) node
    (let ((xs (* x *scale*))
	  (ys (* y *scale*)))
      (sdl:draw-circle (sdl:point :x xs :y ys) *radius*)
      (sdl:draw-string-solid-* (format nil "ID: ~a (~a, ~a) signal: ~a RN: ~a" address x y signal-strength refreshing-number) xs ys)
      (sdl:draw-circle (sdl:point :x xs :y ys) (* *scale* signal-strength) :color (if base-station-p sdl:*yellow* color))
      (sdl:draw-string-solid-* (write-to-string address) xs (- 10 ys))
      (sdl:draw-string-solid-*
       (format nil "TABLE: ~a" (routing-table node)) xs (+ 10 ys))
      (sdl:draw-string-solid-*
       (format nil "NEIGHBOURS: ~a" (neighbours node)) xs (+ 20 ys))
      (sdl:draw-string-solid-*
       (format nil "RELAY: ~a" (relay-target node)) xs (+ 30 ys)))))

(defun node-step (node)
  (when (not (base-station-p node))
    (unless (zerop (hash-table-count (message-buffer node)))
      (let ((msg (hash-pop (message-buffer node))))
	(unless (duplicate-message? node msg)
	  (process-ref node msg)
	  (relay-refreshing-msg node msg))))))

(defun render-nodes (nodes)
  (init-topology-refresh *base-station* (new-refreshing-msg *base-station*))
  (loop for node across nodes do
	(when (neighbours node)
	  (node-step node))
	(render-node node)))
      
(defun context-menu ()
  (loop for node across (nodes) do 
    (with-slots (x y) node
      (when (> 10 (sdl:distance-* (* x *scale*) (* y *scale*) (sdl:mouse-x) (sdl:mouse-y)))
	  (sdl:draw-string-shaded-* (format nil "~d" node) (sdl:mouse-x) (sdl:mouse-y) sdl:*white* sdl:*blue*)
	  (sdl:draw-string-shaded-* (format nil "~d" (neighbours node)) (sdl:mouse-x) (+ 10 (sdl:mouse-y)) sdl:*blue* sdl:*white*)))))

(defun drag (s x y)
  (if (eq s sdl:mouse-left)
      (loop for node across (nodes) do
	(with-slots (x y) node
	  (when (and (> 20 (sdl:distance-* (* x *scale*) (* y *scale*) (sdl:mouse-x) (sdl:mouse-y))) (sdl:mouse-held-p sdl:mouse-left))
	    (setf x (/ (sdl:mouse-x) *scale*))
	    (setf y (/ (sdl:mouse-y) *scale*))
	    (render-node node))))))

(defun handle-key (key)
  (case key
    (:sdl-key-q
     (sdl:push-quit-event))
    (:sdl-key-r
     (run))
    (:sdl-key-s
     (init-topology-refresh *base-station* (new-refreshing-msg *base-station*)))))

(defun viewer ()
  (setf *running* t
      *paused* nil)
  (sdl:with-init ()
    (sdl:window *width* *height*)
    (sdl:initialise-default-font)
    (setf (sdl:frame-rate) 30)
    (bootstrap)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (handle-key key))
      (:mouse-motion-event (:state s :x-rel dx :y-rel dy :x x :y y)
			   (drag s x y))
      (:idle ()
	     (sdl:clear-display sdl:*black*)
	     (when (sdl:mouse-right-p)
	       (context-menu))
	     (let ((connection
		    (or swank::*emacs-connection* (swank::default-connection))))
	       (when (and connection (not (eql swank:*communication-style* :spawn)))
		 (swank::handle-requests connection t)))
	     (start-simulation)))))
