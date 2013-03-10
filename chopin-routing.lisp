;;;; chopin-routing.lisp

(in-package #:chopin-routing)

(defparameter *base-station* nil)
(defparameter *nodes* nil)

(defparameter *message-types* '(:ref "REF" :rreq "RREQ"))

(defparameter *ref-period* 1200 "Topology Refreshing Period - ms")

(defclass table-entry ()
  ((bs-id :initarg :bs-id
	  :accessor bs-id)
   (next-hop :initarg :next-hop
	     :accessor next-hop)
   (hop-count :initarg :hop-count
	      :accessor hop-count)
   (rn :initarg :rn
       :accessor rn)
   (bid :initarg :bid
	:accessor bid)))

(defmethod print-object ((object table-entry) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (bs-id next-hop hop-count rn) object
      (format stream "bs-id: ~d next-hop: ~d hop-count: ~d :rn ~d" bs-id next-hop hop-count rn))))

(defclass refreshing-message ()
  ((uid :initarg :uid
	:accessor uid
	:documentation "Unique Identifier")
   (msg-type :initarg :msg-type
	     :accessor msg-type)
   (bs-id :initarg :bs-id
	  :accessor bs-id)
   (bs-metric :initarg :bs-metric
	      :accessor bs-metric)
   (rn :initarg :rn
       :accessor rn)
   (relayed :initarg :relayed
	    :accessor relayed
	    :documentation "Node that relayed this message."))
  (:default-initargs
   :rn 0
   :relayed 0))

(defmethod initialize-instance :after ((message refreshing-message) &key)
  (setf (slot-value message 'uid)
	(ironclad:byte-array-to-hex-string
	 (ironclad:digest-sequence :sha1 (ironclad:ascii-string-to-byte-array (format nil "~a" message))))))

(defmethod print-object ((object refreshing-message) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (msg-type bs-id bs-metric rn relayed) object
      (format stream "TYPE: ~a BS: ~d hops: ~d RN: ~d RELAYED: ~A" msg-type bs-id bs-metric rn relayed))))

(defun nodes () *nodes*)

(defclass node ()
  ((address :initarg :address
	    :accessor address
	    :documentation "Node Address")
   (routing-table :initarg :routing-table
		  :accessor routing-table
		  :documentation "This node's routing table")
   (neighbors :initarg :neighbors
	      :accessor neighbors
	      :documentation "This node's neighbors")
   (signal-strength :initarg :signal-strength
		    :accessor signal-strength
		    :documentation "Wireless signal strength")
   (message-buffer :initarg :message-buffer
		   :accessor message-buffer
		   :documentation "Duplicate buffer")
   (refreshing-number :initarg :refreshing-number
		      :accessor refreshing-number)
   (broadcast-id :initarg :broadcast-id
		 :accessor broadcast-id)
   (x :initarg :x
      :accessor x
      :documentation "X coord")
   (y :initarg :y
      :accessor y
      :documentation "Y coord")
   (base-station-p :initarg :base-station-p
		   :accessor base-station-p
		   :documentation "If true this node is base station")
   (color :initarg :color
	  :accessor color
	  :documentation "Node's color"))
  (:default-initargs
   :address 0
   :routing-table (make-array 5 :element-type 'table-entry :fill-pointer 0 :adjustable t)
   :neighbors (make-array 5 :element-type 'node :fill-pointer 0 :adjustable t)
   :signal-strength 1
   :message-buffer (make-hash-table :test 'equal)
   :refreshing-number 0
   :broadcast-id 0
   :x 0
   :y 0
   :base-station-p nil
   :color sdl:*red*))

(defmethod initialize-instance :after ((node node) &key)
  (setf (slot-value node 'address) (vector-push-extend node (nodes))))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (signal-strength x y base-station-p address refreshing-number broadcast-id routing-table) object
      (format stream "UID: ~d BS: ~d :signal ~d :pos (~d,~d) :rn ~d :bid ~d :RT ~a" address base-station-p signal-strength x y refreshing-number broadcast-id routing-table))))

(defun distance (node1 node2)
  (with-accessors ((x1 x) (y1 y)) node1
    (with-accessors ((x2 x) (y2 y)) node2
      (sqrt (+ (expt (- x1 x2) 2)
	       (expt (- y1 y2) 2))))))

(defun hash-pop (hashtable)
  (gethash (car (first (alexandria:hash-table-alist hashtable))) hashtable))

(defgeneric neighbours (node)
  (:documentation "The neighbours of the node."))

(defmethod neighbours ((node node))
  (remove-if-not #'(lambda (neighbour)
		     (<= (+ (distance node neighbour) (signal-strength neighbour))
			 (+ (signal-strength node)
			    (signal-strength neighbour))))
		 (remove-if #'(lambda (n) (equal node n)) (nodes))))

(defun relay-target (node)
  (let ((count (hash-table-count (message-buffer node))))
    (unless (zerop count)
      (when (not (zerop (hash-table-count (message-buffer node))))
	(remove-if #'(lambda (n) (or (base-station-p n) (= (relayed (hash-pop (message-buffer node))) (address n)))) (neighbours node))))))

(defgeneric broadcast (node message)
  (:documentation "Broadcasts a message originated on node, i.e, publishes it to all topics."))

(defmethod broadcast ((node node) (message refreshing-message))
  (let ((targets (or (relay-target node) (neighbours node))))
    (with-accessors ((key uid)) message
      (loop for neighbour across targets do
	    (with-accessors ((buf message-buffer)) neighbour
	      (unless (gethash key buf)
		(setf (gethash key buf) message))
	      (unless (base-station-p node)
		(process-ref neighbour message)))))))
;;; Routing

(defun new-refreshing-msg (base-station)
  "Periodic topology refresh from base station. Each new message increments RN."
  (assert (base-station-p base-station) nil "Node supplied not the Base Station.")
  (with-accessors ((rn refreshing-number) (addr address)) base-station
    (make-instance 'refreshing-message :msg-type (getf *message-types* :ref)
		   :bs-id addr
		   :bs-metric 0
		   :rn (incf rn))))

(defmethod duplicate-message? ((node node) u)
  (loop for k being the hash-keys in (message-buffer node) using (hash-value v) do
	(when (equal u (uid v))
	  (return v))))

(defun relay-refreshing-msg (node message)
  (unless (zerop (length (relay-target node)))
    (with-slots (msg-type bs-id bs-metric rn relayed) message
      (broadcast node (make-instance 'refreshing-message :msg-type msg-type
				     :bs-id bs-id
				     :bs-metric (1+ bs-metric)
				     :rn rn
				     :relayed (address node))))))

(defun init-topology-refresh (base-station message)
  (broadcast base-station message))

(defun process-ref (node ref-message)
  "Topology refreshing: process ref-message. If message RN is higher than table entry RN, update table"
  (unless (duplicate-message? node ref-message)
    (with-accessors ((message-rn rn) (bs-id bs-id) (hop-count bs-metric) (relayed relayed)) ref-message
      (let ((rt (routing-table node)))
	(if (zerop (length rt))
	    (progn
	      (vector-push (make-instance 'table-entry :bs-id bs-id :next-hop (or relayed)
					  :hop-count (1+ hop-count) :rn message-rn) rt))
	    (progn
	      (let ((old-table-entry (aref rt 0)))
		(with-accessors ((entry-rn rn) (entry-hop-count bs-metric)) old-table-entry
		  (when (> message-rn entry-rn) ;(<= hop-count entry-hop-count)
		    (vector-pop rt)
		    (vector-push (make-instance 'table-entry :bs-id bs-id :next-hop (or relayed)
						:hop-count (1+ hop-count) :rn message-rn) rt))))))
	ref-message))))

;;; Simulation

(defun bootstrap ()
  (let ((sig-strength 5))
    (setf *nodes* (make-array 10 :element-type 'node :fill-pointer 0 :adjustable t))
    (setf *base-station* (make-instance 'node :signal-strength sig-strength
					:x (+ (random 15) 3)
					:y (+ (random 15) 3)
					:base-station-p t))
    (loop repeat 5 do
	  (make-instance 'node
			 :signal-strength sig-strength
			 :x (+ (random 10) 3)
			 :y (+ (random 10) 3)))))

(defun run ()
  (bootstrap)
  (nodes))
