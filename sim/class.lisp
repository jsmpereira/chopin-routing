;;; -*- Mode: Lisp -*-

(in-package :chopin-sim)

;; Table Entry class definition
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

;; Node Announce Message class definition
(defclass announce-message ()
  ((msg-type :initarg :msg-type
	    :accessor msg-type)
   (origin :initarg :origin
	   :accessor origin)
   (hop-count :initarg :hop-count
	      :accessor hop-count)
   (rn :initarg :rn
       :accessor rn)
   (path :initarg :path
	 :accessor path)))

(defmethod print-object ((object announce-message) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (msg-type origin hop-count path rn) object
      (format stream "msg-type: ~a origin: ~a hop-count: ~a path: ~a rn: ~a" msg-type origin hop-count path rn))))

;; Group Announce Message class definition
(defclass group-message ()
  ((msg-type :initarg :msg-type
	     :accessor msg-type)
   (origin :initarg :origin
	   :accessor origin)
   (hop-count :initarg :hop-count
	      :accessor hop-count)
   (group-id :initarg :group-id
	     :accessor group-id)
   (rn :initarg :rn
       :accessor rn)))

(defmethod print-object ((object group-message) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (msg-type origin hop-count group-id rn) object
      (format stream "msg-type: ~a origin: ~a hop-count: ~a group-id: ~a rn: ~a" msg-type origin hop-count group-id rn))))

;; Refreshing Message class definition
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

;; Node class definition
(defclass node ()
  ((address :initarg :address
	    :accessor address
	    :documentation "Node Address")
   (group-id :initarg :group-id
	     :accessor group-id
	     :documentation "Group ID the node belongs to")
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
    (with-slots (signal-strength x y base-station-p address refreshing-number broadcast-id routing-table group-id) object
      (format stream "UID: ~d Group: ~a BS: ~d :signal ~d :pos (~d,~d) :rn ~d :bid ~d :RT ~a" address group-id base-station-p signal-strength x y refreshing-number broadcast-id routing-table))))


