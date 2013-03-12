;;;; chopin-routing.lisp

(in-package #:chopin-routing)

(defparameter *base-station* nil)
(defparameter *nodes* nil)
(defparameter *message-types* '(:ref "REF" :rreq "RREQ" :na "Node Announce"))
(defparameter *ref-period* 1200 "Topology Refreshing Period - ms")
(defun nodes () *nodes*)

(defgeneric neighbours (node)
  (:documentation "The neighbours of the node."))

(defmethod neighbours ((node node))
  (remove-if-not #'(lambda (neighbour)
		     (<= (+ (distance node neighbour) (signal-strength neighbour))
			 (+ (signal-strength node)
			    (signal-strength neighbour))))
		 (remove-if #'(lambda (n) (equal node n)) (nodes))))

(defun my-neighbour? (node1 node2)
  (member node2 (coerce (neighbours node1) 'list)))

(defun relay-target (node)
  "Nodes that did not relay messages to current node"
  (let ((count (hash-table-count (message-buffer node))))
    (unless (zerop count)
      (remove-if #'(lambda (n) (or (base-station-p n) (= (relayed (hash-pop (message-buffer node))) (address n)))) (neighbours node)))))

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
  "Generate new message and relay to neighbours, except those where
the message came from."
  (unless (zerop (length (relay-target node)))
    (with-slots (msg-type bs-id bs-metric rn relayed) message
      (broadcast node (make-instance 'refreshing-message :msg-type msg-type
				     :bs-id bs-id
				     :bs-metric (1+ bs-metric)
				     :rn rn
				     :relayed (address node))))))

(defun relay-target (node)
  "Neighbours from which the node did not receive a relayed message."
  (let ((count (hash-table-count (message-buffer node))))
    (unless (zerop count)
      (when (not (zerop (hash-table-count (message-buffer node))))
	(remove-if #'(lambda (n) (or (base-station-p n) (= (relayed (hash-pop (message-buffer node))) (address n)))) (neighbours node))))))

(defun new-announce-msg (node)
  "Node announce message"
  (with-slots (address refreshing-number) node
    (make-instance 'announce-message :msg-type (getf *message-types* :na)
		   :origin address
		   :hop-count 0
		   :path `(,address)
		   :rn (incf refreshing-number))))

(defun has-path? (base-station node-id)
  "Checks if there's an entry for _node-id_ in the routing table."
  (with-accessors ((rt routing-table)) base-station
    (node-by-id node-id rt #'bs-id)))

(defun valid-path? (base-station node-id)
  "The routing table entry path info can be successfuly traversed."
  (let ((path-map (has-multihop-path? base-station node-id)))
    (when path-map
      (notany #'null path-map))))

(defun has-multihop-path? (base-station node-id)
  "See if we can reach a node from the base-station."
  (with-accessors ((rt routing-table)) base-station
    (let ((table-entry (node-by-id node-id rt #'bs-id)))
      (when table-entry
	(let ((path (next-hop table-entry)))
	  (loop for e in path
		for current = base-station then node
		for node = (node-by-id e (neighbours current))
		collect node
		while node))))))

(defun node-announce (node announce-message)
  "Periodic node announce."
  (unless (zerop (length (routing-table node)))
    (unless (base-station-p node)
      (let ((next-hop (if (base-station-p node)
			  node
			  (node-by-id (next-hop (aref (routing-table node) 0))))))
	(process-announce next-hop announce-message)))))

(defun process-announce (node ann-message)
  "Base station updates routing table.
Nodes relay to base station."
  (let ((to-relay ann-message))
    (with-slots (origin hop-count path rn) to-relay
      (if (base-station-p node)
	  (progn
	    (let ((current (has-path? node origin)))
	      (if (and
		   ;(valid-path? node origin)
		   current (> rn (rn current)))
		  (progn
		    (setf (rn current) rn)
		    (setf (next-hop current) path)
		    (setf (hop-count current) (1+ hop-count)))
		  (unless (node-by-id origin (routing-table node) #'bs-id)
		    (vector-push (make-instance 'table-entry :bs-id origin
						:next-hop path
						:hop-count (1+ hop-count)
						:rn rn) (routing-table node))))))
	  (progn
	    (incf hop-count)
	    (setf path (adjoin (address node) path))
	    (node-announce node to-relay))))))

(defun init-topology-refresh (base-station message)
  (broadcast base-station message))

(defun process-ref (node ref-message)
  "Topology refreshing: process ref-message. If message RN is higher than table entry RN, update table. REFACTOR!"
  (unless (duplicate-message? node ref-message)
    (with-accessors ((message-rn rn) (bs-id bs-id) (hop-count bs-metric) (relayed relayed)) ref-message
      (let ((rt (routing-table node)))
	(if (zerop (length rt))
	    (vector-push (make-instance 'table-entry :bs-id bs-id :next-hop (or relayed)
					:hop-count (1+ hop-count) :rn message-rn) rt)
	    (progn
	      (let ((old-table-entry (aref rt 0)))
		(with-accessors ((entry-rn rn) (entry-hop-count bs-metric)) old-table-entry
		  (when (> message-rn entry-rn) ;(<= hop-count entry-hop-count)
		    (vector-pop rt) ; keep single entry for base station
		    (vector-push (make-instance 'table-entry :bs-id bs-id :next-hop (or relayed)
						:hop-count (1+ hop-count) :rn message-rn) rt))))))
	ref-message))))

;;; Utils

(defun hash-pop (hashtable)
  (gethash (car (first (alexandria:hash-table-alist hashtable))) hashtable))

(defun node-by-id (id &optional (table (nodes)) (id-func #'address))
  (some #'(lambda (node) (when (= (funcall id-func node) id)
			   node))
	(coerce table 'list)))

;;; Simulation

(defun generate-node ()
  (make-instance 'node
		 :signal-strength 5
		 :x (+ (random 10) 3)
		 :y (+ (random 10) 3)))

(defun bootstrap ()
  (let ((sig-strength 5))
    (setf *nodes* (make-array 10 :element-type 'node :fill-pointer 0 :adjustable t))
    (setf *base-station* (make-instance 'node :signal-strength sig-strength
					:x (+ (random 15) 3)
					:y (+ (random 15) 3)
					:base-station-p t))
    (loop repeat 3 do
	    (generate-node))))

(defun run ()
  (bootstrap)
  (nodes))
