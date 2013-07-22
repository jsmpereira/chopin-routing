;;; -*- Mode: Lisp -*-
(in-package :chopin-routing)

(defstruct config interface host-address broadcast-address port hop-limit refresh-interval dup-hold-time neighb-hold-time timer-repeat-interval)

(defstruct rt-entry destination next-hop hop-count seq-num)

(defclass duplicate-tuple ()
  ((orig-addr :initarg :orig-addr :accessor orig-addr)
   (msg-type :initarg :msg-type :accessor msg-type)
   (seq-num :initarg :seq-num :accessor seq-num)
   (exp-time :initarg :exp-time :accessor exp-time)))

(defmethod print-object ((object duplicate-tuple) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (msg-type orig-addr seq-num exp-time) object
      (format stream "~A ~A ~A ~A" msg-type (usocket:hbo-to-dotted-quad orig-addr) seq-num exp-time))))

(defclass link-tuple ()
  ((l-neighbor-iface-addr :initarg :neighbor-iface-addr :accessor l-neighbor-iface-addr)
   (l-heard-time :initarg :heard-time :accessor l-heard-time)
   (l-sym-time :initarg :sym-time :accessor l-sym-time)
   (l-quality :initarg :quality :accessor l-quality)
   (l-lost :initarg :lost :accessor l-lost)
   (l-status :initarg :status :accessor l-status)
   (l-time :initarg :time :accessor l-time))
  (:default-initargs
   :status (getf *link-status* :heard)))

(defmethod print-object ((object link-tuple) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (l-neighbor-iface-addr l-heard-time l-sym-time l-quality l-lost l-status l-time) object
      (format stream "~A ~A" (usocket:hbo-to-dotted-quad l-neighbor-iface-addr) l-status))))

(defclass packet ()
  ((pkt-header :initarg :pkt-header :accessor pkt-header)
   (message :initarg :message :accessor message)))

(defmethod print-object ((object packet) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (message) object
      (format stream "PKT-HEADER => ~A MESSAGE => SEQ: ~A ORIG: ~A BLOCK: ~A" (pkt-header object) (msg-seq-num (msg-header message)) (usocket:hbo-to-dotted-quad (msg-orig-addr (msg-header message))) (addr+tlv message)))))

(defclass pkt-header ()
  ((version :initarg :version
	   :accessor version
	   :type '(unsigned-byte 4))
   (pkt-flags :initarg :pkt-flags
	      :accessor pkt-flags
	      :type '(unsigned-byte 4))
   (pkt-seq-num :initarg :pkt-seq-num
		:accessor pkt-seq-num
		:type '(unsigned-byte 16)))
  (:default-initargs
   :version 0
   :pkt-flags #b1000
   :pkt-seq-num *pkt-seq-num*))

(defclass message ()
  ((msg-header :initarg :msg-header :accessor msg-header)
   (tlv-block :initarg :tlv-block :accessor tlv-block)
   (addr+tlv :initarg :addr+tlv :accessor addr+tlv
	     :type 'addr+tlv)) ; (<addr-block><tlv-block>)*
  (:default-initargs
   :tlv-block (make-instance 'tlv-block)
   :addr+tlv nil))

(defstruct addr+tlv address-block tlv-block)

(defclass msg-header ()
  ((msg-type :initarg :msg-type
	     :accessor msg-type
	     :type '(unsigned-byte 8))
   (msg-flags :initarg :msg-flags
	      :accessor msg-flags
	      :type '(unsigned-byte 4))
   (msg-addr-length :initarg :msg-addr-length
		    :accessor msg-addr-length
		    :type '(unsigned-byte 4))
   (msg-size :initarg :msg-size
	     :accessor msg-size
	     :type '(unsigned-byte 16))
   (msg-orig-addr :initarg :msg-orig-addr
		  :accessor msg-orig-addr
		  :type '(unsigned-byte 32))
   (msg-hop-limit :initarg :msg-hop-limit
		  :accessor msg-hop-limit
		  :type '(unsigned-byte 8))
   (msg-hop-count :initarg :msg-hop-count
		  :accessor msg-hop-count
		  :type '(unsigned-byte 8))
   (msg-seq-num :initarg :msg-seq-num
		:accessor msg-seq-num
		:type '(unsigned-byte 16)))
  (:default-initargs
   :msg-type nil
   :msg-flags #b1111
   :msg-addr-length #b0011 ; IPv4
   :msg-size 0
   :msg-orig-addr (usocket:host-byte-order (config-host-address *config*))
   :msg-hop-limit (config-hop-limit *config*)
   :msg-hop-count 0
   :msg-seq-num *msg-seq-num*))

(defmethod initialize-instance :after ((msg-header msg-header) &key)
  (with-slots (msg-seq-num msg-type) msg-header
    (setf msg-type (getf *msg-types* msg-type))
    (setf msg-seq-num (incf *msg-seq-num*))))

(defclass address-block ()
  ((num-addr :initarg :num-addr :accessor num-addr
	     :type '(unsigned-byte 8))
   (addr-flags :initarg :addr-flags :accessor addr-flags
	       :type '(unsigned-byte 8))
   (head-length :initarg :head-length :accessor head-length
		:type '(unsigned-byte 8))
   (head :initarg :head :accessor head)
   (mid :initarg :mid :accessor mid))
  (:default-initargs
   :num-addr nil
   :addr-flags #b10000000
   :head-length 3)) ; default is Class C IPv4 Address, netmask 255.255.255.0

(defmethod initialize-instance :after ((addr-block address-block) &key addr-list)
  (with-slots (num-addr head-length head mid) addr-block
    (when addr-list
      (setf num-addr (length addr-list))
      (setf (values head mid) (address-block-head-mid addr-list head-length)))))

(defun address-block-head-mid (addr-list head-length)
  "Loop throuh `addr-list' and return head-length leftmost octets common to all the addresses and
and remaining non-common octets of all addresses."
  (loop for addr in addr-list 
	for octets = (usocket:integer-to-octet-buffer addr (make-array 4 :element-type '(unsigned-byte 8)) 4)
	collect (subseq octets head-length) into mid
	finally (return (values (subseq octets 0 head-length) mid))))

(defclass tlv-block ()
  ((tlvs-length :initarg :tlvs-length :accessor tlvs-length
		:type '(unsigned-byte 16))
   (tlvs :initarg :tlvs :accessor tlvs))
  (:default-initargs
   :tlvs-length 0
   :tlvs nil))

(defmethod path-destination ((tlv-block tlv-block))
  "Return last element of `tlv-block'."
  (with-slots (tlv) tlv-block
    (value (first (last tlv)))))

(defmethod next-hop ((tlv-block tlv-block))
  "Return next hop on `tlv-block'."
  (with-slots (tlv) tlv-block
    (value (first tlv))))

(defclass tlv ()
  ((tlv-type :initarg :tlv-type
	     :accessor tlv-type
	     :type '(unsigned-byte 8))
   (tlv-flags :initarg :tlv-flags
	      :accessor tlv-flags
	      :type '(unsigned-byte 8))
   (length :initarg :length
	   :accessor vlength
	   :type '(unsigned-byte 8))
   (value :initarg :value
	  :accessor value)))

(defmethod print-object ((object tlv) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (tlv-type tlv-flags length value) object
      (format stream "~A ~A ~A ~A" tlv-type tlv-flags length value))))
