;;; -*- Mode: Lisp -*-
(in-package :chopin-routing)

(defstruct config host-address broadcast-address port hop-limit refresh-interval dup-hold-time neighb-hold-time timer-repeat-interval)

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
  ((local-addr :initarg :local-addr :accessor local-addr)
   (neighbor-addr :initarg :neighbor-addr :accessor neighbor-addr)
   (l-time :initarg :l-time :accessor l-time)))

(defmethod print-object ((object link-tuple) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (local-addr neighbor-addr l-time) object
      (format stream "~A ~A ~A" local-addr neighbor-addr l-time))))

(defclass packet ()
  ((pkt-header :initarg :pkt-header :reader pkt-header)
   (message :initarg :message :reader message)))

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
  ((msg-header :initarg :msg-header :reader msg-header)
   (tlv-block :initarg :tlv-block :reader tlv-block)))

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
   :msg-flags #b1111
   :msg-addr-length #b0011
   :msg-size 0
   :msg-orig-addr (usocket:host-byte-order (config-host-address *config*))
   :msg-hop-limit (config-hop-limit *config*)
   :msg-hop-count 0
   :msg-seq-num *msg-seq-num*))

(defclass tlv-block ()
  ((tlvs-length :initarg :tlvs-length :reader tlvs-length
		:type '(unsigned-byte 16))
   (tlv :initarg :tlv :reader tlv)))

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
	  :accessor value
	  :type '(unsigned-byte 32)))
  (:default-initargs
   :tlv-flags #b00010000
   :length 4)) ; for 32-bit address
