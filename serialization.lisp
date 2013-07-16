;;; -*- Mode: Lisp -*-
(in-package :chopin-routing)

;; As per RFC 5444 there are some 4 bit fields. Since they occur in pairs, we encode
;; them in a unsigned-byte 8.

(defun merge-4bit-fields (a b)
  (logior (dpb a (byte 4 4) 0)
          (dpb b (byte 4 0) 0)))

(defun extract-4bit-fields (v)
  (values (ldb (byte 4 4) v)
          (ldb (byte 4 0) v)))

(defun version+pkt-flags (pkt-header)
  (merge-4bit-fields (version pkt-header) (pkt-flags pkt-header)))

(defun (setf version+pkt-flags) (value pkt-header)
  (multiple-value-bind (version flags) (extract-4bit-fields value)
    (setf (version pkt-header) version
	  (pkt-flags pkt-header) flags)))

(userial:make-accessor-serializer (:pkt-header ph-instance (make-instance 'pkt-header))
				  :uint8 version+pkt-flags
				  :uint16 pkt-seq-num)

(defun serialize-pkt-header (pkt-header)
  (userial:serialize :pkt-header pkt-header))

(defun unserialize-pkt-header (pkt-header)
  (userial:unserialize :pkt-header :ph-instance pkt-header))

;;; msg-header
(defun msg-flags+msg-addr-length (msg-header)
  (merge-4bit-fields (msg-flags msg-header) (msg-addr-length msg-header)))

(defun (setf msg-flags+msg-addr-length) (value msg-header)
  (multiple-value-bind (flags length) (extract-4bit-fields value)                
    (setf (msg-flags msg-header) flags
          (msg-addr-length msg-header) length)))

(userial:make-accessor-serializer (:msg-header mh-instance (make-instance 'msg-header))
				  :uint8 msg-type
				  :uint8 msg-flags+msg-addr-length
				  :uint16 msg-size
				  :uint32 msg-orig-addr
				  :uint8 msg-hop-limit
				  :uint8 msg-hop-count
				  :uint16 msg-seq-num)

(defun serialize-msg-header (msg-header)
  (userial:serialize :msg-header msg-header))

(defun unserialize-msg-header (msg-header)
  (userial:unserialize :msg-header :mh-instance msg-header))

;;; addr-block
(userial:make-accessor-serializer (:address-block address-block-instance (make-instance 'address-block))
				  :uint8 num-addr
				  :uint8 addr-flags
				  :uint8 head-length
				  :head-vector head
				  :mid mid)

(userial:make-vector-serializer :head-vector :uint8 3) ; Address head is 192.168.0, length 3

(userial:make-vector-serializer :mid-item :uint8 1) ; Address mid is x, in 192.168.0.x

(userial:define-serializer (:mid mid)
  (dolist (mid-item mid)
    (userial:serialize :mid-item mid-item)))

(userial:define-unserializer (:mid :extra (num-addr))
  (loop repeat num-addr
	collect (userial:unserialize :mit-item)))

(defun serialize-address-block (address-block)
  (userial:unserialize :address-block address-block))

(defun unserialize-address-block (address-block)
  (userial:unserialize :address-block :address-block-instance address-block))

;;; tlv-block
(userial:make-accessor-serializer (:tlv-block tlv-block-instance (make-instance 'tlv-block))
				  :uint16 tlvs-length
				  :tlvs tlvs)

(userial:define-serializer (:tlvs tlvs)
  (dolist (entry tlvs)
    (serialize-tlv entry)))

(userial:define-unserializer (:tlvs :extra (num-tlvs))
  (loop repeat num-tlvs
	collect (unserialize-tlv (make-instance 'tlv))))

(defun serialize-tlv-block (tlv-block)
  (userial:serialize :tlv-block tlv-block))

(defun unserialize-tlv-block (tlv-block)
  (userial:unserialize :tlv-block :tlv-block-instance tlv-block))

;;; tlv
(userial:make-accessor-serializer (:tlv tlv-instance (make-instance 'tlv))
				  :uint8 tlv-type
				  :uint8 tlv-flags
				  :uint8 vlength
				  :value value)

(userial:define-serializer (:value value)
  (when (listp value)
    (userial:serialize :raw-bytes
		       (make-array (length value)
				   :element-type '(unsigned-byte 8)
				   :initial-contents value))))

(defun serialize-tlv (tlv)
  (userial:serialize :tlv tlv))

(defun unserialize-tlv (tlv)
  (userial:unserialize :tlv :tlv-instance tlv))

(userial:make-accessor-serializer (:addr+tlv addr+tlv-instance (make-addr+tlv))
				  :address-block addr+tlv-address-block
				  :tlv-block addr+tlv-tlv-block)

;;; addr-block + tlv-block
(defun serialize-addr+tlv (addr+tlv)
  "Serialize each `addr+tlv' struct in the given `message' addr+tlv."
  (userial:serialize :addr+tlv addr+tlv))

(defun unserialize-addr+tlv (addr+tlv)
  (userial:unserialize :addr+tlv :addr+tlv-instance addr+tlv))

;;; message
(userial:make-accessor-serializer (:message msg-instance (make-instance 'message))
				  :msg-header msg-header
				  :tlv-block tlv-block
				  :addr+tlv addr+tlv)

(defun serialize-message (message)
  (with-slots (msg-header tlv-block addr+tlv) message
    (userial:serialize :msg-header msg-header)
    (when tlv-block
      (userial:serialize :tlv-block tlv-block))
    (when addr+tlv
      (userial:serialize :addr+tlv addr+tlv)))
  (userial:get-buffer))

(defun unserialize-message (message)
  (userial:unserialize :message :msg-instance message))

;;; packet
(userial:make-accessor-serializer (:packet pkt-instance (make-instance 'packet))
				  :pkt-header pkt-header
				  :message message)

(defun serialize-packet (packet)
  (userial:serialize :packet packet))

(defun unserialize-packet (packet)
  (userial:unserialize :packet :pkt-instance packet))
