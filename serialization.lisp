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

(userial:make-accessor-serializer (:tlv-block tlv-block-instance (make-instance 'tlv-block))
				  :uint16 tlvs-length)

(defun serialize-tlv-block (tlv-block)
  (with-accessors ((tlv tlv)) tlv-block
    (userial:serialize :tlv-block tlv-block)
    (dolist (entry tlv)
      (serialize-tlv entry))))

(defun unserialize-tlv-block (tlv-block)
  (userial:unserialize :tlv-block :tlv-block-instance tlv-block))

(userial:make-accessor-serializer (:tlv tlv-instance (make-instance 'tlv))
				  :uint8 tlv-type
				  :uint8 tlv-flags
				  :uint8 vlength
				  :uint32 value)

(defun serialize-tlv (tlv)
  (userial:serialize :tlv tlv))

(defun unserialize-tlv (tlv)
  (userial:unserialize :tlv :tlv-instance tlv))

(defun serialize-packet (packet)
  "Packet, Message and Tlv-Block are encapsulation. What we want is the bytes from
pkt-header, msg-header and tlv."
  (let ((buffer (userial:make-buffer)))
    (userial:with-buffer buffer
      (let ((pkt-header (pkt-header packet))
	    (msg-header (msg-header (message packet)))
	    (tlv-block (tlv-block (message packet))))
	(serialize-pkt-header pkt-header)
	(serialize-msg-header msg-header)
	(serialize-tlv-block tlv-block)
	buffer))))
