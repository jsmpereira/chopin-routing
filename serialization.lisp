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
				  :uint8 head-length)

(userial:make-vector-serializer :head-vector :uint8 3) ; Address head is 192.168.0, length 3

(userial:make-vector-serializer :mid :uint8 1) ; Address mid is x, in 192.168.0.x

(defun serialize-address-block (address-block)
  (with-slots (head mid) address-block
    (userial:serialize :address-block address-block)
    (userial:serialize :head-vector head)
    (dolist (mid-item mid)
      (userial:serialize :mid mid-item))))

(defun unserialize-address-block (address-block)
  (let ((addr-block (userial:unserialize :address-block :address-block-instance address-block)))
    (with-slots (num-addr head mid) addr-block
      (setf head (userial:unserialize :head-vector))
      (setf mid (loop repeat num-addr
		      collect (userial:unserialize :mid))))
    addr-block))

;;; tlv-block
(userial:make-accessor-serializer (:tlv-block tlv-block-instance (make-instance 'tlv-block))
				  :uint16 tlvs-length)

(defun serialize-tlv-block (tlv-block)
  (with-accessors ((tlv tlv)) tlv-block
    (userial:serialize :tlv-block tlv-block)
    (dolist (entry tlv)
      (serialize-tlv entry)))
  (userial:get-buffer))

(defun unserialize-tlv-block (tlv-block)
  "A bit of a hack. Since we're using 32bit addresses, the tlv field is always 7 octets."
  (let* ((tlvb (userial:unserialize :tlv-block :tlv-block-instance tlv-block)))
    (setf (tlv tlvb)
	  (loop repeat (/ (tlvs-length tlvb) 7)
		collect (unserialize-tlv (make-instance 'tlv))))
    tlvb))

;;; tlv
(userial:make-accessor-serializer (:tlv tlv-instance (make-instance 'tlv))
				  :uint8 tlv-type
				  :uint8 tlv-flags
				  :uint8 vlength)

(defun serialize-tlv (tlv)
  (with-slots (value) tlv
    (userial:serialize :tlv tlv)
    (when (listp value)
      (userial:serialize :raw-bytes
			 (make-array (length value)
				     :element-type '(unsigned-byte 8)
				     :initial-contents value)))))

(defun unserialize-tlv (tlv)
  (userial:unserialize :tlv :tlv-instance tlv))


;;; addr-block + tlv-block
(defun serialize-addr+tlv (addr+tlv)
  "Serialize each `addr+tlv' struct in the given `message' addr+tlv."
  (serialize-address-block (addr+tlv-address-block addr+tlv))
  (serialize-tlv-block (addr+tlv-tlv-block addr+tlv)))

;;; message
(defun serialize-message (message)
  (with-slots (msg-header tlv-block addr+tlv) message
    (serialize-msg-header msg-header)
    (when tlv-block
      (serialize-tlv-block tlv-block))
    (when addr+tlv
      (serialize-addr+tlv addr+tlv))))

;;; packet
(defun serialize-packet (packet)
  "Packet, Message and Tlv-Block are encapsulation. What we want is the bytes from
pkt-header, msg-header and tlv-block."
  (let ((buffer (userial:make-buffer)))
    (userial:with-buffer buffer
      (let ((pkt-header (pkt-header packet))
	    (msg-header (msg-header (message packet)))
	    (tlv-block (tlv-block (message packet))))
	(serialize-pkt-header pkt-header)
	(serialize-msg-header msg-header)
	(serialize-tlv-block tlv-block)
	buffer))))

(defun unserialize-packet (buffer)
  "pkt-header, msg-header, tlvs-length, tlvs"
  (userial:with-buffer buffer
    (userial:buffer-rewind)
    (let* ((pkt-header (unserialize-pkt-header (make-instance 'pkt-header)))
	   (msg-header (unserialize-msg-header (make-instance 'msg-header)))
	   (tlv-block (unserialize-tlv-block (make-instance 'tlv-block))))
      (values pkt-header msg-header tlv-block))))
