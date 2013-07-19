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

(defun unserialize-msg-header ()
  (userial:unserialize :msg-header))

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
	collect (userial:unserialize :mid-item)))

(userial:define-unserializer (:address-block)
  (userial:unserialize-let* (:uint8 num-addr
			     :uint8 addr-flags
			     :uint8 head-length
			     :head-vector head)
    (make-instance 'address-block :num-addr num-addr :addr-flags addr-flags :head-length head-length
		   :head head :mid (userial:unserialize :mid :num-addr num-addr))))

(defun serialize-address-block (address-block)
  (userial:unserialize :address-block address-block))

(defun unserialize-address-block ()
  (userial:unserialize :address-block))

;;; tlv-block
(userial:make-accessor-serializer (:tlv-block tlv-block-instance (make-instance 'tlv-block))
				  :uint16 tlvs-length
				  :tlvs tlvs)

(userial:define-serializer (:tlvs tlvs)
  (dolist (entry tlvs)
    (serialize-tlv entry)))

(userial:define-unserializer (:tlvs)
  (userial:unserialize :tlv))

(defun serialize-tlv-block (tlv-block)
  (userial:serialize :tlv-block tlv-block))

(defun unserialize-tlv-block ()
  (userial:unserialize :tlv-block))

;;; tlv
(userial:make-accessor-serializer (:tlv tlv-instance (make-instance 'tlv))
				  :uint8 tlv-type
				  :uint8 tlv-flags
				  :uint8 vlength
				  :value value)

(userial:define-serializer (:value value)
  (userial:serialize :raw-bytes
		     (make-array (length value)
				 :element-type '(unsigned-byte 8)
				 :initial-contents value)))

(userial:define-unserializer (:value :extra (vlength))
  (loop repeat vlength
	collect (userial:unserialize :uint8)))

(defun serialize-tlv (tlv)
  (userial:serialize :tlv tlv))

(userial:define-unserializer (:tlv)
  (userial:unserialize-let* (:uint8 tlv-type
			     :uint8 tlv-flags
			     :uint8 vlength)
    (make-instance 'tlv :tlv-type tlv-type :tlv-flags tlv-flags
		   :length vlength :value (userial:unserialize :value :vlength vlength))))

(userial:make-accessor-serializer (:addr+tlv addr+tlv-instance (make-addr+tlv))
				  :address-block addr+tlv-address-block
				  :tlv-block addr+tlv-tlv-block)

;;; addr-block + tlv-block
(defun serialize-addr+tlv (addr+tlv)
  "Serialize each `addr+tlv' struct in the given `message' addr+tlv."
  (userial:serialize :addr+tlv addr+tlv))

(defun unserialize-addr+tlv ()
  (userial:unserialize :addr+tlv))

;;; message
(userial:define-serializer (:message message)
  (with-slots (msg-header tlv-block addr+tlv) message
    (userial:serialize :msg-header msg-header)
    (when tlv-block
      (userial:serialize :tlv-block tlv-block))
    (when addr+tlv
      (userial:serialize :addr+tlv addr+tlv)))
  (userial:get-buffer))

(userial:define-unserializer (:message)
  (userial:unserialize-let* (:msg-header msg-header
			      :addr+tlv addr+tlv)
    (make-instance 'message :msg-header msg-header :addr+tlv addr+tlv)))

;;; packet
(userial:make-accessor-serializer (:packet pkt-instance (make-instance 'packet))
				  :pkt-header pkt-header
				  :message message)

(defun serialize-packet (packet)
  (let ((buffer (userial:make-buffer)))
    (userial:with-buffer buffer
      (userial:serialize :packet packet))
    buffer))

(defun unserialize-packet (buffer)
  (userial:with-buffer buffer
    (userial:buffer-rewind)
    (userial:unserialize :packet)))
