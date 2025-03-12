;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-uffi.cl
;;;; Purpose:       Common functions for interfaces using UFFI
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-cffi)


(defun canonicalize-type-list (types auto-list)
  "Ensure a field type list meets expectations"
  (declare (optimize (speed 3) (safety 0))
           (type list types auto-list))
  (do ((i 0 (1+ i))
       (new-types '())
       (length-types (length types))
       (length-auto-list (length auto-list)))
      ((= i length-auto-list)
       (nreverse new-types))
    (declare (fixnum length-types length-auto-list i))
    (if (>= i length-types)
        (push t new-types) ;; types is shorter than num-fields
        (push
         (case (nth i types)
           (:int
            (case (nth i auto-list)
              (:int32
               :int32)
              (:int64
               :int64)
              (t
               t)))
           (:double
            (case (nth i auto-list)
              (:double
               :double)
              (t
               t)))
           (:int32
            (if (eq :int32 (nth i auto-list))
                :int32
                t))
           (:int64
            (if (eq :int64 (nth i auto-list))
                :int64
              t))
           (:blob
            :blob)
           (:uint
            :uint)
           (t
            t))
         new-types))))

(cffi:defcfun "atoi" :int
  (str (:pointer :unsigned-char)))

(cffi:defcfun ("strtoul" c-strtoul) :unsigned-long
  (str (:pointer :unsigned-char))
  (endptr (:pointer :unsigned-char))
  (radix :int))

#-windows
(cffi:defcfun ("strtoull" c-strtoull) :unsigned-long-long
  (str (:pointer :unsigned-char))
  (endptr (:pointer :unsigned-char))
  (radix :int))

#-windows
(cffi:defcfun ("strtoll" c-strtoll) :long-long
  (str (:pointer :unsigned-char))
  (endptr (:pointer :unsigned-char))
  (radix :int))

#+windows
(cffi:defcfun ("_strtoui64" c-strtoull)  :unsigned-long-long
  (str (:pointer :unsigned-char))
  (endptr (:pointer :unsigned-char))
  (radix :int))

#+windows
(cffi:defcfun ("_strtoi64" c-strtoll) :long-long
  (str (:pointer :unsigned-char))
  (endptr (:pointer :unsigned-char))
  (radix :int))

(cffi:defcfun "atol" :long
  (str (:pointer :unsigned-char)))

(cffi:defcfun "atof" :double
  (str (:pointer :unsigned-char)))

(defconstant +2^32+ 4294967296)
(defconstant +2^64+ 18446744073709551616)
(defconstant +2^32-1+ (1- +2^32+))

(defmacro make-64-bit-integer (high32 low32)
  `(if (zerop (ldb (byte 1 31) ,high32))
       (+ ,low32 (ash ,high32 32))
     (- (+ ,low32 (ash ,high32 32)) +2^64+)))

;; From high to low ints
(defmacro make-128-bit-integer (a b c d)
  `(+ ,d (ash ,c 32) (ash ,b 64) (ash ,a 96)))

(defmacro split-64-bit-integer (int64)
  `(values (ash ,int64 -32) (logand ,int64 +2^32-1+)))

(defun strtoul (char-ptr)
  (declare (optimize (speed 3) (safety 0) (space 0))
           (type cffi:foreign-pointer char-ptr))
  (c-strtoul char-ptr (cffi:null-pointer) 10))

(defun strtoull (char-ptr)
  (declare (optimize (speed 3) (safety 0) (space 0))
           (type cffi:foreign-pointer char-ptr))
  (c-strtoull char-ptr (cffi:null-pointer) 10))

(defun strtoll (char-ptr)
  (declare (optimize (speed 3) (safety 0) (space 0))
           (type cffi:foreign-pointer char-ptr))
  (c-strtoll char-ptr (cffi:null-pointer) 10))

(defun convert-raw-field (char-ptr type &key length encoding)
  (declare (optimize (speed 3) (safety 0) (space 0))
           (type cffi:foreign-pointer char-ptr)
           (type (or null (integer 0 #.array-dimension-limit)) length))
  (unless (cffi:null-pointer-p char-ptr)
    (case type
      (:double (atof char-ptr))
      (:int (atol char-ptr))
      (:int32 (atoi char-ptr))
      (:uint32 (strtoul char-ptr))
      (:uint (strtoul char-ptr))
      (:int64 (strtoll char-ptr))
      (:uint64 (strtoull char-ptr))
      (:blob
       (if length
           (let ((a (make-array length :element-type '(unsigned-byte 8))))
             (dotimes (i length a)
               (setf (aref a i) (cffi:mem-ref char-ptr :unsigned-char i))))
           (error "Can't return blob since length is not specified.")))
      (t
       ;; NB: this used to manually expand the arg list based on if length and encoding
       ;; were provided.  If this is required the macro is aweful and should be rewritten
       ;; to accept nil args (as it appears to)
       (cffi:foreign-string-to-lisp
        char-ptr
        :count length
        :encoding encoding)))))
