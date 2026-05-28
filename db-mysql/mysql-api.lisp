;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-api.lisp
;;;; Purpose:       Low-level MySQL interface using UFFI
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                Original code by Pierre R. Mai
;;;; Date Started:  Feb 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2009 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:mysql)

;;;; Modifications from original code
;;;;  - Updated C-structures to conform to structures in MySQL 3.23.46
;;;;  - Changed from CMUCL interface to UFFI
;;;;  - Added and call a C-helper file to support 64-bit integers
;;;;    that are used in a few routines.
;;;;  - Removed all references to interiors of C-structions, this will
;;;;    increase robustness when MySQL's internal structures change.

;;;; Type definitions

;;; Basic Types
(cffi:defctype mysql-socket :int)
(cffi:defctype mysql-bool :char)
(cffi:defctype mysql-byte :unsigned-char)

(cffi:defcenum mysql-net-type
  :tcp-ip
  :socket
  :named-pipe)

(cffi:defctype mysql-row (:pointer (:pointer :unsigned-char)))

;;; MYSQL-FIELD
(cffi:defcenum mysql-field-types
  :decimal
  :tiny
  :short
  :long
  :float
  :double
  :null
  :timestamp
  :longlong
  :int24
  :date
  :time
  :datetime
  :year
  :newdate
  (:enum 247)
  (:set 248)
  (:tiny-blob 249)
  (:medium-blob 250)
  (:long-blob 251)
  (:blob 252)
  (:var-string 253)
  (:string 254)
  (:geometry 255))

(cffi:defcenum mysql-option
  :connect-timeout
  :compress
  :named-pipe
  :init-command
  :read-default-file
  :read-default-group
  :set-charset-dir
  :set-charset-name
  :local-infile
  :protocol
  :shared-memory-base-name
  :read-timeout
  :write-timeout
  :use-result
  :use-remote-connection
  :use-embedded-connection
  :guess-connection
  :set-client-ip
  :secure-auth
  :report-data-truncation
  :reconnect
  :ssl-verify-server-cert)

(defvar +mysql-option-parameter-map+
  '((:connect-timeout . :uint-ptr)
    (:compress . :none)
    (:named-pipe . :none)
    (:init-command . :char-ptr)
    (:read-default-file . :char-ptr)
    (:read-default-group . :char-ptr)
    (:set-charset-dir . :char-ptr)
    (:set-charset-name . :char-ptr)
    (:local-infile . :uint-ptr)
    (:protocol . :uint-ptr)
    (:shared-memory-base-name . :char-ptr)
    (:read-timeout . :uint-ptr)
    (:write-timeout . :uint-ptr)
    (:use-result . :none)
    (:use-remote-connection . :none)
    (:use-embedded-connection . :none)
    (:guess-connection . :none)
    (:set-client-ip . :char-ptr)
    (:secure-auth . :boolean-ptr)
    (:report-data-truncation . :boolean-ptr)
    (:reconnect . :boolean-ptr)
    (:ssl-verify-server-cert . :boolean-ptr)))

(cffi:defcenum mysql-status
  :ready
  :get-result
  :use-result)

;;; Opaque pointers to mysql C-defined structures
(cffi:defctype mysql-mysql :pointer)
(cffi:defctype mysql-mysql-res :pointer)
(cffi:defctype mysql-field :pointer)
(cffi:defctype mysql-bind :pointer)
(cffi:defctype mysql-time :pointer)

;;;; The Foreign C routines
(declaim (inline mysql-init))
(cffi:defcfun "mysql_init" mysql-mysql
  (mysql mysql-mysql))

;; Need to comment this out for LW 4.2.6
;; ? bug in LW version
#-lispworks (declaim (inline mysql-real-connect))
(cffi:defcfun "mysql_real_connect" mysql-mysql
  (mysql mysql-mysql)
  (host :string)
  (user :string)
  (passwd :string)
  (db :string)
  (port :unsigned-int)
  (unix-socket :string)
  (clientflag :unsigned-long))

(declaim (inline mysql-close))
(cffi:defcfun "mysql_close" :void
  (sock mysql-mysql))

(declaim (inline mysql-select-db))
(cffi:defcfun "mysql_select_db" :int
  (mysql mysql-mysql)
  (db :string))

(declaim (inline mysql-query))
(cffi:defcfun "mysql_query" :int
  (mysql mysql-mysql)
  (query :string))

 ;;; I doubt that this function is really useful for direct Lisp usage,
;;; but it is here for completeness...

(declaim (inline mysql-real-query))
(cffi:defcfun "mysql_real_query" :int
  (mysql mysql-mysql)
  (query :string)
  (length :unsigned-int))

(declaim (inline mysql-shutdown))
(cffi:defcfun "mysql_shutdown" :int
  (mysql mysql-mysql))

(declaim (inline mysql-dump-debug-info))
(cffi:defcfun "mysql_dump_debug_info" :int
  (mysql mysql-mysql))

(declaim (inline mysql-refresh))
(cffi:defcfun "mysql_refresh" :int
  (mysql mysql-mysql)
  (refresh-options :unsigned-int))

(declaim (inline mysql-kill))
(cffi:defcfun "mysql_kill" :int
  (mysql mysql-mysql)
  (pid :unsigned-long))

(declaim (inline mysql-ping))
(cffi:defcfun "mysql_ping" :int
  (mysql mysql-mysql))

(declaim (inline mysql-stat))
(uffi:def-function "mysql_stat"
  ((mysql mysql-mysql))
  :module "mysql"
  :returning :cstring)

(declaim (inline mysql-get-server-info))
(cffi:defcfun "mysql_get_server_info" :string
  (mysql mysql-mysql))

(declaim (inline mysql-get-host-info))
(cffi:defcfun "mysql_get_host_info" :string
  (mysql mysql-mysql))

(declaim (inline mysql-get-proto-info))
(cffi:defcfun "mysql_get_proto_info" :unsigned-int
  (mysql mysql-mysql))

(declaim (inline mysql-list-dbs))
(cffi:defcfun "mysql_list_dbs" mysql-mysql-res
  (mysql mysql-mysql)
  (wild :string))

(declaim (inline mysql-list-tables))
(cffi:defcfun "mysql_list_tables" mysql-mysql-res
  (mysql mysql-mysql)
  (wild :string))

(declaim (inline mysql-list-fields))
(cffi:defcfun "mysql_list_fields" mysql-mysql-res
  (mysql mysql-mysql)
  (table :string)
  (wild :string))

(declaim (inline mysql-list-processes))
(cffi:defcfun "mysql_list_processes" mysql-mysql-res
  (mysql mysql-mysql))

(declaim (inline mysql-store-result))
(cffi:defcfun "mysql_store_result" mysql-mysql-res
  (mysql mysql-mysql))

(declaim (inline mysql-use-result))
(cffi:defcfun "mysql_use_result" mysql-mysql-res
  (mysql mysql-mysql))

(declaim (inline mysql-options))
(cffi:defcfun "mysql_options" :int
  (mysql mysql-mysql)
  (option mysql-option)
  (arg :pointer))

(declaim (inline mysql-free-result))
(cffi:defcfun "mysql_free_result" :void
  (res mysql-mysql-res))

(declaim (inline mysql-next-result))
(cffi:defcfun "mysql_next_result" :int
  (mysql mysql-mysql))

(declaim (inline mysql-fetch-row))
(cffi:defcfun "mysql_fetch_row" (:pointer (:pointer :char))
  (res mysql-mysql-res))

(declaim (inline mysql-fetch-lengths))
(cffi:defcfun "mysql_fetch_lengths" (:pointer :unsigned-long)
  (res mysql-mysql-res))

(declaim (inline mysql-fetch-field))
(cffi:defcfun "mysql_fetch_field" mysql-field
  (res mysql-mysql-res))

(declaim (inline mysql-field-seek))
(cffi:defcfun "mysql_field_seek" :unsigned-int
  (res mysql-mysql-res)
  (offset :unsigned-int))

(declaim (inline mysql-fetch-fields))
(cffi:defcfun "mysql_fetch_fields" mysql-field
  (res mysql-mysql-res))

(declaim (inline mysql-fetch-field-direct))
(cffi:defcfun "mysql_fetch_field_direct" mysql-field
  (res mysql-mysql-res)
  (field-num :unsigned-int))

(declaim (inline mysql-escape-string))
(cffi:defcfun "mysql_escape_string" :unsigned-int
  (to (:pointer :unsigned-char))
  (from (:pointer :unsigned-char))
  (length :unsigned-int))

(declaim (inline mysql-debug))
(cffi:defcfun "mysql_debug" :void
  (debug :string))

(declaim (inline clsql-mysql-num-rows))
(cffi:defcfun "clsql_mysql_num_rows" :unsigned-int
  (res mysql-mysql-res)
  (p-high32 (:pointer :unsigned-int)))

(cffi:defctype mysql-stmt-ptr :pointer)

(cffi:defcfun "mysql_stmt_init" mysql-stmt-ptr
  (res mysql-mysql-res))

(cffi:defcfun "mysql_stmt_prepare" :int
  (stmt mysql-stmt-ptr)
  (query :string)
  (length :unsigned-long))

(cffi:defcfun "mysql_stmt_param_count" :unsigned-int
  (stmt mysql-stmt-ptr))

(cffi:defcfun "mysql_stmt_bind_param" :short
  (stmt mysql-stmt-ptr)
  (bind mysql-bind))

(cffi:defcfun "mysql_stmt_bind_result" :short
  (stmt mysql-stmt-ptr)
  (bind mysql-bind))

(cffi:defcfun "mysql_stmt_result_metadata" mysql-mysql-res
  (stmt mysql-stmt-ptr))


(cffi:defcfun "mysql_stmt_execute" :int
  (stmt mysql-stmt-ptr))

(cffi:defcfun "mysql_stmt_store_result" :int
  (stmt mysql-stmt-ptr))

(cffi:defcfun "mysql_stmt_fetch" :int
  (stmt mysql-stmt-ptr))

(cffi:defcfun "mysql_stmt_free_result" :short
  (stmt mysql-stmt-ptr))

(cffi:defcfun "mysql_stmt_close" :short
  (stmt mysql-stmt-ptr))

(cffi:defcfun "mysql_stmt_errno" :unsigned-int
  (stmt mysql-stmt-ptr))

(cffi:defcfun "mysql_stmt_error" :pointer
  (stmt mysql-stmt-ptr))


;;;; Equivalents of C Macro definitions for accessing various fields
;;;; in the internal MySQL Datastructures


(declaim (inline mysql-num-rows))
(defun mysql-num-rows (res)
  (cffi:with-foreign-object (p-high32 :unsigned-int)
    (let ((low32 (clsql-mysql-num-rows res p-high32))
          (high32 (cffi:mem-ref p-high32 :unsigned-int)))
      (if (zerop high32)
          low32
          (make-64-bit-integer high32 low32)))))

(cffi:defcfun "clsql_mysql_affected_rows" :unsigned-int
  (mysql mysql-mysql)
  (p-high32 (:pointer :unsigned-int)))

(defun mysql-affected-rows (mysql)
  (cffi:with-foreign-object (p-high32 :unsigned-int)
    (let ((low32 (clsql-mysql-affected-rows mysql p-high32))
          (high32 (cffi:mem-ref p-high32 :unsigned-int)))
      (if (zerop high32)
          low32
          (make-64-bit-integer high32 low32)))))

(cffi:defcfun "clsql_mysql_insert_id" :unsigned-int
  (res mysql-mysql)
  (p-high32 (:pointer :unsigned-int)))

(defun mysql-insert-id (mysql)
  (cffi:with-foreign-object (p-high32 :unsigned-int)
    (let ((low32 (clsql-mysql-insert-id mysql p-high32))
          (high32 (cffi:mem-ref p-high32 :unsigned-int)))
      (if (zerop high32)
          low32
          (make-64-bit-integer high32 low32)))))


(declaim (inline mysql-num-fields))
(cffi:defcfun "mysql_num_fields" :unsigned-int
  (res mysql-mysql-res))

(declaim (inline clsql-mysql-eof))
(cffi:defcfun ("mysql_eof" clsql-mysql-eof) :char
  (res mysql-mysql-res))

(declaim (inline mysql-eof))
(defun mysql-eof (res)
  (if (zerop (clsql-mysql-eof res))
      nil
    t))

(declaim (inline mysql-error))
(cffi:defcfun ("mysql_error" mysql-error) :pointer
  (mysql mysql-mysql))

(declaim (inline mysql-errno))
(cffi:defcfun "mysql_errno" :unsigned-int
  (mysql mysql-mysql))

(declaim (inline mysql-info))
(cffi:defcfun ("mysql_info" mysql-info) :string
  (mysql mysql-mysql))

(declaim (inline clsql-mysql-data-seek))
(uffi:def-function "clsql_mysql_data_seek"
  ((res mysql-mysql-res)
   (offset-high32 :unsigned-int)
   (offset-low32 :unsigned-int))
  :module "clsql-mysql"
  :returning :void)

(declaim (inline clsql-mysql-field-name))
(cffi:defcfun "clsql_mysql_field_name" :string
  (res mysql-field))

(declaim (inline clsql-mysql-field-flags))
(cffi:defcfun "clsql_mysql_field_flags" :unsigned-int
  (res mysql-field))

(declaim (inline clsql-mysql-field-type))
(cffi:defcfun "clsql_mysql_field_type" :unsigned-int
  (res mysql-field))

(defun mysql-data-seek (res offset)
  (multiple-value-bind (high32 low32) (split-64-bit-integer offset)
    (clsql-mysql-data-seek res high32 low32)))
