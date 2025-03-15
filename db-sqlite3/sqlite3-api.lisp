;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     sqlite3-api.lisp
;;;; Purpose:  Low-level SQLite3 interface using UFFI
;;;; Authors:  Aurelio Bignoli
;;;; Created:  Oct 2004
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Aurelio Bignoli
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:sqlite3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Return values for sqlite_exec() and sqlite_step()
;;;;
(defconstant SQLITE-OK           0   "Successful result")
(defconstant SQLITE-ERROR        1   "SQL error or missing database")
(defconstant SQLITE-INTERNAL     2   "An internal logic error in SQLite")
(defconstant SQLITE-PERM         3   "Access permission denied")
(defconstant SQLITE-ABORT        4   "Callback routine requested an abort")
(defconstant SQLITE-BUSY         5   "The database file is locked")
(defconstant SQLITE-LOCKED       6   "A table in the database is locked")
(defconstant SQLITE-NOMEM        7   "A malloc() failed")
(defconstant SQLITE-READONLY     8   "Attempt to write a readonly database")
(defconstant SQLITE-INTERRUPT    9   "Operation terminated by sqlite3_interrupt()")
(defconstant SQLITE-IOERR       10   "Some kind of disk I/O error occurred")
(defconstant SQLITE-CORRUPT     11   "The database disk image is malformed")
(defconstant SQLITE-NOTFOUND    12   "(Internal Only) Table or record not found")
(defconstant SQLITE-FULL        13   "Insertion failed because database is full")
(defconstant SQLITE-CANTOPEN    14   "Unable to open the database file")
(defconstant SQLITE-PROTOCOL    15   "Database lock protocol error")
(defconstant SQLITE-EMPTY       16   "Database is empty")
(defconstant SQLITE-SCHEMA      17   "The database schema changed")
(defconstant SQLITE-TOOBIG      18   "Too much data for one row of a table")
(defconstant SQLITE-CONSTRAINT  19   "Abort due to contraint violation")
(defconstant SQLITE-MISMATCH    20   "Data type mismatch")
(defconstant SQLITE-MISUSE      21   "Library used incorrectly")
(defconstant SQLITE-NOLFS       22   "Uses OS features not supported on host")
(defconstant SQLITE-AUTH        23   "Authorization denied")
(defconstant SQLITE-FORMAT      24   "Auxiliary database format error")
(defconstant SQLITE-RANGE       25   "2nd parameter to sqlite3_bind out of range")
(defconstant SQLITE-NOTADB      26   "File opened that is not a database file")
(declaim (type fixnum sqlite-row))
(defconstant SQLITE-ROW         100  "sqlite3_step() has another row ready")
(defconstant SQLITE-DONE        101  "sqlite3_step() has finished executing")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Column types.
;;;;
(defconstant SQLITE-INTEGER  1)
(defconstant SQLITE-FLOAT    2)
(defconstant SQLITE-TEXT     3)
(defconstant SQLITE-BLOB     4)
(defconstant SQLITE-NULL     5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Foreign types definitions.
;;;;
(cffi:defctype sqlite3-db :pointer)
(cffi:defctype sqlite3-stmt (:pointer :void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Lisp types definitions.
;;;;
(cffi:defctype sqlite3-db-type sqlite3-db)
(cffi:defctype sqlite3-db-ptr-type (:pointer sqlite3-db))
(cffi:defctype sqlite3-stmt-type sqlite3-stmt)
(cffi:defctype sqlite3-stmt-ptr-type (:pointer sqlite3-stmt))
(cffi:defctype unsigned-char-ptr-type (:pointer :unsigned-char))

(defparameter null-stmt (cffi:null-pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hash tables for db and statement pointers.
;;;
(defvar *db-pointers* (make-hash-table))
(defvar *stmt-pointers* (make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Conditions.
;;;;
(define-condition sqlite3-error ()
  ((message :initarg :message :reader sqlite3-error-message :initform "")
   (code :initarg :code :reader sqlite3-error-code))
  (:report (lambda (condition stream)
             (format stream "Sqlite3 error [~A]: ~A"
                     (sqlite3-error-code condition)
                     (sqlite3-error-message condition)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Library functions.
;;;;

(cffi:define-foreign-type sqlite-code ()
  ()
  (:actual-type :int)
  (:simple-parser sqlite-code))

(defmethod cffi:translate-from-foreign (value (type sqlite-code))
  (cond ((or (= value SQLITE-OK)
             (= value SQLITE-ROW))
         t)
        ((= value SQLITE-DONE)
         nil)
        (t
         (error 'sqlite3-error
               :message (sqlite3-errstr value)
               :code value))))

(cffi:defcfun "sqlite3_extended_result_codes" :int
  (db sqlite3-db)
  (onoff :bool))

(cffi:defcfun "sqlite3_errcode" :int
  (db sqlite3-db))

(cffi:defcfun "sqlite3_errstr" :string
  (code :int))

(cffi:defcfun "sqlite3_errmsg" :string
  (db sqlite3-db))

(cffi:defcfun "sqlite3_open" sqlite-code
  (dbname :string)
  (db (:pointer sqlite3-db)))

(cffi:defcfun "sqlite3_close_v2" sqlite-code
  (db sqlite3-db))

(cffi:defcfun "sqlite3_prepare_v2" sqlite-code
  (db sqlite3-db)
  (sql :string)
  (len :int)
  (stmt (:pointer sqlite3-stmt))
  (sql-tail (:pointer (:pointer :unsigned-char))))

(cffi:defcfun "sqlite3_step" sqlite-code
  (stmt sqlite3-stmt))

(cffi:defcfun "sqlite3_finalize" sqlite-code
  (stmt sqlite3-stmt))

(cffi:defcfun "sqlite3_column_count" :int
  (stmt sqlite3-stmt))

(cffi:defcfun "sqlite3_column_name" :string
  (stmt sqlite3-stmt)
  (n-col :int))

(cffi:defcfun "sqlite3_column_type" :int
  (stmt sqlite3-stmt)
  (n-col :int))

(cffi:defcfun "sqlite3_column_text" (:pointer :unsigned-char)
  (stmt sqlite3-stmt)
  (n-col :int))

(cffi:defcfun "sqlite3_column_bytes" :int
  (stmt sqlite3-stmt)
  (n-col :int))

(cffi:defcfun "sqlite3_column_blob" (:pointer :void)
  (stmt sqlite3-stmt)
  (n-col :int))

(cffi:defcfun "sqlite3_column_int" :int32
  (stmt sqlite3-stmt)
  (n-col :int))

(cffi:defcfun "sqlite3_column_int64" :int64
  (stmt sqlite3-stmt)
  (n-col :int))

(cffi:defcfun "sqlite3_column_double" :double
  (stmt sqlite3-stmt)
  (n-col :int))
