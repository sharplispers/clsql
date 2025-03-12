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

(in-package #:cl-user)

(defpackage #:sqlite3
  (:use #:common-lisp)
  (:export
   ;; Conditions
   #:sqlite3-error
   #:sqlite3-error-code
   #:sqlite3-error-message

   ;; API functions.
   #:sqlite3-open
   #:sqlite3-close

   #:sqlite3-prepare
   #:sqlite3-step
   #:sqlite3-finalize

   #:sqlite3-column-count
   #:sqlite3-column-name
   #:sqlite3-column-type
   #:sqlite3-column-text
   #:sqlite3-column-bytes
   #:sqlite3-column-blob

   ;; Types.
   #:sqlite3-db
   #:sqlite3-db-type
   #:sqlite3-stmt-type
   #:unsigned-char-ptr-type
   #:null-stmt

   ;; Columnt types.
   #:SQLITE-INTEGER
   #:SQLITE-FLOAT
   #:SQLITE-TEXT
   #:SQLITE-BLOB
   #:SQLITE-NULL))

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
(defconstant SQLITE-ROW         100  "sqlite3_step() has another row ready")
(defconstant SQLITE-DONE        101  "sqlite3_step() has finished executing")

(defparameter error-codes
  (list
   (cons SQLITE-OK "not an error")
   (cons SQLITE-ERROR "SQL logic error or missing database")
   (cons SQLITE-INTERNAL "internal SQLite implementation flaw")
   (cons SQLITE-PERM "access permission denied")
   (cons SQLITE-ABORT "callback requested query abort")
   (cons SQLITE-BUSY "database is locked")
   (cons SQLITE-LOCKED "database table is locked")
   (cons SQLITE-NOMEM "out of memory")
   (cons SQLITE-READONLY "attempt to write a readonly database")
   (cons SQLITE-INTERRUPT "interrupted")
   (cons SQLITE-IOERR "disk I/O error")
   (cons SQLITE-CORRUPT "database disk image is malformed")
   (cons SQLITE-NOTFOUND "table or record not found")
   (cons SQLITE-FULL "database is full")
   (cons SQLITE-CANTOPEN "unable to open database file")
   (cons SQLITE-PROTOCOL "database locking protocol failure")
   (cons SQLITE-EMPTY "table contains no data")
   (cons SQLITE-SCHEMA "database schema has changed")
   (cons SQLITE-TOOBIG "too much data for one table row")
   (cons SQLITE-CONSTRAINT "constraint failed")
   (cons SQLITE-MISMATCH "datatype mismatch")
   (cons SQLITE-MISUSE "library routine called out of sequence")
   (cons SQLITE-NOLFS "kernel lacks large file support")
   (cons SQLITE-AUTH "authorization denied")
   (cons SQLITE-FORMAT "auxiliary database format error")
   (cons SQLITE-RANGE "bind index out of range")
   (cons SQLITE-NOTADB "file is encrypted or is not a database"))
  "Association list of error messages.")

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

(defgeneric signal-sqlite3-error (db))
(defmethod signal-sqlite3-error (db)
  (let ((condition
         (make-condition 'sqlite3-error
                         :code (sqlite3-errcode db)
                         :message (sqlite3-errmsg db))))
    (unless (signal condition)
      (invoke-debugger condition))))

(defmethod signal-sqlite3-error ((code number))
  (let ((condition
         (make-condition 'sqlite3-error
                         :code code
                         :message (let ((s (cdr (assoc code error-codes))))
                                    (if s
                                        s
                                        "unknown error")))))
    (unless (signal condition)
      (invoke-debugger condition))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Library functions.
;;;;

(cffi:defcfun "sqlite3_errcode" :int
  (db sqlite3-db))

(cffi:defcfun "sqlite3_errmsg" :string
  (db sqlite3-db))

(cffi:defcfun ("sqlite3_open" %open) :int
  (dbname :string)
  (db (:pointer sqlite3-db)))

(cffi:defcfun ("sqlite3_close" %close) :int
  (db sqlite3-db))

(cffi:defcfun ("sqlite3_prepare" %prepare) :int
  (db sqlite3-db)
  (sql :string)
  (len :int)
  (stmt (:pointer sqlite3-stmt))
  (sql-tail (:pointer (:pointer :unsigned-char))))

(cffi:defcfun ("sqlite3_step" %step) :int
  (stmt sqlite3-stmt))

(cffi:defcfun ("sqlite3_finalize" %finalize) :int
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; wrapper functions.
;;;;
(defun sqlite3-open (db &optional (mode 0)
                     &aux (db-name (etypecase db
                                     (pathname (namestring db))
                                     (string db))))
  (declare (ignore mode) (type string db-name))
  (let ((dbp (cffi:foreign-alloc 'sqlite3-db)))
    (declare (type cffi:foreign-pointer dbp))
    (cffi:with-foreign-string (db-name-native db-name)
      (let ((result (%open db-name-native dbp)))
        (if (/=  result 0)
            (progn
              ;; According to docs, the db must be closed even in case
              ;; of error.
              (%close (cffi:mem-ref dbp 'sqlite3-db))
              (cffi:foreign-free dbp)
              (signal-sqlite3-error result))
            (let ((db (cffi:mem-ref dbp 'sqlite3-db)))
              (declare (type cffi:foreign-pointer db))
              (setf (gethash db *db-pointers*) dbp)
              db))))))

(declaim (ftype (function (cffi:foreign-pointer) t) sqlite3-close))
(defun sqlite3-close (db)
  (declare (type cffi:foreign-pointer db))
  (let ((result (%close db)))
    (if (/= result 0)
        (signal-sqlite3-error result)
        (progn
          (cffi:foreign-free (gethash db *db-pointers*))
          (remhash db *db-pointers*)
          t))))

(declaim (ftype (function (cffi:foreign-pointer string) cffi:foreign-pointer) sqlite3-prepare))
(defun sqlite3-prepare (db sql)
  (declare (type cffi:foreign-pointer db))
  (cffi:with-foreign-string (sql-native sql)
    (let ((stmtp (cffi:foreign-alloc 'sqlite3-stmt)))
      (declare (type cffi:foreign-pointer stmtp))
      (cffi:with-foreign-object (sql-tail '(:pointer :unsigned-char))
        (let ((result (%prepare db sql-native -1 stmtp sql-tail)))
          (if (/= result SQLITE-OK)
              (progn
                (unless (cffi:null-pointer-p stmtp)
                  ;; There is an error, but a statement has been allocated:
                  ;; finalize it (better safe than sorry).
                  (%finalize (cffi:mem-ref stmtp 'sqlite3-stmt)))
                (cffi:foreign-free stmtp)
                (signal-sqlite3-error db))
              (let ((stmt (cffi:mem-ref stmtp 'sqlite3-stmt)))
                (declare (type cffi:foreign-pointer stmt))
                (setf (gethash stmt *stmt-pointers*) stmtp)
                stmt)))))))

(declaim (ftype (function (cffi:foreign-pointer) t) sqlite3-step))
(defun sqlite3-step (stmt)
  (declare (type cffi:foreign-pointer stmt))
  (let ((result (%step stmt)))
    (cond ((= result SQLITE-ROW) t)
          ((= result SQLITE-DONE) nil)
          (t (signal-sqlite3-error result)))))

(declaim (ftype (function (cffi:foreign-pointer) t) sqlite3-finalize))
(defun sqlite3-finalize (stmt)
  (declare (type cffi:foreign-pointer stmt))
  (let ((result (%finalize  stmt)))
    (if (/= result SQLITE-OK)
        (signal-sqlite3-error result)
        (progn
          (cffi:foreign-free (gethash stmt *stmt-pointers*))
          (remhash stmt *stmt-pointers*)
          t))))
