;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sqlite-package.lisp
;;;; Purpose:       Package definition for low-level SQLite3 interface
;;;; Programmer:    Aurelio Bignoli
;;;; Date Started:  Oct 2004
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Aurelio Bignoli
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defpackage #:sqlite3
  (:use #:common-lisp)
  (:export
   ;; Conditions
   #:sqlite3-error
   #:sqlite3-error-code
   #:sqlite3-error-message

   ;; API functions.
   #:sqlite3-open
   #:sqlite3-close-v2
   #:sqlite3-errmsg
   #:sqlite3-errstr
   #:sqlite3-extended-result-codes

   #:sqlite3-prepare-v2
   #:sqlite3-step
   #:sqlite3-finalize

   #:sqlite3-column-count
   #:sqlite3-column-name
   #:sqlite3-column-type
   #:sqlite3-column-text
   #:sqlite3-column-bytes
   #:sqlite3-column-blob
   #:sqlite3-column-int
   #:sqlite3-column-int64
   #:sqlite3-column-double

   ;; Types.
   #:sqlite3-db
   #:sqlite3-db-type
   #:sqlite3-stmt
   #:sqlite3-stmt-type
   #:unsigned-char-ptr-type
   #:null-stmt

   ;; Return codes
   #:SQLITE-ROW
   #:SQLITE-DONE

   ;; Columnt types.
   #:SQLITE-INTEGER
   #:SQLITE-FLOAT
   #:SQLITE-TEXT
   #:SQLITE-BLOB
   #:SQLITE-NULL))