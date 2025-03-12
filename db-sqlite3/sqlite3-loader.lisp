;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sqlite3-loader.lisp
;;;; Purpose:       Sqlite3 library loader using CFFI
;;;; Programmer:    Aurelio Bignoli
;;;; Date Started:  Oct 2004
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Aurelio Bignoli
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sqlite3)

(cffi:define-foreign-library libsqlite3
  (:darwin (:or "libsqlite3.dylib" "libsqlite3.0.dylib"))
  (:unix (:or "libsqlite3.so.0" "libsqlite3.so"))
  (t (:or (:default "libsqlite3") (:default "sqlite3"))))

(defmethod database-type-library-loaded ((database-type (eql :sqlite3)))
  "T if foreign library was able to be loaded successfully. "
  (cffi:foreign-library-loaded-p 'libsqlite3))

(defmethod database-type-load-foreign ((database-type (eql :sqlite3)))
  (setf cffi:*foreign-library-directories*
        (remove-duplicates (append clsql:*foreign-library-search-paths*
                                   cffi:*foreign-library-directories*)
                           :test #'equal))
  (cffi:load-foreign-library 'libsqlite3))

(clsql-sys:database-type-load-foreign :sqlite3)
