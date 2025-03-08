;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          odbc-loader.sql
;;;; Purpose:       ODBC library loader using CFFI
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  April 2004
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:odbc)

(cffi:define-foreign-library libodbc
  (:darwin (:or "libodbc.2.dylib" "libodbc.dylib" "libiodbc.2.dylib" "libiodbc.dylib"))
  (:unix (:or "libodbc.so.2" "libodbc.so" "libiodbc.so.2" "libiodbc.so"))
  (:windows (:or "odbc32.dll" "odbc32.lib" "odbc32.so")))

(defmethod clsql-sys:database-type-library-loaded ((database-type (eql :odbc)))
  (cffi:foreign-library-loaded-p 'libodbc))

(defmethod clsql-sys:database-type-load-foreign ((database-type (eql :odbc)))
  (setf cffi:*foreign-library-directories*
        (remove-duplicates (append clsql:*foreign-library-search-paths*
                                   cffi:*foreign-library-directories*)
                           :test #'equal))
  (cffi:load-foreign-library 'libodbc))

(clsql-sys:database-type-load-foreign :odbc)



