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

(defvar *odbc-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the Odbc client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *odbc-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defmethod clsql-sys:database-type-library-loaded ((database-type (eql :odbc)))
  *odbc-library-loaded*)

(defmethod clsql-sys:database-type-load-foreign ((database-type (eql :odbc)))
  (mapc (lambda (path)
          (pushnew path cffi:*foreign-library-directories* :test #'equal))
        clsql:*foreign-library-search-paths*)
  (cffi:load-foreign-library 'libodbc)
  (setq *odbc-library-loaded* t))

(clsql-sys:database-type-load-foreign :odbc)



