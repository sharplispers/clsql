;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     mysql-loader.sql
;;;; Purpose:  MySQL library loader using UFFI
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  Feb 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:mysql)

(cffi:define-foreign-library clsql-mysql
  (t (:default "clsql_mysql64" "clsql_mysql")))

(cffi:define-foreign-library libmysql
  (t (:default "libmysqlclient" "libmysql")))

(defmethod clsql-sys:database-type-library-loaded ((database-type (eql :mysql)))
  (and (cffi:foreign-library-loaded-p 'clsql-mysql)
       (cffi:foreign-library-loaded-p 'libmysql)))

(defmethod clsql-sys:database-type-load-foreign ((database-type (eql :mysql)))
  (clsql:push-library-path clsql-mysql-system::*library-file-dir*)
  (setf cffi:*foreign-library-directories*
        (remove-duplicates (append clsql:*foreign-library-search-paths*
                                   cffi:*foreign-library-directories*)
                           :test #'equal))
  (cffi:load-foreign-library 'clsql-mysql)
  (cffi:load-foreign-library 'libmysql))

(clsql-sys:database-type-load-foreign :mysql)
