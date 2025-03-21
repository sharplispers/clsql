;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     clsql-odbc.asd
;;;; Purpose:  ASDF definition file for CLSQL ODBC backend
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  April 2004
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 200d42 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defsystem clsql-odbc
  :name "clsql-odbc"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL ODBC Driver"
  :long-description "cl-sql-odbc package provides a database driver to the ODBC database system."

  :depends-on (:clsql :cffi)
  :defsystem-depends-on ((:feature :clsql-grovel :cffi-grovel))
  :components
  ((:module "db-odbc"
    :components
    ((:file "odbc-package")
     (:file "odbc-loader" :depends-on ("odbc-package"))
     #+clsql-grovel
     (:cffi-grovel-file "sql-constants" :depends-on ("odbc-loader" "odbc-package"))
     (:file "odbc-constants" :depends-on ("odbc-loader" (:feature :clsql-grovel "sql-constants")))
     (:file "odbc-ff-interface" :depends-on ("odbc-constants"))
     (:file "odbc-api" :depends-on ("odbc-ff-interface" "odbc-constants"))
     (:file "odbc-dbi" :depends-on ("odbc-api"))
     (:file "odbc-sql" :depends-on ("odbc-dbi"))))))

