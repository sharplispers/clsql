;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-uffi.asd
;;;; Purpose:       ASDF definition file for CLSQL UFFI Helper package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defsystem clsql-uffi
  :name "cl-sql-base"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common UFFI Helper functions for Common Lisp SQL Interface Library"
  :long-description "cl-sql-uffi package provides common helper functions using the UFFI for the CLSQL package."

  :depends-on (clsql #-:clsql-cffi (:version uffi "2.0")
                     #+:clsql-cffi cffi-uffi-compat)

  :components
  ((:module :uffi
	    :components
	    ((:file "clsql-uffi-package")
             (:file "clsql-uffi-loader" :depends-on ("clsql-uffi-package"))
	     (:file "clsql-uffi" :depends-on ("clsql-uffi-package"))))))
