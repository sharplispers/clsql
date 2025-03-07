;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     clsql-cffi.asd
;;;; Purpose:  ASDF definition file for CLSQL UFFI Helper package
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  Jan 2010
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(push :clsql-cffi cl:*features*)

(defsystem clsql-cffi
  :name "CLSQL-CFFI"
  :author "Kevin Rosenberg <kevin@rosenberg.net>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common CFFI Helper functions for Common Lisp SQL Interface Library"
  :long-description "clsql-cffi package provides common helper functions using the CFFI for the CLSQL package."

  :depends-on (:clsql :cffi :cffi-uffi-compat)

  :components
  ((:module :clsql-cffi
	    :components
	    ((:file "clsql-cffi-package")
	     (:file "clsql-cffi" :depends-on ("clsql-cffi-package"))))))
