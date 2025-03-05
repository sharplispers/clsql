;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     clsql-cffi.asd
;;;; Purpose:  ASDF System definition for CLSQL using CFFI-UFFI-COMPAT
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
    :description "CLSQL using CFFI-UFFI-COMPAT interface"
    :depends-on (clsql)
    :components nil)
