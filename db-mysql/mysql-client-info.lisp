;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-client-info.lisp
;;;; Purpose:       Check mysql client version
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  April 2004
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:mysql)

(declaim (inline mysql-get-client-info))

(defvar *mysql-client-info* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (uffi:def-function ("mysql_get_client_info" mysql-get-client-info)
      ()
    :module "mysql"
    :returning :cstring)

  (setf *mysql-client-info* (uffi:convert-from-cstring (mysql-get-client-info))))
