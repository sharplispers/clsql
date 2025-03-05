;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     clsql-uffi-loader.sql
;;;; Purpose:  Library loader using CLSQL UFFI helper library
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  Mar 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-cffi)

(defun foreign-library-types ()
  "Returns list of string naming possible library types for platform,
sorted by preference"
  #+(or win32 cygwin mswindows) '("dll" "lib" "so")
  #+(or macos macosx darwin ccl-5.0) '("dylib" "bundle")
  #-(or win32 cygwin mswindows macos macosx darwin ccl-5.0) '("so" "a" "o"))

(defun find-and-load-foreign-library (filenames &key module supporting-libraries (errorp t))
  "Attempt to load a foreign library. This will search for any of the filenames, as
well as any of the filenames in any of the clsql:*foreign-library-search-paths*"
  (declare (ignore module supporting-libraries))
  (let ((filenames (if (listp filenames) filenames (list filenames))))
    (flet ((try-load (testpath)
             (handler-case
                 (cffi:load-foreign-library testpath :search-path clsql:*foreign-library-search-paths*)
               (error nil)))) ;(c) (warn "~A" c) nil))))
      (or
       (loop for type in (foreign-library-types)
             thereis
             (loop for name in filenames
                   for pn = (make-pathname :name name :type type)
                   thereis (try-load pn)))
       (when errorp
         (error "Couldn't load foreign librar~@P ~{~S~^, ~}. (searched ~S: ~S)"
                (length filenames) filenames
                'clsql:*foreign-library-search-paths* clsql:*foreign-library-search-paths*))))))
