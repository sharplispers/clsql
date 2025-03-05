;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     clsql.asd
;;;; Purpose:  ASDF System definition for CLSQL
;;;; Authors:  Marcus Pearce and Kevin M. Rosenberg
;;;; Created:  March 2004
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

#+(and clisp (not :clsql-cffi))
(asdf:operate 'asdf:load-op 'clsql-cffi)

;; need to load uffi for below perform :after method
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+:clsql-cffi
  (unless (find-package 'cffi-uffi-compat)
    #+quicklisp
    (ql:quickload :cffi-uffi-compat)
    #-quicklisp
    (asdf:operate 'asdf:load-op 'cffi-uffi-compat))
  #-:clsql-cffi
  (unless (find-package 'uffi)
    (asdf:operate 'asdf:load-op 'uffi)))

(defsystem clsql
  :name "CLSQL"
  :author "Kevin Rosenberg <kevin@rosenberg.net>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "MIT"
  :description "Common Lisp SQL Interface library"
  :long-description "A Common Lisp interface to SQL RDBMS based on
the Xanalys CommonSQL interface for Lispworks. It provides low-level
database interfaces as well as a functional and an object
oriented interface."
  :version "6.4"
  :components
  ((:module "sql"
    :components
    ((:module "base"
      :components ((:file "cmucl-compat")
                   (:file "package")
                   (:file "kmr-mop" :depends-on ("package"))
                   (:file "base-classes" :depends-on ("package"))
                   (:file "conditions" :depends-on ("base-classes"))
                   (:file "db-interface" :depends-on ("conditions"))
                   (:file "decimals" :depends-on ("package" "db-interface"))
                   (:file "utils" :depends-on ("package" "db-interface"))
                   (:file "time" :depends-on ("package" "conditions" "utils"))
                   (:file "generics" :depends-on ("package"))))
     (:module "database"
      :depends-on ("base")
      :components ((:file "initialize")
                   (:file "database" :depends-on ("initialize"))
                   (:file "recording" :depends-on ("database"))
                   (:file "pool" :depends-on ("database"))))
     (:module "syntax"
      :depends-on ("database")
      :components ((:file "expressions")
                   (:file "operations" :depends-on ("expressions"))
                   (:file "syntax" :depends-on ("operations"))))
     (:module "functional"
      :depends-on ("syntax")
      :components ((:file "fdml")
                   (:file "transaction" :depends-on ("fdml"))
                   #+clisp (:file "ansi-loop")
                   (:file "loop-extension" :depends-on ("fdml" #+clisp "ansi-loop"))
                   (:file "fddl" :depends-on ("fdml"))))
     (:module "object"
      :depends-on ("functional")
      :components ((:file "metaclasses")
                   (:file "ooddl" :depends-on ("metaclasses"))
                   (:file "oodml" :depends-on ("ooddl"))))
     (:module "generic"
      :depends-on ("functional")
      :components ((:file "generic-postgresql")
                   (:file "generic-odbc")
                   (:file "sequences")
                   (:file "command-object"))))))
  :in-order-to ((test-op (test-op "clsql-tests"))))

(defmethod perform :after ((o load-op) (c (eql (find-system 'clsql))))
  (let* ((init-var (uiop:getenv "CLSQLINIT"))
         (init-file (or (when init-var (probe-file init-var))
                        (probe-file (merge-pathnames ".clsql-init.lisp" (user-homedir-pathname)))
                        (probe-file "/etc/clsql-init.lisp")
                        #+(or mswin windows win32 win64 mswindows)
                        (probe-file "c:\\etc\\clsql-init.lisp"))))
    (when init-file (load init-file))))
