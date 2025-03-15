;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     sqlite-sql.lisp
;;;; Purpose:  High-level SQLite3 interface
;;;; Authors:  Aurelio Bignoli & Kevin Rosenberg
;;;; Created:  Oct 2004
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004-2010 by Aurelio Bignoli & Kevin Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sqlite3)

(defclass sqlite3-database (database)
  ((sqlite3-db :initarg :sqlite3-db :accessor sqlite3-db)
   (sqlite3-db-pointer :initarg :sqlite3-db-pointer :accessor sqlite3-db-pointer)))

(defmethod database-type ((database sqlite3-database))
  :sqlite3)

(defmethod database-initialize-database-type ((database-type (eql :sqlite3)))
  t)

(defun check-sqlite3-connection-spec (connection-spec)
  (check-connection-spec connection-spec :sqlite3 (name &optional init-foreign-func)))

(defmethod database-name-from-spec (connection-spec
                                    (database-type (eql :sqlite3)))
  (check-sqlite3-connection-spec connection-spec)
  (princ-to-string (first connection-spec)))

(defmethod database-connect (connection-spec (database-type (eql :sqlite3)))
  (check-sqlite3-connection-spec connection-spec)
  (handler-case
      (cffi:with-foreign-string (db-name (first connection-spec))
        (let ((dbp (cffi:foreign-alloc 'sqlite3:sqlite3-db)))
          (sqlite3:sqlite3-open (first connection-spec) dbp)
          (let ((db (cffi:mem-ref dbp 'sqlite3:sqlite3-db))
                (init-foreign-func (second connection-spec)))
            (declare (type cffi:foreign-pointer db))
            (sqlite3:sqlite3-extended-result-codes db t)
            (when init-foreign-func
              (unwind-protect
                   (funcall init-foreign-func db)
                (sqlite3:sqlite3-close-v2 db)))
            (make-instance 'sqlite3-database
                           :name (database-name-from-spec connection-spec :sqlite3)
                           :database-type :sqlite3
                           :connection-spec connection-spec
                           :sqlite3-db db
                           :sqlite3-db-pointer dbp))))
    (sqlite3:sqlite3-error (err)
      (error 'sql-connection-error
             :database-type database-type
             :connection-spec connection-spec
             :error-id (sqlite3:sqlite3-error-code err)
             :message (sqlite3:sqlite3-error-message err)))))

(defmethod database-disconnect ((database sqlite3-database))
  (sqlite3:sqlite3-close-v2 (sqlite3-db database))
  (cffi:foreign-free (sqlite3-db-pointer database))
  (setf (sqlite3-db database) nil)
  t)

(defmethod database-execute-command (sql-expression (database sqlite3-database))
  (handler-case
      (cffi:with-foreign-string (sql sql-expression)
        (cffi:with-foreign-objects ((stmtp 'sqlite3:sqlite3-stmt)
                                    (sql-tail '(:pointer :unsigned-char)))
          (sqlite3:sqlite3-prepare-v2 (sqlite3-db database) sql -1 stmtp sql-tail)
          (let ((stmt (cffi:mem-ref stmtp 'sqlite3:sqlite3-stmt)))
            (sqlite3:sqlite3-step stmt)
            (sqlite3:sqlite3-finalize stmt))))
    (sqlite3:sqlite3-error (err)
      (error 'sql-database-data-error
             :database database
             :expression sql-expression
             :error-id (sqlite3:sqlite3-error-code err)
             :message (sqlite3:sqlite3-errmsg (sqlite3-db database)))))
  t)

(defstruct sqlite3-result-set
  (stmt (cffi:null-pointer) :type cffi:foreign-pointer)
  (stmt-pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (n-col 0 :type fixnum)
  (col-names '() :type list)
  (result-types '() :type list))

(declaim (ftype (function (cffi:foreign-pointer fixnum t) list) get-result-types))
(defun get-result-types (stmt n-col result-types)
  (declare (type cffi:foreign-pointer stmt) (type fixnum n-col))
  (if (eq :auto result-types)
      (loop for n from 0 below n-col
            collect (let ((column-type (sqlite3:sqlite3-column-type stmt n)))
                      (cond
                        ((= column-type sqlite3:SQLITE-INTEGER) :int64)
                        ((= column-type sqlite3:SQLITE-FLOAT) :double)
                        ((= column-type sqlite3:SQLITE-TEXT) :string)
                        ((= column-type sqlite3:SQLITE-BLOB) :blob)
                        ((= column-type sqlite3:SQLITE-NULL) :string)
                        (t :string))))
      (loop for type in result-types
            collect (case type
                      ((:int :integer :tinyint) :int32)
                      (:long #+(or x86-64 64bit) :int64 #-(or x86-64 64bit) :int32)
                      (:bigint :int64)
                      ((:float :double) :double)
                      ((:numeric) :number)
                      (otherwise :string)))))

(defmethod database-query-result-set ((query-expression string)
                                      (database sqlite3-database)
                                      &key result-types full-set)
  (let ((stmt (cffi:null-pointer))
        (stmt-pointer (cffi:null-pointer)))
    (declare (type cffi:foreign-pointer stmt))
    (handler-case
        (cffi:with-foreign-string (sql query-expression)
          (setf stmt-pointer (cffi:foreign-alloc 'sqlite3:sqlite3-stmt))
          (cffi:with-foreign-object (sql-tail '(:pointer :unsigned-char))
            (sqlite3:sqlite3-prepare-v2 (sqlite3-db database) sql -1 stmt-pointer sql-tail)
            (setf stmt (cffi:mem-ref stmt-pointer 'sqlite3:sqlite3-stmt))
            (let* ((n-col (if (sqlite3:sqlite3-step stmt)
                              ;; Non empty result set.
                              (sqlite3:sqlite3-column-count stmt)
                              ;; Empty result set.
                              0))
                   (result-set (make-sqlite3-result-set
                                :stmt stmt
                                :stmt-pointer stmt-pointer
                                :n-col n-col
                                :col-names (loop for n from 0 below n-col
                                                 collect (sqlite3:sqlite3-column-name stmt n))
                                :result-types (when (> n-col 0)
                                                (get-result-types stmt n-col result-types)))))
              (if full-set
                  (values result-set n-col nil)
                  (values result-set n-col)))))
      (sqlite3:sqlite3-error (err)
        (progn
          (unless (cffi:null-pointer-p stmt)
            (ignore-errors (sqlite3:sqlite3-finalize stmt)
                           (cffi:foreign-free stmt-pointer)))
          (error 'sql-database-data-error
                 :database database
                 :expression query-expression
                 :error-id (sqlite3:sqlite3-error-code err)
                 :message (sqlite3:sqlite3-errmsg (sqlite3-db database))))))))

(defmethod database-dump-result-set (result-set (database sqlite3-database))
  (handler-case
      (progn
        (sqlite3:sqlite3-finalize (sqlite3-result-set-stmt result-set))
        (cffi:foreign-free (sqlite3-result-set-stmt-pointer result-set)))
    (sqlite3:sqlite3-error (err)
      (error 'sql-database-error
             :message
             (format nil "Error finalizing SQLite3 statement: ~A"
                     (sqlite3:sqlite3-error-message err))))))

(defmethod database-store-next-row (result-set (database sqlite3-database) list)
  (let ((n-col (sqlite3-result-set-n-col result-set)))
    (if (= n-col 0)
        ;; empty result set.
        nil
        ;; Non-empty set.
        (let ((stmt (sqlite3-result-set-stmt result-set)))
          (declare (type cffi:foreign-pointer stmt))
          ;; Store row in list.
          (loop for i = 0 then (1+ i)
                for rest on list
                for type in (sqlite3-result-set-result-types result-set)
                do (setf (car rest)
                         (if (eq  type :blob)
                             (clsql-cffi:convert-raw-field
                              (sqlite3:sqlite3-column-blob stmt i)
                              type
                              :length (sqlite3:sqlite3-column-bytes stmt i)
                              :encoding (encoding database))
                             (clsql-cffi:convert-raw-field
                              (sqlite3:sqlite3-column-text stmt i)
                              type
                              :encoding (encoding database)))
                         ))
          ;; Advance result set cursor.
          (handler-case
              (unless (sqlite3:sqlite3-step stmt)
                (setf (sqlite3-result-set-n-col result-set) 0))
            (sqlite3:sqlite3-error (err)
              (error 'sql-database-error
                     :message (format nil "Error in sqlite3-step: ~A"
                                      (sqlite3:sqlite3-error-message err)))))
          t))))


(defmethod database-query (query-expression (database sqlite3-database) result-types field-names)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (handler-case
      (cffi:with-foreign-string (sql query-expression)
        (cffi:with-foreign-objects ((stmt-pointer 'sqlite3:sqlite3-stmt)
                                    (sql-tail '(:pointer :unsigned-char)))
          (sqlite3:sqlite3-prepare-v2 (sqlite3-db database) sql -1 stmt-pointer sql-tail)
          (let ((stmt (cffi:mem-ref stmt-pointer 'sqlite3:sqlite3-stmt)))
            (declare (type cffi:foreign-pointer stmt))
            (unwind-protect
                 (when (sqlite3:sqlite3-step stmt)
                   (let ((n-col (sqlite3:sqlite3-column-count stmt)))
                     (flet ((extract-row-data ()
                              (loop for i fixnum from 0 below n-col
                                    for types = (get-result-types stmt n-col result-types) then (rest types)
                                    collect (if (eq (first types) :blob)
                                                (clsql-cffi:convert-raw-field
                                                 (sqlite3:sqlite3-column-blob stmt i)
                                                 (car types)
                                                 :length (sqlite3:sqlite3-column-bytes stmt i)
                                                 :encoding (encoding database))
                                                (clsql-cffi:convert-raw-field
                                                 (sqlite3:sqlite3-column-text stmt i)
                                                 (car types)
                                                 :encoding (encoding database))))))
                       (values
                        (loop :collect (extract-row-data)
                              :while (sqlite3:sqlite3-step stmt))
                        (when field-names
                          (loop for n fixnum from 0 below n-col
                                collect (sqlite3:sqlite3-column-name stmt n)))))))
              (sqlite3:sqlite3-finalize stmt)))))
    (sqlite3:sqlite3-error (err)
      (error 'sql-database-data-error
             :database database
             :expression query-expression
             :error-id (sqlite3:sqlite3-error-code err)
             :message (sqlite3:sqlite3-errmsg (sqlite3-db sqlite3-database))))))

;;; Object listing

(defmethod database-list-tables-and-sequences ((database sqlite3-database) &key owner)
  (declare (ignore owner))
  ;; Query is copied from .table command of sqlite3 command line utility.
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='table' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='table' ORDER BY name"
                 database nil nil)))

(defmethod database-list-tables ((database sqlite3-database) &key owner)
  (remove-if #'(lambda (s)
                 (and (>= (length s) 11)
                      (string-equal (subseq s 0 11) "_CLSQL_SEQ_")))
             (database-list-tables-and-sequences database :owner owner)))

(defmethod database-list-views ((database sqlite3-database)
                                &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='view' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='view' ORDER BY name"
                 database nil nil)))

(defmethod database-list-indexes ((database sqlite3-database)
                                  &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='index' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='index' ORDER BY name"
                 database nil nil)))

(defmethod database-list-table-indexes (table (database sqlite3-database)
                                        &key (owner nil))
  (declare (ignore owner))
  (let ((*print-circle* nil))
    (mapcar #'car
            (database-query
             (format
              nil
              "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='~A' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='index' AND tbl_name='~A' ORDER BY name"
              table table)
             database nil nil))))

(declaim (inline sqlite3-table-info))
(defun sqlite3-table-info (table database)
  (let ((sql (format nil "PRAGMA table_info('~A')"
                     (clsql-sys::unescaped-database-identifier table))))
    (database-query sql database nil nil)))

(defmethod database-list-attributes (table (database sqlite3-database)
                                           &key (owner nil))
  (declare (ignore owner))
  (mapcar #'(lambda (table-info) (second table-info))
          (sqlite3-table-info table database)))

(defmethod database-attribute-type (attribute table
                                    (database sqlite3-database)
                                    &key (owner nil))
  (declare (ignore owner))
  
  (loop for field-info in (sqlite3-table-info table database)
      when (string= (clsql-sys::unescaped-database-identifier attribute)
                    (second field-info))
      return
        (let* ((raw-type (third field-info))
               (start-length (position #\( raw-type))
               (type (string-trim clsql-sys::+whitespace-chars+
				  (if start-length
				      (subseq raw-type 0 start-length)
				      raw-type)))
               (length (if start-length
                           (parse-integer (subseq raw-type (1+ start-length))
                                          :junk-allowed t)
                         nil)))
          (values (when type (ensure-keyword type))
                  length
                  nil
                  (if (string-equal (fourth field-info) "0")
                      1 0)))))

(defmethod database-last-auto-increment-id ((database sqlite3-database) table column)
  (declare (ignore table column))
  (car (query "SELECT LAST_INSERT_ROWID();"
	      :flatp t :field-names nil
	      :database database)))

(defmethod database-create (connection-spec (type (eql :sqlite3)))
  (declare (ignore connection-spec))
  ;; databases are created automatically by Sqlite3
  t)

(defmethod database-destroy (connection-spec (type (eql :sqlite3)))
  (destructuring-bind (name) connection-spec
    (if (probe-file name)
        (delete-file name)
        nil)))

(defmethod database-probe (connection-spec (type (eql :sqlite3)))
  (destructuring-bind (name) connection-spec
    ;; TODO: Add a test that this file is a real sqlite3 database
    (or (string-equal ":memory:" name)
        (and (probe-file name) t))))

(defmethod database-get-type-specifier ((type (eql 'integer))
                                        args database
                                        (db-type (eql :sqlite3)))
  (declare (ignore database))
  (if args
      (format nil "INTEGER(~A)" (car args))
      "INTEGER"))

(defmethod database-get-type-specifier ((type (eql 'integer))
                                        args database
                                        (db-type (eql :sqlite3)))
  (declare (ignore database))
  (if args
      (format nil "INTEGER(~A)" (car args))
      "INTEGER"))

;;; Database capabilities

(defmethod db-type-has-boolean-where? ((db-type (eql :sqlite3)))
  nil)

(defmethod db-type-has-auto-increment? ((db-type (eql :sqlite3)))
  t)

(when (clsql-sys:database-type-library-loaded :sqlite3)
  (clsql-sys:initialize-database-type :database-type :sqlite3))
