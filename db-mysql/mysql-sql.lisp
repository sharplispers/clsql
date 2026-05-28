;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-sql.lisp
;;;; Purpose:       High-level MySQL interface using UFFI
;;;; Date Started:  Feb 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defpackage #:clsql-mysql
    (:use #:common-lisp #:clsql-sys #:mysql)
    (:export #:mysql-database)
    (:import-from :clsql-sys
     :escaped :unescaped :combine-database-identifiers
     :escaped-database-identifier :unescaped-database-identifier :database-identifier
     :%sequence-name-to-table :%table-name-to-sequence-name)
    (:documentation "This is the CLSQL interface to MySQL."))

(in-package #:clsql-mysql)

;;; Field conversion functions

(defun result-field-names (res-ptr)
  (let ((names '()))
    (mysql-field-seek res-ptr 0)
    (loop
       (let ((field (mysql-fetch-field res-ptr)))
         (when (cffi:null-pointer-p field) (return))
         (push (cffi:foreign-string-to-lisp (clsql-mysql-field-name field)) names)))
    (nreverse names)))

(defun make-type-list-for-auto (res-ptr)
  (let ((new-types '()))
    (mysql-field-seek res-ptr 0)
    (loop
       (let ((field (mysql-fetch-field res-ptr)))
         (when (cffi:null-pointer-p field) (return))
         (let* ((flags (clsql-mysql-field-flags field))
                (unsigned (plusp (logand flags 32)))
                (type (clsql-mysql-field-type field)))
           (push
            (case type
              ((#.(cffi:foreign-enum-value 'mysql-field-types :tiny)
                #.(cffi:foreign-enum-value 'mysql-field-types :short)
                #.(cffi:foreign-enum-value 'mysql-field-types :int24))
               (if unsigned
                   :uint32
                   :int32))
              (#.(cffi:foreign-enum-value 'mysql-field-types :long)
               (if unsigned
                   :uint
                   :int))
              (#.(cffi:foreign-enum-value 'mysql-field-types :longlong)
               (if unsigned
                   :uint64
                   :int64))
              ((#.(cffi:foreign-enum-value 'mysql-field-types :double)
                #.(cffi:foreign-enum-value 'mysql-field-types :float)
                #.(cffi:foreign-enum-value 'mysql-field-types :decimal))
               :double)
              (otherwise
               t))
            new-types))))
    (nreverse new-types)))

(defun canonicalize-types (types res-ptr)
  (when types
    (let ((auto-list (make-type-list-for-auto res-ptr)))
      (cond
        ((listp types)
         (canonicalize-type-list types auto-list))
        ((eq types :auto)
         auto-list)
        (t
         nil)))))

(defmethod database-initialize-database-type ((database-type (eql :mysql)))
  t)

;;(uffi:def-type mysql-mysql-ptr-def (* mysql-mysql))
;;(uffi:def-type mysql-mysql-res-ptr-def (* mysql-mysql-res))
(deftype mysql-mysql-ptr-def () t)
(deftype mysql-mysql-res-ptr-def () t)
(deftype mysql-row-def () t)

(defclass mysql-database (database)
  ((mysql-ptr :accessor database-mysql-ptr :initarg :mysql-ptr
              :type mysql-mysql-ptr-def)
   (server-info :accessor database-server-info :initarg :server-info
              :type string)))

(defmethod database-type ((database mysql-database))
  :mysql)

(defmethod database-name-from-spec (connection-spec (database-type (eql :mysql)))
  (check-connection-spec connection-spec database-type
                         (host db user password &optional port options))
  (destructuring-bind (host db user password &optional port options) connection-spec
    (declare (ignore password options))
    (concatenate 'string
                 (etypecase host
                   (null "localhost")
                   (pathname (namestring host))
                   (string host))
                 (if port
                     (concatenate 'string
                                  ":"
                                  (etypecase port
                                    (integer (write-to-string port))
                                    (string port)))
                     "")
                 "/" db "/" user)))

(defun lookup-option-code (option)
  (if (assoc option +mysql-option-parameter-map+)
      (symbol-value (intern
                     (concatenate 'string (symbol-name-default-case "mysql-option#")
                                  (symbol-name option))
                     (symbol-name '#:mysql)))
      (progn
        (warn "Unknown mysql option name ~A - ignoring.~%" option)
        nil)))

(defun set-mysql-options (mysql-ptr options)
  (flet ((lookup-option-type (option)
           (cdr (assoc option +mysql-option-parameter-map+))))
    (dolist (option options)
      (if (atom option)
          (let ((option-code (lookup-option-code option)))
            (when option-code
              (mysql-options mysql-ptr option-code (cffi:null-pointer))))
          (destructuring-bind (name . value) option
            (let ((option-code (lookup-option-code name)))
              (when option-code
                (case (lookup-option-type name)
                  (:none
                   (mysql-options mysql-ptr option-code (cffi:null-pointer)))
                  (:char-ptr
                   (if (stringp value)
                       (cffi:with-foreign-string (fs value)
                         (mysql-options mysql-ptr option-code fs))
                       (warn "Expecting string argument for mysql option ~A, got ~A ~
- ignoring.~%"
                             name value)))
                  (:uint-ptr
                   (if (integerp value)
                       (cffi:with-foreign-object (fo :unsigned-int)
                         (setf (cffi:mem-ref fo ':unsigned-int) value)
                         (mysql-options mysql-ptr option-code fo))
                       (warn "Expecting integer argument for mysql option ~A, got ~A ~
- ignoring.~%"
                             name value)))
                  (:boolean-ptr
                   (cffi:with-foreign-object (fo :byte)
                     (setf (cffi:mem-ref fo ':char)
                           (case value
                             ((nil 0) 0)
                             (t 1)))
                     (mysql-options mysql-ptr option-code fo)))))))))))

(defmethod database-connect (connection-spec (database-type (eql :mysql)))
  (check-connection-spec connection-spec database-type
                         (host db user password &optional port options))
  (destructuring-bind (host db user password &optional port options) connection-spec
    (let ((mysql-ptr (mysql-init (cffi-sys:null-pointer)))
          (socket nil))
      (if (cffi:null-pointer-p mysql-ptr)
          (error 'sql-connection-error
                 :database-type database-type
                 :connection-spec connection-spec
                 :error-id (mysql-errno mysql-ptr)
                 :message (mysql-error mysql-ptr))
          (when options
            (set-mysql-options mysql-ptr options))
          (let ((error-occurred nil))
            (unwind-protect
                 (if (cffi:null-pointer-p
                      (mysql-real-connect mysql-ptr host user password db
                                          (etypecase port
                                            (null 0)
                                            (integer port)
                                            (string (parse-integer port)))
                                          socket
                                          0))
                     (progn
                       (setq error-occurred t)
                       (error 'sql-connection-error
                              :database-type database-type
                              :connection-spec connection-spec
                             :error-id (mysql-errno mysql-ptr)
                             :message (mysql-error mysql-ptr)))
                    (let* ((db
                            (make-instance 'mysql-database
                                           :name (database-name-from-spec connection-spec
                                                                          database-type)
                                           :database-type :mysql
                                           :connection-spec connection-spec
                                           :server-info (mysql:mysql-get-server-info mysql-ptr)
                                           :mysql-ptr mysql-ptr))
                           (cmd "SET SESSION sql_mode='ANSI'"))
                      (if (zerop (mysql-real-query mysql-ptr cmd-cs (babel:string-size-in-octets cmd :encoding (encoding db))))
                          db
                          (progn
                            (warn "Error setting ANSI mode for MySQL.")
                            db))))
              (when error-occurred (mysql-close mysql-ptr))))))))


(defmethod database-disconnect ((database mysql-database))
  (mysql-close (database-mysql-ptr database))
  (setf (database-mysql-ptr database) nil)
  t)

(defmethod database-execute-command (sql-expression (database mysql-database))
  (let ((mysql-ptr (database-mysql-ptr database)))
    (declare (type mysql-mysql-ptr-def mysql-ptr))
    (if (zerop (mysql-real-query mysql-ptr sql-expression
                                 (babel:string-size-in-octets sql-expression :encoding (encoding database))))
        t
        (error 'sql-database-data-error
               :database database
               :expression sql-expression
               :error-id (mysql-errno mysql-ptr)
               :message (mysql-error mysql-ptr)))))


(defmethod database-query (query-expression (database mysql-database)
			   result-types field-names)
  (declare (optimize (speed 3)))
  (let ((mysql-ptr (database-mysql-ptr database))
        (results nil) ;; all the results and column-names in reverse-order
        (res-ptr nil)
        (num-fields 0))
    (declare (type mysql-mysql-ptr-def mysql-ptr)
             (type (or null mysql-mysql-res-ptr-def) res-ptr)
             (fixnum num-fields))
    (when (database-execute-command query-expression database)
      (labels
          ((get-row (row lengths)
             "Pull a single row value from the results"
             (loop for i from 0 below num-fields
                   collect
                      (convert-raw-field
                       (cffi:mem-aref row :pointer (the fixnum i))
                       (nth i result-types)
                       :length
                       (cffi:mem-aref lengths :unsigned-long (the fixnum i))
                       :encoding (encoding database))))
           (get-result-rows ()
             "get all the rows out of the now valid results set"
             (loop for row = (mysql-fetch-row res-ptr)
                   for lengths = (mysql-fetch-lengths res-ptr)
                   until (cffi:null-pointer-p row)
                   collect (get-row row lengths)))
           (do-result-set ()
             "for a mysql-ptr, grab and return a results set"
             (setf res-ptr (mysql-use-result mysql-ptr))
             (cond
               ((or (null res-ptr) (cffi:null-pointer-p res-ptr))
                (unless (zerop (mysql-errno mysql-ptr))
                  ;;from http://dev.mysql.com/doc/refman/5.0/en/mysql-field-count.html
                  ;; if mysql_use_result or mysql_store_result return a null ptr,
                  ;; we use a mysql_errno check to see if it had a problem or just
                  ;; was a query without a result. If no error, just return nil.
                  (error 'sql-database-data-error
                         :database database
                         :expression query-expression
                         :error-id (mysql-errno mysql-ptr)
                         :message (mysql-error mysql-ptr))))
               (t 
                (unwind-protect                           
                     (progn (setf num-fields (mysql-num-fields res-ptr)
                                  result-types (canonicalize-types
                                                result-types res-ptr)) 
                            (push (get-result-rows) results)
                            (push (when field-names
                                    (result-field-names res-ptr))
                                  results))
                  (mysql-free-result res-ptr))))))
        
        (loop
          do (do-result-set)
          while (let ((next (mysql-next-result mysql-ptr)))
                  (case next
                    (0 t)            ;Successful and there are more results
                    (-1 nil)         ;Successful and there are no more results
                    (t nil)          ;errors
                    )))
        (values-list (nreverse results))))))


(defstruct mysql-result-set
  (res-ptr (cffi:null-pointer) :type mysql-mysql-res-ptr-def)
  (types nil :type list)
  (num-fields 0 :type fixnum)
  (full-set nil :type boolean))


(defmethod database-query-result-set ((query-expression string)
                                      (database mysql-database)
                                      &key full-set result-types)
  ;; TODO: REFACTOR THIS IN TERMS OF database-query or vice-versa
  ;; This doesnt seem to free database results reliably, dont think
  ;; that we should allow that particularly, OTOH, dont know how
  ;; we support cursoring without it
  (let ((mysql-ptr (database-mysql-ptr database)))
    (declare (type mysql-mysql-ptr-def mysql-ptr))
    (when (database-execute-command query-expression database)
      (let ((res-ptr (if full-set
			 (mysql-store-result mysql-ptr)
			 (mysql-use-result mysql-ptr))))
	(declare (type mysql-mysql-res-ptr-def res-ptr))
	(if (not (cffi:null-pointer-p res-ptr))
	    (let* ((num-fields (mysql-num-fields res-ptr))
		   (result-set (make-mysql-result-set
				:res-ptr res-ptr
				:num-fields num-fields
				:full-set full-set
				:types
				(canonicalize-types
				 result-types res-ptr))))
	      (if full-set
		  (values result-set
			  num-fields
			  (mysql-num-rows res-ptr))
		  (values result-set
			  num-fields)))
	    (error 'sql-database-data-error
		   :database database
		   :expression query-expression
		   :error-id (mysql-errno mysql-ptr)
		   :message (mysql-error mysql-ptr))))
      )))

(defmethod database-dump-result-set (result-set (database mysql-database))
  (mysql-free-result (mysql-result-set-res-ptr result-set))
  t)


(defmethod database-store-next-row (result-set (database mysql-database) list)
  (let* ((res-ptr (mysql-result-set-res-ptr result-set))
         (row (mysql-fetch-row res-ptr))
         (lengths (mysql-fetch-lengths res-ptr))
         (types (mysql-result-set-types result-set)))
    (declare (type mysql-mysql-res-ptr-def res-ptr)
             (type mysql-row-def row))
    (unless (cffi:null-pointer-p row)
      (loop for i from 0 below (mysql-result-set-num-fields result-set)
            for rest on list
            do
            (setf (car rest)
                  (convert-raw-field
                   (cffi:mem-aref row :pointer i)
                   (nth i types)
                   :length
                   (cffi:mem-aref lengths :unsigned-long i)
                   :encoding (encoding database))))
      list)))


;; Table and attribute introspection

(defmethod database-list-tables ((database mysql-database) &key (owner nil))
  (declare (ignore owner))
  (cond
   ((eql #\5 (char (database-server-info database) 0))
    (loop for (name type) in (database-query "SHOW FULL TABLES" database nil nil)
          when (and (string-equal type "base table")
                    (not (and (>= (length name) 11)
                              (string-equal (subseq name 0 11) "_CLSQL_SEQ_"))))
          collect name))
   (t
    (remove-if #'(lambda (s)
                   (and (>= (length s) 11)
                        (string-equal (subseq s 0 11) "_CLSQL_SEQ_")))
               (mapcar #'car (database-query "SHOW TABLES" database nil nil))))))

(defmethod database-list-views ((database mysql-database)
                                &key (owner nil))
  (declare (ignore owner))
  (cond
   ((eql #\5 (char (database-server-info database) 0))
    (loop for (name type) in (database-query "SHOW FULL TABLES" database nil nil)
          when (string-equal type "view")
          collect name))
     (t
      nil)))

(defmethod database-list-indexes ((database mysql-database)
                                  &key (owner nil))
  (let ((result '()))
    (dolist (table (database-list-tables database :owner owner) result)
      (setq result
        (append (database-list-table-indexes table database :owner owner)
                result)))))

(defmethod database-list-table-indexes (table (database mysql-database)
                                        &key (owner nil))
  (declare (ignore owner))
  (do ((results nil)
       (rows (database-query
              (format nil "SHOW INDEX FROM ~A" (escaped-database-identifier
                                                table database))
              database nil nil)
             (cdr rows)))
      ((null rows) (nreverse results))
    (let ((col (nth 2 (car rows))))
      (unless (find col results :test #'string-equal)
        (push col results)))))

(defmethod database-list-attributes ((table clsql-sys::%database-identifier)
                                     (database mysql-database)
                                     &key (owner nil)
                                     &aux (table (unescaped-database-identifier table)))
  (declare (ignore owner))
  (mapcar #'car
          (database-query
           (format nil "SHOW COLUMNS FROM ~A" (escaped-database-identifier
                                                table database))
           database nil nil)))

(defmethod database-attribute-type ((attribute clsql-sys::%database-identifier)
                                    (table clsql-sys::%database-identifier)
                                    (database mysql-database)
                                    &key (owner nil)
                                    &aux (table (unescaped-database-identifier table))
                                    (attribute (unescaped-database-identifier attribute)))
  (declare (ignore owner))
  (let ((row (car (database-query
                   (format nil
                           "SHOW COLUMNS FROM ~A LIKE '~A'"
                           (escaped-database-identifier
                            table database)
                           (unescaped-database-identifier
                            attribute database))
                   database nil nil))))
    (let* ((raw-type (second row))
           (null (third row))
           (start-length (position #\( raw-type))
           (type (if start-length
                     (subseq raw-type 0 start-length)
                     raw-type))
           (length (when start-length
                     (parse-integer (subseq raw-type (1+ start-length))
                                    :junk-allowed t))))
      (when type
        (values (ensure-keyword type) length nil (if (string-equal null "YES") 1 0))))))

;;; Sequence functions

(defmethod database-create-sequence (sequence-name
                                     (database mysql-database))
  (let ((table-name (%sequence-name-to-table sequence-name database)))
    (database-execute-command
     (concatenate 'string "CREATE TABLE " table-name
                  " (id int NOT NULL PRIMARY KEY AUTO_INCREMENT)")
     database)
    (database-execute-command
     (concatenate 'string "INSERT INTO " table-name
                  " VALUES (-1)")
     database)))

(defmethod database-drop-sequence (sequence-name
                                   (database mysql-database))
  (database-execute-command
   (concatenate 'string "DROP TABLE "
                (%sequence-name-to-table sequence-name database))
   database))

(defmethod database-list-sequences ((database mysql-database)
                                    &key (owner nil))
  (declare (ignore owner))
  (mapcan #'(lambda (s)
              (let ((sn (%table-name-to-sequence-name (car s))))
                (and sn (list (car s) sn))))
          (database-query "SHOW TABLES" database nil nil)))

(defmethod database-set-sequence-position (sequence-name
                                           (position integer)
                                           (database mysql-database))
  (database-execute-command
   (format nil "UPDATE ~A SET id=~A" (%sequence-name-to-table sequence-name database)
           position)
   database)
  (mysql:mysql-insert-id (clsql-mysql::database-mysql-ptr database)))

(defmethod database-sequence-next (sequence-name (database mysql-database))
  (without-interrupts
   (database-execute-command
    (concatenate 'string "UPDATE " (%sequence-name-to-table sequence-name database)
                 " SET id=LAST_INSERT_ID(id+1)")
    database)
   (mysql:mysql-insert-id (clsql-mysql::database-mysql-ptr database))))

(defmethod database-sequence-last (sequence-name (database mysql-database))
  (without-interrupts
    (caar (database-query
           (concatenate 'string "SELECT id from "
                        (%sequence-name-to-table sequence-name database))
           database :auto nil))))

(defmethod database-last-auto-increment-id ((database mysql-database) table column)
  (declare (ignore table column))
  (car (query "SELECT LAST_INSERT_ID();"
             :flatp t :field-names nil
             :database database)))

(defmethod database-create (connection-spec (type (eql :mysql)))
  (destructuring-bind (host name user password) connection-spec
    (let ((database (database-connect (list host "" user password)
                                      type)))
      (setf (slot-value database 'clsql-sys::state) :open)
      (unwind-protect
           (database-execute-command (format nil "create database ~A" name) database)
        (database-disconnect database)))))

(defmethod database-destroy (connection-spec (type (eql :mysql)))
  (destructuring-bind (host name user password) connection-spec
    (let ((database (database-connect (list host "" user password)
                                      type)))
      (setf (slot-value database 'clsql-sys::state) :open)
      (unwind-protect
          (database-execute-command (format nil "drop database ~A" name) database)
        (database-disconnect database)))))

(defmethod database-probe (connection-spec (type (eql :mysql)))
  (when (find (second connection-spec) (database-list connection-spec type)
              :test #'string-equal)
    t))

(defmethod database-list (connection-spec (type (eql :mysql)))
  (destructuring-bind (host name user password &optional port options) connection-spec
    (declare (ignore options))
    (let ((database (database-connect (list host (or name "mysql")
                                            user password port) type)))
      (unwind-protect
           (progn
             (setf (slot-value database 'clsql-sys::state) :open)
             (mapcar #'car (database-query "show databases" database :auto nil)))
        (progn
          (database-disconnect database)
          (setf (slot-value database 'clsql-sys::state) :closed))))))


;;; Prepared statements

(defclass mysql-stmt ()
  ((database :initarg :database :reader database)
   (stmt :initarg :stmt :accessor stmt)
   (input-bind :initarg :input-bind :reader input-bind)
   (output-bind :initarg :output-bind :reader output-bind)
   (types :initarg :types :reader types)
   (result-set :initarg :result-set :reader result-set)
   (num-fields :initarg :num-fields :reader num-fields)
   (field-names :initarg :field-names :accessor stmt-field-names)
   (length-ptr :initarg :length-ptr :reader length-ptr)
   (is-null-ptr :initarg :is-null-ptr :reader is-null-ptr)
   (result-types :initarg :result-types :reader result-types)))

(defun clsql-type->mysql-type (type)
  (cond
    ((in type :null) mysql-field-types#null)
    ((in type :int :integer) mysql-field-types#long)
    ((in type :short) mysql-field-types#short)
    ((in type :bigint) mysql-field-types#longlong)
    ((in type :float :double :number) mysql-field-types#double)
    ((and (consp type) (in (car type) :char :string :varchar)) mysql-field-types#var-string)
    ((or (eq type :blob) (and (consp type) (in (car type) :blob))) mysql-field-types#var-string)
    (t
       (error 'sql-user-error
              :message
              (format nil "Unknown clsql type ~A." type)))))

(defmethod database-prepare (sql-stmt types (database mysql-database) result-types field-names)
  (let* ((mysql-ptr (database-mysql-ptr database))
         (stmt (mysql-stmt-init mysql-ptr)))
    (when (cffi:null-pointer-p stmt)
      (error 'sql-database-error
             :error-id (mysql-errno mysql-ptr)
             :message (mysql-error mysql-ptr)))

    (unless (zerop (mysql-stmt-prepare stmt sql-stmt (babel:string-size-in-octets sql-stmt :encoding (encoding database))))
      (mysql-stmt-close stmt)
      (error 'sql-database-error
             :error-id (mysql-errno mysql-ptr)
             :message (mysql-error mysql-ptr)))

    (unless (= (mysql-stmt-param-count stmt) (length types))
      (mysql-stmt-close stmt)
      (error 'sql-database-error
             :message
             (format nil "Mysql param count (~D) does not match number of types (~D)"
                     (mysql-stmt-param-count stmt) (length types))))

    (let ((rs (mysql-stmt-result-metadata stmt)))
      (when (cffi:null-pointer-p rs)
        (warn "mysql_stmt_result_metadata returned NULL")
        #+nil
        (mysql-stmt-close stmt)
        #+nil
        (error 'sql-database-error
               :message "mysql_stmt_result_metadata returned NULL"))

      (let ((input-bind (cffi:foreign-alloc 'mysql-bind :count (length types)))
            (mysql-types (mapcar 'clsql-type->mysql-type types))
            field-vec num-fields is-null-ptr output-bind length-ptr)

        (print 'a)
        (dotimes (i (length types))
          (let* ((binding (cffi:mem-aref input-bind 'mysql-bind i)))
            (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer-type)
                  (nth i mysql-types))
            (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer-length) 0)))

        (print 'b)
        (unless (cffi:null-pointer-p rs)
          (setq field-vec (mysql-fetch-fields rs)
                num-fields (mysql-num-fields rs)
                is-null-ptr (cffi:foreign-alloc :char :count num-fields)
                output-bind (cffi:foreign-alloc 'mysql-bind :count num-fields)
                length-ptr (cffi:foreign-alloc :unsigned-long :count num-fields))
          (dotimes (i num-fields)
            (declare (fixnum i))
            (let* ((field (cffi:mem-aref field-vec 'mysql-field i))
                   (type (cffi:foreign-slot-value field '(:struct mysql-field) 'type))
                   (binding (cffi:mem-aref output-bind 'mysql-bind i)))
              (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer-type) type)

              (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer-length) 0)
              #+need-to-allocate-foreign-object-for-this
              (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::is-null)
                (+ i (uffi:pointer-address is-null-ptr)))
              #+need-to-allocate-foreign-object-for-this
              (setf (uffi:get-slot-value binding 'mysql-bind 'length)
                (+ (* i 8) (uffi:pointer-address length-ptr)))

              (case type
                ((#.(cffi:foreign-enum-value 'mysql-field-types :var-string) #.(cffi:foreign-enum-value 'mysql-field-types :string)
                  #.(cffi:foreign-enum-value 'mysql-field-types :tiny-blob) #.(cffi:foreign-enum-value 'mysql-field-types :blob)
                  #.(cffi:foreign-enum-value 'mysql-field-types :medium-blob) #.(cffi:foreign-enum-value 'mysql-field-types :long-blob))
                 (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer-length) 1024)
                 (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer)
                   (cffi:foreign-alloc :unsigned-char :count 1024)))
                (#.(cffi:foreign-enum-value 'mysql-field-types :tiny)
                 (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer)
                   (cffi:foreign-alloc :char :count 1)))
                (#.(cffi:foreign-enum-value 'mysql-field-types :short)
                 (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer)
                   (cffi:foreign-alloc :short :count 1)))
                (#.(cffi:foreign-enum-value 'mysql-field-types :long)
                 (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer)
                   ;; segfaults if supply :int on amd64
                   (cffi:foreign-alloc :long :count 1)))
                #+(or 64bit 64-bit)
                (#.(cffi:foreign-enum-value 'mysql-field-types :longlong)
                 (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer)
                   (cffi:foreign-alloc :long :count 1)))
                (#.(cffi:foreign-enum-value 'mysql-field-types :float)
                 (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer)
                   (cffi:foreign-alloc :float :count 1)))
                (#.(cffi:foreign-enum-value 'mysql-field-types :double)
                 (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer)
                   (cffi:foreign-alloc :double :count 1)))
                ((#.(cffi:foreign-enum-value 'mysql-field-types :time) #.(cffi:foreign-enum-value 'mysql-field-types :date)
                  #.(cffi:foreign-enum-value 'mysql-field-types :datetime) #.(cffi:foreign-enum-value 'mysql-field-types :timestamp))
                 (cffi:foreign-alloc 'mysql-time :count 1))
                (t
                 (error "mysql type ~D not supported." type)))))

          (unless (zerop (mysql-stmt-bind-result stmt output-bind))
            (mysql-stmt-close stmt)
            (error 'sql-database-error
                   :error-id (mysql-stmt-errno stmt)
                   :message  (mysql-stmt-error stmt))))

        (make-instance 'mysql-stmt
          :database database
          :stmt stmt
          :num-fields num-fields
          :input-bind input-bind
          :output-bind output-bind
          :result-set rs
          :result-types result-types
          :length-ptr length-ptr
          :is-null-ptr is-null-ptr
          :types mysql-types
          :field-names field-names)))))

(defmethod database-bind-parameter ((stmt mysql-stmt) position value)
  ;; FIXME: will need to allocate bind structure. This should probably be
  ;; done in C since the API is not mature and may change
  (let ((binding (cffi:mem-aref (input-bind stmt) 'mysql-bind (1- position)))
        (type (nth (1- position) (types stmt))))
    (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'length) 0)
    (cond
     ((null value)
      (when (is-null-ptr stmt)
        (setf (cffi:mem-aref (is-null-ptr stmt) :char (1- position)) 1)))
     (t
      (when (is-null-ptr stmt)
        (setf (cffi:mem-aref (is-null-ptr stmt) :char (1- position)) 0))
      (case type
        (#.(cffi:foreign-enum-value 'mysql-field-types :long)
         (setf (cffi:foreign-slot-value binding '(:struct mysql-bind) 'mysql::buffer) value))
        (t
         (warn "Unknown input bind type ~D." type))
        )))))

(defmethod database-run-prepared ((stmt mysql-stmt))
  (print 'a1)
  (when (input-bind stmt)
    (unless (zerop (mysql-stmt-bind-param (stmt stmt) (input-bind stmt)))
      (error 'sql-database-error
             :error-id (mysql-stmt-errno (stmt stmt))
             :message (mysql-stmt-error (stmt stmt)))))
  (print 'a2)
  (unless (zerop (mysql-stmt-execute (stmt stmt)))
    (error 'sql-database-error
           :error-id (mysql-stmt-errno (stmt stmt))
           :message (mysql-stmt-error (stmt stmt))))
  (print 'a3)
  (unless (zerop (mysql-stmt-store-result (stmt stmt)))
    (error 'sql-database-error
           :error-id (mysql-stmt-errno (stmt stmt))
           :message (mysql-stmt-error (stmt stmt))))
  (database-fetch-prepared-rows stmt))

(defun database-fetch-prepared-rows (stmt)
  (do ((rc (mysql-stmt-fetch (stmt stmt)) (mysql-stmt-fetch (stmt stmt)))
       (num-fields (num-fields stmt))
       (rows '()))
      ((not (zerop rc)) (nreverse rows))
    (push
     (loop for i from 0 below num-fields
           collect
           (let ((is-null
                  (not (zerop (cffi:mem-aref (is-null-ptr stmt) :char i)))))
             (unless is-null
               (let* ((bind (cffi:mem-aref (output-bind stmt) 'mysql-bind i))
                      (type (cffi:foreign-slot-value bind '(:struct mysql-bind) 'mysql::buffer-type))
                      (buffer (cffi:foreign-slot-value bind '(:struct mysql-bind) 'mysql::buffer)))
                 (case type
                   ((#.(cffi:foreign-enum-value 'mysql-field-types :var-string) #.(cffi:foreign-enum-value 'mysql-field-types :string)
                     #.(cffi:foreign-enum-value 'mysql-field-types :tiny-blob) #.(cffi:foreign-enum-value 'mysql-field-types :blob)
                     #.(cffi:foreign-enum-value 'mysql-field-types :medium-blob) #.(cffi:foreign-enum-value 'mysql-field-types :long-blob))
                    (cffi:foreign-string-to-lisp buffer :encoding (encoding (database stmt))))
                   (#.(cffi:foreign-enum-value 'mysql-field-types :tiny)
                    (cffi:mem-ref buffer :char))
                   (#.(cffi:foreign-enum-value 'mysql-field-types :short)
                    (cffi:mem-ref buffer :short))
                   (#.(cffi:foreign-enum-value 'mysql-field-types :long)
                    (cffi:mem-ref buffer :int))
                   #+(or 64bit 64-bit)
                   (#.(cffi:foreign-enum-value 'mysql-field-types :longlong)
                     (cffi:mem-ref buffer :long))
                   (#.(cffi:foreign-enum-value 'mysql-field-types :float)
                    (cffi:mem-ref buffer :float))
                   (#.(cffi:foreign-enum-value 'mysql-field-types :double)
                    (cffi:mem-ref buffer :double))
                   ((#.(cffi:foreign-enum-value 'mysql-field-types :time) #.(cffi:foreign-enum-value 'mysql-field-types :date)
                       #.(cffi:foreign-enum-value 'mysql-field-types :datetime) #.(cffi:foreign-enum-value 'mysql-field-types :timestamp))
                    (let ((year (cffi:foreign-slot-value buffer '(:struct mysql-time) 'mysql::year))
                          (month (cffi:foreign-slot-value buffer '(:struct mysql-time) 'mysql::month))
                          (day (cffi:foreign-slot-value buffer '(:struct mysql-time) 'mysql::day))
                          (hour (cffi:foreign-slot-value buffer '(:struct mysql-time) 'mysql::hour))
                          (minute (cffi:foreign-slot-value buffer '(:struct mysql-time) 'mysql::minute))
                          (second (cffi:foreign-slot-value buffer '(:struct mysql-time) 'mysql::second)))
                      (db-timestring
                       (make-time :year year :month month :day day :hour hour
                                  :minute minute :second second))))
                   (t
                    (list type)))))))
     rows)))




(defmethod database-free-prepared ((stmt mysql-stmt))
  (with-slots (stmt) stmt
    (mysql-stmt-close stmt)))


;;; Database capabilities

(defmethod db-type-use-column-on-drop-index? ((db-type (eql :mysql)))
  t)

(defmethod db-type-transaction-capable? ((db-type (eql :mysql)) database)
  (let ((tuple (car (database-query "SHOW VARIABLES LIKE 'HAVE_INNODB'" database :auto nil))))
    (and tuple (string-equal "YES" (second tuple)))))

(defmethod db-type-has-prepared-stmt? ((db-type (eql :mysql)))
  t)

(defmethod db-type-has-auto-increment? ((db-type (eql :mysql)))
  t)

(when (clsql-sys:database-type-library-loaded :mysql)
  (clsql-sys:initialize-database-type :database-type :mysql))
