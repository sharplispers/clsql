;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: odbc -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     odbc-ff-interface.lisp
;;;; Purpose:  Function definitions for CFFI interface to ODBC
;;;; Author:   Kevin M. Rosenberg
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Kevin M. Rosenberg
;;;; and Copyright (C) Paul Meurer 1999 - 2001. All rights reserved.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:odbc)

(cffi:defctype sql-handle :pointer)
(cffi:defctype sql-handle-ptr (:pointer sql-handle))
(cffi:defctype string-ptr (:pointer :unsigned-char))

;; ODBC3
(cffi:defcfun "SQLAllocHandle" :short
  (handle-type :short)
  (input-handle sql-handle)
  (*phenv sql-handle-ptr))

;; ODBC3 version of SQLFreeStmt, SQLFreeConnect, and SSQLFreeStmt
(cffi:defcfun "SQLFreeHandle" :short ; RETCODE_SQL_API
  (handle-type :short)        ; HandleType
  (input-handle sql-handle))  ; Handle

;; deprecated
(cffi:defcfun "SQLAllocEnv" :short  ; RETCODE_SQL_API
  (*phenv sql-handle-ptr))     ; HENV   FAR *phenv

;; deprecated
(cffi:defcfun "SQLAllocConnect" :short ; RETCODE_SQL_API
  (henv sql-handle)          ; HENV        henv
  (*phdbc sql-handle-ptr))    ; HDBC   FAR *phdbc

(cffi:defcfun "SQLConnect" :short ; RETCODE_SQL_API
  (hdbc sql-handle)          ; HDBC        hdbc
  (*szDSN :string)        ; UCHAR  FAR *szDSN
  (cbDSN :short)             ; SWORD       cbDSN
  (*szUID :string)        ; UCHAR  FAR *szUID
  (cbUID :short)             ; SWORD       cbUID
  (*szAuthStr :string)    ; UCHAR  FAR *szAuthStr
  (cbAuthStr :short))         ; SWORD       cbAuthStr

(cffi:defcfun "SQLDriverConnect" :short ; RETCODE_SQL_API
  (hdbc sql-handle)          ; HDBC        hdbc
  (hwnd sql-handle)          ; SQLHWND     hwnd
  (*szConnStrIn :string)    ; UCHAR  FAR *szConnStrIn
  (cbConnStrIn :short)       ; SWORD       cbConnStrIn
  (*szConnStrOut string-ptr) ; UCHAR  FAR *szConnStrOut
  (cbConnStrOutMax :short)   ; SWORD       cbConnStrOutMax
  (*pcbConnStrOut :pointer)      ; SWORD  FAR *pcbConnStrOut
  (fDriverCompletion :short)) ; UWORD       fDriverCompletion

(cffi:defcfun "SQLDisconnect" :short ; RETCODE_SQL_API
  (hdbc sql-handle))         ; HDBC        hdbc

;;deprecated
(cffi:defcfun "SQLFreeConnect" :short ; RETCODE_SQL_API
  (hdbc sql-handle))        ; HDBC        hdbc

;; deprecated
(cffi:defcfun "SQLAllocStmt" :short ; RETCODE_SQL_API
  (hdbc sql-handle)          ; HDBC        hdbc
  (*phstmt sql-handle-ptr))   ; HSTMT  FAR *phstmt

(cffi:defcfun "SQLGetInfo" :short ; RETCODE_SQL_API
  (hdbc sql-handle)          ; HDBC        hdbc
  (fInfoType :short)         ; UWORD       fInfoType
  (rgbInfoValue :pointer)        ; PTR         rgbInfoValue
  (cbInfoValueMax :short)    ; SWORD       cbInfoValueMax
  (*pcbInfoValue :pointer))       ; SWORD  FAR *pcbInfoValue

(cffi:defcfun "SQLPrepare" :short ; RETCODE_SQL_API
  (hstmt sql-handle)         ; HSTMT       hstmt
  (*szSqlStr :string)     ; UCHAR  FAR *szSqlStr
  (cbSqlStr :int))           ; SDWORD      cbSqlStr

(cffi:defcfun "SQLExecute" :short ; RETCODE_SQL_API
  (hstmt sql-handle))         ; HSTMT       hstmt

(cffi:defcfun "SQLExecDirect" :short ; RETCODE_SQL_API
  (hstmt sql-handle)         ; HSTMT       hstmt
  (*szSqlStr :string)     ; UCHAR  FAR *szSqlStr
  (cbSqlStr :int))           ; SDWORD      cbSqlStr

(cffi:defcfun "SQLFreeStmt" :short ; RETCODE_SQL_API
  (hstmt sql-handle)         ; HSTMT       hstmt
  (fOption :short))         ; UWORD       fOption

(cffi:defcfun "SQLCancel" :short ; RETCODE_SQL_API
  (hstmt sql-handle))         ; HSTMT       hstmt

(cffi:defcfun "SQLError" :short ; RETCODE_SQL_API
  (henv sql-handle)          ; HENV        henv
  (hdbc sql-handle)          ; HDBC        hdbc
  (hstmt sql-handle)         ; HSTMT       hstmt
  (*szSqlState string-ptr)   ; UCHAR  FAR *szSqlState
  (*pfNativeError (:pointer :int))      ; SDWORD FAR *pfNativeError
  (*szErrorMsg string-ptr)   ; UCHAR  FAR *szErrorMsg
  (cbErrorMsgMax :short)     ; SWORD       cbErrorMsgMax
  (*pcbErrorMsg (:pointer :short)))        ; SWORD  FAR *pcbErrorMsg

(cffi:defcfun "SQLNumResultCols" :short ; RETCODE_SQL_API
  (hstmt sql-handle)         ; HSTMT       hstmt
  (*pccol (:pointer :short)))              ; SWORD  FAR *pccol

(cffi:defcfun "SQLRowCount" :short ; RETCODE_SQL_API
  (hstmt sql-handle)         ; HSTMT       hstmt
  (*pcrow (:pointer :int)))             ; SDWORD FAR *pcrow

(cffi:defcfun "SQLDescribeCol" :short
  (hstmt sql-handle)         ; HSTMT       hstmt
  (icol :short)              ; UWORD       icol
  (*szColName string-ptr)    ; UCHAR  FAR *szColName
  (cbColNameMax :short)      ; SWORD       cbColNameMax
  (*pcbColName (:pointer :short))         ; SWORD  FAR *pcbColName
  (*pfSqlType (:pointer :short))          ; SWORD  FAR *pfSqlType
  (*pcbColDef (:pointer #.$ODBC-ULONG-TYPE))          ; UDWORD FAR *pcbColDef
  (*pibScale (:pointer :short))           ; SWORD  FAR *pibScale
  (*pfNullable (:pointer :short)))         ; SWORD  FAR *pfNullable

(cffi:defcfun "SQLColAttributes" :short
  (hstmt sql-handle)         ; HSTMT       hstmt
  (icol :short)              ; UWORD       icol
  (fDescType :short)         ; UWORD       fDescType
  (rgbDesc string-ptr)             ; PTR         rgbDesc
  (cbDescMax :short)         ; SWORD       cbDescMax
  (*pcbDesc (:pointer :short))            ; SWORD  FAR *pcbDesc
  (*pfDesc (:pointer :int)))           ; SDWORD FAR *pfDesc

(cffi:defcfun "SQLColumns" :short
  (hstmt sql-handle)             ; HSTMT       hstmt
  (*szTableQualifier :string) ; UCHAR  FAR *szTableQualifier
  (cbTableQualifier :short)      ; SWORD       cbTableQualifier
  (*szTableOwner :string)     ; UCHAR  FAR *szTableOwner
  (cbTableOwner :short)          ; SWORD       cbTableOwner
  (*szTableName :string)      ; UCHAR  FAR *szTableName
  (cbTableName :short)           ; SWORD       cbTableName
  (*szColumnName :string)     ; UCHAR  FAR *szColumnName
  (cbColumnName :short))          ; SWORD       cbColumnName

(cffi:defcfun "SQLBindCol" :short
  (hstmt sql-handle)         ; HSTMT       hstmt
  (icol :short)              ; UWORD       icol
  (fCType :short)            ; SWORD       fCType
  (rgbValue :pointer)            ; PTR         rgbValue
  (cbValueMax :int)         ; SDWORD      cbValueMax
  (*pcbValue (:pointer :int)))           ; SDWORD FAR *pcbValue

(cffi:defcfun "SQLFetch" :short
  (hstmt sql-handle))         ; HSTMT       hstmt

(cffi:defcfun "SQLTransact" :short
  (henv sql-handle)          ; HENV        henv
  (hdbc sql-handle)          ; HDBC        hdbc
  (fType :short))             ; UWORD       fType ($SQL_COMMIT or $SQL_ROLLBACK)

;; ODBC 2.0
(cffi:defcfun "SQLDescribeParam" :short
  (hstmt sql-handle)         ; HSTMT       hstmt
  (ipar :short)              ; UWORD       ipar
  (*pfSqlType (:pointer :short))          ; SWORD  FAR *pfSqlType
  (*pcbColDef (:pointer :unsigned-int))          ; UDWORD FAR *pcbColDef
  (*pibScale (:pointer :short))           ; SWORD  FAR *pibScale
  (*pfNullable (:pointer :short)))        ; SWORD  FAR *pfNullable

;; ODBC 2.0
(cffi:defcfun "SQLBindParameter" :short
  (hstmt sql-handle)         ; HSTMT       hstmt
  (ipar :short)              ; UWORD       ipar
  (fParamType :short)        ; SWORD       fParamType
  (fCType :short)            ; SWORD       fCType
  (fSqlType :short)          ; SWORD       fSqlType
  (cbColDef :int)           ; UDWORD      cbColDef
  (ibScale :short)           ; SWORD       ibScale
  (rgbValue :pointer)            ; PTR         rgbValue
  (cbValueMax :int)         ; SDWORD      cbValueMax
  (*pcbValue :pointer))           ; SDWORD FAR *pcbValue

;; level 1
(cffi:defcfun "SQLGetData" :short
  (hstmt sql-handle)         ; HSTMT       hstmt
  (icol :short)              ; UWORD       icol
  (fCType :short)            ; SWORD       fCType
  (rgbValue :pointer)            ; PTR         rgbValue
  (cbValueMax :int)         ; SDWORD      cbValueMax
  (*pcbValue :pointer))           ; SDWORD FAR *pcbValue

(cffi:defcfun "SQLParamData" :short
  (hstmt sql-handle)         ; HSTMT       hstmt
  (*prgbValue :pointer))          ; PTR    FAR *prgbValue

(cffi:defcfun "SQLPutData" :short
  (hstmt sql-handle)         ; HSTMT       hstmt
  (rgbValue :pointer)            ; PTR         rgbValue
  (cbValue :int))            ; SDWORD      cbValue

(cffi:defcfun "SQLGetConnectOption" :short
  (hdbc sql-handle)          ; HDBC        hdbc
  (fOption :short)           ; UWORD       fOption
  (pvParam :pointer))             ; PTR         pvParam

(cffi:defcfun "SQLSetConnectOption" :short
  (hdbc sql-handle)          ; HDBC        hdbc
  (fOption :short)           ; UWORD       fOption
  (vParam :int))             ; UDWORD      vParam

(cffi:defcfun "SQLSetPos" :short
  (hstmt sql-handle)         ; HSTMT       hstmt
  (irow :short)              ; UWORD       irow
  (fOption :short)           ; UWORD       fOption
  (fLock :short))             ; UWORD       fLock

                                        ; level 2
(cffi:defcfun "SQLExtendedFetch" :short
  (hstmt sql-handle)         ; HSTMT       hstmt
  (fFetchType :short)        ; UWORD       fFetchType
  (irow :int)               ; SDWORD      irow
  (*pcrow :pointer)              ; UDWORD FAR *pcrow
  (*rgfRowStatus :pointer))       ; UWORD  FAR *rgfRowStatus

(cffi:defcfun "SQLDataSources" :short
  (henv sql-handle)          ; HENV        henv
  (fDirection :short)
  (*szDSN string-ptr)        ; UCHAR  FAR *szDSN
  (cbDSNMax :short)          ; SWORD       cbDSNMax
  (*pcbDSN (:pointer :short))             ; SWORD      *pcbDSN
  (*szDescription string-ptr) ; UCHAR     *szDescription
  (cbDescriptionMax :short)  ; SWORD       cbDescriptionMax
  (*pcbDescription (:pointer :short)))     ; SWORD      *pcbDescription

(cffi:defcfun "SQLFreeEnv" :short
  (henv sql-handle))          ; HSTMT       hstmt


;;; foreign type definitions

;;(defmacro %sql-len-data-at-exec (length)
;;  `(- $SQL_LEN_DATA_AT_EXEC_OFFSET ,length))


(cffi:defcstruct sql-c-time
  (hour :short)
  (minute :short)
  (second :short))

(cffi:defcstruct sql-c-date
  (year :short)
  (month :short)
  (day :short))

(cffi:defcstruct sql-c-timestamp
  (year :short)
  (month :short)
  (day :short)
  (hour :short)
  (minute :short)
  (second :short)
  (fraction :int))

;;; Added by KMR

(cffi:defcfun "SQLSetEnvAttr" :short
  (henv sql-handle)          ; HENV        henv
  (attr :int)
  (*value :pointer)
  (szLength :int))

(cffi:defcfun "SQLGetEnvAttr" :short
  (henv sql-handle)          ; HENV        henv
  (attr :int)
  (*value :pointer)
  (szLength :int)
  (string-length-ptr (:pointer :int)))

(cffi:defcfun "SQLTables" :short
  (hstmt :pointer)
  (catalog-name :pointer)
  (catalog-name-length :short)
  (schema-name :pointer)
  (schema-name-length :short)
  (table-name :pointer)
  (table-name-length :short)
  (table-type-name :pointer)
  (table-type-name-length :short))


(cffi:defcfun "SQLStatistics" :short
  (hstmt :pointer)
  (catalog-name :pointer)
  (catalog-name-length :short)
  (schema-name :pointer)
  (schema-name-length :short)
  (table-name :string)
  (table-name-length :short)
  (unique :short)
  (reserved :short))
