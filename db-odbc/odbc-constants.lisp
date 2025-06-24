 ;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: odbc -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     odbc-constants.lisp
;;;; Purpose:  Constants for CFFI interface to ODBC
;;;; Authors:  Kevin M. Rosenberg and Paul Meurer
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Kevin M. Rosenberg
;;;; and Copyright (C) Paul Meurer 1999 - 2001. All rights reserved.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:odbc)

;; on SuSE AMD64 9.0, unixODBC is compiled with with SQLLEN being 4 bytes long
(clsql-sys:defconstant-if-unbound $ODBC-LONG-TYPE :int)
(clsql-sys:defconstant-if-unbound $ODBC-ULONG-TYPE :unsigned-int)
(clsql-sys:defconstant-if-unbound $ODBC-BIG-TYPE :long-long)

;; (clsql-sys:defconstant-if-unbound $ODBCVER        #x0210)


;; for new SQLAllocHandle functiion
(clsql-sys:defconstant-if-unbound $SQL_HANDLE_ENV 1)
(clsql-sys:defconstant-if-unbound $SQL_HANDLE_DBC 2)
(clsql-sys:defconstant-if-unbound $SQL_HANDLE_STMT 3)
(clsql-sys:defconstant-if-unbound $SQL_HANDLE_DESC 4)

;; generally useful constants
(clsql-sys:defconstant-if-unbound $SQL_SPEC_MAJOR 2)         ;; Major version of specification
(clsql-sys:defconstant-if-unbound $SQL_SPEC_MINOR 10)        ;; Minor version of specification
(clsql-sys:defconstant-if-unbound $SQL_SQLSTATE_SIZE 5)              ;; size of SQLSTATE
(clsql-sys:defconstant-if-unbound $SQL_MAX_MESSAGE_LENGTH 512)       ;; message buffer size
(clsql-sys:defconstant-if-unbound $SQL_MAX_DSN_LENGTH 32)            ;; maximum data source name size

;; RETCODEs
(clsql-sys:defconstant-if-unbound $SQL_INVALID_HANDLE -2)
(clsql-sys:defconstant-if-unbound $SQL_ERROR -1)
(clsql-sys:defconstant-if-unbound $SQL_SUCCESS 0)
(clsql-sys:defconstant-if-unbound $SQL_SUCCESS_WITH_INFO 1)
(clsql-sys:defconstant-if-unbound $SQL_NO_DATA_FOUND 100)

;; Standard SQL datatypes, using ANSI type numbering
(clsql-sys:defconstant-if-unbound $SQL_CHAR 1)
(clsql-sys:defconstant-if-unbound $SQL_NUMERIC 2)
(clsql-sys:defconstant-if-unbound $SQL_DECIMAL 3)
(clsql-sys:defconstant-if-unbound $SQL_INTEGER 4)
(clsql-sys:defconstant-if-unbound $SQL_SMALLINT 5)
(clsql-sys:defconstant-if-unbound $SQL_FLOAT 6)
(clsql-sys:defconstant-if-unbound $SQL_REAL 7)
(clsql-sys:defconstant-if-unbound $SQL_DOUBLE 8)
(clsql-sys:defconstant-if-unbound $SQL_VARCHAR 12)

(clsql-sys:defconstant-if-unbound $SQL_TYPE_MIN $SQL_CHAR)
(clsql-sys:defconstant-if-unbound $SQL_TYPE_NULL 0)
(clsql-sys:defconstant-if-unbound $SQL_TYPE_MAX $SQL_VARCHAR)

;; C datatype to SQL datatype mapping   SQL types

(clsql-sys:defconstant-if-unbound $SQL_C_CHAR $SQL_CHAR)             ;; CHAR, VARCHAR, DECIMAL, NUMERIC
(clsql-sys:defconstant-if-unbound $SQL_C_LONG $SQL_INTEGER)          ;; INTEGER
(clsql-sys:defconstant-if-unbound $SQL_C_SHORT $SQL_SMALLINT)        ;; SMALLINT
(clsql-sys:defconstant-if-unbound $SQL_C_FLOAT $SQL_REAL)            ;; REAL
(clsql-sys:defconstant-if-unbound $SQL_C_DOUBLE $SQL_DOUBLE)         ;; FLOAT, DOUBLE
(clsql-sys:defconstant-if-unbound $SQL_C_DEFAULT 99)

;; NULL status constants.  These are used in SQLColumns, SQLColAttributes,
;;SQLDescribeCol, SQLDescribeParam, and SQLSpecialColumns to describe the
;;nullablity of a column in a table.

(clsql-sys:defconstant-if-unbound $SQL_NO_NULLS 0)
(clsql-sys:defconstant-if-unbound $SQL_NULLABLE 1)
(clsql-sys:defconstant-if-unbound $SQL_NULLABLE_UNKNOWN 2)

;; Special length values
(clsql-sys:defconstant-if-unbound $SQL_NULL_DATA -1)
(clsql-sys:defconstant-if-unbound $SQL_DATA_AT_EXEC -2)
(clsql-sys:defconstant-if-unbound $SQL_NTS -3)

;; SQLFreeStmt defines
(clsql-sys:defconstant-if-unbound $SQL_CLOSE 0)
(clsql-sys:defconstant-if-unbound $SQL_DROP 1)
(clsql-sys:defconstant-if-unbound $SQL_UNBIND 2)
(clsql-sys:defconstant-if-unbound $SQL_RESET_PARAMS 3)

;; SQLTransact defines
(clsql-sys:defconstant-if-unbound $SQL_COMMIT 0)
(clsql-sys:defconstant-if-unbound $SQL_ROLLBACK 1)

;; SQLColAttributes defines
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_COUNT 0)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_NAME 1)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_TYPE 2)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_LENGTH 3)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_PRECISION 4)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_SCALE 5)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_DISPLAY_SIZE 6)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_NULLABLE 7)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_UNSIGNED 8)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_MONEY 9)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_UPDATABLE 10)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_AUTO_INCREMENT 11)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_CASE_SENSITIVE 12)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_SEARCHABLE 13)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_TYPE_NAME 14)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_TABLE_NAME 15)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_OWNER_NAME 16)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_QUALIFIER_NAME 17)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_LABEL 18)
(clsql-sys:defconstant-if-unbound $SQL_COLATT_OPT_MAX $SQL_COLUMN_LABEL)

(clsql-sys:defconstant-if-unbound $SQL_COLUMN_DRIVER_START 1000)

(clsql-sys:defconstant-if-unbound $SQL_COLATT_OPT_MIN $SQL_COLUMN_COUNT)

;; SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE
(clsql-sys:defconstant-if-unbound $SQL_ATTR_READONLY 0)
(clsql-sys:defconstant-if-unbound $SQL_ATTR_WRITE 1)
(clsql-sys:defconstant-if-unbound $SQL_ATTR_READWRITE_UNKNOWN 2)

;; SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE
;; These are also used by SQLGetInfo
(clsql-sys:defconstant-if-unbound $SQL_UNSEARCHABLE 0)
(clsql-sys:defconstant-if-unbound $SQL_LIKE_ONLY 1)
(clsql-sys:defconstant-if-unbound $SQL_ALL_EXCEPT_LIKE 2)
(clsql-sys:defconstant-if-unbound $SQL_SEARCHABLE 3)

;; SQLError defines
(clsql-sys:defconstant-if-unbound $SQL_NULL_HENV 0)
(clsql-sys:defconstant-if-unbound $SQL_NULL_HDBC 0)
(clsql-sys:defconstant-if-unbound $SQL_NULL_HSTMT 0)

;; Defines for SQLGetFunctions
;; Core Functions
;;
(clsql-sys:defconstant-if-unbound $SQL_API_SQLALLOCCONNECT      1)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLALLOCENV          2)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLALLOCSTMT         3)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLBINDCOL           4)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLCANCEL            5)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLCOLATTRIBUTES     6)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLCONNECT           7)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLDESCRIBECOL       8)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLDISCONNECT        9)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLERROR            10)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLEXECDIRECT       11)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLEXECUTE          12)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLFETCH            13)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLFREECONNECT      14)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLFREEENV          15)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLFREESTMT         16)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLGETCURSORNAME    17)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLNUMRESULTCOLS    18)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLPREPARE          19)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLROWCOUNT         20)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLSETCURSORNAME    21)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLSETPARAM         22)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLTRANSACT         23)
(clsql-sys:defconstant-if-unbound $SQL_NUM_FUNCTIONS           23)
(clsql-sys:defconstant-if-unbound $SQL_EXT_API_START           40)

;; Level 1 Functions

(clsql-sys:defconstant-if-unbound $SQL_API_SQLCOLUMNS          40)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLDRIVERCONNECT    41)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLGETCONNECTOPTION 42)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLGETDATA          43)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLGETFUNCTIONS     44)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLGETINFO          45)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLGETSTMTOPTION    46)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLGETTYPEINFO      47)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLPARAMDATA        48)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLPUTDATA          49)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLSETCONNECTOPTION 50)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLSETSTMTOPTION    51)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLSPECIALCOLUMNS   52)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLSTATISTICS       53)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLTABLES           54)

;; Level 2 Functions

(clsql-sys:defconstant-if-unbound $SQL_API_SQLBROWSECONNECT    55)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLCOLUMNPRIVILEGES 56)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLDATASOURCES      57)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLDESCRIBEPARAM    58)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLEXTENDEDFETCH    59)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLFOREIGNKEYS      60)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLMORERESULTS      61)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLNATIVESQL        62)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLNUMPARAMS        63)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLPARAMOPTIONS     64)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLPRIMARYKEYS      65)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLPROCEDURECOLUMNS 66)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLPROCEDURES       67)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLSETPOS           68)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLSETSCROLLOPTIONS 69)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLTABLEPRIVILEGES  70)

;/*             SDK 2.0 Additions               */
(clsql-sys:defconstant-if-unbound $SQL_API_SQLDRIVERS 71)
(clsql-sys:defconstant-if-unbound $SQL_API_SQLBINDPARAMETER  72)
(clsql-sys:defconstant-if-unbound $SQL_EXT_API_LAST $SQL_API_SQLBINDPARAMETER)

(clsql-sys:defconstant-if-unbound $SQL_API_ALL_FUNCTIONS 0)

(clsql-sys:defconstant-if-unbound $SQL_NUM_EXTENSIONS (- $SQL_EXT_API_LAST $SQL_EXT_API_START -1))
(clsql-sys:defconstant-if-unbound $SQL_API_LOADBYORDINAL 199)

;;; Defines for SQLGetInfo
(clsql-sys:defconstant-if-unbound $SQL_INFO_FIRST                       0)
(clsql-sys:defconstant-if-unbound $SQL_ACTIVE_CONNECTIONS               0)
(clsql-sys:defconstant-if-unbound $SQL_ACTIVE_STATEMENTS                1)
(clsql-sys:defconstant-if-unbound $SQL_DATA_SOURCE_NAME                 2)
(clsql-sys:defconstant-if-unbound $SQL_DRIVER_HDBC                      3)
(clsql-sys:defconstant-if-unbound $SQL_DRIVER_HENV                      4)
(clsql-sys:defconstant-if-unbound $SQL_DRIVER_HSTMT                     5)
(clsql-sys:defconstant-if-unbound $SQL_DRIVER_NAME                      6)
(clsql-sys:defconstant-if-unbound $SQL_DRIVER_VER                       7)
(clsql-sys:defconstant-if-unbound $SQL_FETCH_DIRECTION                  8)
(clsql-sys:defconstant-if-unbound $SQL_ODBC_API_CONFORMANCE             9)
(clsql-sys:defconstant-if-unbound $SQL_ODBC_VER                        10)
(clsql-sys:defconstant-if-unbound $SQL_ROW_UPDATES                     11)
(clsql-sys:defconstant-if-unbound $SQL_ODBC_SAG_CLI_CONFORMANCE        12)
(clsql-sys:defconstant-if-unbound $SQL_SERVER_NAME                     13)
(clsql-sys:defconstant-if-unbound $SQL_SEARCH_PATTERN_ESCAPE           14)
(clsql-sys:defconstant-if-unbound $SQL_ODBC_SQL_CONFORMANCE            15)

(clsql-sys:defconstant-if-unbound $SQL_DBMS_NAME                       17)
(clsql-sys:defconstant-if-unbound $SQL_DBMS_VER                        18)

(clsql-sys:defconstant-if-unbound $SQL_ACCESSIBLE_TABLES               19)
(clsql-sys:defconstant-if-unbound $SQL_ACCESSIBLE_PROCEDURES           20)
(clsql-sys:defconstant-if-unbound $SQL_PROCEDURES                      21)
(clsql-sys:defconstant-if-unbound $SQL_CONCAT_NULL_BEHAVIOR            22)
(clsql-sys:defconstant-if-unbound $SQL_CURSOR_COMMIT_BEHAVIOR          23)
(clsql-sys:defconstant-if-unbound $SQL_CURSOR_ROLLBACK_BEHAVIOR        24)
(clsql-sys:defconstant-if-unbound $SQL_DATA_SOURCE_READ_ONLY           25)
(clsql-sys:defconstant-if-unbound $SQL_DEFAULT_TXN_ISOLATION           26)
(clsql-sys:defconstant-if-unbound $SQL_EXPRESSIONS_IN_ORDERBY          27)
(clsql-sys:defconstant-if-unbound $SQL_IDENTIFIER_CASE                 28)
(clsql-sys:defconstant-if-unbound $SQL_IDENTIFIER_QUOTE_CHAR           29)
(clsql-sys:defconstant-if-unbound $SQL_MAX_COLUMN_NAME_LEN             30)
(clsql-sys:defconstant-if-unbound $SQL_MAX_CURSOR_NAME_LEN             31)
(clsql-sys:defconstant-if-unbound $SQL_MAX_OWNER_NAME_LEN              32)
(clsql-sys:defconstant-if-unbound $SQL_MAX_PROCEDURE_NAME_LEN          33)
(clsql-sys:defconstant-if-unbound $SQL_MAX_QUALIFIER_NAME_LEN          34)
(clsql-sys:defconstant-if-unbound $SQL_MAX_TABLE_NAME_LEN              35)
(clsql-sys:defconstant-if-unbound $SQL_MULT_RESULT_SETS                36)
(clsql-sys:defconstant-if-unbound $SQL_MULTIPLE_ACTIVE_TXN             37)
(clsql-sys:defconstant-if-unbound $SQL_OUTER_JOINS                     38)
(clsql-sys:defconstant-if-unbound $SQL_OWNER_TERM                      39)
(clsql-sys:defconstant-if-unbound $SQL_PROCEDURE_TERM                  40)
(clsql-sys:defconstant-if-unbound $SQL_QUALIFIER_NAME_SEPARATOR        41)
(clsql-sys:defconstant-if-unbound $SQL_QUALIFIER_TERM                  42)
(clsql-sys:defconstant-if-unbound $SQL_SCROLL_CONCURRENCY              43)
(clsql-sys:defconstant-if-unbound $SQL_SCROLL_OPTIONS                  44)
(clsql-sys:defconstant-if-unbound $SQL_TABLE_TERM                      45)
(clsql-sys:defconstant-if-unbound $SQL_TXN_CAPABLE                     46)
(clsql-sys:defconstant-if-unbound $SQL_USER_NAME                       47)

(clsql-sys:defconstant-if-unbound $SQL_CONVERT_FUNCTIONS               48)
(clsql-sys:defconstant-if-unbound $SQL_NUMERIC_FUNCTIONS               49)
(clsql-sys:defconstant-if-unbound $SQL_STRING_FUNCTIONS                50)
(clsql-sys:defconstant-if-unbound $SQL_SYSTEM_FUNCTIONS                51)
(clsql-sys:defconstant-if-unbound $SQL_TIMEDATE_FUNCTIONS              52)

(clsql-sys:defconstant-if-unbound $SQL_CONVERT_BIGINT                  53)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_BINARY                  54)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_BIT                     55)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_CHAR                    56)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_DATE                    57)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_DECIMAL                 58)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_DOUBLE                  59)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_FLOAT                   60)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_INTEGER                 61)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_LONGVARCHAR             62)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_NUMERIC                 63)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_REAL                    64)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_SMALLINT                65)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_TIME                    66)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_TIMESTAMP               67)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_TINYINT                 68)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_VARBINARY               69)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_VARCHAR                 70)
(clsql-sys:defconstant-if-unbound $SQL_CONVERT_LONGVARBINARY           71)

(clsql-sys:defconstant-if-unbound $SQL_TXN_ISOLATION_OPTION            72)
(clsql-sys:defconstant-if-unbound $SQL_ODBC_SQL_OPT_IEF                73)

;;; ODBC SDK 1.0 Additions
(clsql-sys:defconstant-if-unbound $SQL_CORRELATION_NAME 74)
(clsql-sys:defconstant-if-unbound $SQL_NON_NULLABLE_COLUMNS 75)

;;; ODBC SDK 2.0 Additions
(clsql-sys:defconstant-if-unbound $SQL_DRIVER_HLIB                   76)
(clsql-sys:defconstant-if-unbound $SQL_DRIVER_ODBC_VER               77)
(clsql-sys:defconstant-if-unbound $SQL_LOCK_TYPES                    78)
(clsql-sys:defconstant-if-unbound $SQL_POS_OPERATIONS                79)
(clsql-sys:defconstant-if-unbound $SQL_POSITIONED_STATEMENTS         80)
(clsql-sys:defconstant-if-unbound $SQL_GETDATA_EXTENSIONS            81)
(clsql-sys:defconstant-if-unbound $SQL_BOOKMARK_PERSISTENCE          82)
(clsql-sys:defconstant-if-unbound $SQL_STATIC_SENSITIVITY            83)
(clsql-sys:defconstant-if-unbound $SQL_FILE_USAGE                    84)
(clsql-sys:defconstant-if-unbound $SQL_NULL_COLLATION                85)
(clsql-sys:defconstant-if-unbound $SQL_ALTER_TABLE                   86)
(clsql-sys:defconstant-if-unbound $SQL_COLUMN_ALIAS                  87)
(clsql-sys:defconstant-if-unbound $SQL_GROUP_BY                      88)
(clsql-sys:defconstant-if-unbound $SQL_KEYWORDS                      89)
(clsql-sys:defconstant-if-unbound $SQL_ORDER_BY_COLUMNS_IN_SELECT    90)
(clsql-sys:defconstant-if-unbound $SQL_OWNER_USAGE                   91)
(clsql-sys:defconstant-if-unbound $SQL_QUALIFIER_USAGE               92)
(clsql-sys:defconstant-if-unbound $SQL_QUOTED_IDENTIFIER_CASE        93)
(clsql-sys:defconstant-if-unbound $SQL_SPECIAL_CHARACTERS            94)
(clsql-sys:defconstant-if-unbound $SQL_SUBQUERIES                    95)
(clsql-sys:defconstant-if-unbound $SQL_UNION                         96)
(clsql-sys:defconstant-if-unbound $SQL_MAX_COLUMNS_IN_GROUP_BY       97)
(clsql-sys:defconstant-if-unbound $SQL_MAX_COLUMNS_IN_INDEX          98)
(clsql-sys:defconstant-if-unbound $SQL_MAX_COLUMNS_IN_ORDER_BY       99)
(clsql-sys:defconstant-if-unbound $SQL_MAX_COLUMNS_IN_SELECT        100)
(clsql-sys:defconstant-if-unbound $SQL_MAX_COLUMNS_IN_TABLE             101)
(clsql-sys:defconstant-if-unbound $SQL_MAX_INDEX_SIZE                                   102)
(clsql-sys:defconstant-if-unbound $SQL_MAX_ROW_SIZE_INCLUDES_LONG       103)
(clsql-sys:defconstant-if-unbound $SQL_MAX_ROW_SIZE                             104)
(clsql-sys:defconstant-if-unbound $SQL_MAX_STATEMENT_LEN                        105)
(clsql-sys:defconstant-if-unbound $SQL_MAX_TABLES_IN_SELECT 106)
(clsql-sys:defconstant-if-unbound $SQL_MAX_USER_NAME_LEN 107)
(clsql-sys:defconstant-if-unbound $SQL_MAX_CHAR_LITERAL_LEN 108)
(clsql-sys:defconstant-if-unbound $SQL_TIMEDATE_ADD_INTERVALS 109)
(clsql-sys:defconstant-if-unbound $SQL_TIMEDATE_DIFF_INTERVALS          110)
(clsql-sys:defconstant-if-unbound $SQL_NEED_LONG_DATA_LEN 111)
(clsql-sys:defconstant-if-unbound $SQL_MAX_BINARY_LITERAL_LEN                   112)
(clsql-sys:defconstant-if-unbound $SQL_LIKE_ESCAPE_CLAUSE                       113)
(clsql-sys:defconstant-if-unbound $SQL_QUALIFIER_LOCATION                       114)
(clsql-sys:defconstant-if-unbound $SQL_ACTIVE_ENVIRONMENTS 116)

#|

/*** ODBC SDK 2.01 Additions ***/)
(clsql-sys:defconstant-if-unbound $SQL_OJ_CAPABILITIES                        65003  ;; Temp value until ODBC 3.0

(clsql-sys:defconstant-if-unbound $SQL_INFO_LAST                                             SQL_QUALIFIER_LOCATION
)
(clsql-sys:defconstant-if-unbound $SQL_INFO_DRIVER_START             1000

;; SQL_CONVERT_*  return value bitmasks
)
(clsql-sys:defconstant-if-unbound $SQL_CVT_CHAR                              #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_NUMERIC                   #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_DECIMAL                   #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_INTEGER                   #x00000008L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_SMALLINT                  #x00000010L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_FLOAT                             #x00000020L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_REAL                              #x00000040L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_DOUBLE                            #x00000080L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_VARCHAR                   #x00000100L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_LONGVARCHAR               #x00000200L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_BINARY                            #x00000400L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_VARBINARY                 #x00000800L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_BIT                               #x00001000L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_TINYINT                   #x00002000L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_BIGINT                            #x00004000L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_DATE                              #x00008000L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_TIME                              #x00010000L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_TIMESTAMP                 #x00020000L)
(clsql-sys:defconstant-if-unbound $SQL_CVT_LONGVARBINARY             #x00040000L)

;; SQL_CONVERT_FUNCTIONS functions)
(clsql-sys:defconstant-if-unbound $SQL_FN_CVT_CONVERT                        #x00000001L)

;; SQL_STRING_FUNCTIONS functions

(clsql-sys:defconstant-if-unbound $SQL_FN_STR_CONCAT                 #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_INSERT                 #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_LEFT                   #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_LTRIM                  #x00000008L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_LENGTH                 #x00000010L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_LOCATE                 #x00000020L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_LCASE                  #x00000040L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_REPEAT                 #x00000080L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_REPLACE                        #x00000100L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_RIGHT                  #x00000200L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_RTRIM                  #x00000400L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_SUBSTRING              #x00000800L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_UCASE                  #x00001000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_ASCII                  #x00002000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_CHAR                   #x00004000L
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_DIFFERENCE             #x00008000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_LOCATE_2               #x00010000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_SOUNDEX                        #x00020000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_STR_SPACE                  #x00040000L

;; SQL_NUMERIC_FUNCTIONS functions
)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_ABS                            #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_ACOS                   #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_ASIN                   #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_ATAN                   #x00000008L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_ATAN2                  #x00000010L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_CEILING                        #x00000020L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_COS                            #x00000040L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_COT                            #x00000080L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_EXP                            #x00000100L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_FLOOR                  #x00000200L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_LOG                            #x00000400L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_MOD                            #x00000800L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_SIGN                   #x00001000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_SIN                            #x00002000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_SQRT                   #x00004000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_TAN                            #x00008000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_PI                             #x00010000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_RAND                   #x00020000L
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_DEGREES                        #x00040000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_LOG10                  #x00080000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_POWER                  #x00100000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_RADIANS                        #x00200000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_ROUND                  #x00400000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_NUM_TRUNCATE               #x00800000L

;; SQL_TIMEDATE_FUNCTIONS functions
)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_NOW                             #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_CURDATE                 #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_DAYOFMONTH              #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_DAYOFWEEK               #x00000008L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_DAYOFYEAR               #x00000010L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_MONTH                   #x00000020L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_QUARTER                 #x00000040L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_WEEK                            #x00000080L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_YEAR                            #x00000100L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_CURTIME                 #x00000200L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_HOUR                            #x00000400L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_MINUTE                  #x00000800L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_SECOND                  #x00001000L
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_TIMESTAMPADD            #x00002000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_TIMESTAMPDIFF   #x00004000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_DAYNAME                 #x00008000L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TD_MONTHNAME               #x00010000L

;; SQL_SYSTEM_FUNCTIONS functions
)
(clsql-sys:defconstant-if-unbound $SQL_FN_SYS_USERNAME               #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_FN_SYS_DBNAME                 #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_FN_SYS_IFNULL                 #x00000004L

;; SQL_TIMEDATE_ADD_INTERVALS and SQL_TIMEDATE_DIFF_INTERVALS functions

(clsql-sys:defconstant-if-unbound $SQL_FN_TSI_FRAC_SECOND            #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TSI_SECOND                 #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TSI_MINUTE                 #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TSI_HOUR                   #x00000008L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TSI_DAY                            #x00000010L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TSI_WEEK                   #x00000020L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TSI_MONTH                  #x00000040L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TSI_QUARTER                        #x00000080L)
(clsql-sys:defconstant-if-unbound $SQL_FN_TSI_YEAR                   #x00000100L

;; SQL_ODBC_API_CONFORMANCE values
)
(clsql-sys:defconstant-if-unbound $SQL_OAC_NONE                              #x0000)
(clsql-sys:defconstant-if-unbound $SQL_OAC_LEVEL1                            #x0001)
(clsql-sys:defconstant-if-unbound $SQL_OAC_LEVEL2                            #x0002

;; SQL_ODBC_SAG_CLI_CONFORMANCE values
)
(clsql-sys:defconstant-if-unbound $SQL_OSCC_NOT_COMPLIANT            #x0000)
(clsql-sys:defconstant-if-unbound $SQL_OSCC_COMPLIANT                        #x0001

;; SQL_ODBC_SQL_CONFORMANCE values
)
(clsql-sys:defconstant-if-unbound $SQL_OSC_MINIMUM                   #x0000)
(clsql-sys:defconstant-if-unbound $SQL_OSC_CORE                              #x0001)
(clsql-sys:defconstant-if-unbound $SQL_OSC_EXTENDED                  #x0002

;; SQL_CONCAT_NULL_BEHAVIOR values
)
(clsql-sys:defconstant-if-unbound $SQL_CB_NULL                               #x0000)
(clsql-sys:defconstant-if-unbound $SQL_CB_NON_NULL                   #x0001

;; SQL_CURSOR_COMMIT_BEHAVIOR and SQL_CURSOR_ROLLBACK_BEHAVIOR values
)
(clsql-sys:defconstant-if-unbound $SQL_CB_DELETE                             #x0000)
(clsql-sys:defconstant-if-unbound $SQL_CB_CLOSE                              #x0001)
(clsql-sys:defconstant-if-unbound $SQL_CB_PRESERVE                           #x0002

;; SQL_IDENTIFIER_CASE values
)
(clsql-sys:defconstant-if-unbound $SQL_IC_UPPER                              #x0001)
(clsql-sys:defconstant-if-unbound $SQL_IC_LOWER                              #x0002)
(clsql-sys:defconstant-if-unbound $SQL_IC_SENSITIVE                  #x0003)
(clsql-sys:defconstant-if-unbound $SQL_IC_MIXED                              #x0004

;; SQL_TXN_CAPABLE values
|#

(clsql-sys:defconstant-if-unbound $SQL_TC_NONE 0)
(clsql-sys:defconstant-if-unbound $SQL_TC_DML 1)
(clsql-sys:defconstant-if-unbound $SQL_TC_ALL 2)

(clsql-sys:defconstant-if-unbound $SQL_TC_DDL_COMMIT 3)
(clsql-sys:defconstant-if-unbound $SQL_TC_DDL_IGNORE 4)

;; SQL_SCROLL_OPTIONS masks


(clsql-sys:defconstant-if-unbound $SQL_SO_FORWARD_ONLY #x00000001)
(clsql-sys:defconstant-if-unbound $SQL_SO_KEYSET_DRIVEN #x00000002)
(clsql-sys:defconstant-if-unbound $SQL_SO_DYNAMIC #x00000004)
(clsql-sys:defconstant-if-unbound $SQL_SO_MIXED #x00000008)
(clsql-sys:defconstant-if-unbound $SQL_SO_STATIC #x00000010)

;; SQL_SCROLL_CONCURRENCY masks

(clsql-sys:defconstant-if-unbound $SQL_SCCO_READ_ONLY #x00000001)
(clsql-sys:defconstant-if-unbound $SQL_SCCO_LOCK #x00000002)
(clsql-sys:defconstant-if-unbound $SQL_SCCO_OPT_ROWVER #x00000004)
(clsql-sys:defconstant-if-unbound $SQL_SCCO_OPT_VALUES #x00000008)

;; SQL_FETCH_DIRECTION masks

(clsql-sys:defconstant-if-unbound $SQL_FD_FETCH_NEXT #x00000001)
(clsql-sys:defconstant-if-unbound $SQL_FD_FETCH_FIRST #x00000002)
(clsql-sys:defconstant-if-unbound $SQL_FD_FETCH_LAST #x00000004)
(clsql-sys:defconstant-if-unbound $SQL_FD_FETCH_PRIOR #x00000008)
(clsql-sys:defconstant-if-unbound $SQL_FD_FETCH_ABSOLUTE #x00000010)
(clsql-sys:defconstant-if-unbound $SQL_FD_FETCH_RELATIVE #x00000020)
(clsql-sys:defconstant-if-unbound $SQL_FD_FETCH_RESUME #x00000040)
(clsql-sys:defconstant-if-unbound $SQL_FD_FETCH_BOOKMARK #x00000080)

#|
;; SQL_TXN_ISOLATION_OPTION masks
)
(clsql-sys:defconstant-if-unbound $SQL_TXN_READ_UNCOMMITTED  #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_TXN_READ_COMMITTED            #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_TXN_REPEATABLE_READ   #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_TXN_SERIALIZABLE              #x00000008L)
(clsql-sys:defconstant-if-unbound $SQL_TXN_VERSIONING                        #x00000010L

;; SQL_CORRELATION_NAME values
)
(clsql-sys:defconstant-if-unbound $SQL_CN_NONE                               #x0000)
(clsql-sys:defconstant-if-unbound $SQL_CN_DIFFERENT                  #x0001)
(clsql-sys:defconstant-if-unbound $SQL_CN_ANY                                        #x0002

;; SQL_NON_NULLABLE_COLUMNS values
)
(clsql-sys:defconstant-if-unbound $SQL_NNC_NULL                              #x0000)
(clsql-sys:defconstant-if-unbound $SQL_NNC_NON_NULL                  #x0001

;; SQL_NULL_COLLATION values
                                                                          )
(clsql-sys:defconstant-if-unbound $SQL_NC_HIGH                               #x0000)
(clsql-sys:defconstant-if-unbound $SQL_NC_LOW                                        #x0001)
(clsql-sys:defconstant-if-unbound $SQL_NC_START                              #x0002)
(clsql-sys:defconstant-if-unbound $SQL_NC_END                                        #x0004

;; SQL_FILE_USAGE values
)
(clsql-sys:defconstant-if-unbound $SQL_FILE_NOT_SUPPORTED            #x0000)
(clsql-sys:defconstant-if-unbound $SQL_FILE_TABLE                            #x0001)
(clsql-sys:defconstant-if-unbound $SQL_FILE_QUALIFIER                        #x0002

;; SQL_GETDATA_EXTENSIONS values
)
(clsql-sys:defconstant-if-unbound $SQL_GD_ANY_COLUMN                 #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_GD_ANY_ORDER                  #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_GD_BLOCK                              #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_GD_BOUND                              #x00000008L

;; SQL_ALTER_TABLE values
)
(clsql-sys:defconstant-if-unbound $SQL_AT_ADD_COLUMN                 #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_AT_DROP_COLUMN                        #x00000002L

;; SQL_POSITIONED_STATEMENTS masks
)
(clsql-sys:defconstant-if-unbound $SQL_PS_POSITIONED_DELETE  #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_PS_POSITIONED_UPDATE  #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_PS_SELECT_FOR_UPDATE  #x00000004L

;; SQL_GROUP_BY values
)
(clsql-sys:defconstant-if-unbound $SQL_GB_NOT_SUPPORTED                      #x0000)
(clsql-sys:defconstant-if-unbound $SQL_GB_GROUP_BY_EQUALS_SELECT     #x0001)
(clsql-sys:defconstant-if-unbound $SQL_GB_GROUP_BY_CONTAINS_SELECT   #x0002)
(clsql-sys:defconstant-if-unbound $SQL_GB_NO_RELATION                                #x0003

;; SQL_OWNER_USAGE masks
)
(clsql-sys:defconstant-if-unbound $SQL_OU_DML_STATEMENTS             #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_OU_PROCEDURE_INVOCATION #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_OU_TABLE_DEFINITION   #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_OU_INDEX_DEFINITION   #x00000008L)
(clsql-sys:defconstant-if-unbound $SQL_OU_PRIVILEGE_DEFINITION #x00000010L

;; SQL_QUALIFIER_USAGE masks
)
(clsql-sys:defconstant-if-unbound $SQL_QU_DML_STATEMENTS             #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_QU_PROCEDURE_INVOCATION #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_QU_TABLE_DEFINITION   #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_QU_INDEX_DEFINITION   #x00000008L)
(clsql-sys:defconstant-if-unbound $SQL_QU_PRIVILEGE_DEFINITION #x00000010L

;; SQL_SUBQUERIES masks
)
(clsql-sys:defconstant-if-unbound $SQL_SQ_COMPARISON                         #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_SQ_EXISTS                                     #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_SQ_IN                                         #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_SQ_QUANTIFIED                         #x00000008L)
(clsql-sys:defconstant-if-unbound $SQL_SQ_CORRELATED_SUBQUERIES      #x00000010L

;; SQL_UNION masks
)
(clsql-sys:defconstant-if-unbound $SQL_U_UNION                                               #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_U_UNION_ALL                                   #x00000002L

;; SQL_BOOKMARK_PERSISTENCE values
)
(clsql-sys:defconstant-if-unbound $SQL_BP_CLOSE                              #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_BP_DELETE                             #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_BP_DROP                               #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_BP_TRANSACTION                        #x00000008L)
(clsql-sys:defconstant-if-unbound $SQL_BP_UPDATE                             #x00000010L)
(clsql-sys:defconstant-if-unbound $SQL_BP_OTHER_HSTMT                        #x00000020L)
(clsql-sys:defconstant-if-unbound $SQL_BP_SCROLL                             #x00000040L

;; SQL_STATIC_SENSITIVITY values
)
(clsql-sys:defconstant-if-unbound $SQL_SS_ADDITIONS                  #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_SS_DELETIONS                  #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_SS_UPDATES                            #x00000004L

;; SQL_LOCK_TYPESL masks
)
(clsql-sys:defconstant-if-unbound $SQL_LCK_NO_CHANGE                 #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_LCK_EXCLUSIVE                 #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_LCK_UNLOCK                            #x00000004L

;; SQL_POS_OPERATIONS masks
|#

(clsql-sys:defconstant-if-unbound $SQL_POS_POSITION 1) ;; #x00000001L
(clsql-sys:defconstant-if-unbound $SQL_POS_REFRESH 2)  ;; #x00000002L
(clsql-sys:defconstant-if-unbound $SQL_POS_UPDATE 4)   ;; #x00000004L
(clsql-sys:defconstant-if-unbound $SQL_POS_DELETE 8)   ;; #x00000008L
(clsql-sys:defconstant-if-unbound $SQL_POS_ADD 16)     ;; #x00000010L

#|
;; SQL_QUALIFIER_LOCATION values
)
(clsql-sys:defconstant-if-unbound $SQL_QL_START                              #x0001L)
(clsql-sys:defconstant-if-unbound $SQL_QL_END                                        #x0002L

;; SQL_OJ_CAPABILITIES values

(clsql-sys:defconstant-if-unbound $SQL_OJ_LEFT                                       #x00000001L)
(clsql-sys:defconstant-if-unbound $SQL_OJ_RIGHT                              #x00000002L)
(clsql-sys:defconstant-if-unbound $SQL_OJ_FULL                                       #x00000004L)
(clsql-sys:defconstant-if-unbound $SQL_OJ_NESTED                             #x00000008L)
(clsql-sys:defconstant-if-unbound $SQL_OJ_NOT_ORDERED                        #x00000010L)
(clsql-sys:defconstant-if-unbound $SQL_OJ_INNER                              #x00000020L)
(clsql-sys:defconstant-if-unbound $SQL_OJ_ALL_COMPARISON_OPS #x00000040L

;; options for SQLGetStmtOption/SQLSetStmtOption)
(clsql-sys:defconstant-if-unbound $SQL_QUERY_TIMEOUT                 0)
(clsql-sys:defconstant-if-unbound $SQL_MAX_ROWS                              1)
(clsql-sys:defconstant-if-unbound $SQL_NOSCAN                                        2)
(clsql-sys:defconstant-if-unbound $SQL_MAX_LENGTH                            3)
(clsql-sys:defconstant-if-unbound $SQL_ASYNC_ENABLE                  4)
(clsql-sys:defconstant-if-unbound $SQL_BIND_TYPE                             5
(clsql-sys:defconstant-if-unbound $SQL_CURSOR_TYPE                   6)
(clsql-sys:defconstant-if-unbound $SQL_CONCURRENCY                   7)
(clsql-sys:defconstant-if-unbound $SQL_KEYSET_SIZE                   8)
(clsql-sys:defconstant-if-unbound $SQL_ROWSET_SIZE                   9)
(clsql-sys:defconstant-if-unbound $SQL_SIMULATE_CURSOR               10)
(clsql-sys:defconstant-if-unbound $SQL_RETRIEVE_DATA                 11)
(clsql-sys:defconstant-if-unbound $SQL_USE_BOOKMARKS                 12)
(clsql-sys:defconstant-if-unbound $SQL_GET_BOOKMARK                  13      /*      GetStmtOption Only)
(clsql-sys:defconstant-if-unbound $SQL_ROW_NUMBER                            14      /*      GetStmtOption Only)
(clsql-sys:defconstant-if-unbound $SQL_STMT_OPT_MAX                  SQL_ROW_NUMBER
)
(clsql-sys:defconstant-if-unbound $SQL_STMT_OPT_MIN                  SQL_QUERY_TIMEOUT


;; SQL_QUERY_TIMEOUT options)
(clsql-sys:defconstant-if-unbound $SQL_QUERY_TIMEOUT_DEFAULT 0UL

;; SQL_MAX_ROWS options)
(clsql-sys:defconstant-if-unbound $SQL_MAX_ROWS_DEFAULT              0UL

;; SQL_NOSCAN options)
(clsql-sys:defconstant-if-unbound $SQL_NOSCAN_OFF                            0UL     /*      1.0 FALSE)
(clsql-sys:defconstant-if-unbound $SQL_NOSCAN_ON                             1UL     /*      1.0 TRUE)
(clsql-sys:defconstant-if-unbound $SQL_NOSCAN_DEFAULT                        SQL_NOSCAN_OFF

;; SQL_MAX_LENGTH options)
(clsql-sys:defconstant-if-unbound $SQL_MAX_LENGTH_DEFAULT            0UL

;; SQL_ASYNC_ENABLE options)
(clsql-sys:defconstant-if-unbound $SQL_ASYNC_ENABLE_OFF              0UL)
(clsql-sys:defconstant-if-unbound $SQL_ASYNC_ENABLE_ON                       1UL)
(clsql-sys:defconstant-if-unbound $SQL_ASYNC_ENABLE_DEFAULT  SQL_ASYNC_ENABLE_OFF

;; SQL_BIND_TYPE options)
(clsql-sys:defconstant-if-unbound $SQL_BIND_BY_COLUMN                        0UL)
(clsql-sys:defconstant-if-unbound $SQL_BIND_TYPE_DEFAULT             SQL_BIND_BY_COLUMN              ;; Default value

;; SQL_CONCURRENCY options)
(clsql-sys:defconstant-if-unbound $SQL_CONCUR_READ_ONLY              1)
(clsql-sys:defconstant-if-unbound $SQL_CONCUR_LOCK                   2)
(clsql-sys:defconstant-if-unbound $SQL_CONCUR_ROWVER                 3)
(clsql-sys:defconstant-if-unbound $SQL_CONCUR_VALUES                 4)
(clsql-sys:defconstant-if-unbound $SQL_CONCUR_DEFAULT                        SQL_CONCUR_READ_ONLY    ;; Default value

;; SQL_CURSOR_TYPE options)
(clsql-sys:defconstant-if-unbound $SQL_CURSOR_FORWARD_ONLY   0UL)
(clsql-sys:defconstant-if-unbound $SQL_CURSOR_KEYSET_DRIVEN  1UL)
(clsql-sys:defconstant-if-unbound $SQL_CURSOR_DYNAMIC                        2UL)
(clsql-sys:defconstant-if-unbound $SQL_CURSOR_STATIC                 3UL)
(clsql-sys:defconstant-if-unbound $SQL_CURSOR_TYPE_DEFAULT           SQL_CURSOR_FORWARD_ONLY ;; Default value

;; SQL_ROWSET_SIZE options)
(clsql-sys:defconstant-if-unbound $SQL_ROWSET_SIZE_DEFAULT   1UL

;; SQL_KEYSET_SIZE options)
(clsql-sys:defconstant-if-unbound $SQL_KEYSET_SIZE_DEFAULT           0UL

;; SQL_SIMULATE_CURSOR options)
(clsql-sys:defconstant-if-unbound $SQL_SC_NON_UNIQUE                 0UL)
(clsql-sys:defconstant-if-unbound $SQL_SC_TRY_UNIQUE                 1UL)
(clsql-sys:defconstant-if-unbound $SQL_SC_UNIQUE                             2UL

;; SQL_RETRIEVE_DATA options)
(clsql-sys:defconstant-if-unbound $SQL_RD_OFF                                        0UL)
(clsql-sys:defconstant-if-unbound $SQL_RD_ON                                 1UL)
(clsql-sys:defconstant-if-unbound $SQL_RD_DEFAULT                            SQL_RD_ON

;; SQL_USE_BOOKMARKS options)
(clsql-sys:defconstant-if-unbound $SQL_UB_OFF                                        0UL)
(clsql-sys:defconstant-if-unbound $SQL_UB_ON                                 1UL)
(clsql-sys:defconstant-if-unbound $SQL_UB_DEFAULT                            SQL_UB_OFF


|#

;; options for SQLSetConnectOption/SQLGetConnectOption)
(clsql-sys:defconstant-if-unbound $SQL_ACCESS_MODE 101)
(clsql-sys:defconstant-if-unbound $SQL_AUTOCOMMIT 102)
(clsql-sys:defconstant-if-unbound $SQL_LOGIN_TIMEOUT 103)
(clsql-sys:defconstant-if-unbound $SQL_OPT_TRACE 104)
(clsql-sys:defconstant-if-unbound $SQL_OPT_TRACEFILE 105)
(clsql-sys:defconstant-if-unbound $SQL_TRANSLATE_DLL 106)
(clsql-sys:defconstant-if-unbound $SQL_TRANSLATE_OPTION 107)
(clsql-sys:defconstant-if-unbound $SQL_TXN_ISOLATION 108)
(clsql-sys:defconstant-if-unbound $SQL_CURRENT_QUALIFIER 109)
(clsql-sys:defconstant-if-unbound $SQL_ODBC_CURSORS 110)
(clsql-sys:defconstant-if-unbound $SQL_QUIET_MODE 111)
(clsql-sys:defconstant-if-unbound $SQL_PACKET_SIZE 112)
(clsql-sys:defconstant-if-unbound $SQL_CONN_OPT_MAX $SQL_PACKET_SIZE)
(clsql-sys:defconstant-if-unbound $SQL_CONNECT_OPT_DRVR_START 1000)

;;#define       SQL_CONN_OPT_MIN                        SQL_ACCESS_MODE

;; SQL_ACCESS_MODE options
(clsql-sys:defconstant-if-unbound $SQL_MODE_READ_WRITE 0) ; 0UL
(clsql-sys:defconstant-if-unbound $SQL_MODE_READ_ONLY 1)  ; 1UL
(clsql-sys:defconstant-if-unbound $SQL_MODE_DEFAULT $SQL_MODE_READ_WRITE)

;; SQL_AUTOCOMMIT options)
(clsql-sys:defconstant-if-unbound $SQL_AUTOCOMMIT_OFF 0) ;0UL
(clsql-sys:defconstant-if-unbound $SQL_AUTOCOMMIT_ON 1) ;1UL
(clsql-sys:defconstant-if-unbound $SQL_AUTOCOMMIT_DEFAULT $SQL_AUTOCOMMIT_ON)

;; SQL_LOGIN_TIMEOUT options)
(clsql-sys:defconstant-if-unbound $SQL_LOGIN_TIMEOUT_DEFAULT 15) ; 15UL

;; SQL_OPT_TRACE options)
(clsql-sys:defconstant-if-unbound $SQL_OPT_TRACE_OFF 0) ; 0UL
(clsql-sys:defconstant-if-unbound $SQL_OPT_TRACE_ON 1) ; 1UL
(clsql-sys:defconstant-if-unbound $SQL_OPT_TRACE_DEFAULT $SQL_OPT_TRACE_OFF)
; #ifndef SQL_OPT_TRACE_FILE_DEFAULT
; (clsql-sys:defconstant-if-unbound $SQL_OPT_TRACE_FILE_DEFAULT      "\\SQL.LOG"
;; #endif

(clsql-sys:defconstant-if-unbound $SQL_CUR_USE_IF_NEEDED 0) ; 0UL
(clsql-sys:defconstant-if-unbound $SQL_CUR_USE_ODBC 1) ; 1UL
(clsql-sys:defconstant-if-unbound $SQL_CUR_USE_DRIVER 2) ; 2UL
(clsql-sys:defconstant-if-unbound $SQL_CUR_DEFAULT $SQL_CUR_USE_DRIVER)

#|
;; Column types and scopes in SQLSpecialColumns. )
(clsql-sys:defconstant-if-unbound $SQL_BEST_ROWID 1)
(clsql-sys:defconstant-if-unbound $SQL_ROWVER 2)
)
(clsql-sys:defconstant-if-unbound $SQL_SCOPE_CURROW                  0)
(clsql-sys:defconstant-if-unbound $SQL_SCOPE_TRANSACTION             1)
(clsql-sys:defconstant-if-unbound $SQL_SCOPE_SESSION                 2

;; Defines for SQLSetPos)
(clsql-sys:defconstant-if-unbound $SQL_ENTIRE_ROWSET                 0
|#

;; Operations in SQLSetPos

(clsql-sys:defconstant-if-unbound $SQL_POSITION 0) ;; 1.0 FALSE
(clsql-sys:defconstant-if-unbound $SQL_REFRESH 1)  ;; 1.0 TRUE
(clsql-sys:defconstant-if-unbound $SQL_UPDATE 2)
(clsql-sys:defconstant-if-unbound $SQL_DELETE 3)
(clsql-sys:defconstant-if-unbound $SQL_ADD 4)

;; Lock options in SQLSetPos)
(clsql-sys:defconstant-if-unbound $SQL_LOCK_NO_CHANGE 0) ;; 1.0 FALSE
(clsql-sys:defconstant-if-unbound $SQL_LOCK_EXCLUSIVE 1) ;; 1.0 TRUE
(clsql-sys:defconstant-if-unbound $SQL_LOCK_UNLOCK 2)

;; SQLBindParameter extensions
(clsql-sys:defconstant-if-unbound $SQL_DEFAULT_PARAM -5)
(clsql-sys:defconstant-if-unbound $SQL_IGNORE -6)
(clsql-sys:defconstant-if-unbound $SQL_LEN_DATA_AT_EXEC_OFFSET -100)
;(clsql-sys:defconstant-if-unbound $SQL_LEN_DATA_AT_EXEC(length) (-length+SQL_LEN_DATA_AT_EXEC_OFFSET)

;; Special return values for SQLGetData
(clsql-sys:defconstant-if-unbound $SQL_NO_TOTAL -4)

#|
;; Macros for SQLSetPos)
(clsql-sys:defconstant-if-unbound $SQL_POSITION_TO(hstmt,irow) SQLSetPos(hstmt,irow,SQL_POSITION,SQL_LOCK_NO_CHANGE))
(clsql-sys:defconstant-if-unbound $SQL_LOCK_RECORD(hstmt,irow,fLock) SQLSetPos(hstmt,irow,SQL_POSITION,fLock))
(clsql-sys:defconstant-if-unbound $SQL_REFRESH_RECORD(hstmt,irow,fLock) SQLSetPos(hstmt,irow,SQL_REFRESH,fLock))
(clsql-sys:defconstant-if-unbound $SQL_UPDATE_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_UPDATE,SQL_LOCK_NO_CHANGE))
(clsql-sys:defconstant-if-unbound $SQL_DELETE_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_DELETE,SQL_LOCK_NO_CHANGE))
(clsql-sys:defconstant-if-unbound $SQL_ADD_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_ADD,SQL_LOCK_NO_CHANGE)

; #ifndef RC_INVOKED

/*      This define is too large for RC)
(clsql-sys:defconstant-if-unbound $SQL_ODBC_KEYWORDS \
"ABSOLUTE,ACTION,ADA,ADD,ALL,ALLOCATE,ALTER,AND,ANY,ARE,AS,"\
"ASC,ASSERTION,AT,AUTHORIZATION,AVG,"\
"BEGIN,BETWEEN,BIT,BIT_LENGTH,BOTH,BY,CASCADE,CASCADED,CASE,CAST,CATALOG,"\
"CHAR,CHAR_LENGTH,CHARACTER,CHARACTER_LENGTH,CHECK,CLOSE,COALESCE,"\
"COBOL,COLLATE,COLLATION,COLUMN,COMMIT,CONNECT,CONNECTION,CONSTRAINT,"\
"CONSTRAINTS,CONTINUE,CONVERT,CORRESPONDING,COUNT,CREATE,CROSS,CURRENT,"\
"CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,CURSOR,"\
"DATE,DAY,DEALLOCATE,DEC,DECIMAL,DECLARE,DEFAULT,DEFERRABLE,"\
"DEFERRED,DELETE,DESC,DESCRIBE,DESCRIPTOR,DIAGNOSTICS,DISCONNECT,"\
"DISTINCT,DOMAIN,DOUBLE,DROP,"\
"ELSE,END,END-EXEC,ESCAPE,EXCEPT,EXCEPTION,EXEC,EXECUTE,"\
"EXISTS,EXTERNAL,EXTRACT,"\
"FALSE,FETCH,FIRST,FLOAT,FOR,FOREIGN,FORTRAN,FOUND,FROM,FULL,"\
"GET,GLOBAL,GO,GOTO,GRANT,GROUP,HAVING,HOUR,"\
"IDENTITY,IMMEDIATE,IN,INCLUDE,INDEX,INDICATOR,INITIALLY,INNER,"\
"INPUT,INSENSITIVE,INSERT,INTEGER,INTERSECT,INTERVAL,INTO,IS,ISOLATION,"\
"JOIN,KEY,LANGUAGE,LAST,LEADING,LEFT,LEVEL,LIKE,LOCAL,LOWER,"\
"MATCH,MAX,MIN,MINUTE,MODULE,MONTH,MUMPS,"\
"NAMES,NATIONAL,NATURAL,NCHAR,NEXT,NO,NONE,NOT,NULL,NULLIF,NUMERIC,"\
"OCTET_LENGTH,OF,ON,ONLY,OPEN,OPTION,OR,ORDER,OUTER,OUTPUT,OVERLAPS,"\
"PAD,PARTIAL,PASCAL,PLI,POSITION,PRECISION,PREPARE,PRESERVE,"\
"PRIMARY,PRIOR,PRIVILEGES,PROCEDURE,PUBLIC,"\
"REFERENCES,RELATIVE,RESTRICT,REVOKE,RIGHT,ROLLBACK,ROWS,"\
"SCHEMA,SCROLL,SECOND,SECTION,SELECT,SEQUENCE,SESSION,SESSION_USER,SET,SIZE,"\
"SMALLINT,SOME,SPACE,SQL,SQLCA,SQLCODE,SQLERROR,SQLSTATE,SQLWARNING,"\
"SUBSTRING,SUM,SYSTEM_USER,"\
"TABLE,TEMPORARY,THEN,TIME,TIMESTAMP,TIMEZONE_HOUR,TIMEZONE_MINUTE,"\
"TO,TRAILING,TRANSACTION,TRANSLATE,TRANSLATION,TRIM,TRUE,"\
"UNION,UNIQUE,UNKNOWN,UPDATE,UPPER,USAGE,USER,USING,"\
"VALUE,VALUES,VARCHAR,VARYING,VIEW,WHEN,WHENEVER,WHERE,WITH,WORK,YEAR")
|#

(clsql-sys:defconstant-if-unbound $SQL_PARAM_TYPE_UNKNOWN 0)
(clsql-sys:defconstant-if-unbound $SQL_PARAM_INPUT 1)
(clsql-sys:defconstant-if-unbound $SQL_PARAM_INPUT_OUTPUT 2)
(clsql-sys:defconstant-if-unbound $SQL_RESULT_COL 3)
(clsql-sys:defconstant-if-unbound $SQL_PARAM_OUTPUT 4)
(clsql-sys:defconstant-if-unbound $SQL_RETURN_VALUE 5)


;; Defines used by both Level 1 and Level 2 functions

;; generally useful constants
(clsql-sys:defconstant-if-unbound $SQL_MAX_OPTION_STRING_LENGTH 256)

;; Additional return codes)
(clsql-sys:defconstant-if-unbound $SQL_STILL_EXECUTING 2)
(clsql-sys:defconstant-if-unbound $SQL_NEED_DATA 99)

;; SQL extended datatypes)
(clsql-sys:defconstant-if-unbound $SQL_DATE 9)
(clsql-sys:defconstant-if-unbound $SQL_TIME 10)
(clsql-sys:defconstant-if-unbound $SQL_TIMESTAMP 11)
(clsql-sys:defconstant-if-unbound $SQL_LONGVARCHAR -1)
(clsql-sys:defconstant-if-unbound $SQL_BINARY -2)
(clsql-sys:defconstant-if-unbound $SQL_VARBINARY -3)
(clsql-sys:defconstant-if-unbound $SQL_LONGVARBINARY -4)
(clsql-sys:defconstant-if-unbound $SQL_BIGINT -5)
(clsql-sys:defconstant-if-unbound $SQL_TINYINT -6)
(clsql-sys:defconstant-if-unbound $SQL_BIT -7)

;; For ODBC3
(clsql-sys:defconstant-if-unbound $SQL_TYPE_DATE 91)
(clsql-sys:defconstant-if-unbound $SQL_TYPE_TIME 92)
(clsql-sys:defconstant-if-unbound $SQL_TYPE_TIMESTAMP 93)

(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_YEAR -80)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_MONTH -81)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_YEAR_TO_MONTH -82)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_DAY -83)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_HOUR -84)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_MINUTE -85)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_SECOND -86)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_DAY_TO_HOUR -87)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_DAY_TO_MINUTE -88)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_DAY_TO_SECOND -89)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_HOUR_TO_MINUTE -90)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_HOUR_TO_SECOND -91)
(clsql-sys:defconstant-if-unbound $SQL_INTERVAL_MINUTE_TO_SECOND -92)
(clsql-sys:defconstant-if-unbound $SQL_UNICODE -95)
(clsql-sys:defconstant-if-unbound $SQL_TYPE_DRIVER_START $SQL_INTERVAL_YEAR)
(clsql-sys:defconstant-if-unbound $SQL_TYPE_DRIVER_END $SQL_UNICODE)


(clsql-sys:defconstant-if-unbound $SQL_SIGNED_OFFSET -20)
(clsql-sys:defconstant-if-unbound $SQL_UNSIGNED_OFFSET -22)

;; C datatype to SQL datatype mapping
(clsql-sys:defconstant-if-unbound $SQL_C_DATE $SQL_DATE)
(clsql-sys:defconstant-if-unbound $SQL_C_TIME $SQL_TIME)
(clsql-sys:defconstant-if-unbound $SQL_C_TIMESTAMP $SQL_TIMESTAMP)
(clsql-sys:defconstant-if-unbound $SQL_C_BINARY $SQL_BINARY)
(clsql-sys:defconstant-if-unbound $SQL_C_BIT $SQL_BIT)
(clsql-sys:defconstant-if-unbound $SQL_C_TINYINT $SQL_TINYINT)
(clsql-sys:defconstant-if-unbound $SQL_C_SBIGINT (+ $SQL_BIGINT $SQL_SIGNED_OFFSET))
(clsql-sys:defconstant-if-unbound $SQL_C_SLONG (+ $SQL_C_LONG $SQL_SIGNED_OFFSET)) ;; SIGNED INTEGER
(clsql-sys:defconstant-if-unbound $SQL_C_SSHORT (+ $SQL_C_SHORT $SQL_SIGNED_OFFSET)) ;; SIGNED SMALLINT
(clsql-sys:defconstant-if-unbound $SQL_C_STINYINT (+ $SQL_TINYINT $SQL_SIGNED_OFFSET)) ;; SIGNED TINYINT
(clsql-sys:defconstant-if-unbound $SQL_C_ULONG (+ $SQL_C_LONG $SQL_UNSIGNED_OFFSET)) ;; UNSIGNED INTEGER
(clsql-sys:defconstant-if-unbound $SQL_C_USHORT (+ $SQL_C_SHORT $SQL_UNSIGNED_OFFSET)) ;; UNSIGNED SMALLINT
(clsql-sys:defconstant-if-unbound $SQL_C_UTINYINT (+ $SQL_TINYINT $SQL_UNSIGNED_OFFSET)) ;;UNSIGNED TINYINT
(clsql-sys:defconstant-if-unbound $SQL_C_BOOKMARK $SQL_C_ULONG) ;; BOOKMARK

;;; ODBC3
(clsql-sys:defconstant-if-unbound $SQL_C_TYPE_DATE $SQL_TYPE_DATE)
(clsql-sys:defconstant-if-unbound $SQL_C_TYPE_TIME $SQL_TYPE_TIME)
(clsql-sys:defconstant-if-unbound $SQL_C_TYPE_TIMESTAMP $SQL_TYPE_TIMESTAMP)

;; Options for SQLDriverConnect
(clsql-sys:defconstant-if-unbound $SQL_DRIVER_NOPROMPT 0)
(clsql-sys:defconstant-if-unbound $SQL_DRIVER_COMPLETE 1)
(clsql-sys:defconstant-if-unbound $SQL_DRIVER_PROMPT 2)
(clsql-sys:defconstant-if-unbound $SQL_DRIVER_COMPLETE_REQUIRED 3)

(clsql-sys:defconstant-if-unbound $SQL_MAX_CONN_OUT 1024)

;; Level 2 Functions

;; SQLExtendedFetch "fFetchType" values
(clsql-sys:defconstant-if-unbound $SQL_FETCH_NEXT 1)
(clsql-sys:defconstant-if-unbound $SQL_FETCH_FIRST 2)
(clsql-sys:defconstant-if-unbound $SQL_FETCH_LAST 3)
(clsql-sys:defconstant-if-unbound $SQL_FETCH_PRIOR 4)
(clsql-sys:defconstant-if-unbound $SQL_FETCH_ABSOLUTE 5)
(clsql-sys:defconstant-if-unbound $SQL_FETCH_RELATIVE 6)
(clsql-sys:defconstant-if-unbound $SQL_FETCH_BOOKMARK 8)

;;; ODBC3 constants, added by KMR

(clsql-sys:defconstant-if-unbound $SQL_ATTR_ODBC_VERSION 200)
(clsql-sys:defconstant-if-unbound $SQL_OV_ODBC2 2)
(clsql-sys:defconstant-if-unbound $SQL_OV_ODBC3 3)
(clsql-sys:defconstant-if-unbound $SQL_INDEX_UNIQUE 0)
(clsql-sys:defconstant-if-unbound $SQL_INDEX_ALL 1)
(clsql-sys:defconstant-if-unbound $SQL_QUICK 0)
(clsql-sys:defconstant-if-unbound $SQL_ENSURE 1)
