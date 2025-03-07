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

(include "sqlext.h")

;; (constant ($ODBCVER "ODBCVER") :optional t)


;; for new SQLAllocHandle functiion
(constant ($SQL_HANDLE_ENV "SQL_HANDLE_ENV") :optional t)
(constant ($SQL_HANDLE_DBC "SQL_HANDLE_DBC") :optional t)
(constant ($SQL_HANDLE_STMT "SQL_HANDLE_STMT") :optional t)
(constant ($SQL_HANDLE_DESC "SQL_HANDLE_DESC") :optional t)

;; generally useful constants
(constant ($SQL_SPEC_MAJOR "SQL_SPEC_MAJOR") :optional t)         ;; Major version of specification
(constant ($SQL_SPEC_MINOR "SQL_SPEC_MINOR") :optional t)        ;; Minor version of specification
(constant ($SQL_SQLSTATE_SIZE "SQL_SQLSTATE_SIZE") :optional t)              ;; size of SQLSTATE
(constant ($SQL_MAX_MESSAGE_LENGTH "SQL_MAX_MESSAGE_LENGTH") :optional t)       ;; message buffer size
(constant ($SQL_MAX_DSN_LENGTH "SQL_MAX_DSN_LENGTH") :optional t)            ;; maximum data source name size

;; RETCODEs
(constant ($SQL_INVALID_HANDLE "SQL_INVALID_HANDLE") :optional t)
(constant ($SQL_ERROR "SQL_ERROR") :optional t)
(constant ($SQL_SUCCESS "SQL_SUCCESS") :optional t)
(constant ($SQL_SUCCESS_WITH_INFO "SQL_SUCCESS_WITH_INFO") :optional t)
(constant ($SQL_NO_DATA_FOUND "SQL_NO_DATA_FOUND") :optional t)

;; Standard SQL datatypes, using ANSI type numbering
(constant ($SQL_CHAR "SQL_CHAR") :optional t)
(constant ($SQL_NUMERIC "SQL_NUMERIC") :optional t)
(constant ($SQL_DECIMAL "SQL_DECIMAL") :optional t)
(constant ($SQL_INTEGER "SQL_INTEGER") :optional t)
(constant ($SQL_SMALLINT "SQL_SMALLINT") :optional t)
(constant ($SQL_FLOAT "SQL_FLOAT") :optional t)
(constant ($SQL_REAL "SQL_REAL") :optional t)
(constant ($SQL_DOUBLE "SQL_DOUBLE") :optional t)
(constant ($SQL_VARCHAR "SQL_VARCHAR") :optional t)

(constant ($SQL_TYPE_MIN "SQL_TYPE_MIN") :optional t)
(constant ($SQL_TYPE_NULL "SQL_TYPE_NULL") :optional t)
(constant ($SQL_TYPE_MAX "SQL_TYPE_MAX") :optional t)

;; C datatype to SQL datatype mapping   SQL types

(constant ($SQL_C_CHAR "SQL_C_CHAR") :optional t)             ;; CHAR, VARCHAR, DECIMAL, NUMERIC
(constant ($SQL_C_LONG "SQL_C_LONG") :optional t)          ;; INTEGER
(constant ($SQL_C_SHORT "SQL_C_SHORT") :optional t)        ;; SMALLINT
(constant ($SQL_C_FLOAT "SQL_C_FLOAT") :optional t)            ;; REAL
(constant ($SQL_C_DOUBLE "SQL_C_DOUBLE") :optional t)         ;; FLOAT, DOUBLE
(constant ($SQL_C_DEFAULT "SQL_C_DEFAULT") :optional t)

;; NULL status constants.  These are used in SQLColumns, SQLColAttributes,
;;SQLDescribeCol, SQLDescribeParam, and SQLSpecialColumns to describe the
;;nullablity of a column in a table.

(constant ($SQL_NO_NULLS "SQL_NO_NULLS") :optional t)
(constant ($SQL_NULLABLE "SQL_NULLABLE") :optional t)
(constant ($SQL_NULLABLE_UNKNOWN "SQL_NULLABLE_UNKNOWN") :optional t)

;; Special length values
(constant ($SQL_NULL_DATA "SQL_NULL_DATA") :optional t)
(constant ($SQL_DATA_AT_EXEC "SQL_DATA_AT_EXEC") :optional t)
(constant ($SQL_NTS "SQL_NTS") :optional t)

;; SQLFreeStmt defines
(constant ($SQL_CLOSE "SQL_CLOSE") :optional t)
(constant ($SQL_DROP "SQL_DROP") :optional t)
(constant ($SQL_UNBIND "SQL_UNBIND") :optional t)
(constant ($SQL_RESET_PARAMS "SQL_RESET_PARAMS") :optional t)

;; SQLTransact defines
(constant ($SQL_COMMIT "SQL_COMMIT") :optional t)
(constant ($SQL_ROLLBACK "SQL_ROLLBACK") :optional t)

;; SQLColAttributes defines
(constant ($SQL_COLUMN_COUNT "SQL_COLUMN_COUNT") :optional t)
(constant ($SQL_COLUMN_NAME "SQL_COLUMN_NAME") :optional t)
(constant ($SQL_COLUMN_TYPE "SQL_COLUMN_TYPE") :optional t)
(constant ($SQL_COLUMN_LENGTH "SQL_COLUMN_LENGTH") :optional t)
(constant ($SQL_COLUMN_PRECISION "SQL_COLUMN_PRECISION") :optional t)
(constant ($SQL_COLUMN_SCALE "SQL_COLUMN_SCALE") :optional t)
(constant ($SQL_COLUMN_DISPLAY_SIZE "SQL_COLUMN_DISPLAY_SIZE") :optional t)
(constant ($SQL_COLUMN_NULLABLE "SQL_COLUMN_NULLABLE") :optional t)
(constant ($SQL_COLUMN_UNSIGNED "SQL_COLUMN_UNSIGNED") :optional t)
(constant ($SQL_COLUMN_MONEY "SQL_COLUMN_MONEY") :optional t)
(constant ($SQL_COLUMN_UPDATABLE "SQL_COLUMN_UPDATABLE") :optional t)
(constant ($SQL_COLUMN_AUTO_INCREMENT "SQL_COLUMN_AUTO_INCREMENT") :optional t)
(constant ($SQL_COLUMN_CASE_SENSITIVE "SQL_COLUMN_CASE_SENSITIVE") :optional t)
(constant ($SQL_COLUMN_SEARCHABLE "SQL_COLUMN_SEARCHABLE") :optional t)
(constant ($SQL_COLUMN_TYPE_NAME "SQL_COLUMN_TYPE_NAME") :optional t)
(constant ($SQL_COLUMN_TABLE_NAME "SQL_COLUMN_TABLE_NAME") :optional t)
(constant ($SQL_COLUMN_OWNER_NAME "SQL_COLUMN_OWNER_NAME") :optional t)
(constant ($SQL_COLUMN_QUALIFIER_NAME "SQL_COLUMN_QUALIFIER_NAME") :optional t)
(constant ($SQL_COLUMN_LABEL "SQL_COLUMN_LABEL") :optional t)
(constant ($SQL_COLATT_OPT_MAX "SQL_COLATT_OPT_MAX") :optional t)

(constant ($SQL_COLUMN_DRIVER_START "SQL_COLUMN_DRIVER_START") :optional t)

(constant ($SQL_COLATT_OPT_MIN "SQL_COLATT_OPT_MIN") :optional t)

;; SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE
(constant ($SQL_ATTR_READONLY "SQL_ATTR_READONLY") :optional t)
(constant ($SQL_ATTR_WRITE "SQL_ATTR_WRITE") :optional t)
(constant ($SQL_ATTR_READWRITE_UNKNOWN "SQL_ATTR_READWRITE_UNKNOWN") :optional t)

;; SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE
;; These are also used by SQLGetInfo
(constant ($SQL_UNSEARCHABLE "SQL_UNSEARCHABLE") :optional t)
(constant ($SQL_LIKE_ONLY "SQL_LIKE_ONLY") :optional t)
(constant ($SQL_ALL_EXCEPT_LIKE "SQL_ALL_EXCEPT_LIKE") :optional t)
(constant ($SQL_SEARCHABLE "SQL_SEARCHABLE") :optional t)

;; SQLError defines
(constant ($SQL_NULL_HENV "SQL_NULL_HENV") :optional t)
(constant ($SQL_NULL_HDBC "SQL_NULL_HDBC") :optional t)
(constant ($SQL_NULL_HSTMT "SQL_NULL_HSTMT") :optional t)

;; Defines for SQLGetFunctions
;; Core Functions
;;
(constant ($SQL_API_SQLALLOCCONNECT "SQL_API_SQLALLOCCONNECT") :optional t)
(constant ($SQL_API_SQLALLOCENV "SQL_API_SQLALLOCENV") :optional t)
(constant ($SQL_API_SQLALLOCSTMT "SQL_API_SQLALLOCSTMT") :optional t)
(constant ($SQL_API_SQLBINDCOL "SQL_API_SQLBINDCOL") :optional t)
(constant ($SQL_API_SQLCANCEL "SQL_API_SQLCANCEL") :optional t)
(constant ($SQL_API_SQLCOLATTRIBUTES "SQL_API_SQLCOLATTRIBUTES") :optional t)
(constant ($SQL_API_SQLCONNECT "SQL_API_SQLCONNECT") :optional t)
(constant ($SQL_API_SQLDESCRIBECOL "SQL_API_SQLDESCRIBECOL") :optional t)
(constant ($SQL_API_SQLDISCONNECT "SQL_API_SQLDISCONNECT") :optional t)
(constant ($SQL_API_SQLERROR "SQL_API_SQLERROR") :optional t)
(constant ($SQL_API_SQLEXECDIRECT "SQL_API_SQLEXECDIRECT") :optional t)
(constant ($SQL_API_SQLEXECUTE "SQL_API_SQLEXECUTE") :optional t)
(constant ($SQL_API_SQLFETCH "SQL_API_SQLFETCH") :optional t)
(constant ($SQL_API_SQLFREECONNECT "SQL_API_SQLFREECONNECT") :optional t)
(constant ($SQL_API_SQLFREEENV "SQL_API_SQLFREEENV") :optional t)
(constant ($SQL_API_SQLFREESTMT "SQL_API_SQLFREESTMT") :optional t)
(constant ($SQL_API_SQLGETCURSORNAME "SQL_API_SQLGETCURSORNAME") :optional t)
(constant ($SQL_API_SQLNUMRESULTCOLS "SQL_API_SQLNUMRESULTCOLS") :optional t)
(constant ($SQL_API_SQLPREPARE "SQL_API_SQLPREPARE") :optional t)
(constant ($SQL_API_SQLROWCOUNT "SQL_API_SQLROWCOUNT") :optional t)
(constant ($SQL_API_SQLSETCURSORNAME "SQL_API_SQLSETCURSORNAME") :optional t)
(constant ($SQL_API_SQLSETPARAM "SQL_API_SQLSETPARAM") :optional t)
(constant ($SQL_API_SQLTRANSACT "SQL_API_SQLTRANSACT") :optional t)
(constant ($SQL_NUM_FUNCTIONS "SQL_NUM_FUNCTIONS") :optional t)
(constant ($SQL_EXT_API_START "SQL_EXT_API_START") :optional t)

;; Level 1 Functions

(constant ($SQL_API_SQLCOLUMNS "SQL_API_SQLCOLUMNS") :optional t)
(constant ($SQL_API_SQLDRIVERCONNECT "SQL_API_SQLDRIVERCONNECT") :optional t)
(constant ($SQL_API_SQLGETCONNECTOPTION "SQL_API_SQLGETCONNECTOPTION") :optional t)
(constant ($SQL_API_SQLGETDATA "SQL_API_SQLGETDATA") :optional t)
(constant ($SQL_API_SQLGETFUNCTIONS "SQL_API_SQLGETFUNCTIONS") :optional t)
(constant ($SQL_API_SQLGETINFO "SQL_API_SQLGETINFO") :optional t)
(constant ($SQL_API_SQLGETSTMTOPTION "SQL_API_SQLGETSTMTOPTION") :optional t)
(constant ($SQL_API_SQLGETTYPEINFO "SQL_API_SQLGETTYPEINFO") :optional t)
(constant ($SQL_API_SQLPARAMDATA "SQL_API_SQLPARAMDATA") :optional t)
(constant ($SQL_API_SQLPUTDATA "SQL_API_SQLPUTDATA") :optional t)
(constant ($SQL_API_SQLSETCONNECTOPTION "SQL_API_SQLSETCONNECTOPTION") :optional t)
(constant ($SQL_API_SQLSETSTMTOPTION "SQL_API_SQLSETSTMTOPTION") :optional t)
(constant ($SQL_API_SQLSPECIALCOLUMNS "SQL_API_SQLSPECIALCOLUMNS") :optional t)
(constant ($SQL_API_SQLSTATISTICS "SQL_API_SQLSTATISTICS") :optional t)
(constant ($SQL_API_SQLTABLES "SQL_API_SQLTABLES") :optional t)

;; Level 2 Functions

(constant ($SQL_API_SQLBROWSECONNECT "SQL_API_SQLBROWSECONNECT") :optional t)
(constant ($SQL_API_SQLCOLUMNPRIVILEGES "SQL_API_SQLCOLUMNPRIVILEGES") :optional t)
(constant ($SQL_API_SQLDATASOURCES "SQL_API_SQLDATASOURCES") :optional t)
(constant ($SQL_API_SQLDESCRIBEPARAM "SQL_API_SQLDESCRIBEPARAM") :optional t)
(constant ($SQL_API_SQLEXTENDEDFETCH "SQL_API_SQLEXTENDEDFETCH") :optional t)
(constant ($SQL_API_SQLFOREIGNKEYS "SQL_API_SQLFOREIGNKEYS") :optional t)
(constant ($SQL_API_SQLMORERESULTS "SQL_API_SQLMORERESULTS") :optional t)
(constant ($SQL_API_SQLNATIVESQL "SQL_API_SQLNATIVESQL") :optional t)
(constant ($SQL_API_SQLNUMPARAMS "SQL_API_SQLNUMPARAMS") :optional t)
(constant ($SQL_API_SQLPARAMOPTIONS "SQL_API_SQLPARAMOPTIONS") :optional t)
(constant ($SQL_API_SQLPRIMARYKEYS "SQL_API_SQLPRIMARYKEYS") :optional t)
(constant ($SQL_API_SQLPROCEDURECOLUMNS "SQL_API_SQLPROCEDURECOLUMNS") :optional t)
(constant ($SQL_API_SQLPROCEDURES "SQL_API_SQLPROCEDURES") :optional t)
(constant ($SQL_API_SQLSETPOS "SQL_API_SQLSETPOS") :optional t)
(constant ($SQL_API_SQLSETSCROLLOPTIONS "SQL_API_SQLSETSCROLLOPTIONS") :optional t)
(constant ($SQL_API_SQLTABLEPRIVILEGES "SQL_API_SQLTABLEPRIVILEGES") :optional t)

;/*             SDK 2.0 Additions               */
(constant ($SQL_API_SQLDRIVERS "SQL_API_SQLDRIVERS") :optional t)
(constant ($SQL_API_SQLBINDPARAMETER "SQL_API_SQLBINDPARAMETER") :optional t)
(constant ($SQL_EXT_API_LAST "SQL_EXT_API_LAST") :optional t)

(constant ($SQL_API_ALL_FUNCTIONS "SQL_API_ALL_FUNCTIONS") :optional t)

(constant ($SQL_NUM_EXTENSIONS "SQL_NUM_EXTENSIONS") :optional t)
(constant ($SQL_API_LOADBYORDINAL "SQL_API_LOADBYORDINAL") :optional t)

;;; Defines for SQLGetInfo
(constant ($SQL_INFO_FIRST "SQL_INFO_FIRST") :optional t)
(constant ($SQL_ACTIVE_CONNECTIONS "SQL_ACTIVE_CONNECTIONS") :optional t)
(constant ($SQL_ACTIVE_STATEMENTS "SQL_ACTIVE_STATEMENTS") :optional t)
(constant ($SQL_DATA_SOURCE_NAME "SQL_DATA_SOURCE_NAME") :optional t)
(constant ($SQL_DRIVER_HDBC "SQL_DRIVER_HDBC") :optional t)
(constant ($SQL_DRIVER_HENV "SQL_DRIVER_HENV") :optional t)
(constant ($SQL_DRIVER_HSTMT "SQL_DRIVER_HSTMT") :optional t)
(constant ($SQL_DRIVER_NAME "SQL_DRIVER_NAME") :optional t)
(constant ($SQL_DRIVER_VER "SQL_DRIVER_VER") :optional t)
(constant ($SQL_FETCH_DIRECTION "SQL_FETCH_DIRECTION") :optional t)
(constant ($SQL_ODBC_API_CONFORMANCE "SQL_ODBC_API_CONFORMANCE") :optional t)
(constant ($SQL_ODBC_VER "SQL_ODBC_VER") :optional t)
(constant ($SQL_ROW_UPDATES "SQL_ROW_UPDATES") :optional t)
(constant ($SQL_ODBC_SAG_CLI_CONFORMANCE "SQL_ODBC_SAG_CLI_CONFORMANCE") :optional t)
(constant ($SQL_SERVER_NAME "SQL_SERVER_NAME") :optional t)
(constant ($SQL_SEARCH_PATTERN_ESCAPE "SQL_SEARCH_PATTERN_ESCAPE") :optional t)
(constant ($SQL_ODBC_SQL_CONFORMANCE "SQL_ODBC_SQL_CONFORMANCE") :optional t)

(constant ($SQL_DBMS_NAME "SQL_DBMS_NAME") :optional t)
(constant ($SQL_DBMS_VER "SQL_DBMS_VER") :optional t)

(constant ($SQL_ACCESSIBLE_TABLES "SQL_ACCESSIBLE_TABLES") :optional t)
(constant ($SQL_ACCESSIBLE_PROCEDURES "SQL_ACCESSIBLE_PROCEDURES") :optional t)
(constant ($SQL_PROCEDURES "SQL_PROCEDURES") :optional t)
(constant ($SQL_CONCAT_NULL_BEHAVIOR "SQL_CONCAT_NULL_BEHAVIOR") :optional t)
(constant ($SQL_CURSOR_COMMIT_BEHAVIOR "SQL_CURSOR_COMMIT_BEHAVIOR") :optional t)
(constant ($SQL_CURSOR_ROLLBACK_BEHAVIOR "SQL_CURSOR_ROLLBACK_BEHAVIOR") :optional t)
(constant ($SQL_DATA_SOURCE_READ_ONLY "SQL_DATA_SOURCE_READ_ONLY") :optional t)
(constant ($SQL_DEFAULT_TXN_ISOLATION "SQL_DEFAULT_TXN_ISOLATION") :optional t)
(constant ($SQL_EXPRESSIONS_IN_ORDERBY "SQL_EXPRESSIONS_IN_ORDERBY") :optional t)
(constant ($SQL_IDENTIFIER_CASE "SQL_IDENTIFIER_CASE") :optional t)
(constant ($SQL_IDENTIFIER_QUOTE_CHAR "SQL_IDENTIFIER_QUOTE_CHAR") :optional t)
(constant ($SQL_MAX_COLUMN_NAME_LEN "SQL_MAX_COLUMN_NAME_LEN") :optional t)
(constant ($SQL_MAX_CURSOR_NAME_LEN "SQL_MAX_CURSOR_NAME_LEN") :optional t)
(constant ($SQL_MAX_OWNER_NAME_LEN "SQL_MAX_OWNER_NAME_LEN") :optional t)
(constant ($SQL_MAX_PROCEDURE_NAME_LEN "SQL_MAX_PROCEDURE_NAME_LEN") :optional t)
(constant ($SQL_MAX_QUALIFIER_NAME_LEN "SQL_MAX_QUALIFIER_NAME_LEN") :optional t)
(constant ($SQL_MAX_TABLE_NAME_LEN "SQL_MAX_TABLE_NAME_LEN") :optional t)
(constant ($SQL_MULT_RESULT_SETS "SQL_MULT_RESULT_SETS") :optional t)
(constant ($SQL_MULTIPLE_ACTIVE_TXN "SQL_MULTIPLE_ACTIVE_TXN") :optional t)
(constant ($SQL_OUTER_JOINS "SQL_OUTER_JOINS") :optional t)
(constant ($SQL_OWNER_TERM "SQL_OWNER_TERM") :optional t)
(constant ($SQL_PROCEDURE_TERM "SQL_PROCEDURE_TERM") :optional t)
(constant ($SQL_QUALIFIER_NAME_SEPARATOR "SQL_QUALIFIER_NAME_SEPARATOR") :optional t)
(constant ($SQL_QUALIFIER_TERM "SQL_QUALIFIER_TERM") :optional t)
(constant ($SQL_SCROLL_CONCURRENCY "SQL_SCROLL_CONCURRENCY") :optional t)
(constant ($SQL_SCROLL_OPTIONS "SQL_SCROLL_OPTIONS") :optional t)
(constant ($SQL_TABLE_TERM "SQL_TABLE_TERM") :optional t)
(constant ($SQL_TXN_CAPABLE "SQL_TXN_CAPABLE") :optional t)
(constant ($SQL_USER_NAME "SQL_USER_NAME") :optional t)

(constant ($SQL_CONVERT_FUNCTIONS "SQL_CONVERT_FUNCTIONS") :optional t)
(constant ($SQL_NUMERIC_FUNCTIONS "SQL_NUMERIC_FUNCTIONS") :optional t)
(constant ($SQL_STRING_FUNCTIONS "SQL_STRING_FUNCTIONS") :optional t)
(constant ($SQL_SYSTEM_FUNCTIONS "SQL_SYSTEM_FUNCTIONS") :optional t)
(constant ($SQL_TIMEDATE_FUNCTIONS "SQL_TIMEDATE_FUNCTIONS") :optional t)

(constant ($SQL_CONVERT_BIGINT "SQL_CONVERT_BIGINT") :optional t)
(constant ($SQL_CONVERT_BINARY "SQL_CONVERT_BINARY") :optional t)
(constant ($SQL_CONVERT_BIT "SQL_CONVERT_BIT") :optional t)
(constant ($SQL_CONVERT_CHAR "SQL_CONVERT_CHAR") :optional t)
(constant ($SQL_CONVERT_DATE "SQL_CONVERT_DATE") :optional t)
(constant ($SQL_CONVERT_DECIMAL "SQL_CONVERT_DECIMAL") :optional t)
(constant ($SQL_CONVERT_DOUBLE "SQL_CONVERT_DOUBLE") :optional t)
(constant ($SQL_CONVERT_FLOAT "SQL_CONVERT_FLOAT") :optional t)
(constant ($SQL_CONVERT_INTEGER "SQL_CONVERT_INTEGER") :optional t)
(constant ($SQL_CONVERT_LONGVARCHAR "SQL_CONVERT_LONGVARCHAR") :optional t)
(constant ($SQL_CONVERT_NUMERIC "SQL_CONVERT_NUMERIC") :optional t)
(constant ($SQL_CONVERT_REAL "SQL_CONVERT_REAL") :optional t)
(constant ($SQL_CONVERT_SMALLINT "SQL_CONVERT_SMALLINT") :optional t)
(constant ($SQL_CONVERT_TIME "SQL_CONVERT_TIME") :optional t)
(constant ($SQL_CONVERT_TIMESTAMP "SQL_CONVERT_TIMESTAMP") :optional t)
(constant ($SQL_CONVERT_TINYINT "SQL_CONVERT_TINYINT") :optional t)
(constant ($SQL_CONVERT_VARBINARY "SQL_CONVERT_VARBINARY") :optional t)
(constant ($SQL_CONVERT_VARCHAR "SQL_CONVERT_VARCHAR") :optional t)
(constant ($SQL_CONVERT_LONGVARBINARY "SQL_CONVERT_LONGVARBINARY") :optional t)

(constant ($SQL_TXN_ISOLATION_OPTION "SQL_TXN_ISOLATION_OPTION") :optional t)
(constant ($SQL_ODBC_SQL_OPT_IEF "SQL_ODBC_SQL_OPT_IEF") :optional t)

;;; ODBC SDK 1.0 Additions
(constant ($SQL_CORRELATION_NAME "SQL_CORRELATION_NAME") :optional t)
(constant ($SQL_NON_NULLABLE_COLUMNS "SQL_NON_NULLABLE_COLUMNS") :optional t)

;;; ODBC SDK 2.0 Additions
(constant ($SQL_DRIVER_HLIB "SQL_DRIVER_HLIB") :optional t)
(constant ($SQL_DRIVER_ODBC_VER "SQL_DRIVER_ODBC_VER") :optional t)
(constant ($SQL_LOCK_TYPES "SQL_LOCK_TYPES") :optional t)
(constant ($SQL_POS_OPERATIONS "SQL_POS_OPERATIONS") :optional t)
(constant ($SQL_POSITIONED_STATEMENTS "SQL_POSITIONED_STATEMENTS") :optional t)
(constant ($SQL_GETDATA_EXTENSIONS "SQL_GETDATA_EXTENSIONS") :optional t)
(constant ($SQL_BOOKMARK_PERSISTENCE "SQL_BOOKMARK_PERSISTENCE") :optional t)
(constant ($SQL_STATIC_SENSITIVITY "SQL_STATIC_SENSITIVITY") :optional t)
(constant ($SQL_FILE_USAGE "SQL_FILE_USAGE") :optional t)
(constant ($SQL_NULL_COLLATION "SQL_NULL_COLLATION") :optional t)
(constant ($SQL_ALTER_TABLE "SQL_ALTER_TABLE") :optional t)
(constant ($SQL_COLUMN_ALIAS "SQL_COLUMN_ALIAS") :optional t)
(constant ($SQL_GROUP_BY "SQL_GROUP_BY") :optional t)
(constant ($SQL_KEYWORDS "SQL_KEYWORDS") :optional t)
(constant ($SQL_ORDER_BY_COLUMNS_IN_SELECT "SQL_ORDER_BY_COLUMNS_IN_SELECT") :optional t)
(constant ($SQL_OWNER_USAGE "SQL_OWNER_USAGE") :optional t)
(constant ($SQL_QUALIFIER_USAGE "SQL_QUALIFIER_USAGE") :optional t)
(constant ($SQL_QUOTED_IDENTIFIER_CASE "SQL_QUOTED_IDENTIFIER_CASE") :optional t)
(constant ($SQL_SPECIAL_CHARACTERS "SQL_SPECIAL_CHARACTERS") :optional t)
(constant ($SQL_SUBQUERIES "SQL_SUBQUERIES") :optional t)
(constant ($SQL_UNION "SQL_UNION") :optional t)
(constant ($SQL_MAX_COLUMNS_IN_GROUP_BY "SQL_MAX_COLUMNS_IN_GROUP_BY") :optional t)
(constant ($SQL_MAX_COLUMNS_IN_INDEX "SQL_MAX_COLUMNS_IN_INDEX") :optional t)
(constant ($SQL_MAX_COLUMNS_IN_ORDER_BY "SQL_MAX_COLUMNS_IN_ORDER_BY") :optional t)
(constant ($SQL_MAX_COLUMNS_IN_SELECT "SQL_MAX_COLUMNS_IN_SELECT") :optional t)
(constant ($SQL_MAX_COLUMNS_IN_TABLE "SQL_MAX_COLUMNS_IN_TABLE") :optional t)
(constant ($SQL_MAX_INDEX_SIZE "SQL_MAX_INDEX_SIZE") :optional t)
(constant ($SQL_MAX_ROW_SIZE_INCLUDES_LONG "SQL_MAX_ROW_SIZE_INCLUDES_LONG") :optional t)
(constant ($SQL_MAX_ROW_SIZE "SQL_MAX_ROW_SIZE") :optional t)
(constant ($SQL_MAX_STATEMENT_LEN "SQL_MAX_STATEMENT_LEN") :optional t)
(constant ($SQL_MAX_TABLES_IN_SELECT "SQL_MAX_TABLES_IN_SELECT") :optional t)
(constant ($SQL_MAX_USER_NAME_LEN "SQL_MAX_USER_NAME_LEN") :optional t)
(constant ($SQL_MAX_CHAR_LITERAL_LEN "SQL_MAX_CHAR_LITERAL_LEN") :optional t)
(constant ($SQL_TIMEDATE_ADD_INTERVALS "SQL_TIMEDATE_ADD_INTERVALS") :optional t)
(constant ($SQL_TIMEDATE_DIFF_INTERVALS "SQL_TIMEDATE_DIFF_INTERVALS") :optional t)
(constant ($SQL_NEED_LONG_DATA_LEN "SQL_NEED_LONG_DATA_LEN") :optional t)
(constant ($SQL_MAX_BINARY_LITERAL_LEN "SQL_MAX_BINARY_LITERAL_LEN") :optional t)
(constant ($SQL_LIKE_ESCAPE_CLAUSE "SQL_LIKE_ESCAPE_CLAUSE") :optional t)
(constant ($SQL_QUALIFIER_LOCATION "SQL_QUALIFIER_LOCATION") :optional t)
(constant ($SQL_ACTIVE_ENVIRONMENTS "SQL_ACTIVE_ENVIRONMENTS") :optional t)

#|

/*** ODBC SDK 2.01 Additions ***/)
(defconstant $SQL_OJ_CAPABILITIES                        65003  ;; Temp value until ODBC 3.0

(constant ($SQL_INFO_LAST "SQL_INFO_LAST") :optional t)
(defconstant $SQL_INFO_DRIVER_START             1000

;; SQL_CONVERT_*  return value bitmasks
)
(constant ($SQL_CVT_CHAR "SQL_CVT_CHAR") :optional t)
(constant ($SQL_CVT_NUMERIC "SQL_CVT_NUMERIC") :optional t)
(constant ($SQL_CVT_DECIMAL "SQL_CVT_DECIMAL") :optional t)
(constant ($SQL_CVT_INTEGER "SQL_CVT_INTEGER") :optional t)
(constant ($SQL_CVT_SMALLINT "SQL_CVT_SMALLINT") :optional t)
(constant ($SQL_CVT_FLOAT "SQL_CVT_FLOAT") :optional t)
(constant ($SQL_CVT_REAL "SQL_CVT_REAL") :optional t)
(constant ($SQL_CVT_DOUBLE "SQL_CVT_DOUBLE") :optional t)
(constant ($SQL_CVT_VARCHAR "SQL_CVT_VARCHAR") :optional t)
(constant ($SQL_CVT_LONGVARCHAR "SQL_CVT_LONGVARCHAR") :optional t)
(constant ($SQL_CVT_BINARY "SQL_CVT_BINARY") :optional t)
(constant ($SQL_CVT_VARBINARY "SQL_CVT_VARBINARY") :optional t)
(constant ($SQL_CVT_BIT "SQL_CVT_BIT") :optional t)
(constant ($SQL_CVT_TINYINT "SQL_CVT_TINYINT") :optional t)
(constant ($SQL_CVT_BIGINT "SQL_CVT_BIGINT") :optional t)
(constant ($SQL_CVT_DATE "SQL_CVT_DATE") :optional t)
(constant ($SQL_CVT_TIME "SQL_CVT_TIME") :optional t)
(constant ($SQL_CVT_TIMESTAMP "SQL_CVT_TIMESTAMP") :optional t)
(constant ($SQL_CVT_LONGVARBINARY "SQL_CVT_LONGVARBINARY") :optional t)

;; SQL_CONVERT_FUNCTIONS functions)
(constant ($SQL_FN_CVT_CONVERT "SQL_FN_CVT_CONVERT") :optional t)

;; SQL_STRING_FUNCTIONS functions

(constant ($SQL_FN_STR_CONCAT "SQL_FN_STR_CONCAT") :optional t)
(constant ($SQL_FN_STR_INSERT "SQL_FN_STR_INSERT") :optional t)
(constant ($SQL_FN_STR_LEFT "SQL_FN_STR_LEFT") :optional t)
(constant ($SQL_FN_STR_LTRIM "SQL_FN_STR_LTRIM") :optional t)
(constant ($SQL_FN_STR_LENGTH "SQL_FN_STR_LENGTH") :optional t)
(constant ($SQL_FN_STR_LOCATE "SQL_FN_STR_LOCATE") :optional t)
(constant ($SQL_FN_STR_LCASE "SQL_FN_STR_LCASE") :optional t)
(constant ($SQL_FN_STR_REPEAT "SQL_FN_STR_REPEAT") :optional t)
(constant ($SQL_FN_STR_REPLACE "SQL_FN_STR_REPLACE") :optional t)
(constant ($SQL_FN_STR_RIGHT "SQL_FN_STR_RIGHT") :optional t)
(constant ($SQL_FN_STR_RTRIM "SQL_FN_STR_RTRIM") :optional t)
(constant ($SQL_FN_STR_SUBSTRING "SQL_FN_STR_SUBSTRING") :optional t)
(constant ($SQL_FN_STR_UCASE "SQL_FN_STR_UCASE") :optional t)
(constant ($SQL_FN_STR_ASCII "SQL_FN_STR_ASCII") :optional t)
(defconstant $SQL_FN_STR_CHAR                   #x00004000L
(constant ($SQL_FN_STR_DIFFERENCE "SQL_FN_STR_DIFFERENCE") :optional t)
(constant ($SQL_FN_STR_LOCATE_2 "SQL_FN_STR_LOCATE_2") :optional t)
(constant ($SQL_FN_STR_SOUNDEX "SQL_FN_STR_SOUNDEX") :optional t)
(defconstant $SQL_FN_STR_SPACE                  #x00040000L

;; SQL_NUMERIC_FUNCTIONS functions
)
(constant ($SQL_FN_NUM_ABS "SQL_FN_NUM_ABS") :optional t)
(constant ($SQL_FN_NUM_ACOS "SQL_FN_NUM_ACOS") :optional t)
(constant ($SQL_FN_NUM_ASIN "SQL_FN_NUM_ASIN") :optional t)
(constant ($SQL_FN_NUM_ATAN "SQL_FN_NUM_ATAN") :optional t)
(constant ($SQL_FN_NUM_ATAN2 "SQL_FN_NUM_ATAN2") :optional t)
(constant ($SQL_FN_NUM_CEILING "SQL_FN_NUM_CEILING") :optional t)
(constant ($SQL_FN_NUM_COS "SQL_FN_NUM_COS") :optional t)
(constant ($SQL_FN_NUM_COT "SQL_FN_NUM_COT") :optional t)
(constant ($SQL_FN_NUM_EXP "SQL_FN_NUM_EXP") :optional t)
(constant ($SQL_FN_NUM_FLOOR "SQL_FN_NUM_FLOOR") :optional t)
(constant ($SQL_FN_NUM_LOG "SQL_FN_NUM_LOG") :optional t)
(constant ($SQL_FN_NUM_MOD "SQL_FN_NUM_MOD") :optional t)
(constant ($SQL_FN_NUM_SIGN "SQL_FN_NUM_SIGN") :optional t)
(constant ($SQL_FN_NUM_SIN "SQL_FN_NUM_SIN") :optional t)
(constant ($SQL_FN_NUM_SQRT "SQL_FN_NUM_SQRT") :optional t)
(constant ($SQL_FN_NUM_TAN "SQL_FN_NUM_TAN") :optional t)
(constant ($SQL_FN_NUM_PI "SQL_FN_NUM_PI") :optional t)
(defconstant $SQL_FN_NUM_RAND                   #x00020000L
(constant ($SQL_FN_NUM_DEGREES "SQL_FN_NUM_DEGREES") :optional t)
(constant ($SQL_FN_NUM_LOG10 "SQL_FN_NUM_LOG10") :optional t)
(constant ($SQL_FN_NUM_POWER "SQL_FN_NUM_POWER") :optional t)
(constant ($SQL_FN_NUM_RADIANS "SQL_FN_NUM_RADIANS") :optional t)
(constant ($SQL_FN_NUM_ROUND "SQL_FN_NUM_ROUND") :optional t)
(defconstant $SQL_FN_NUM_TRUNCATE               #x00800000L

;; SQL_TIMEDATE_FUNCTIONS functions
)
(constant ($SQL_FN_TD_NOW "SQL_FN_TD_NOW") :optional t)
(constant ($SQL_FN_TD_CURDATE "SQL_FN_TD_CURDATE") :optional t)
(constant ($SQL_FN_TD_DAYOFMONTH "SQL_FN_TD_DAYOFMONTH") :optional t)
(constant ($SQL_FN_TD_DAYOFWEEK "SQL_FN_TD_DAYOFWEEK") :optional t)
(constant ($SQL_FN_TD_DAYOFYEAR "SQL_FN_TD_DAYOFYEAR") :optional t)
(constant ($SQL_FN_TD_MONTH "SQL_FN_TD_MONTH") :optional t)
(constant ($SQL_FN_TD_QUARTER "SQL_FN_TD_QUARTER") :optional t)
(constant ($SQL_FN_TD_WEEK "SQL_FN_TD_WEEK") :optional t)
(constant ($SQL_FN_TD_YEAR "SQL_FN_TD_YEAR") :optional t)
(constant ($SQL_FN_TD_CURTIME "SQL_FN_TD_CURTIME") :optional t)
(constant ($SQL_FN_TD_HOUR "SQL_FN_TD_HOUR") :optional t)
(constant ($SQL_FN_TD_MINUTE "SQL_FN_TD_MINUTE") :optional t)
(defconstant $SQL_FN_TD_SECOND                  #x00001000L
(constant ($SQL_FN_TD_TIMESTAMPADD "SQL_FN_TD_TIMESTAMPADD") :optional t)
(constant ($SQL_FN_TD_TIMESTAMPDIFF "SQL_FN_TD_TIMESTAMPDIFF") :optional t)
(constant ($SQL_FN_TD_DAYNAME "SQL_FN_TD_DAYNAME") :optional t)
(defconstant $SQL_FN_TD_MONTHNAME               #x00010000L

;; SQL_SYSTEM_FUNCTIONS functions
)
(constant ($SQL_FN_SYS_USERNAME "SQL_FN_SYS_USERNAME") :optional t)
(constant ($SQL_FN_SYS_DBNAME "SQL_FN_SYS_DBNAME") :optional t)
(defconstant $SQL_FN_SYS_IFNULL                 #x00000004L

;; SQL_TIMEDATE_ADD_INTERVALS and SQL_TIMEDATE_DIFF_INTERVALS functions

(constant ($SQL_FN_TSI_FRAC_SECOND "SQL_FN_TSI_FRAC_SECOND") :optional t)
(constant ($SQL_FN_TSI_SECOND "SQL_FN_TSI_SECOND") :optional t)
(constant ($SQL_FN_TSI_MINUTE "SQL_FN_TSI_MINUTE") :optional t)
(constant ($SQL_FN_TSI_HOUR "SQL_FN_TSI_HOUR") :optional t)
(constant ($SQL_FN_TSI_DAY "SQL_FN_TSI_DAY") :optional t)
(constant ($SQL_FN_TSI_WEEK "SQL_FN_TSI_WEEK") :optional t)
(constant ($SQL_FN_TSI_MONTH "SQL_FN_TSI_MONTH") :optional t)
(constant ($SQL_FN_TSI_QUARTER "SQL_FN_TSI_QUARTER") :optional t)
(defconstant $SQL_FN_TSI_YEAR                   #x00000100L

;; SQL_ODBC_API_CONFORMANCE values
)
(constant ($SQL_OAC_NONE "SQL_OAC_NONE") :optional t)
(constant ($SQL_OAC_LEVEL1 "SQL_OAC_LEVEL1") :optional t)
(defconstant $SQL_OAC_LEVEL2                            #x0002

;; SQL_ODBC_SAG_CLI_CONFORMANCE values
)
(constant ($SQL_OSCC_NOT_COMPLIANT "SQL_OSCC_NOT_COMPLIANT") :optional t)
(defconstant $SQL_OSCC_COMPLIANT                        #x0001

;; SQL_ODBC_SQL_CONFORMANCE values
)
(constant ($SQL_OSC_MINIMUM "SQL_OSC_MINIMUM") :optional t)
(constant ($SQL_OSC_CORE "SQL_OSC_CORE") :optional t)
(defconstant $SQL_OSC_EXTENDED                  #x0002

;; SQL_CONCAT_NULL_BEHAVIOR values
)
(constant ($SQL_CB_NULL "SQL_CB_NULL") :optional t)
(defconstant $SQL_CB_NON_NULL                   #x0001

;; SQL_CURSOR_COMMIT_BEHAVIOR and SQL_CURSOR_ROLLBACK_BEHAVIOR values
)
(constant ($SQL_CB_DELETE "SQL_CB_DELETE") :optional t)
(constant ($SQL_CB_CLOSE "SQL_CB_CLOSE") :optional t)
(defconstant $SQL_CB_PRESERVE                           #x0002

;; SQL_IDENTIFIER_CASE values
)
(constant ($SQL_IC_UPPER "SQL_IC_UPPER") :optional t)
(constant ($SQL_IC_LOWER "SQL_IC_LOWER") :optional t)
(constant ($SQL_IC_SENSITIVE "SQL_IC_SENSITIVE") :optional t)
(defconstant $SQL_IC_MIXED                              #x0004

;; SQL_TXN_CAPABLE values
|#

(constant ($SQL_TC_NONE "SQL_TC_NONE") :optional t)
(constant ($SQL_TC_DML "SQL_TC_DML") :optional t)
(constant ($SQL_TC_ALL "SQL_TC_ALL") :optional t)

(constant ($SQL_TC_DDL_COMMIT "SQL_TC_DDL_COMMIT") :optional t)
(constant ($SQL_TC_DDL_IGNORE "SQL_TC_DDL_IGNORE") :optional t)

;; SQL_SCROLL_OPTIONS masks


(constant ($SQL_SO_FORWARD_ONLY "SQL_SO_FORWARD_ONLY") :optional t)
(constant ($SQL_SO_KEYSET_DRIVEN "SQL_SO_KEYSET_DRIVEN") :optional t)
(constant ($SQL_SO_DYNAMIC "SQL_SO_DYNAMIC") :optional t)
(constant ($SQL_SO_MIXED "SQL_SO_MIXED") :optional t)
(constant ($SQL_SO_STATIC "SQL_SO_STATIC") :optional t)

;; SQL_SCROLL_CONCURRENCY masks

(constant ($SQL_SCCO_READ_ONLY "SQL_SCCO_READ_ONLY") :optional t)
(constant ($SQL_SCCO_LOCK "SQL_SCCO_LOCK") :optional t)
(constant ($SQL_SCCO_OPT_ROWVER "SQL_SCCO_OPT_ROWVER") :optional t)
(constant ($SQL_SCCO_OPT_VALUES "SQL_SCCO_OPT_VALUES") :optional t)

;; SQL_FETCH_DIRECTION masks

(constant ($SQL_FD_FETCH_NEXT "SQL_FD_FETCH_NEXT") :optional t)
(constant ($SQL_FD_FETCH_FIRST "SQL_FD_FETCH_FIRST") :optional t)
(constant ($SQL_FD_FETCH_LAST "SQL_FD_FETCH_LAST") :optional t)
(constant ($SQL_FD_FETCH_PRIOR "SQL_FD_FETCH_PRIOR") :optional t)
(constant ($SQL_FD_FETCH_ABSOLUTE "SQL_FD_FETCH_ABSOLUTE") :optional t)
(constant ($SQL_FD_FETCH_RELATIVE "SQL_FD_FETCH_RELATIVE") :optional t)
(constant ($SQL_FD_FETCH_RESUME "SQL_FD_FETCH_RESUME") :optional t)
(constant ($SQL_FD_FETCH_BOOKMARK "SQL_FD_FETCH_BOOKMARK") :optional t)

#|
;; SQL_TXN_ISOLATION_OPTION masks
)
(constant ($SQL_TXN_READ_UNCOMMITTED "SQL_TXN_READ_UNCOMMITTED") :optional t)
(constant ($SQL_TXN_READ_COMMITTED "SQL_TXN_READ_COMMITTED") :optional t)
(constant ($SQL_TXN_REPEATABLE_READ "SQL_TXN_REPEATABLE_READ") :optional t)
(constant ($SQL_TXN_SERIALIZABLE "SQL_TXN_SERIALIZABLE") :optional t)
(defconstant $SQL_TXN_VERSIONING                        #x00000010L

;; SQL_CORRELATION_NAME values
)
(constant ($SQL_CN_NONE "SQL_CN_NONE") :optional t)
(constant ($SQL_CN_DIFFERENT "SQL_CN_DIFFERENT") :optional t)
(defconstant $SQL_CN_ANY                                        #x0002

;; SQL_NON_NULLABLE_COLUMNS values
)
(constant ($SQL_NNC_NULL "SQL_NNC_NULL") :optional t)
(defconstant $SQL_NNC_NON_NULL                  #x0001

;; SQL_NULL_COLLATION values
                                                                          )
(constant ($SQL_NC_HIGH "SQL_NC_HIGH") :optional t)
(constant ($SQL_NC_LOW "SQL_NC_LOW") :optional t)
(constant ($SQL_NC_START "SQL_NC_START") :optional t)
(defconstant $SQL_NC_END                                        #x0004

;; SQL_FILE_USAGE values
)
(constant ($SQL_FILE_NOT_SUPPORTED "SQL_FILE_NOT_SUPPORTED") :optional t)
(constant ($SQL_FILE_TABLE "SQL_FILE_TABLE") :optional t)
(defconstant $SQL_FILE_QUALIFIER                        #x0002

;; SQL_GETDATA_EXTENSIONS values
)
(constant ($SQL_GD_ANY_COLUMN "SQL_GD_ANY_COLUMN") :optional t)
(constant ($SQL_GD_ANY_ORDER "SQL_GD_ANY_ORDER") :optional t)
(constant ($SQL_GD_BLOCK "SQL_GD_BLOCK") :optional t)
(defconstant $SQL_GD_BOUND                              #x00000008L

;; SQL_ALTER_TABLE values
)
(constant ($SQL_AT_ADD_COLUMN "SQL_AT_ADD_COLUMN") :optional t)
(defconstant $SQL_AT_DROP_COLUMN                        #x00000002L

;; SQL_POSITIONED_STATEMENTS masks
)
(constant ($SQL_PS_POSITIONED_DELETE "SQL_PS_POSITIONED_DELETE") :optional t)
(constant ($SQL_PS_POSITIONED_UPDATE "SQL_PS_POSITIONED_UPDATE") :optional t)
(defconstant $SQL_PS_SELECT_FOR_UPDATE  #x00000004L

;; SQL_GROUP_BY values
)
(constant ($SQL_GB_NOT_SUPPORTED "SQL_GB_NOT_SUPPORTED") :optional t)
(constant ($SQL_GB_GROUP_BY_EQUALS_SELECT "SQL_GB_GROUP_BY_EQUALS_SELECT") :optional t)
(constant ($SQL_GB_GROUP_BY_CONTAINS_SELECT "SQL_GB_GROUP_BY_CONTAINS_SELECT") :optional t)
(defconstant $SQL_GB_NO_RELATION                                #x0003

;; SQL_OWNER_USAGE masks
)
(constant ($SQL_OU_DML_STATEMENTS "SQL_OU_DML_STATEMENTS") :optional t)
(constant ($SQL_OU_PROCEDURE_INVOCATION "SQL_OU_PROCEDURE_INVOCATION") :optional t)
(constant ($SQL_OU_TABLE_DEFINITION "SQL_OU_TABLE_DEFINITION") :optional t)
(constant ($SQL_OU_INDEX_DEFINITION "SQL_OU_INDEX_DEFINITION") :optional t)
(defconstant $SQL_OU_PRIVILEGE_DEFINITION #x00000010L

;; SQL_QUALIFIER_USAGE masks
)
(constant ($SQL_QU_DML_STATEMENTS "SQL_QU_DML_STATEMENTS") :optional t)
(constant ($SQL_QU_PROCEDURE_INVOCATION "SQL_QU_PROCEDURE_INVOCATION") :optional t)
(constant ($SQL_QU_TABLE_DEFINITION "SQL_QU_TABLE_DEFINITION") :optional t)
(constant ($SQL_QU_INDEX_DEFINITION "SQL_QU_INDEX_DEFINITION") :optional t)
(defconstant $SQL_QU_PRIVILEGE_DEFINITION #x00000010L

;; SQL_SUBQUERIES masks
)
(constant ($SQL_SQ_COMPARISON "SQL_SQ_COMPARISON") :optional t)
(constant ($SQL_SQ_EXISTS "SQL_SQ_EXISTS") :optional t)
(constant ($SQL_SQ_IN "SQL_SQ_IN") :optional t)
(constant ($SQL_SQ_QUANTIFIED "SQL_SQ_QUANTIFIED") :optional t)
(defconstant $SQL_SQ_CORRELATED_SUBQUERIES      #x00000010L

;; SQL_UNION masks
)
(constant ($SQL_U_UNION "SQL_U_UNION") :optional t)
(defconstant $SQL_U_UNION_ALL                                   #x00000002L

;; SQL_BOOKMARK_PERSISTENCE values
)
(constant ($SQL_BP_CLOSE "SQL_BP_CLOSE") :optional t)
(constant ($SQL_BP_DELETE "SQL_BP_DELETE") :optional t)
(constant ($SQL_BP_DROP "SQL_BP_DROP") :optional t)
(constant ($SQL_BP_TRANSACTION "SQL_BP_TRANSACTION") :optional t)
(constant ($SQL_BP_UPDATE "SQL_BP_UPDATE") :optional t)
(constant ($SQL_BP_OTHER_HSTMT "SQL_BP_OTHER_HSTMT") :optional t)
(defconstant $SQL_BP_SCROLL                             #x00000040L

;; SQL_STATIC_SENSITIVITY values
)
(constant ($SQL_SS_ADDITIONS "SQL_SS_ADDITIONS") :optional t)
(constant ($SQL_SS_DELETIONS "SQL_SS_DELETIONS") :optional t)
(defconstant $SQL_SS_UPDATES                            #x00000004L

;; SQL_LOCK_TYPESL masks
)
(constant ($SQL_LCK_NO_CHANGE "SQL_LCK_NO_CHANGE") :optional t)
(constant ($SQL_LCK_EXCLUSIVE "SQL_LCK_EXCLUSIVE") :optional t)
(defconstant $SQL_LCK_UNLOCK                            #x00000004L

;; SQL_POS_OPERATIONS masks
|#

(constant ($SQL_POS_POSITION "SQL_POS_POSITION") :optional t) ;; #x00000001L
(constant ($SQL_POS_REFRESH "SQL_POS_REFRESH") :optional t)  ;; #x00000002L
(constant ($SQL_POS_UPDATE "SQL_POS_UPDATE") :optional t)   ;; #x00000004L
(constant ($SQL_POS_DELETE "SQL_POS_DELETE") :optional t)   ;; #x00000008L
(constant ($SQL_POS_ADD "SQL_POS_ADD") :optional t)     ;; #x00000010L

#|
;; SQL_QUALIFIER_LOCATION values
)
(constant ($SQL_QL_START "SQL_QL_START") :optional t)
(defconstant $SQL_QL_END                                        #x0002L

;; SQL_OJ_CAPABILITIES values

(constant ($SQL_OJ_LEFT "SQL_OJ_LEFT") :optional t)
(constant ($SQL_OJ_RIGHT "SQL_OJ_RIGHT") :optional t)
(constant ($SQL_OJ_FULL "SQL_OJ_FULL") :optional t)
(constant ($SQL_OJ_NESTED "SQL_OJ_NESTED") :optional t)
(constant ($SQL_OJ_NOT_ORDERED "SQL_OJ_NOT_ORDERED") :optional t)
(constant ($SQL_OJ_INNER "SQL_OJ_INNER") :optional t)
(defconstant $SQL_OJ_ALL_COMPARISON_OPS #x00000040L

;; options for SQLGetStmtOption/SQLSetStmtOption)
(constant ($SQL_QUERY_TIMEOUT "SQL_QUERY_TIMEOUT") :optional t)
(constant ($SQL_MAX_ROWS "SQL_MAX_ROWS") :optional t)
(constant ($SQL_NOSCAN "SQL_NOSCAN") :optional t)
(constant ($SQL_MAX_LENGTH "SQL_MAX_LENGTH") :optional t)
(constant ($SQL_ASYNC_ENABLE "SQL_ASYNC_ENABLE") :optional t)
(defconstant $SQL_BIND_TYPE                             5
(constant ($SQL_CURSOR_TYPE "SQL_CURSOR_TYPE") :optional t)
(constant ($SQL_CONCURRENCY "SQL_CONCURRENCY") :optional t)
(constant ($SQL_KEYSET_SIZE "SQL_KEYSET_SIZE") :optional t)
(constant ($SQL_ROWSET_SIZE "SQL_ROWSET_SIZE") :optional t)
(constant ($SQL_SIMULATE_CURSOR "SQL_SIMULATE_CURSOR") :optional t)
(constant ($SQL_RETRIEVE_DATA "SQL_RETRIEVE_DATA") :optional t)
(constant ($SQL_USE_BOOKMARKS "SQL_USE_BOOKMARKS") :optional t)
(defconstant $SQL_GET_BOOKMARK                  13      /*      GetStmtOption Only)
(defconstant $SQL_ROW_NUMBER                            14      /*      GetStmtOption Only)
(constant ($SQL_STMT_OPT_MAX "SQL_STMT_OPT_MAX") :optional t)
(defconstant $SQL_STMT_OPT_MIN                  SQL_QUERY_TIMEOUT


;; SQL_QUERY_TIMEOUT options)
(defconstant $SQL_QUERY_TIMEOUT_DEFAULT 0UL

;; SQL_MAX_ROWS options)
(defconstant $SQL_MAX_ROWS_DEFAULT              0UL

;; SQL_NOSCAN options)
(defconstant $SQL_NOSCAN_OFF                            0UL     /*      1.0 FALSE)
(defconstant $SQL_NOSCAN_ON                             1UL     /*      1.0 TRUE)
(defconstant $SQL_NOSCAN_DEFAULT                        SQL_NOSCAN_OFF

;; SQL_MAX_LENGTH options)
(defconstant $SQL_MAX_LENGTH_DEFAULT            0UL

;; SQL_ASYNC_ENABLE options)
(constant ($SQL_ASYNC_ENABLE_OFF "SQL_ASYNC_ENABLE_OFF") :optional t)
(constant ($SQL_ASYNC_ENABLE_ON "SQL_ASYNC_ENABLE_ON") :optional t)
(defconstant $SQL_ASYNC_ENABLE_DEFAULT  SQL_ASYNC_ENABLE_OFF

;; SQL_BIND_TYPE options)
(constant ($SQL_BIND_BY_COLUMN "SQL_BIND_BY_COLUMN") :optional t)
(defconstant $SQL_BIND_TYPE_DEFAULT             SQL_BIND_BY_COLUMN              ;; Default value

;; SQL_CONCURRENCY options)
(constant ($SQL_CONCUR_READ_ONLY "SQL_CONCUR_READ_ONLY") :optional t)
(constant ($SQL_CONCUR_LOCK "SQL_CONCUR_LOCK") :optional t)
(constant ($SQL_CONCUR_ROWVER "SQL_CONCUR_ROWVER") :optional t)
(constant ($SQL_CONCUR_VALUES "SQL_CONCUR_VALUES") :optional t)
(defconstant $SQL_CONCUR_DEFAULT                        SQL_CONCUR_READ_ONLY    ;; Default value

;; SQL_CURSOR_TYPE options)
(constant ($SQL_CURSOR_FORWARD_ONLY "SQL_CURSOR_FORWARD_ONLY") :optional t)
(constant ($SQL_CURSOR_KEYSET_DRIVEN "SQL_CURSOR_KEYSET_DRIVEN") :optional t)
(constant ($SQL_CURSOR_DYNAMIC "SQL_CURSOR_DYNAMIC") :optional t)
(constant ($SQL_CURSOR_STATIC "SQL_CURSOR_STATIC") :optional t)
(defconstant $SQL_CURSOR_TYPE_DEFAULT           SQL_CURSOR_FORWARD_ONLY ;; Default value

;; SQL_ROWSET_SIZE options)
(defconstant $SQL_ROWSET_SIZE_DEFAULT   1UL

;; SQL_KEYSET_SIZE options)
(defconstant $SQL_KEYSET_SIZE_DEFAULT           0UL

;; SQL_SIMULATE_CURSOR options)
(constant ($SQL_SC_NON_UNIQUE "SQL_SC_NON_UNIQUE") :optional t)
(constant ($SQL_SC_TRY_UNIQUE "SQL_SC_TRY_UNIQUE") :optional t)
(defconstant $SQL_SC_UNIQUE                             2UL

;; SQL_RETRIEVE_DATA options)
(constant ($SQL_RD_OFF "SQL_RD_OFF") :optional t)
(constant ($SQL_RD_ON "SQL_RD_ON") :optional t)
(defconstant $SQL_RD_DEFAULT                            SQL_RD_ON

;; SQL_USE_BOOKMARKS options)
(constant ($SQL_UB_OFF "SQL_UB_OFF") :optional t)
(constant ($SQL_UB_ON "SQL_UB_ON") :optional t)
(defconstant $SQL_UB_DEFAULT                            SQL_UB_OFF


|#

;; options for SQLSetConnectOption/SQLGetConnectOption)
(constant ($SQL_ACCESS_MODE "SQL_ACCESS_MODE") :optional t)
(constant ($SQL_AUTOCOMMIT "SQL_AUTOCOMMIT") :optional t)
(constant ($SQL_LOGIN_TIMEOUT "SQL_LOGIN_TIMEOUT") :optional t)
(constant ($SQL_OPT_TRACE "SQL_OPT_TRACE") :optional t)
(constant ($SQL_OPT_TRACEFILE "SQL_OPT_TRACEFILE") :optional t)
(constant ($SQL_TRANSLATE_DLL "SQL_TRANSLATE_DLL") :optional t)
(constant ($SQL_TRANSLATE_OPTION "SQL_TRANSLATE_OPTION") :optional t)
(constant ($SQL_TXN_ISOLATION "SQL_TXN_ISOLATION") :optional t)
(constant ($SQL_CURRENT_QUALIFIER "SQL_CURRENT_QUALIFIER") :optional t)
(constant ($SQL_ODBC_CURSORS "SQL_ODBC_CURSORS") :optional t)
(constant ($SQL_QUIET_MODE "SQL_QUIET_MODE") :optional t)
(constant ($SQL_PACKET_SIZE "SQL_PACKET_SIZE") :optional t)
(constant ($SQL_CONN_OPT_MAX "SQL_CONN_OPT_MAX") :optional t)
(constant ($SQL_CONNECT_OPT_DRVR_START "SQL_CONNECT_OPT_DRVR_START") :optional t)

;;#define       SQL_CONN_OPT_MIN                        SQL_ACCESS_MODE

;; SQL_ACCESS_MODE options
(constant ($SQL_MODE_READ_WRITE "SQL_MODE_READ_WRITE") :optional t) ; 0UL
(constant ($SQL_MODE_READ_ONLY "SQL_MODE_READ_ONLY") :optional t)  ; 1UL
(constant ($SQL_MODE_DEFAULT "SQL_MODE_DEFAULT") :optional t)

;; SQL_AUTOCOMMIT options)
(constant ($SQL_AUTOCOMMIT_OFF "SQL_AUTOCOMMIT_OFF") :optional t) ;0UL
(constant ($SQL_AUTOCOMMIT_ON "SQL_AUTOCOMMIT_ON") :optional t) ;1UL
(constant ($SQL_AUTOCOMMIT_DEFAULT "SQL_AUTOCOMMIT_DEFAULT") :optional t)

;; SQL_LOGIN_TIMEOUT options)
(constant ($SQL_LOGIN_TIMEOUT_DEFAULT "SQL_LOGIN_TIMEOUT_DEFAULT") :optional t) ; 15UL

;; SQL_OPT_TRACE options)
(constant ($SQL_OPT_TRACE_OFF "SQL_OPT_TRACE_OFF") :optional t) ; 0UL
(constant ($SQL_OPT_TRACE_ON "SQL_OPT_TRACE_ON") :optional t) ; 1UL
(constant ($SQL_OPT_TRACE_DEFAULT "SQL_OPT_TRACE_DEFAULT") :optional t)
; #ifndef SQL_OPT_TRACE_FILE_DEFAULT
; (defconstant $SQL_OPT_TRACE_FILE_DEFAULT      "\\SQL.LOG"
;; #endif

(constant ($SQL_CUR_USE_IF_NEEDED "SQL_CUR_USE_IF_NEEDED") :optional t) ; 0UL
(constant ($SQL_CUR_USE_ODBC "SQL_CUR_USE_ODBC") :optional t) ; 1UL
(constant ($SQL_CUR_USE_DRIVER "SQL_CUR_USE_DRIVER") :optional t) ; 2UL
(constant ($SQL_CUR_DEFAULT "SQL_CUR_DEFAULT") :optional t)

#|
;; Column types and scopes in SQLSpecialColumns. )
(constant ($SQL_BEST_ROWID "SQL_BEST_ROWID") :optional t)
(constant ($SQL_ROWVER "SQL_ROWVER") :optional t)
)
(constant ($SQL_SCOPE_CURROW "SQL_SCOPE_CURROW") :optional t)
(constant ($SQL_SCOPE_TRANSACTION "SQL_SCOPE_TRANSACTION") :optional t)
(defconstant $SQL_SCOPE_SESSION                 2

;; Defines for SQLSetPos)
(defconstant $SQL_ENTIRE_ROWSET                 0
|#

;; Operations in SQLSetPos

(constant ($SQL_POSITION "SQL_POSITION") :optional t) ;; 1.0 FALSE
(constant ($SQL_REFRESH "SQL_REFRESH") :optional t)  ;; 1.0 TRUE
(constant ($SQL_UPDATE "SQL_UPDATE") :optional t)
(constant ($SQL_DELETE "SQL_DELETE") :optional t)
(constant ($SQL_ADD "SQL_ADD") :optional t)

;; Lock options in SQLSetPos)
(constant ($SQL_LOCK_NO_CHANGE "SQL_LOCK_NO_CHANGE") :optional t) ;; 1.0 FALSE
(constant ($SQL_LOCK_EXCLUSIVE "SQL_LOCK_EXCLUSIVE") :optional t) ;; 1.0 TRUE
(constant ($SQL_LOCK_UNLOCK "SQL_LOCK_UNLOCK") :optional t)

;; SQLBindParameter extensions
(constant ($SQL_DEFAULT_PARAM "SQL_DEFAULT_PARAM") :optional t)
(constant ($SQL_IGNORE "SQL_IGNORE") :optional t)
(constant ($SQL_LEN_DATA_AT_EXEC_OFFSET "SQL_LEN_DATA_AT_EXEC_OFFSET") :optional t)
;(constant ($SQL_LEN_DATA_AT_EXEC(length) "SQL_LEN_DATA_AT_EXEC(length)") :optional t)

;; Special return values for SQLGetData
(constant ($SQL_NO_TOTAL "SQL_NO_TOTAL") :optional t)

#|
;; Macros for SQLSetPos)
(constant ($SQL_POSITION_TO(hstmt,irow) "SQL_POSITION_TO(hstmt,irow)") :optional t))
(constant ($SQL_LOCK_RECORD(hstmt,irow,fLock) "SQL_LOCK_RECORD(hstmt,irow,fLock)") :optional t))
(constant ($SQL_REFRESH_RECORD(hstmt,irow,fLock) "SQL_REFRESH_RECORD(hstmt,irow,fLock)") :optional t))
(constant ($SQL_UPDATE_RECORD(hstmt,irow) "SQL_UPDATE_RECORD(hstmt,irow)") :optional t))
(constant ($SQL_DELETE_RECORD(hstmt,irow) "SQL_DELETE_RECORD(hstmt,irow)") :optional t))
(constant ($SQL_ADD_RECORD(hstmt,irow) "SQL_ADD_RECORD(hstmt,irow)") :optional t)

; #ifndef RC_INVOKED

/*      This define is too large for RC)
(constant ($SQL_ODBC_KEYWORDS "SQL_ODBC_KEYWORDS") :optional t)
|#

(constant ($SQL_PARAM_TYPE_UNKNOWN "SQL_PARAM_TYPE_UNKNOWN") :optional t)
(constant ($SQL_PARAM_INPUT "SQL_PARAM_INPUT") :optional t)
(constant ($SQL_PARAM_INPUT_OUTPUT "SQL_PARAM_INPUT_OUTPUT") :optional t)
(constant ($SQL_RESULT_COL "SQL_RESULT_COL") :optional t)
(constant ($SQL_PARAM_OUTPUT "SQL_PARAM_OUTPUT") :optional t)
(constant ($SQL_RETURN_VALUE "SQL_RETURN_VALUE") :optional t)


;; Defines used by both Level 1 and Level 2 functions

;; generally useful constants
(constant ($SQL_MAX_OPTION_STRING_LENGTH "SQL_MAX_OPTION_STRING_LENGTH") :optional t)

;; Additional return codes)
(constant ($SQL_STILL_EXECUTING "SQL_STILL_EXECUTING") :optional t)
(constant ($SQL_NEED_DATA "SQL_NEED_DATA") :optional t)

;; SQL extended datatypes)
(constant ($SQL_DATE "SQL_DATE") :optional t)
(constant ($SQL_TIME "SQL_TIME") :optional t)
(constant ($SQL_TIMESTAMP "SQL_TIMESTAMP") :optional t)
(constant ($SQL_LONGVARCHAR "SQL_LONGVARCHAR") :optional t)
(constant ($SQL_BINARY "SQL_BINARY") :optional t)
(constant ($SQL_VARBINARY "SQL_VARBINARY") :optional t)
(constant ($SQL_LONGVARBINARY "SQL_LONGVARBINARY") :optional t)
(constant ($SQL_BIGINT "SQL_BIGINT") :optional t)
(constant ($SQL_TINYINT "SQL_TINYINT") :optional t)
(constant ($SQL_BIT "SQL_BIT") :optional t)

;; For ODBC3
(constant ($SQL_TYPE_DATE "SQL_TYPE_DATE") :optional t)
(constant ($SQL_TYPE_TIME "SQL_TYPE_TIME") :optional t)
(constant ($SQL_TYPE_TIMESTAMP "SQL_TYPE_TIMESTAMP") :optional t)

(constant ($SQL_INTERVAL_YEAR "SQL_INTERVAL_YEAR") :optional t)
(constant ($SQL_INTERVAL_MONTH "SQL_INTERVAL_MONTH") :optional t)
(constant ($SQL_INTERVAL_YEAR_TO_MONTH "SQL_INTERVAL_YEAR_TO_MONTH") :optional t)
(constant ($SQL_INTERVAL_DAY "SQL_INTERVAL_DAY") :optional t)
(constant ($SQL_INTERVAL_HOUR "SQL_INTERVAL_HOUR") :optional t)
(constant ($SQL_INTERVAL_MINUTE "SQL_INTERVAL_MINUTE") :optional t)
(constant ($SQL_INTERVAL_SECOND "SQL_INTERVAL_SECOND") :optional t)
(constant ($SQL_INTERVAL_DAY_TO_HOUR "SQL_INTERVAL_DAY_TO_HOUR") :optional t)
(constant ($SQL_INTERVAL_DAY_TO_MINUTE "SQL_INTERVAL_DAY_TO_MINUTE") :optional t)
(constant ($SQL_INTERVAL_DAY_TO_SECOND "SQL_INTERVAL_DAY_TO_SECOND") :optional t)
(constant ($SQL_INTERVAL_HOUR_TO_MINUTE "SQL_INTERVAL_HOUR_TO_MINUTE") :optional t)
(constant ($SQL_INTERVAL_HOUR_TO_SECOND "SQL_INTERVAL_HOUR_TO_SECOND") :optional t)
(constant ($SQL_INTERVAL_MINUTE_TO_SECOND "SQL_INTERVAL_MINUTE_TO_SECOND") :optional t)
(constant ($SQL_UNICODE "SQL_UNICODE") :optional t)
(constant ($SQL_TYPE_DRIVER_START "SQL_TYPE_DRIVER_START") :optional t)
(constant ($SQL_TYPE_DRIVER_END "SQL_TYPE_DRIVER_END") :optional t)


(constant ($SQL_SIGNED_OFFSET "SQL_SIGNED_OFFSET") :optional t)
(constant ($SQL_UNSIGNED_OFFSET "SQL_UNSIGNED_OFFSET") :optional t)

;; C datatype to SQL datatype mapping
(constant ($SQL_C_DATE "SQL_C_DATE") :optional t)
(constant ($SQL_C_TIME "SQL_C_TIME") :optional t)
(constant ($SQL_C_TIMESTAMP "SQL_C_TIMESTAMP") :optional t)
(constant ($SQL_C_BINARY "SQL_C_BINARY") :optional t)
(constant ($SQL_C_BIT "SQL_C_BIT") :optional t)
(constant ($SQL_C_TINYINT "SQL_C_TINYINT") :optional t)
(constant ($SQL_C_SBIGINT "SQL_C_SBIGINT") :optional t)
(constant ($SQL_C_SLONG "SQL_C_SLONG") :optional t) ;; SIGNED INTEGER
(constant ($SQL_C_SSHORT "SQL_C_SSHORT") :optional t) ;; SIGNED SMALLINT
(constant ($SQL_C_STINYINT "SQL_C_STINYINT") :optional t) ;; SIGNED TINYINT
(constant ($SQL_C_ULONG "SQL_C_ULONG") :optional t) ;; UNSIGNED INTEGER
(constant ($SQL_C_USHORT "SQL_C_USHORT") :optional t) ;; UNSIGNED SMALLINT
(constant ($SQL_C_UTINYINT "SQL_C_UTINYINT") :optional t) ;;UNSIGNED TINYINT
(constant ($SQL_C_BOOKMARK "SQL_C_BOOKMARK") :optional t) ;; BOOKMARK

;;; ODBC3
(constant ($SQL_C_TYPE_DATE "SQL_C_TYPE_DATE") :optional t)
(constant ($SQL_C_TYPE_TIME "SQL_C_TYPE_TIME") :optional t)
(constant ($SQL_C_TYPE_TIMESTAMP "SQL_C_TYPE_TIMESTAMP") :optional t)

;; Options for SQLDriverConnect
(constant ($SQL_DRIVER_NOPROMPT "SQL_DRIVER_NOPROMPT") :optional t)
(constant ($SQL_DRIVER_COMPLETE "SQL_DRIVER_COMPLETE") :optional t)
(constant ($SQL_DRIVER_PROMPT "SQL_DRIVER_PROMPT") :optional t)
(constant ($SQL_DRIVER_COMPLETE_REQUIRED "SQL_DRIVER_COMPLETE_REQUIRED") :optional t)

(constant ($SQL_MAX_CONN_OUT "SQL_MAX_CONN_OUT") :optional t)

;; Level 2 Functions

;; SQLExtendedFetch "fFetchType" values
(constant ($SQL_FETCH_NEXT "SQL_FETCH_NEXT") :optional t)
(constant ($SQL_FETCH_FIRST "SQL_FETCH_FIRST") :optional t)
(constant ($SQL_FETCH_LAST "SQL_FETCH_LAST") :optional t)
(constant ($SQL_FETCH_PRIOR "SQL_FETCH_PRIOR") :optional t)
(constant ($SQL_FETCH_ABSOLUTE "SQL_FETCH_ABSOLUTE") :optional t)
(constant ($SQL_FETCH_RELATIVE "SQL_FETCH_RELATIVE") :optional t)
(constant ($SQL_FETCH_BOOKMARK "SQL_FETCH_BOOKMARK") :optional t)

;;; ODBC3 constants, added by KMR

(constant ($SQL_ATTR_ODBC_VERSION "SQL_ATTR_ODBC_VERSION") :optional t)
(constant ($SQL_OV_ODBC2 "SQL_OV_ODBC2") :optional t)
(constant ($SQL_OV_ODBC3 "SQL_OV_ODBC3") :optional t)
(constant ($SQL_INDEX_UNIQUE "SQL_INDEX_UNIQUE") :optional t)
(constant ($SQL_INDEX_ALL "SQL_INDEX_ALL") :optional t)
(constant ($SQL_QUICK "SQL_QUICK") :optional t)
(constant ($SQL_ENSURE "SQL_ENSURE") :optional t)


