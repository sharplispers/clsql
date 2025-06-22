# CLSQL

CLSQL is a Common Lisp to SQL engine interface by Kevin M. Rosenberg. It
includes both functional and object oriented subsystems for data definition and
manipulation as well as an integrated symbolic SQL syntax.

CLSQL supports a number of RDBMS and uses the UFFI ibrary for compatibility
with Allegro CL, Lispworks, CMUCL, SBCL and OpenMCL.

This repository aims to continue maintenance and improvements to the CLSQL
library.

## Roadmap

- Port to native CFFI without cffi-uffi-compat.
- Modernize documentation or make it more accessible

## Historical information

The original documentation is available as a PDF file in doc/clsql.pdf and as
HTML files in doc/html.tar.gz.

CLSQL's original home is http://clsql.kpe.io. The repository was in
http://git.kpe.io/?p=clsql.git, but that appears to be down. You can view a
limited snapshot of it on the [Wayback Machine](https://web.archive.org/web/20240721214822/http://git.kpe.io/?p=clsql.git)

CLSQL has incorporated code from the following projects. As of 2004,
development has stopped on these incorporated projects.
- Pierre Mai's MaiSQL
- onShore Development's UncommonSQL
- Paul Meurer's SQL/ODBC
- Cadabra's Oracle interface

## Other known forks

- [UnwashedMeme/clsql](https://github.com/UnwashedMeme/clsql) - The original
  CLSQL README referenced this repository. It contains at least 2 diverging
  commits that merit consideration for integration.

## Similar projects

- [fukamachi/cl-dbi](https://github.com/fukamachi/cl-dbi) - Database independent
  interface for Common Lisp supporting SQLite3, PostgreSQL, and MySQL. See also
  [fukamachi/mito](https://github.com/fukamachi/mito), which provides
  higher-level abstractions such as object-relational mapping.
- [archimag/cl-mssql](https://github.com/archimag/cl-mssql) - A common library
  for interacting with MS SQL Server databases. (Uses FreeTDS library directly,
  as opposed to ODBC in CLSQL)
- [marijnh/Postmodern](https://github.com/marijnh/Postmodern) - A Common Lisp
  PostgreSQL programming interface
- [TeMPOraL/cl-sqlite](https://github.com/TeMPOraL/cl-sqlite) - Common Lisp
  binding for SQLite

## Developer test quickstart

### ODBC

#### SQL Server

You can test ODBC with SQL Server locally using podman. For example:

1. Install required packages
   - Debian
     ```bash
     sudo apt install podman libodbc2 unixodbc-dev tdsodbc
     ```
   - Fedora
     ```bash
     sudo dnf install podman unixODBC unixODBC-devel freetds-libs freetds-devel
     ```
2. Start SQL Server container
   ```bash
   podman run -e "ACCEPT_EULA=Y" -e "MSSQL_SA_PASSWORD=yourStrong(!)Password" -p 1433:1433 -d mcr.microsoft.com/mssql/server:2022-latest
   ```
3. Connect to server in Lisp using clsql-odbc
   ```lisp
   (asdf:load-system :clsql-odbc)
   (clsql:connect (list nil nil nil :connection-string "Server=localhost,1433;Driver=/usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so;UID=sa;PWD=yourStrong(!)Password"))
   ```
