REM Scipio Commerce
REM Copyright (C) Ilscipio GmbH
REM
REM This file is part of Scipio Commerce. Scipio Commerce is free software: you
REM can redistribute it and modify it under the terms of the GNU Affero General
REM Public License, version 3, as published by the Free Software Foundation.
REM Scipio Commerce is distributed in the hope that it will be useful, but
REM WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
REM FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
REM for more details. You should have received a copy of the license with this
REM work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
REM A commercial license is available from Ilscipio GmbH.
REM
REM SPDX-License-Identifier: AGPL-3.0-only
@echo off
setlocal enabledelayedexpansion

echo === Annotation Screen Testing ===
echo.

REM Cookie jar for session persistence
echo. > cookies.txt
echo. > errors.log

REM Test endpoints per component
set PASS=0
set FAIL=0

REM WebTools
call :test_endpoint webtools/control/main
call :test_endpoint webtools/control/entitymaint
call :test_endpoint webtools/control/ServiceList

REM CMS
call :test_endpoint cms/control/main
call :test_endpoint cms/control/pages
call :test_endpoint cms/control/editPage

REM Order Manager
call :test_endpoint ordermgr/control/main
call :test_endpoint ordermgr/control/findorders

REM Party Manager
call :test_endpoint partymgr/control/main
call :test_endpoint partymgr/control/viewprofile

REM Catalog
call :test_endpoint catalog/control/main
call :test_endpoint catalog/control/FindProduct

REM Accounting
call :test_endpoint accounting/control/main
call :test_endpoint accounting/control/findInvoices

echo.
echo ============================================
echo Results: !PASS! passed, !FAIL! failed
echo ============================================
if exist errors.log (
    for %%A in (errors.log) do if %%~zA gtr 0 (
        echo.
        echo Errors logged to errors.log
    )
)
goto :eof

:test_endpoint
set URL=https://localhost:8443/%~1
curl -s -k -L -c cookies.txt -b cookies.txt -o response.html -w "%%{http_code}" "%URL%" > status.txt 2>&1
set /p STATUS=<status.txt
if "!STATUS!"=="200" (
    echo [PASS] %~1
    set /a PASS+=1
) else (
    echo [FAIL] %~1 - HTTP !STATUS!
    set /a FAIL+=1
    echo === %~1 === >> errors.log
    findstr /i "error exception Error Exception" response.html >> errors.log 2>&1
)
goto :eof
