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
echo === Checking Annotation Errors in Logs ===
echo.

set LOGDIR=runtime\logs
if not exist %LOGDIR% (
    echo Log directory not found: %LOGDIR%
    exit /b 1
)

echo --- Screen Not Found Errors ---
findstr /i /c:"Could not find screen" %LOGDIR%\ofbiz.log 2>nul
findstr /i /c:"Screen not found" %LOGDIR%\ofbiz.log 2>nul
findstr /i /c:"screen with name" %LOGDIR%\ofbiz.log 2>nul

echo.
echo --- Annotation Loading Errors ---
findstr /i /c:"Error loading annotation" %LOGDIR%\ofbiz.log 2>nul
findstr /i /c:"Error creating screens from annotations" %LOGDIR%\ofbiz.log 2>nul
findstr /i /c:"annotation screen" %LOGDIR%\ofbiz.log | findstr /i "error" 2>nul

echo.
echo --- Widget Rendering Errors ---
findstr /i /c:"Error rendering" %LOGDIR%\ofbiz.log 2>nul
findstr /i /c:"Widget rendering error" %LOGDIR%\ofbiz.log 2>nul
findstr /i /c:"render error" %LOGDIR%\ofbiz.log 2>nul

echo.
echo --- Form Not Found Errors ---
findstr /i /c:"Could not find form" %LOGDIR%\ofbiz.log 2>nul
findstr /i /c:"Form not found" %LOGDIR%\ofbiz.log 2>nul

echo.
echo --- General Exceptions ---
findstr /i "NullPointerException" %LOGDIR%\ofbiz.log 2>nul | head -5
findstr /i "IllegalArgumentException" %LOGDIR%\ofbiz.log 2>nul | head -5
findstr /i "ClassNotFoundException" %LOGDIR%\ofbiz.log 2>nul | head -5

echo.
echo --- Location Alias Issues ---
findstr /i "location alias" %LOGDIR%\ofbiz.log 2>nul
findstr /i "getScreenFromLocationAlias" %LOGDIR%\ofbiz.log 2>nul

echo.
echo === Done ===
