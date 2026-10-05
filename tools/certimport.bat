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
echo
echo This will import an X.509 SSL certificate into the keystore for the JVM
echo
echo Press Control+C to abort.
pause
SETLOCAL
set JAVA_SECURITY="%JAVA_HOME%\jre\lib\security"

rem -------------------------------------------------
rem 1) SET THE CERTIFICATE NAME AND ALIAS HERE
rem -------------------------------------------------
echo ^
echo Certificate name (e.g.: mycert.cer):
set /P CERT_NAME=
echo ^
echo Certificat alias (e.g.: mycert):
set /P CERT_ALIAS=
echo ...copying %~dp0%CERT_NAME% to target directory %JAVA_SECURITY%
xcopy /y /s %~dp0%CERT_NAME% %JAVA_SECURITY%

rem -------------------------------------------------
rem 2) SET THE KEYTOOL PASSWORD HERE
rem -------------------------------------------------
echo ^
echo Certificate password(changeit):
set /P KEYTOOL_PASS=

rem -------------------------------------------------
rem DO NOT EDIT BELOW THIS LINE
rem -------------------------------------------------
set CERT=%JAVA_SECURITY%\%CERT_NAME%
"%JAVA_HOME%\jre\bin\keytool" -import -trustcacerts -keystore %JAVA_SECURITY%\cacerts -storepass %KEYTOOL_PASS% -noprompt -alias %CERT_ALIAS% -file %CERT%
ENDLOCAL
pause