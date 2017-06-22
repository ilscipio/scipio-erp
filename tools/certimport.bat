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