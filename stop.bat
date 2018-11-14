echo off
rem #####################################################################
rem This file is subject to the terms and conditions defined in the\n
rem files 'LICENSE' and 'NOTICE', which are part of this source\n
rem code package.
rem #####################################################################

%~d0
set OFBIZ_HOME=%~p0

rem ### Simple easy to read line
cd %OFBIZ_HOME%
echo on
"%JAVA_HOME%\bin\java" -jar ofbiz.jar -shutdown
echo off
rem ### If you would prefer the console output to be logged rather than displayed switch out the above line for this one
rem "%JAVA_HOME%\bin\java" -Xms128M -Xmx512M -XX:MaxPermSize=512m -jar ofbiz.jar > runtime\logs\console.log
 
