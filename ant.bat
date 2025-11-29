@ECHO OFF
SETLOCAL EnableDelayedExpansion
REM #####################################################################
REM # Licensed to the Apache Software Foundation (ASF) under one
REM # or more contributor license agreements.  See the NOTICE file
REM # distributed with this work for additional information
REM # regarding copyright ownership.  The ASF licenses this file
REM # to you under the Apache License, Version 2.0 (the
REM # "License"); you may not use this file except in compliance
REM # with the License.  You may obtain a copy of the License at
REM #
REM # http://www.apache.org/licenses/LICENSE-2.0
REM #
REM # Unless required by applicable law or agreed to in writing,
REM # software distributed under the License is distributed on an
REM # "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
REM # KIND, either express or implied.  See the License for the
REM # specific language governing permissions and limitations
REM # under the License.
REM #####################################################################

IF DEFINED JAVA_HOME (
  SET JAVA="%JAVA_HOME%\bin\java"
) ELSE (
  SET JAVA="java"
)

SET TOP=%~dp0
SET LAUNCHER_JAR=
SET BASE_LIB=%TOP%\framework\base\lib
SET ANT_LIB=%BASE_LIB%\ant
SET ANT_JS_DIR=%BASE_LIB%\ant-ext\js
REM SCIPIO: NOTE: 2017-02-02: launcher is now under ant subfolder
FOR %%G IN (%ANT_LIB%\ant-*-ant-launcher.jar) DO SET LAUNCHER_JAR=%%G
REM ECHO %LAUNCHER_JAR%
REM FIXME: 2018-03-26: oro: adding libs outside base/lib/ant for ivy from within builds does not work without this
FOR %%G IN (%BASE_LIB%\oro-*.jar) DO SET ORO_LIB=%%G
IF [%LAUNCHER_JAR%] == [] (
  ECHO "Couldn't find ant-launcher.jar"
  EXIT /B 1
)

REM SCIPIO: Check for JDK 15+ and download Nashorn JS engine if needed
FOR /F "tokens=3" %%G IN ('%JAVA% -version 2^>^&1 ^| findstr /i "version"') DO SET JAVA_VER_RAW=%%G
SET JAVA_VER=!JAVA_VER_RAW:"=!
REM Extract major version (handles both "1.x" and "x.y" formats)
FOR /F "tokens=1 delims=." %%A IN ("!JAVA_VER!") DO SET JAVA_MAJOR=%%A
IF "!JAVA_MAJOR!"=="1" (
  FOR /F "tokens=2 delims=." %%A IN ("!JAVA_VER!") DO SET JAVA_MAJOR=%%A
)
REM Check if major version is 15 or higher and Nashorn JARs are missing
SET NEED_NASHORN=0
IF !JAVA_MAJOR! GEQ 15 SET NEED_NASHORN=1
IF !NEED_NASHORN!==1 (
  SET JS_JAR_EXISTS=0
  IF EXIST "%ANT_JS_DIR%\*.jar" (
    FOR %%G IN ("%ANT_JS_DIR%\*.jar") DO SET JS_JAR_EXISTS=1
  )
  IF !JS_JAR_EXISTS!==0 (
    ECHO Scipio: JDK15+ detected; fetching Ant JS script engine (if issue occurs, try ant.bat download-ant-js^)
    %JAVA% -jar "%LAUNCHER_JAR%" -lib "%ANT_LIB%" -lib "%ORO_LIB%" download-ant-js
  )
)

ECHO %JAVA% -jar "%LAUNCHER_JAR%" -lib "%ANT_LIB%" -lib "%ORO_LIB%" %*
%JAVA% -jar "%LAUNCHER_JAR%" -lib "%ANT_LIB%" -lib "%ORO_LIB%" %*
ENDLOCAL

