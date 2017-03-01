echo off
rem #####################################################################
rem Licensed to the Apache Software Foundation (ASF) under one
rem or more contributor license agreements.  See the NOTICE file
rem distributed with this work for additional information
rem regarding copyright ownership.  The ASF licenses this file
rem to you under the Apache License, Version 2.0 (the
rem "License"); you may not use this file except in compliance
rem with the License.  You may obtain a copy of the License at
rem
rem http://www.apache.org/licenses/LICENSE-2.0
rem
rem Unless required by applicable law or agreed to in writing,
rem software distributed under the License is distributed on an
rem "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
rem KIND, either express or implied.  See the License for the
rem specific language governing permissions and limitations
rem under the License.
rem #####################################################################

%~d0
set SCIPIO_HOME=%~p0
:MENU
CLS
ECHO   _____    _____   _   _____    _    ____      _____   _____    _____
ECHO  / ____^|  / ____^| ^| ^| ^|  __ \  ^| ^|  / __ \    ^|  ___^| ^|  __ \  ^|  __ \
ECHO ^| (___   ^| ^|      ^| ^| ^| ^|__) ^| ^| ^| ^| ^|  ^| ^|   ^| ^|___  ^| ^|__) ^| ^| ^|__) ^|
ECHO  \___ \  ^| ^|      ^| ^| ^|  ___/  ^| ^| ^| ^|  ^| ^|   ^|  ___^| ^|  _  /  ^|  ___/
ECHO  ____) ^| ^| ^|____  ^| ^| ^| ^|      ^| ^| ^| ^|__^| ^|   ^| ^|___  ^| ^| \ \  ^| ^|
ECHO ^|_____/   \_____^| ^|_^| ^|_^|      ^|_^|  \____/    ^|_____^| ^|_^|  \_\ ^|_^|
ECHO. 
ECHO.
ECHO. 
ECHO ============ INSTALLER ==============
ECHO.
ECHO Please make a selection
ECHO -------------------------------------
ECHO 1.  Install for development [compile, load seed ^& demo data]
ECHO 2.  Install for production [compile, load seed  ^& create-admin-user-login]
ECHO -------------------------------------
ECHO 3.  Recompile [compile]
ECHO 4.  List compiler information 
ECHO.
ECHO ==========PRESS 'Q' TO QUIT==========
ECHO.

SET INPUT=
SET /P "INPUT=Please select a number: "

IF /I '%INPUT%'=='1' GOTO Selection1
IF /I '%INPUT%'=='2' GOTO Selection2
IF /I '%INPUT%'=='3' GOTO Selection3
IF /I '%INPUT%'=='4' GOTO Selection4
IF /I '%INPUT%'=='Q' GOTO Quit

CLS

ECHO ============INVALID INPUT============
ECHO -------------------------------------
ECHO Please select a number from the Main
ECHO Menu [1-4] or select 'Q' to quit.
ECHO -------------------------------------
ECHO ======PRESS ANY KEY TO CONTINUE======

PAUSE > NUL
GOTO MENU

:Selection1

ant build load-demo

:Selection2

ant build load-extseed create-admin-user-login

:Selection3

ant build

:Selection4

ant -p

:Quit
CLS

ECHO ==============THANK YOU==============
ECHO. 
ECHO To start the application, type:
ECHO Linux ./start.sh
ECHO OS X: bash ./start.sh
ECHO Windows: start.bat
ECHO. 
ECHO ======PRESS ANY KEY TO CONTINUE======

PAUSE>NUL
EXIT
