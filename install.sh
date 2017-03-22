#!/bin/bash
# #####################################################################
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
# #####################################################################
echo "   _____    _____   _   _____    _    ____      _____   _____    _____"
echo "  / ____|  / ____| | | |  __ \\  | |  / __ \\    |  ___| |  __ \\  |  __ \\"
echo " | (___   | |      | | | |__) | | | | |  | |   | |___  | |__) | | |__) |"
echo "  \\___ \\  | |      | | |  ___/  | | | |  | |   |  ___| |  _  /  |  ___/"
echo "  ____) | | |____  | | | |      | | | |__| |   | |___  | | \\ \\  | |"
echo " |_____/   \\_____| |_| |_|      |_|  \\____/    |_____| |_|  \\_\\ |_|"
echo ""
echo ""
echo ""
echo " ============ INSTALLER =============="
echo ""
echo " Please make a selection"
echo " -------------------------------------"
echo " 1.  Install for development [compile, load seed & demo data]"
echo " 2.  Install for production [compile, load seed & create-admin-user-login]"
echo " -------------------------------------";
echo " 3.  Recompile [compile]"
echo " 4.  List ant compiler information"
echo " 5.  Exit"
echo ""
echo " ====================================="
echo ""

PS3='Please select a number: '
options=("Install for Development" "Install for Production" "Recompile" "List ant info" "Quit")
select opt in "${options[@]}"
do
    case $opt in
        "Install for Development")
            sh ant load-demo
            exit 1
            ;;
        "Install for Production")
            sh ant load-extseed create-admin-user-login
            exit 1
            ;;
        "Recompile")
            sh ant build
            exit 1
            ;;
        "List ant info")
            sh ant -p
            exit 1
            ;;
        "Quit")
            exit 1;
            ;;
        *) echo '' invalid option;;
    esac
done
exit 1
