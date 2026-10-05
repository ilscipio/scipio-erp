# Scipio Commerce
# Copyright (C) Ilscipio GmbH
#
# This file is part of Scipio Commerce. Scipio Commerce is free software: you
# can redistribute it and modify it under the terms of the GNU Affero General
# Public License, version 3, as published by the Free Software Foundation.
# Scipio Commerce is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
# for more details. You should have received a copy of the license with this
# work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
# A commercial license is available from Ilscipio GmbH.
#
# SPDX-License-Identifier: AGPL-3.0-only
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
