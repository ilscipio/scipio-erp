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
@echo %time%
dependency-check -project OFBiz -scan C:\projectASF-Mars\ofbiz --suppression C:\tools\dependency-check\suppress.xml
@echo %time%