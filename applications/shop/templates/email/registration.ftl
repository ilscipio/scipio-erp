<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->

<#-- All fields from Person and UserLogin are available to this template -->
${person.firstName},
<br /><br />
${uiLabelMap.EcommerceThankForRegistering} MyStore.com. <#-- MyStore.com (not a variable why?) must be adapted - JLR 1/6/5 -->
<br /><br /><br />

${uiLabelMap.EcommerceAccountLogin} ${userLogin.userLoginId}<br />
${uiLabelMap.EcommercePassword}<br /><br />

${uiLabelMap.EcommerceLosePassword}<br /><br />

${uiLabelMap.EcommerceThankYou},<br />
MyStore.com <#-- MyStore.com (not a variable why?) must be adapted - JLR 1/6/5 -->
<br /><br />

