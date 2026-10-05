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
<#--
* 
* A set of HTML templating macros, part of standard Scipio Freemarker API.
* Automatically included at all times.
* Intended to be swappable.
*
* Overrides the default SCIPIO styles located in 
* htmlTemplate.ftl - ofbiz_foundation/framework/common/webcommon/scipio/lib/standard/
* 
-->

<#-- Master include: Includes the default macros and allows overrides -->
<#include "component://common/webcommon/includes/scipio/lib/standard/htmlTemplate.ftl"> 

<#-- 
*************
* MACRO OVERRIDES
************
See component://common/webcommon/includes/scipio/lib/standard/htmlTemplate.ftl for documentation about
the macro interfaces used in the standard macros and these overrides.
 -->

<#-- (currently no overrides) -->


<#-- save copy of this namespace so that our macros are able to access its own definitions without overrides (sometimes needed) -->
<#assign scipioShopTmplLib = copyObject(.namespace)>
