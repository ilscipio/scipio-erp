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
<#-- Code common to all the shop template files. -->
<#if !(fndShopCommonDefined!false)>

    <#-- userHasAccount is usually included by shop decorator, but want it here for fallback 
        and support for odd configurations - try to not depend too strictly on ShopActions in theme -->
    <#if !userHasAccount??>
        <#assign dummy = setContextField("permChecksSetGlobal", true)>
        <#assign dummy = Static["org.ofbiz.base.util.GroovyUtil"].runScriptAtLocation("component://shop/webapp/shop/WEB-INF/actions/common/CommonUserChecks.groovy", context)!>
        <#global userHasAccount = globalContext.userHasAccount>
    </#if>
    
    <#global fndShopCommonDefined = true>
</#if>