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
<#-- SCIPIO: New easy-to-use form for category simple text content localized fields  -->

<#include "component://product/webapp/catalog/common/common.ftl">

    <#if stcErrorMsg?has_content>
        <@alert type="error">${stcErrorMsg}</@alert>
    </#if>

    <form action="<@pageUrl>updateProductCategoryContentStcLocFields</@pageUrl>" method="post" id="updatePccStdLocFieldsForm" name="updatePccStdLocFieldsForm">
        <input type="hidden" name="updatePccStcLocFields" value="Y"/>
        <input type="hidden" name="productCategoryId" value="${productCategoryId!}"/>
        
        <@cataloglib.catalogStcLocFields objectType="category" values=(categoryStcViewsByType!{}) params=parameters />
        
        <@field type="submit" name="Update" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}"/>
    </form>

    
