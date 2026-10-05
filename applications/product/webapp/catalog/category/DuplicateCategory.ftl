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
<#-- TODO: License -->

<@modal id="duplicateProductCategory">
<#if productCategoryId?has_content>
    <@section title=uiLabelMap.ProductDuplicateCategory>
        <form action="<@pageUrl>DuplicateProductCategory</@pageUrl>" method="post">
            <input type="hidden" name="oldProductCategoryId" value="${productCategoryId}"/>
            <@field type="input" label=uiLabelMap.ProductDuplicateProductCategorySelected size="20" maxlength="20" name="productCategoryId"/>
                     
            <@field type="generic" label=uiLabelMap.CommonDuplicate>
                <@field type="checkbox" name="duplicateContent" value="Y" checked=true  label=uiLabelMap.ProductCategoryContent/>
                <@field type="checkbox" name="duplicateParentRollup" value="Y" checked=true  label=uiLabelMap.ProductCategoryRollupParentCategories/>
                <@field type="checkbox" name="duplicateChildRollup" value="Y"  label=uiLabelMap.ProductCategoryRollupChildCategories/>
                <@field type="checkbox" name="duplicateMembers" value="Y" checked=true  label=uiLabelMap.ProductProducts/>
                <@field type="checkbox" name="duplicateCatalogs" value="Y" checked=true  label=uiLabelMap.ProductCatalogs/>
                <@field type="checkbox" name="duplicateFeatures" value="Y" checked=true  label=uiLabelMap.ProductFeatures/>
                <@field type="checkbox" name="duplicateRoles" value="Y" checked=true  label=uiLabelMap.PartyParties/>
                <@field type="checkbox" name="duplicateAttributes" value="Y" checked=true  label=uiLabelMap.ProductAttributes/>
            </@field> 
      
            <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_copy!}" text=uiLabelMap.CommonSubmit/>  
        </form>
    </@section>
</#if>
</@modal>
<@script>
    $(document).ready(function() {
        try {
            let modalElem = $('#modal_duplicateProductCategory');
            ${modalControl('modalElem','open')}
        } catch(err) {
        }
    });
</@script>