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

<@modal id="copyCategoryProductMembers">
    <@section title=uiLabelMap.ProductCopyProductCategoryMembersToAnotherCategory>
        <form method="post" action="<@pageUrl>copyCategoryProductMembers</@pageUrl>" name="copyCategoryProductMembersForm">
            <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
            <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
            <@row>
                <@cell columns=12>
                    <@field type="lookup" label=uiLabelMap.ProductTargetProductCategory formName="copyCategoryProductMembersForm" name="productCategoryIdTo" id="productCategoryIdTo" fieldFormName="LookupProductCategory"/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="datetime" label=uiLabelMap.ProductOptionalFilterWithDate name="validDate" value="" size="25" maxlength="30" id="validDate1"/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>                    
                    <@field type="generic" label=uiLabelMap.ProductIncludeSubCategories>
                        <@field type="radio" name="recurse" value="Y" checked=((recurse)!"" == "Y") label=uiLabelMap.CommonYes />
                        <@field type="radio" name="recurse" value="N" checked=((recurse)!"" == "N") label=uiLabelMap.CommonNo />
                    </@field>
                </@cell>
            </@row>
            <@field type="submit" text=uiLabelMap.CommonCopy class="+${styles.link_run_sys!} ${styles.action_copy!}" />
        </form>
    </@section>
</@modal>
<@script>
    $(document).ready(function() {
        try {
            let modalElem = $('#modal_copyCategoryProductMembers');
            ${modalControl('modalElem','open')}
        } catch(err) {
        }
    });
</@script>
