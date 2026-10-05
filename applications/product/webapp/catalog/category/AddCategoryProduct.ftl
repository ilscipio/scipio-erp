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

<@section>
    <form method="post" action="<@pageUrl>addCategoryProductMember</@pageUrl>" name="addProductCategoryMemberForm">
        <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
        <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
        <@row>
            <@cell columns=12>
                <@field type="lookup" label=uiLabelMap.CommonProduct required=true formName="addProductCategoryMemberForm" name="productId" id="productId" fieldFormName="LookupProduct"/>
            </@cell>
        </@row>
        <@row>
             <@cell columns=12>
                <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value="" size="25" maxlength="30" id="fromDate1"/>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="textarea" label=uiLabelMap.CommonComments name="comments" rows="2" cols="40"></@field>
            </@cell>
        </@row>
        <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}" />
    </form>
</@section>