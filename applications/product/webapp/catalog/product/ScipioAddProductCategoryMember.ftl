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

<@section title=sectionTitle>
    <form method="post" name="AddProductCategoryMember" action="<@pageUrl>addProductToCategory</@pageUrl>">
        <input type="hidden" name="productId" value="${parameters.productId!}" />    
        <@row>
            <@cell columns=12>
                <@field type="lookup" id="productCategoryId" name="productCategoryId" label=uiLabelMap.CommonCategory required=true formName="AddProductCategoryMember" fieldFormName="LookupProductCategory" />
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" size="25" maxlength="30" id="fromDate"/>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" size="25" maxlength="30" id="thruDate"/>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="input" name="sequenceNum" label=uiLabelMap.CommonSequenceNum  size=20 maxlength=40 />
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="text" name="sortPriority" label=uiLabelMap.CommonSortPriority size=20 />
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="text" name="quantity" label=uiLabelMap.ProductQuantity size=20 />
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
                <@field type="textarea" name="comments" label=uiLabelMap.ProductComments rows=5 columns=5 />
            </@cell>
        </@row>
        <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}" />
    </form>
</@section>