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

    <@section title=uiLabelMap.ProductApplyFeatureGroupToCategory>
        <form method="post" action="<@pageUrl>createProductFeatureCategoryAppl</@pageUrl>" name="addNewCategoryForm">
          <@fields type="default-nolabelarea">
            <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
            <@row>
                <@cell columns=6>
                    <@field type="select" label=uiLabelMap.ProductFeature name="productFeatureCategoryId">
                            <#list productFeatureCategories as productFeatureCategory>
                                <option value="${(productFeatureCategory.productFeatureCategoryId)!}">${(productFeatureCategory.description)!} [${(productFeatureCategory.productFeatureCategoryId)!}]</option>
                            </#list>
                    </@field>
                </@cell>
                <@cell columns=6>
                    <@field type="datetime" label=uiLabelMap.CommonFrom name="fromDate" value="" size="25" maxlength="30" id="fromDate2"/>
                </@cell>
            </@row>
            <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}" />
          </@fields>
        </form>
    </@section>