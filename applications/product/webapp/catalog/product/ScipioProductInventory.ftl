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
<@section>
    <@table type="fields">
        <#-- inventory -->
        <#if product.salesDiscWhenNotAvail?has_content && product.salesDiscWhenNotAvail=="Y">
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductSalesDiscontinuationNotAvailable}
              </@td>
              <@td colspan="3"><#if product.salesDiscWhenNotAvail=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#if product.requirementMethodEnumId?has_content>
            <#assign productRequirement = product.getRelatedOne("RequirementMethodEnumeration", true) />
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductRequirementMethodEnumId}
              </@td>
              <@td colspan="3">${(productRequirement.get("description",locale))?default(product.requirementMethodEnumId)!}</@td>
            </@tr>
        </#if>

        <#if product.lotIdFilledIn?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductLotId}
              </@td>
              <@td colspan="3">${product.lotIdFilledIn!""}</@td>
            </@tr>
        </#if>

        <#if product.inventoryMessage?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductInventoryMessage}
              </@td>
              <@td colspan="3">${product.inventoryMessage!""}</@td>
            </@tr>
        </#if>
    </@table>


</@section>