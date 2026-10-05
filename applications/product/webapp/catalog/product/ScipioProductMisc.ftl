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
        <#if product.returnable?has_content && product.returnable=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductReturnable}
              </@td>
              <@td colspan="3"><#if product.returnable=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#-- marketing -->
        <#if product.includeInPromotions?has_content && product.includeInPromotions=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductIncludePromotions}
              </@td>
              <@td colspan="3"><#if product.includeInPromotions=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#--
        <#if product.contentInfoText?has_content && product.contentInfoText=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductContentInfoText}
              </@td>
              <@td colspan="3"><#if product.contentInfoText=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>-->

        <#-- tax -->
        <#if product.taxable?has_content && product.taxable=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductTaxable}
              </@td>
              <@td colspan="3"><#if product.taxable=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>
    </@table>


</@section>