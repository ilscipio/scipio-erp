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
<@section title=uiLabelMap.ProductVirtualFieldGroup>

    <@table type="fields">
        <#if product.isVirtual?has_content && product.isVirtual=="Y">
            <@tr>
                <@td class="${styles.grid_large!}2">${uiLabelMap.ProductVirtualProduct}
                </@td>
                <@td colspan="3">
                    <#if product.isVirtual=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if>
                    <#if product.virtualVariantMethodEnum?has_content>
                        <#assign virtualVariantEnum = product.getRelatedOne("VirtualVariantMethodEnumeration", true)/>
                        (${(virtualVariantEnum.get("description",locale))!})
                    </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.isVariant?has_content && product.isVariant=="Y">
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductVariantProduct}
              </@td>
              <@td colspan="3"><#if product.isVariant=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>
    </@table>


</@section>