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
    <#-- General info -->
    <@table type="fields">
        <#-- 
        <#if product.productId?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductId}
              </@td>
              <@td colspan="3">${product.productId}</@td>
            </@tr>
        </#if>-->

        <#if product.largeImageUrl?has_content>
                <@tr>
                  <@td class="${styles.grid_large!}2">${uiLabelMap.ProductLargeImage}</@td>
                  <@td colspan="3"><@img src=(product.largeImageUrl!) height="150px" width="100px" type="contain"/></@td>
                </@tr>
        </#if>

        <#if product.productName?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductName}
              </@td>
              <@td colspan="3">${product.productName}</@td>
            </@tr>
        </#if>

        <#if product.internalName?has_content && (product.productName?has_content && product.productName != product.internalName)>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductInternalName}
              </@td>
              <@td colspan="3">${product.internalName}</@td>
            </@tr>
        </#if>

        <#if product.brandName?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductBrandName}
              </@td>
              <@td colspan="3">${product.brandName}</@td>
            </@tr>
        </#if>

        <#if product.productTypeId?has_content>
            <#assign productType = product.getRelatedOne("ProductType", true) />
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductType}
              </@td>
              <@td colspan="3">${(productType.get("description",locale))!}</@td>
            </@tr>
        </#if>

        <#if product.manufacturerPartyId?has_content>
            <#assign manufacturerParty = product.getRelatedOne("ManufacturerParty", false)!/>
            <#assign manufacturer = manufacturerParty.getRelatedOne("PartyGroup", false)!/>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductOemPartyId}
              </@td>
              <@td colspan="3"><a href="<@serverUrl>/partymgr/control/viewprofile?partyId=${product.manufacturerPartyId!}</@serverUrl>">${(manufacturer.get("description",locale))!product.manufacturerPartyId}</a></@td>
            </@tr>
        </#if>

        <#if product.comments?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.Productomments}
              </@td>
              <@td colspan="3">${product.comments!}</@td>
            </@tr>    
        </#if>
    </@table>
</@section>