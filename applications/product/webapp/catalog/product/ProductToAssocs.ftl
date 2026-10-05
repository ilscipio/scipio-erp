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
<#if productId?? && product??>
        <@table type="data-list" autoAltRows=true>
          <@thead>
            <@tr class="header-row">
            <@th>${uiLabelMap.CommonProduct}</@th>
            <@th>${uiLabelMap.CommonType}</@th>
            <@th>${uiLabelMap.CommonFrom}</@th>
            <@th>${uiLabelMap.CommonThru}</@th>
            <@th class="align-right">${uiLabelMap.CommonSeqNum}</@th>
            <@th class="align-right">${uiLabelMap.CommonQuantity}</@th>
            <@th>&nbsp;</@th>
            <@th>&nbsp;</@th>
            </@tr>
          </@thead>
          <@tbody>  
            <#list assocToProducts as assocToProduct>
            <#assign listToProduct = assocToProduct.getRelatedOne("MainProduct", true)>
            <#assign curProductAssocType = assocToProduct.getRelatedOne("ProductAssocType", true)>
            <@tr valign="middle">
                <@td><a href="<@pageUrl>ViewProduct?productId=${(assocToProduct.productId)!}</@pageUrl>" class="${styles.link_nav_info_idname!}">${(listToProduct.internalName)!} [${(assocToProduct.productId)!}]</a></@td>
                <@td><#if curProductAssocType??> ${(curProductAssocType.get("description",locale))!}<#else> ${(assocToProduct.productAssocTypeId)!}</#if></@td>
                <@td>${(assocToProduct.getTimestamp("fromDate")?date?string.short!)!}&nbsp;</@td>
                <@td>${(assocToProduct.getTimestamp("thruDate")?date?string.short!)!}&nbsp;</@td>
                <@td class="amount">&nbsp;${(assocToProduct.sequenceNum)!}</@td>
                <@td class="amount">&nbsp;${(assocToProduct.quantity)!}</@td>
                <@td>
                <a href="<@pageUrl>EditProductAssoc?productId=${productId}&amp;PRODUCT_ID=${productId}&amp;PRODUCT_ID_TO=${(assocFromProduct.productIdTo)!}&amp;PRODUCT_ASSOC_TYPE_ID=${(assocFromProduct.productAssocTypeId)!}&amp;FROM_DATE=${(assocFromProduct.fromDate)!}&amp;useValues=true</@pageUrl>" class="${styles.link_nav!} ${styles.action_update!}">
                ${uiLabelMap.CommonEdit}</a>
                </@td>
                <@td>
                <a href="<@pageUrl>UpdateProductAssoc?UPDATE_MODE=DELETE&amp;productId=${(assocToProduct.productIdTo)!}&amp;PRODUCT_ID=${(assocToProduct.productId)!}&amp;PRODUCT_ID_TO=${(assocToProduct.productIdTo)!}&amp;PRODUCT_ASSOC_TYPE_ID=${(assocToProduct.productAssocTypeId)!}&amp;FROM_DATE=${(assocToProduct.fromDate)!}&amp;useValues=true</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">
                ${uiLabelMap.CommonDelete}</a>
                </@td>
            </@tr>
            </#list>
          </@tbody>
        </@table>
</#if>
<br />
<span class="tooltip">${uiLabelMap.CommonNote} : ${uiLabelMap.ProductHighlightedExplanation}</span>
