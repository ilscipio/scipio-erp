<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if productId?? && product??>
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
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
                <@td><a href="<@ofbizUrl>ViewProduct?productId=${(assocToProduct.productId)!}</@ofbizUrl>" class="${styles.link_nav_info_idname!}">${(listToProduct.internalName)!} [${(assocToProduct.productId)!}]</a></@td>
                <@td><#if curProductAssocType??> ${(curProductAssocType.get("description",locale))!}<#else> ${(assocToProduct.productAssocTypeId)!}</#if></@td>
                <@td>${(assocToProduct.getTimestamp("fromDate")?date?string.short!)!}&nbsp;</@td>
                <@td>${(assocToProduct.getTimestamp("thruDate")?date?string.short!)!}&nbsp;</@td>
                <@td class="amount">&nbsp;${(assocToProduct.sequenceNum)!}</@td>
                <@td class="amount">&nbsp;${(assocToProduct.quantity)!}</@td>
                <@td>
                <a href="<@ofbizUrl>EditProductAssoc?productId=${productId}&amp;PRODUCT_ID=${productId}&amp;PRODUCT_ID_TO=${(assocFromProduct.productIdTo)!}&amp;PRODUCT_ASSOC_TYPE_ID=${(assocFromProduct.productAssocTypeId)!}&amp;FROM_DATE=${(assocFromProduct.fromDate)!}&amp;useValues=true</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">
                ${uiLabelMap.CommonEdit}</a>
                </@td>
                <@td>
                <a href="<@ofbizUrl>UpdateProductAssoc?UPDATE_MODE=DELETE&amp;productId=${(assocToProduct.productIdTo)!}&amp;PRODUCT_ID=${(assocToProduct.productId)!}&amp;PRODUCT_ID_TO=${(assocToProduct.productIdTo)!}&amp;PRODUCT_ASSOC_TYPE_ID=${(assocToProduct.productAssocTypeId)!}&amp;FROM_DATE=${(assocToProduct.fromDate)!}&amp;useValues=true</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">
                ${uiLabelMap.CommonDelete}</a>
                </@td>
            </@tr>
            </#list>
          </@tbody>
        </@table>
</#if>
<br />
<span class="tooltip">${uiLabelMap.CommonNote} : ${uiLabelMap.ProductHighlightedExplanation}</span>
