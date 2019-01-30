<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
            <#list assocFromProducts as assocFromProduct>
            <#assign listToProduct = assocFromProduct.getRelatedOne("AssocProduct", true)>
            <#assign curProductAssocType = assocFromProduct.getRelatedOne("ProductAssocType", true)>
            <@tr valign="middle">
                <@td><a href="<@pageUrl>ViewProduct?productId=${(assocFromProduct.productIdTo)!}</@pageUrl>" class="${styles.link_nav_info_idname!}">${(listToProduct.internalName)!} [${(assocFromProduct.productIdTo)!}]</a></@td>
                <@td><#if curProductAssocType??> ${(curProductAssocType.get("description",locale))!}<#else>${(assocFromProduct.productAssocTypeId)!}</#if></@td>
                <#assign cellClass><#if (assocFromProduct.getTimestamp("fromDate"))?? && nowDate.before(assocFromProduct.getTimestamp("fromDate"))>+${styles.text_color_alert!}</#if></#assign>
                <@td class=cellClass>
                ${(assocFromProduct.fromDate?date?string.short!)!}&nbsp;</@td>
                <#assign cellClass><#if (assocFromProduct.getTimestamp("thruDate"))?? && nowDate.after(assocFromProduct.getTimestamp("thruDate"))>+${styles.text_color_alert!}</#if></#assign>
                <@td class=cellClass>
                ${(assocFromProduct.thruDate?date?string.short!)!}&nbsp;</@td>
                <@td class="amount">&nbsp;${(assocFromProduct.sequenceNum)!}</@td>
                <@td class="amount">&nbsp;${(assocFromProduct.quantity)!}</@td>
                <@td>
                <a href="<@pageUrl>EditProductAssoc?productId=${productId}&amp;PRODUCT_ID=${productId}&amp;PRODUCT_ID_TO=${(assocFromProduct.productIdTo)!}&amp;PRODUCT_ASSOC_TYPE_ID=${(assocFromProduct.productAssocTypeId)!}&amp;FROM_DATE=${(assocFromProduct.fromDate)!}&amp;useValues=true</@pageUrl>" class="${styles.link_nav!} ${styles.action_update!}">
                ${uiLabelMap.CommonEdit}</a>
                </@td>
                <@td>
                <a href="<@pageUrl>UpdateProductAssoc?UPDATE_MODE=DELETE&amp;productId=${productId}&amp;PRODUCT_ID=${productId}&amp;PRODUCT_ID_TO=${(assocFromProduct.productIdTo)!}&amp;PRODUCT_ASSOC_TYPE_ID=${(assocFromProduct.productAssocTypeId)!}&amp;FROM_DATE=${(assocFromProduct.fromDate)!}&amp;useValues=true</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">
                ${uiLabelMap.CommonDelete}</a>
                </@td>
            </@tr>
            </#list>
          </@tbody>
        </@table>
</#if>
<br />
<span class="tooltip">${uiLabelMap.CommonNote} : ${uiLabelMap.ProductHighlightedExplanation}</span>
