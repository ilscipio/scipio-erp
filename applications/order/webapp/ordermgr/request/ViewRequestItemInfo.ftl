<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<@section title=uiLabelMap.OrderRequestItems>
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
          <@thead>
            <@tr valign="bottom" class="header-row">
                <@th width="10%">${uiLabelMap.ProductItem}</@th>
                <@th width="35%">${uiLabelMap.OrderProduct}</@th>
                <@th width="10%" align="right">${uiLabelMap.ProductQuantity}</@th>
                <@th width="10%" align="right">${uiLabelMap.OrderAmount}</@th>
                <@th width="10%" align="right">${uiLabelMap.OrderRequestMaximumAmount}</@th>
                <@th width="5%" align="right">&nbsp;</@th>
            </@tr>
          </@thead>
          <@tbody>
            <#list requestItems as requestItem>
                <#if requestItem.productId??>
                    <#assign product = requestItem.getRelatedOne("Product", false)>
                </#if>
                <@tr valign="middle">
                    <@td valign="top">
                            <#if showRequestManagementLinks??>
                                <a href="<@ofbizUrl>EditRequestItem?custRequestId=${requestItem.custRequestId}&amp;custRequestItemSeqId=${requestItem.custRequestItemSeqId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${requestItem.custRequestItemSeqId}</a>
                            <#else>
                                ${requestItem.custRequestItemSeqId}
                            </#if>
                        </@td>
                    <@td valign="top">
                            ${(product.internalName)!}&nbsp;
                            <#if showRequestManagementLinks??>
                                <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${requestItem.productId!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${requestItem.productId!}</a>
                            <#else>
                                <a href="<@ofbizUrl>product?product_id=${requestItem.productId!}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${requestItem.productId!}</a>
                            </#if>
                        </@td>
                    <@td align="right" valign="top">${requestItem.quantity!}</@td>
                    <@td align="right" valign="top">${requestItem.selectedAmount!}</@td>
                    <@td align="right" valign="top"><@ofbizCurrency amount=requestItem.maximumAmount isoCode=request.maximumAmountUomId/></@td>
                </@tr>
            </#list>
          </@tbody>
        </@table>
</@section>