<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if (maySelectItems!"N") == "Y">
    <@menuitem type="link" href="javascript:document.addCommonToCartForm.add_all.value='true';document.addCommonToCartForm.submit()" text=uiLabelMap.OrderAddAllToCart class="+${styles.action_run_session!} ${styles.action_add!}" />
  </#if>
  </@menu>
</#macro>

<@section title=uiLabelMap.OrderOrderQuoteItems menuContent=menuContent>

    <@table type="data-complex" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
          <@thead>
            <@tr valign="bottom" class="header-row">
                <@th width="15%">${uiLabelMap.ProductItem}</@th>
                <@th width="20%">${uiLabelMap.CommonProduct}</@th>
                <@th width="10%" class="align-right">${uiLabelMap.CommonQuantity}</@th>
                <@th width="10%" class="align-right">${uiLabelMap.OrderSelAmount}</@th>
                <@th width="5%" class="align-right">&nbsp;</@th>
                <@th width="10%" class="align-right">${uiLabelMap.OrderOrderQuoteUnitPrice}</@th>
                <@th width="10%" class="align-right">${uiLabelMap.OrderAdjustments}</@th>
                <@th width="10%" class="align-right">${uiLabelMap.CommonSubtotal}</@th>
            </@tr>
            <@tr valign="bottom" class="header-row">
                <@th>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;${uiLabelMap.OrderOrderTermType}</@th>
                <@th>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;${uiLabelMap.OrderOrderTermValue}</@th>
                <@th>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;${uiLabelMap.OrderOrderTermDays}</@th>
                <@th>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;${uiLabelMap.OrderQuoteTermDescription}</@th>
                <@th></@th>
                <@th></@th>
                <@th></@th>
                <@th align="right">&nbsp;</@th>
            </@tr>
          </@thead>
          <@tbody>
            <#assign totalQuoteAmount = 0.0>
            <#list quoteItems as quoteItem>
                <#assign selectedAmount = quoteItem.selectedAmount?default(1)>
                <#if selectedAmount == 0>
                    <#assign selectedAmount = 1/>
                </#if>
                <#assign quoteItemAmount = quoteItem.quoteUnitPrice?default(0) * quoteItem.quantity?default(0) * selectedAmount>
                <#assign quoteItemAdjustments = quoteItem.getRelated("QuoteAdjustment", null, null, false)>
                <#assign totalQuoteItemAdjustmentAmount = 0.0>
                <#list quoteItemAdjustments as quoteItemAdjustment>
                    <#assign totalQuoteItemAdjustmentAmount = quoteItemAdjustment.amount?default(0) + totalQuoteItemAdjustmentAmount>
                </#list>
                <#assign totalQuoteItemAmount = quoteItemAmount + totalQuoteItemAdjustmentAmount>
                <#assign totalQuoteAmount = totalQuoteAmount + totalQuoteItemAmount>
                
                <@tr>
                    <@td>
                        <div>
                        <#if showQuoteManagementLinks?? && quoteItem.isPromo?default("N") == "N" && quote.statusId=="QUO_CREATED">
                            <a href="<@ofbizUrl>EditQuoteItem?quoteId=${quoteItem.quoteId}&amp;quoteItemSeqId=${quoteItem.quoteItemSeqId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${quoteItem.quoteItemSeqId}</a>
                        <#else>
                            ${quoteItem.quoteItemSeqId}
                        </#if>
                        </div>
                        <#assign quoteTerms = delegator.findByAnd("QuoteTerm", {"quoteId" : quoteItem.quoteId, "quoteItemSeqId" : quoteItem.quoteItemSeqId})>
                    </@td>
                    <@td valign="top">
                            <#if showQuoteManagementLinks??>
                                  <#if quoteItem.productId??>
                                    <#assign product = quoteItem.getRelatedOne("Product", false)/>
                                    
                                    ${quoteItem.productId} - ${(product.internalName)!}
                                  <#else>
                                    <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${quoteItem.productId!}</@ofbizInterWebappUrl>" class="${styles.link_nav!} ${styles.action_add!}">
                                    ${uiLabelMap.ProductCreateProduct}
                                    </a>
                                  </#if>
                            <#else>
                                <a href="<@ofbizUrl>product?product_id=${quoteItem.productId!}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${quoteItem.productId!}</a>
                            </#if>
                        </@td>
                    <@td class="amount">${quoteItem.quantity!}</@td>
                    <@td align="amount" valign="top">${quoteItem.selectedAmount!}</@td>
                    <@td></@td>
                    <@td class="align-right" valign="top"><@ofbizCurrency amount=quoteItem.quoteUnitPrice isoCode=quote.currencyUomId/></@td>
                    <@td class="amount"><@ofbizCurrency amount=totalQuoteItemAdjustmentAmount isoCode=quote.currencyUomId/></@td>
                    <@td class="amount"><@ofbizCurrency amount=totalQuoteItemAmount isoCode=quote.currencyUomId/></@td>
                </@tr>
                <#list quoteTerms as quoteTerm>
                <#assign termDescription = delegator.findOne("TermType",{"termTypeId":quoteTerm.termTypeId}, false)>
                <@tr groupLast=true>
                    <@td valign="top">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;${termDescription.description!}</@td>
                    <@td valign="top">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;${quoteTerm.termValue!}</@td>
                    <@td valign="top"><#if quoteTerm.termDays??>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;${quoteTerm.termDays!}</#if></@td>
                    <@td valign="top"><#if quoteTerm.description??>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;${quoteTerm.description}</#if></@td>
                    <@td align="right" valign="top"></@td>
                    <@td align="right" valign="top"></@td>
                    <@td align="right" valign="top"></@td>
                    <@td align="right" valign="top"></@td>
                </@tr>
                </#list>
                <#-- now show adjustment details per line item -->
                <#list quoteItemAdjustments as quoteItemAdjustment>
                    <#assign adjustmentType = quoteItemAdjustment.getRelatedOne("OrderAdjustmentType", false)>
                    <@tr groupLast=true>
                        <@td class="align-right" colspan="6">${adjustmentType.get("description",locale)!}</@td>
                        <@td class="amount"><@ofbizCurrency amount=quoteItemAdjustment.amount isoCode=quote.currencyUomId/></@td>
                        <@td>&nbsp;</@td>
                    </@tr>
                </#list>
            </#list>
            <@tr type="util" useAlt=false><@td colspan="10"><hr /></@td></@tr>
            <@tr useAlt=false>
                <@td class="align-right" colspan="7">${uiLabelMap.CommonSubtotal}</@td>
                <@td class="amount"><@ofbizCurrency amount=totalQuoteAmount isoCode=quote.currencyUomId/></@td>
            </@tr>
            <@tr useAlt=false><@td colspan="5"></@td><@td colspan="6"><hr /></@td></@tr>
            <#assign totalQuoteHeaderAdjustmentAmount = 0.0>
            <#assign findAdjustment = false>
            <#list quoteAdjustments as quoteAdjustment>
                <#assign adjustmentType = quoteAdjustment.getRelatedOne("OrderAdjustmentType", false)>
                <#if !quoteAdjustment.quoteItemSeqId??>
                    <#assign totalQuoteHeaderAdjustmentAmount = quoteAdjustment.amount?default(0) + totalQuoteHeaderAdjustmentAmount>
                    <@tr useAlt=false>
                      <@td class="align-right" colspan="6">${adjustmentType.get("description",locale)!}</@td>
                      <@td class="amount"><@ofbizCurrency amount=quoteAdjustment.amount isoCode=quote.currencyUomId/></@td>
                    </@tr>
                </#if>
                <#assign findAdjustment = true>
            </#list>
            <#assign grandTotalQuoteAmount = totalQuoteAmount + totalQuoteHeaderAdjustmentAmount>
            <#if findAdjustment>
                <@tr useAlt=false><@td colspan="5"></@td><@td colspan="6"><hr /></@td></@tr>
            </#if>
            <@tr useAlt=false>
                <@td  class="align-right" colspan="7">${uiLabelMap.OrderGrandTotal}</@td>
                <@td class="align-right">
                    <@ofbizCurrency amount=grandTotalQuoteAmount isoCode=quote.currencyUomId/>
                </@td>
            </@tr>
          </@tbody>
    </@table>
</@section>
