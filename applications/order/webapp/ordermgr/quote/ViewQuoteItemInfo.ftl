<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#macro menuContent menuArgs={}>
  <#-- SCIPIO (2019-02-20): I really don't understand what's the purpose of this. Likely to be removed, commenting out for now -->
  <#--
  <@menu args=menuArgs>
  <#if (maySelectItems!"N") == "Y">
    <@menuitem type="link" href="javascript:document.addCommonToCartForm.add_all.value='true';document.addCommonToCartForm.submit()" text=uiLabelMap.OrderAddAllToCart class="+${styles.action_run_session!} ${styles.action_add!}" />
  </#if>
  </@menu>
  -->
</#macro>

<@section title=uiLabelMap.OrderOrderQuoteItems menuContent=menuContent>

    <@table type="data-complex" autoAltRows=true>
          <@thead>
            <@tr valign="bottom" class="header-row">
                <@th width="20%">${uiLabelMap.ProductItem}</@th>
                <@th width="20%">${uiLabelMap.CommonProduct}</@th>
                <@th width="10%" class="align-right">${uiLabelMap.CommonQuantity}</@th>
                <@th width="10%" class="align-right">${uiLabelMap.OrderSelAmount}</@th>
                <@th width="10%" class="align-right">${uiLabelMap.OrderOrderQuoteUnitPrice}</@th>
                <@th width="10%" class="align-right">${uiLabelMap.OrderAdjustments}</@th>
                <@th width="10%" class="align-right">${uiLabelMap.CommonSubtotal}</@th>
                <@th width="5%">${uiLabelMap.CommonTerms}</@th>
                <@th width="5%">${uiLabelMap.CommonAdjustments}</@th>
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
                        <#if showQuoteManagementLinks?? && quoteItem.isPromo?default("N") == "N" && quote.statusId=="QUO_CREATED">
                            <a href="<@pageUrl>EditQuoteItem?quoteId=${quoteItem.quoteId}&amp;quoteItemSeqId=${quoteItem.quoteItemSeqId}</@pageUrl>" class="${styles.link_nav_info_id!}">${quoteItem.quoteItemSeqId}</a>
                        <#else>
                            ${quoteItem.quoteItemSeqId}
                        </#if>
                    </@td>
                    <@td>
                        <#if showQuoteManagementLinks??>
                              <#if quoteItem.productId??>
                                <#assign product = quoteItem.getRelatedOne("Product", false)/>
                                
                                ${quoteItem.productId} - ${(product.internalName)!}
                              <#else>
                                <a href="<@serverUrl>/catalog/control/ViewProduct?productId=${quoteItem.productId!}</@serverUrl>" class="${styles.link_nav!} ${styles.action_add!}">
                                ${uiLabelMap.ProductCreateProduct}
                                </a>
                              </#if>
                        <#else>
                            <a href="<@pageUrl>product?product_id=${quoteItem.productId!}</@pageUrl>" class="${styles.link_nav_info_id!}">${quoteItem.productId!}</a>
                        </#if>
                    </@td>
                    <@td class="amount">${quoteItem.quantity!0}</@td>
                    <@td class="amount">${quoteItem.selectedAmount!0}</@td>
                    <@td class="amount"><@ofbizCurrency amount=quoteItem.quoteUnitPrice isoCode=quote.currencyUomId/></@td>
                    <@td class="amount"><@ofbizCurrency amount=totalQuoteItemAdjustmentAmount isoCode=quote.currencyUomId/></@td>
                    <@td class="amount"><@ofbizCurrency amount=totalQuoteItemAmount isoCode=quote.currencyUomId/></@td>
                    <@td>
                        <#assign quoteTerms = delegator.findByAnd("QuoteTerm", {"quoteId" : quoteItem.quoteId, "quoteItemSeqId" : quoteItem.quoteItemSeqId},[], true)>
                        <@modal id="displayTerms_${quoteItem.quoteItemSeqId}" label=uiLabelMap.CommonQuoteTerms linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_edit!}">
                            <#-- now show terms details per line item in a modal -->
                            <@heading level=1>${uiLabelMap.CommonQuoteTerms}</@heading>
                            <@table type="data-list" autoAltRows=true inheritAltRows=true>
                                <@thead>
                                    <@tr class="header-row-2">
                                        <@th>${uiLabelMap.OrderOrderTermType}</@th>
                                        <@th>${uiLabelMap.OrderOrderTermValue}</@th>
                                        <@th>${uiLabelMap.OrderOrderTermDays}</@th>
                                        <@th>${uiLabelMap.OrderQuoteTermDescription}</@th>
                                    </@tr>
                                </@thead>
                                <#if quoteTerms?has_content>
                                    <#list quoteTerms as quoteTerm>
                                        <#assign termDescription = delegator.findOne("TermType",{"termTypeId":quoteTerm.termTypeId}, false)>
                                        <@tr groupLast=true>
                                            <@td valign="top">;${termDescription.description!}</@td>
                                            <@td valign="top">${quoteTerm.termValue!}</@td>
                                            <@td valign="top"><#if quoteTerm.termDays?has_content>${quoteTerm.termDays!}</#if></@td>
                                            <@td valign="top"><#if quoteTerm.description?has_content>${quoteTerm.description}</#if></@td>
                                        </@tr>
                                    </#list>
                                </#if>
                            </@table>
                        </@modal>
                    </@td>
                    <@td>
                        <@modal id="displayAdjustments_${quoteItem.quoteItemSeqId}" label=uiLabelMap.CommonQuoteAdjustments linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_edit!}">
                            <#-- now show adjustment details per line item in a modal -->
                            <@heading level=1>${uiLabelMap.CommonQuoteAdjustments}</@heading>
                            <@table type="data-list" autoAltRows=true inheritAltRows=true>
                                <@thead>
                                    <@tr class="header-row-2">
                                        <@th>${uiLabelMap.CommonDescription}</@th>
                                        <@th>${uiLabelMap.CommonAmount}</@th>
                                    </@tr>
                                </@thead>
                                <#list quoteItemAdjustments as quoteItemAdjustment>
                                    <#assign adjustmentType = quoteItemAdjustment.getRelatedOne("OrderAdjustmentType", false)>
                                    <@tr groupLast=true>
                                        <@td class="align-right" colspan="6">${adjustmentType.get("description",locale)!}</@td>
                                        <@td class="amount"><@ofbizCurrency amount=quoteItemAdjustment.amount isoCode=quote.currencyUomId/></@td>
                                    </@tr>
                                </#list>
                            </@table>
                        </@modal>
                    </@td>
                </@tr>
            </#list>
            <@tr type="util" useAlt=false><@td colspan="10"><hr /></@td></@tr>
            <@tr useAlt=false>
                <@td class="align-right" colspan="8">${uiLabelMap.CommonSubtotal}</@td>
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
                      <@td class="align-right" colspan="8">${adjustmentType.get("description",locale)!}</@td>
                      <@td class="amount"><@ofbizCurrency amount=quoteAdjustment.amount isoCode=quote.currencyUomId/></@td>
                    </@tr>
                </#if>
                <#assign findAdjustment = true>
            </#list>
            <#assign grandTotalQuoteAmount = totalQuoteAmount + totalQuoteHeaderAdjustmentAmount>
            <#if findAdjustment>
                <@tr useAlt=false><@td colspan="7"></@td><@td colspan="8"><hr /></@td></@tr>
            </#if>
            <@tr useAlt=false>
                <@td  class="align-right" colspan="8">${uiLabelMap.OrderGrandTotal}</@td>
                <@td class="align-right">
                    <@ofbizCurrency amount=grandTotalQuoteAmount isoCode=quote.currencyUomId/>
                </@td>
            </@tr>
          </@tbody>
    </@table>
</@section>


