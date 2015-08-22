<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<@section title="${uiLabelMap.OrderOrderQuoteItems}">
            <#if maySelectItems?default("N") == "Y">
        <a href="javascript:document.addCommonToCartForm.add_all.value='true';document.addCommonToCartForm.submit()" class="${styles.button_default!}">${uiLabelMap.OrderAddAllToCart}</a>
            </#if>

        <@table type="data" autoAltRows=true cellspacing="0" class="basic-table">
          <@thead>
            <@tr valign="bottom" class="header-row">
                <@th width="15%">${uiLabelMap.ProductItem}</@th>
                <@th width="20%">${uiLabelMap.ProductProduct}</@th>
                <@th width="10%" align="right">${uiLabelMap.ProductQuantity}</@th>
                <@th width="10%" align="right">${uiLabelMap.OrderSelAmount}</@th>
                <@th width="5%" align="right">&nbsp;</@th>
                <@th width="10%" align="right">${uiLabelMap.OrderOrderQuoteUnitPrice}</@th>
                <@th width="10%" align="right">${uiLabelMap.OrderAdjustments}</@th>
                <@th width="10%" align="right">${uiLabelMap.CommonSubtotal}</@th>
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
                            <a href="<@ofbizUrl>EditQuoteItem?quoteId=${quoteItem.quoteId}&amp;quoteItemSeqId=${quoteItem.quoteItemSeqId}</@ofbizUrl>" class="${styles.button_default!}">${quoteItem.quoteItemSeqId}</a>
                        <#else>
                            ${quoteItem.quoteItemSeqId}
                        </#if>
                        </div>
                        <#assign quoteTerms = delegator.findByAnd("QuoteTerm", {"quoteId" : quoteItem.quoteId, "quoteItemSeqId" : quoteItem.quoteItemSeqId})>
                    </@td>
                    <@td valign="top">
                            <#if quoteItem.productId??>
                              <#assign product = quoteItem.getRelatedOne("Product", false)/>
                              ${(product.internalName)!}&nbsp;
                            </#if>
                            <#if showQuoteManagementLinks??>
                                <a href="/catalog/control/EditProduct?productId=${quoteItem.productId!}" class="${styles.button_default!}">
                                  <#if quoteItem.productId??>
                                    ${quoteItem.productId}
                                  <#else>
                                    ${uiLabelMap.ProductCreateProduct}
                                  </#if>
                                </a>
                            <#else>
                                <a href="<@ofbizUrl>product?product_id=${quoteItem.productId!}</@ofbizUrl>" class="${styles.button_default!}">${quoteItem.productId!}</a>
                            </#if>
                        </@td>
                    <@td align="right" valign="top">${quoteItem.quantity!}</@td>
                    <@td align="right" valign="top">${quoteItem.selectedAmount!}</@td>
                    <@td></@td>
                    <@td align="right" valign="top"><@ofbizCurrency amount=quoteItem.quoteUnitPrice isoCode=quote.currencyUomId/></@td>
                    <@td align="right" valign="top"><@ofbizCurrency amount=totalQuoteItemAdjustmentAmount isoCode=quote.currencyUomId/></@td>
                    <@td align="right" valign="top"><@ofbizCurrency amount=totalQuoteItemAmount isoCode=quote.currencyUomId/></@td>
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
                        <@td align="right" colspan="4"><span>${adjustmentType.get("description",locale)!}</span></@td>
                        <@td align="right"><@ofbizCurrency amount=quoteItemAdjustment.amount isoCode=quote.currencyUomId/></@td>
                        <@td>&nbsp;</@td>
                    </@tr>
                </#list>
            </#list>
            <@tr useAlt=false><@td colspan="10"><hr /></@td></@tr>
            <@tr useAlt=false>
                <@td align="right" colspan="7">${uiLabelMap.CommonSubtotal}</@td>
                <@td align="right"><@ofbizCurrency amount=totalQuoteAmount isoCode=quote.currencyUomId/></@td>
            </@tr>
            <@tr useAlt=false><@td colspan="5"></@td><@td colspan="6"><hr /></@td></@tr>
            <#assign totalQuoteHeaderAdjustmentAmount = 0.0>
            <#assign findAdjustment = false>
            <#list quoteAdjustments as quoteAdjustment>
                <#assign adjustmentType = quoteAdjustment.getRelatedOne("OrderAdjustmentType", false)>
                <#if !quoteAdjustment.quoteItemSeqId??>
                    <#assign totalQuoteHeaderAdjustmentAmount = quoteAdjustment.amount?default(0) + totalQuoteHeaderAdjustmentAmount>
                    <@tr useAlt=false>
                      <@td align="right" colspan="6"><span>${adjustmentType.get("description",locale)!}</span></@td>
                      <@td align="right"><@ofbizCurrency amount=quoteAdjustment.amount isoCode=quote.currencyUomId/></@td>
                    </@tr>
                </#if>
                <#assign findAdjustment = true>
            </#list>
            <#assign grandTotalQuoteAmount = totalQuoteAmount + totalQuoteHeaderAdjustmentAmount>
            <#if findAdjustment>
            <@tr useAlt=false><@td colspan="5"></@td><@td colspan="6"><hr /></@td></@tr>
            </#if>
            <@tr useAlt=false>
                <@td align="right" colspan="7">${uiLabelMap.OrderGrandTotal}</@td>
                <@td align="right">
                    <@ofbizCurrency amount=grandTotalQuoteAmount isoCode=quote.currencyUomId/>
                </@td>
            </@tr>
          </@tbody>
        </@table>
    </@section>
