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

<@section title="${uiLabelMap.OrderOrderItems}">
        <div class="boxlink">
            <#if maySelectItems?default(false)>
                <a href="javascript:document.addOrderToCartForm.add_all.value="true";document.addOrderToCartForm.submit()" class="${styles.button_default!}">${uiLabelMap.OrderAddAllToCart}</a>
                <a href="javascript:document.addOrderToCartForm.add_all.value="false";document.addOrderToCartForm.submit()" class="${styles.button_default!}">${uiLabelMap.OrderAddCheckedToCart}</a>
            </#if>
        </div>
        <@table  class="basic-table">
          <@thead>
          <@tr>
            <@th width="65%">${uiLabelMap.ProductProduct}</@th>
            <@th width="5%" class="text-right">${uiLabelMap.OrderQuantity}</@th>
            <@th width="10%" class="text-right">${uiLabelMap.CommonUnitPrice}</@th>
            <@th width="10%" class="text-right">${uiLabelMap.OrderAdjustments}</@th>
            <@th width="10%" class="text-right">${uiLabelMap.OrderSubTotal}</@th>
          </@tr>
          </@thead>
          <#list orderItems! as orderItem>
            <#assign itemType = orderItem.getRelatedOne("OrderItemType", false)!>
            <@tr>
              <#if orderItem.productId?? && orderItem.productId == "_?_">
                <@td colspan="1" valign="top">
                  <b><div> &gt;&gt; ${orderItem.itemDescription}</div></b>
                </@td>
              <#else>
                <@td valign="top">
                    <#if orderItem.productId??>
                      <a href="<@ofbizUrl>product?product_id=${orderItem.productId}</@ofbizUrl>">${orderItem.productId} - ${orderItem.itemDescription}</a>
                    <#else>
                      <b>${(itemType.description)!}</b> : ${orderItem.itemDescription!}
                    </#if>
                  </@td>
                <@td class="text-right" valign="top">${orderItem.quantity?string.number}</@td>
                <@td class="text-right" valign="top"><@ofbizCurrency amount=orderItem.unitPrice isoCode=currencyUomId/></@td>
                <@td class="text-right" valign="top"><@ofbizCurrency amount=localOrderReadHelper.getOrderItemAdjustmentsTotal(orderItem) isoCode=currencyUomId/></@td>
                <@td class="text-right" valign="top"><@ofbizCurrency amount=localOrderReadHelper.getOrderItemSubTotal(orderItem) isoCode=currencyUomId/></@td>
                <#if maySelectItems?default(false)>
                  <@td>
                    <input name="item_id" value="${orderItem.orderItemSeqId}" type="checkbox" />
                  </@td>
                </#if>
              </#if>
            </@tr>
            <#-- show info from workeffort if it was a rental item -->
            <#if orderItem.orderItemTypeId?? && orderItem.orderItemTypeId == "RENTAL_ORDER_ITEM">
                <#assign WorkOrderItemFulfillments = orderItem.getRelated("WorkOrderItemFulfillment", null, null, false)!>
                <#if WorkOrderItemFulfillments?has_content>
                    <#list WorkOrderItemFulfillments as WorkOrderItemFulfillment>
                        <#assign workEffort = WorkOrderItemFulfillment.getRelatedOne("WorkEffort", true)!>
                          <@tr><@td>&nbsp;</@td><@td>&nbsp;</@td><@td colspan="8">${uiLabelMap.CommonFrom}: ${workEffort.estimatedStartDate?string("yyyy-MM-dd")} ${uiLabelMap.CommonTo}: ${workEffort.estimatedCompletionDate?string("yyyy-MM-dd")} ${uiLabelMap.OrderNbrPersons}: ${workEffort.reservPersons}</@td></@tr>
                        <#break><#-- need only the first one -->
                    </#list>
                </#if>
            </#if>

            <#-- now show adjustment details per line item -->
            <#assign itemAdjustments = localOrderReadHelper.getOrderItemAdjustments(orderItem)>
            <#list itemAdjustments as orderItemAdjustment>
              <@tr>
                <@td>
                    <b><i>${uiLabelMap.OrderAdjustment}</i>:</b> <b>${localOrderReadHelper.getAdjustmentType(orderItemAdjustment)}</b>&nbsp;
                    <#if orderItemAdjustment.description?has_content>: ${StringUtil.wrapString(orderItemAdjustment.get("description",locale))}</#if>

                    <#if orderItemAdjustment.orderAdjustmentTypeId == "SALES_TAX">
                      <#if orderItemAdjustment.primaryGeoId?has_content>
                        <#assign primaryGeo = orderItemAdjustment.getRelatedOne("PrimaryGeo", true)/>
                        <#if primaryGeo.geoName?has_content>
                            <b>${uiLabelMap.OrderJurisdiction}:</b> ${primaryGeo.geoName} [${primaryGeo.abbreviation!}]
                        </#if>
                        <#if orderItemAdjustment.secondaryGeoId?has_content>
                          <#assign secondaryGeo = orderItemAdjustment.getRelatedOne("SecondaryGeo", true)/>
                          (<b>in:</b> ${secondaryGeo.geoName} [${secondaryGeo.abbreviation!}])
                        </#if>
                      </#if>
                      <#if orderItemAdjustment.sourcePercentage??><b>${uiLabelMap.OrderRate}:</b> ${orderItemAdjustment.sourcePercentage}%</#if>
                      <#if orderItemAdjustment.customerReferenceId?has_content><b>${uiLabelMap.OrderCustomerTaxId}:</b> ${orderItemAdjustment.customerReferenceId}</#if>
                      <#if orderItemAdjustment.exemptAmount??><b>${uiLabelMap.OrderExemptAmount}:</b> ${orderItemAdjustment.exemptAmount}</#if>
                    </#if>
                  </@td>
                <@td>&nbsp;</@td>
                <@td>&nbsp;</@td>
                <@td class="text-right"><@ofbizCurrency amount=localOrderReadHelper.getOrderItemAdjustmentTotal(orderItem, orderItemAdjustment) isoCode=currencyUomId/></@td>
                <@td>&nbsp;</@td>
                <@td>&nbsp;</@td>
                <#if maySelectItems?default(false)><@td>&nbsp;</@td></#if>
              </@tr>
            </#list>
           </#list>
           <#if !orderItems?has_content>
             <@tr><@td><font color="red">${uiLabelMap.checkhelpertotalsdonotmatchordertotal}</font></@td></@tr>
           </#if>

          <@tr>
            <@td colspan="4"><b>${uiLabelMap.OrderSubTotal}</b></@td>
            <@td class="text-right">&nbsp;<#if orderSubTotal??><@ofbizCurrency amount=orderSubTotal isoCode=currencyUomId/></#if></@td>
          </@tr>
          <#list headerAdjustmentsToShow! as orderHeaderAdjustment>
            <@tr>
              <@td colspan="4"><b>${localOrderReadHelper.getAdjustmentType(orderHeaderAdjustment)}</b></@td>
              <@td class="text-right"><@ofbizCurrency amount=localOrderReadHelper.getOrderAdjustmentTotal(orderHeaderAdjustment) isoCode=currencyUomId/></@td>
            </@tr>
          </#list>
          <@tr><@td colspan=2></@td><@td colspan="8"><hr /></@td></@tr>
          
          <@tr>
            <@td colspan="4"><b>${uiLabelMap.FacilityShippingAndHandling}</b></@td>
            <@td class="text-right"><#if orderShippingTotal??><@ofbizCurrency amount=orderShippingTotal isoCode=currencyUomId/></#if></@td>
          </@tr>
          <@tr>
            <@td colspan="4"><b>${uiLabelMap.OrderSalesTax}</b></@td>
            <@td class="text-right"><#if orderTaxTotal??><@ofbizCurrency amount=orderTaxTotal isoCode=currencyUomId/></#if></@td>
          </@tr>

          <@tr><@td colspan=2></@td><@td colspan="8"><hr /></@td></@tr>
          <@tr>
            <@td colspan="4"><b>${uiLabelMap.OrderGrandTotal}</b></@td>
            <@td class="text-right"><#if orderGrandTotal??><@ofbizCurrency amount=orderGrandTotal isoCode=currencyUomId/></#if></@td>
          </@tr>
        </@table>
</@section>
