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
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: TODO: CONVERT -->

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <#if (maySelectItems!false)>
          <@menuitem type="link" href="javascript:document.addCommonToCartForm.add_all.value='true';document.addCommonToCartForm.submit()" class="+${styles.action_run_session!} ${styles.action_add!}" text=uiLabelMap.OrderAddAllToCart />
          <@menuitem type="link" href="javascript:document.addCommonToCartForm.add_all.value='false';document.addCommonToCartForm.submit()" class="+${styles.action_run_session!} ${styles.action_add!}" text=uiLabelMap.OrderAddCheckedToCart />
      </#if>
    </@menu>
</#macro>
<@section menuContent=menuContent><#--title=uiLabelMap.OrderReturnItems -->
    <form name="selectAllForm" method="post" action="<@ofbizUrl>requestReturn</@ofbizUrl>">
      <input type="hidden" name="_checkGlobalScope" value="Y"/>
      <input type="hidden" name="_useRowSubmit" value="Y"/>
      <input type="hidden" name="returnHeaderTypeId" value="CUSTOMER_RETURN"/>
      <input type="hidden" name="fromPartyId" value="${party.partyId}"/>
      <input type="hidden" name="toPartyId" value="${toPartyId!}"/>
      <input type="hidden" name="orderId" value="${orderId}"/>
      <#if (orderHeader.currencyUom)?has_content>
      <input type="hidden" name="currencyUomId" value="${orderHeader.currencyUom}"/>
      </#if>

      <@table type="fields">
        <@tr>
          <@td colspan="5"><@heading>${uiLabelMap.OrderReturnItemsFromOrder} <a href="<@ofbizUrl>orderstatus?orderId=${orderId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${orderId}</a></@heading></@td>
          <@td align="right">
            <span class="tableheadtext">${uiLabelMap.CommonSelectAll}</span>&nbsp;
            <input type="checkbox" name="selectAll" value="Y" onclick="javascript:toggleAll(this, 'selectAllForm');"/>
          </@td>
        </@tr>
        <@tr>
          <@td>${uiLabelMap.CommonDescription}</@td>
          <@td>${uiLabelMap.CommonQuantity}</@td>
          <@td>${uiLabelMap.EcommercePrice}</@td>
          <@td>${uiLabelMap.OrderReason}</@td>
          <@td>${uiLabelMap.OrderRequestedResponse}</@td>
          <@td>&nbsp;</@td>
        </@tr>
        <@tr type="util"><@td colspan="6"><hr /></@td></@tr>
        <#if returnableItems?has_content>
          <#assign rowCount = 0>
          <#list returnableItems.keySet() as orderItem>
          <#-- SCIPIO: This stock check does not work in FTL (crash); make our own;
              BUT still do the check commented as a fallback in case it's not a GenericValue
          <#if !orderItem.orderAdjustmentId?has_content>    <#- filter orderAdjustments ->
          -->
          <#if orderItem.entityName == "OrderItem" || (!orderItem.entityName?has_content && !orderItem.orderAdjustmentId?has_content)>    <#-- filter orderAdjustments -->
            <@tr>
              <@td>
            <input type="hidden" name="orderId_o_${rowCount}" value="${orderItem.orderId}"/>
            <input type="hidden" name="orderItemSeqId_o_${rowCount}" value="${orderItem.orderItemSeqId}"/>
            <input type="hidden" name="description_o_${rowCount}" value="${orderItem.itemDescription!}"/>
            <#-- <input type="hidden" name="returnItemType_o_${rowCount}" value="ITEM"/> -->
            <#assign returnItemType = returnItemTypeMap.get(returnableItems.get(orderItem).get("itemTypeKey"))/>
            <input type="hidden" name="returnItemTypeId_o_${rowCount}" value="${returnItemType}"/>
            <input type="hidden" name="returnPrice_o_${rowCount}" value="${returnableItems.get(orderItem).get("returnablePrice")}"/>

            <#-- need some order item information -->
            <#assign orderHeader = orderItem.getRelatedOne("OrderHeader", false)>
            <#assign itemCount = orderItem.quantity>
            <#assign itemPrice = orderItem.unitPrice>
            <#-- end of order item information -->

                  <#if orderItem.productId??>
                    &nbsp;<a href="<@ofbizUrl>product?product_id=${orderItem.productId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${orderItem.productId}</a>
                    <input type="hidden" name="productId_o_${rowCount}" value="${orderItem.productId}"/>
                  </#if>
                  ${orderItem.itemDescription}
              </@td>
              <@td>
                <input type="text" size="6" name="returnQuantity_o_${rowCount}" value="${returnableItems.get(orderItem).get("returnableQuantity")}"/>
              </@td>
              <@td><@ofbizCurrency amount=returnableItems.get(orderItem).get("returnablePrice") isoCode=orderHeader.currencyUom/>
              </@td>
              <@td>
                <select name="returnReasonId_o_${rowCount}">
                  <#list returnReasons as reason>
                    <option value="${reason.returnReasonId}">${reason.get("description",locale)?default(reason.returnReasonId)}</option>
                  </#list>
                </select>
              </@td>
              <@td>
                <select name="returnTypeId_o_${rowCount}">
                  <#list returnTypes as type>
                    <option value="${type.returnTypeId}">${type.get("description",locale)?default(type.returnTypeId)}</option>
                  </#list>
                </select>
              </@td>
              <@td align="right">
                <input type="checkbox" name="_rowSubmit_o_${rowCount}" value="Y" onclick="javascript:checkToggle(this, 'selectAllForm');"/>
              </@td>
            </@tr>
            <@tr type="util"><@td colspan="6"><hr /></@td></@tr>
            <#assign rowCount = rowCount + 1>
          </#if>
          </#list>
          <input type="hidden" name="_rowCount" value="${rowCount}"/>
          <@tr>
            <@td colspan="6">${uiLabelMap.OrderSelectShipFromAddress}:</@td>
          </@tr>
          <@tr type="util"><@td colspan="6"><hr /></@td></@tr>
          <@tr>
            <@td colspan="6">
              <@table type="fields"> <#-- orig: cellspacing="1" cellpadding="2" width="100%" -->
                <#list shippingContactMechList as shippingContactMech>
                  <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
                  <@tr>
                    <@td align="right" width="1%" valign="top" nowrap="nowrap">
                      <input type="radio" name="originContactMechId" value="${shippingAddress.contactMechId}"/>
                    </@td>
                    <@td width="99%" valign="top" nowrap="nowrap">
                        <#if shippingAddress.toName?has_content><b>${uiLabelMap.CommonTo}:</b>&nbsp;${shippingAddress.toName}<br /></#if>
                        <#if shippingAddress.attnName?has_content><b>${uiLabelMap.PartyAddrAttnName}:</b>&nbsp;${shippingAddress.attnName}<br /></#if>
                        <#if shippingAddress.address1?has_content>${shippingAddress.address1}<br /></#if>
                        <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br /></#if>
                        <#if shippingAddress.city?has_content>${shippingAddress.city}</#if>
                        <#if shippingAddress.stateProvinceGeoId?has_content><br />${shippingAddress.stateProvinceGeoId}</#if>
                        <#if shippingAddress.postalCode?has_content><br />${shippingAddress.postalCode}</#if>
                        <#if shippingAddress.countryGeoId?has_content><br />${shippingAddress.countryGeoId}</#if>
                        <a href="<@ofbizUrl>editcontactmech?DONE_PAGE=checkoutoptions&amp;contactMechId=${shippingAddress.contactMechId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">[${uiLabelMap.CommonUpdate}]</a>
                    </@td>
                  </@tr>
                </#list>
              </@table>
            </@td>
          </@tr>
          <@tr type="util"><@td colspan="6"><hr /></@td></@tr>
          <@tr>
            <@td colspan="6" align="right">
              <a href="javascript:document.selectAllForm.submit();" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.OrderReturnSelectedItems}</a>
            </@td>
          </@tr>
        <#else>
          <@tr><@td colspan="6"><@commonMsg type="result-norecord">${uiLabelMap.OrderNoReturnableItems} ${orderId}</@commonMsg></@td></@tr>
        </#if>
      </@table>
    </form>
</@section>
