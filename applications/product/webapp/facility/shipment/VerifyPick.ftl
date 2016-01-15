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

<#if security.hasEntityPermission("FACILITY", "_VIEW", session)>
  <#assign showInput = "Y">
  <@section title="${uiLabelMap.ProductVerify}&nbsp;${uiLabelMap.OrderOrder}&nbsp;${uiLabelMap.CommonIn}&nbsp;${facility.facilityName!} [${facility.facilityId!}]">
    <#if (shipmentId?has_content) || (isOrderStatusApproved == false)>
      <#assign showInput = "N">
    </#if>
    <#if shipmentId?has_content>
      <@row>
        <@cell>
        <span>${uiLabelMap.ProductShipmentId}</span> <a href="<@ofbizUrl>/ViewShipment?shipmentId=${shipmentId}</@ofbizUrl>" class="${styles.link_nav_record_id!}">${shipmentId}</a>
        </@cell>
      </@row>
      <#if invoiceIds?? && invoiceIds?has_content>
        <@row>
          <@cell>
          <span>${uiLabelMap.AccountingInvoices}:</span>
          <@menu type="button">
            <#list invoiceIds as invoiceId>
              <@menuitem type="generic">
                ${uiLabelMap.CommonNbr}<a href="/accounting/control/invoiceOverview?invoiceId=${invoiceId}${StringUtil.wrapString(externalKeyParam)}" target="_blank" class="${styles.menu_button_item_link!}">${invoiceId}</a>
                (<a href="/accounting/control/invoice.pdf?invoiceId=${invoiceId}${StringUtil.wrapString(externalKeyParam)}" target="_blank" class="${styles.menu_button_item_link!}">PDF</a>)
              </@menuitem>
            </#list>
          </@menu>
          </@cell>
        </@row>
      </#if>
    </#if>

      <@section>
      <form name="selectOrderForm" method="post" action="<@ofbizUrl>VerifyPick</@ofbizUrl>">
        <input type="hidden" name="facilityId" value="${facility.facilityId!}"/>
          <@field type="generic" label="${uiLabelMap.ProductOrderId}">
              <#if shipmentId?has_content>
                <input type="text" name="orderId" size="20" maxlength="20" value=""/>
              <#else>
                <input type="text" name="orderId" size="20" maxlength="20" value="${orderId!}"/>
              </#if>
              /
              <input type="text" name="shipGroupSeqId" size="6" maxlength="6" value="${shipGroupSeqId?default("00001")}"/>
          </@field>
          <@field type="submitarea">
              <input type="submit" value="${uiLabelMap.ProductVerify}&nbsp;${uiLabelMap.OrderOrder}"/>
          </@field>
      </form>
      </@section>
     
      <@section>
      <#-- select picklist bin form -->
      <form name="selectPicklistBinForm" method="post" action="<@ofbizUrl>VerifyPick</@ofbizUrl>">
        <input type="hidden" name="facilityId" value="${facility.facilityId!}"/>
          <@field type="generic" label="${uiLabelMap.FormFieldTitle_picklistBinId}">
              <input type="text" name="picklistBinId" size="29" maxlength="60" value="${picklistBinId!}"/>
          </@field>
          <@field type="submitarea">
              <input type="submit" value="${uiLabelMap.ProductVerify}&nbsp;${uiLabelMap.OrderOrder}"/>
          </@field>
      </form>
      </@section>

      <form name="clearPickForm" method="post" action="<@ofbizUrl>cancelAllRows</@ofbizUrl>">
        <input type="hidden" name="orderId" value="${orderId!}"/>
        <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}"/>
        <input type="hidden" name="facilityId" value="${facility.facilityId!}"/>
      </form>
  </@section>
  <#if showInput != "N" && orderHeader?? && orderHeader?has_content>
    <#assign sectionTitle>${uiLabelMap.ProductOrderId} ${uiLabelMap.CommonNbr}<a href="/ordermgr/control/orderview?orderId=${orderId}">${orderId}</a> / ${uiLabelMap.ProductOrderShipGroupId} #${shipGroupSeqId}</#assign>
    <@section title=sectionTitle>
        <#if orderItemShipGroup?has_content>
          <#assign postalAddress = orderItemShipGroup.getRelatedOne("PostalAddress", false)>
          <#assign carrier = orderItemShipGroup.carrierPartyId?default("N/A")>
          <@row>
              <@cell columns=4>
                <span>${uiLabelMap.ProductShipToAddress}</span>
                <br />
                ${uiLabelMap.CommonTo}: ${postalAddress.toName?default("")}
                <br />
                <#if postalAddress.attnName?has_content>
                  ${uiLabelMap.CommonAttn}: ${postalAddress.attnName}
                  <br />
                </#if>
                ${postalAddress.address1}
                <br />
                <#if postalAddress.address2?has_content>
                  ${postalAddress.address2}
                  <br />
                </#if>
                ${postalAddress.city!}, ${postalAddress.stateProvinceGeoId!} ${postalAddress.postalCode!}
                <br />
                ${postalAddress.countryGeoId}
                <br />
              </@cell>
              <@cell columns=4>
                <span>${uiLabelMap.ProductCarrierShipmentMethod}</span>
                <br />
                <#if carrier == "USPS">
                  <#assign color = "red">
                <#elseif carrier == "UPS">
                  <#assign color = "green">
                <#else>
                  <#assign color = "black">
                </#if>
                <#if carrier != "_NA_">
                  <font color="${color}">${carrier}</font>
                  &nbsp;
                </#if>
                ${orderItemShipGroup.shipmentMethodTypeId?default("??")}
              </@cell>
              <@cell columns=4>
                <span>${uiLabelMap.OrderInstructions}</span>
                <br />
                ${orderItemShipGroup.shippingInstructions?default("(${uiLabelMap.CommonNone})")}
              </@cell>
          </@row>
        </#if>
        <hr />
        <@section>
        <form name="singlePickForm" method="post" action="<@ofbizUrl>processVerifyPick</@ofbizUrl>">
          <input type="hidden" name="orderId" value="${orderId!}"/>
          <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}"/>
          <input type="hidden" name="facilityId" value="${facility.facilityId!}"/>
                  <span>${uiLabelMap.ProductProductNumber}</span>
                  <input type="text" name="productId" size="20" maxlength="20" value=""/>
                  @
                  <input type="text" name="quantity" size="6" maxlength="6" value="1"/>
                  <input type="submit" value="${uiLabelMap.ProductVerify}&nbsp;${uiLabelMap.OrderItem}"/>
        </form>
        </@section>
        
        <@section>
        <#assign orderItems = orderItems!>
        <form name="multiPickForm" method="post" action="<@ofbizUrl>processBulkVerifyPick</@ofbizUrl>">
          <input type="hidden" name="facilityId" value="${facility.facilityId!}"/>
          <input type="hidden" name="userLoginId" value="${userLoginId!}"/>
          <input type="hidden" name="orderId" value="${orderId!}"/>
          <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}"/>
          <@table type="data-complex" cellspacing="0"> <#-- orig: class="basic-table" -->
            <@thead>
            <@tr class="header-row">
              <@th>&nbsp;</@th>
              <@th>${uiLabelMap.ProductItem} ${uiLabelMap.CommonNbr}</@th>
              <@th>${uiLabelMap.ProductProductId}</@th>
              <@th>${uiLabelMap.ProductInternalName}</@th>
              <@th>${uiLabelMap.ProductCountryOfOrigin}</@th>
              <@th align="right">${uiLabelMap.ProductOrderedQuantity}</@th>
              <@th align="right">${uiLabelMap.ProductVerified}&nbsp;${uiLabelMap.CommonQuantity}</@th>
              <@th align="center">${uiLabelMap.CommonQty}&nbsp;${uiLabelMap.CommonTo}&nbsp;${uiLabelMap.ProductVerify}</@th>
            </@tr>
            </@thead>
            <@tbody>
            <#if orderItems?has_content>
              <#assign rowKey = 1>
              <#assign counter = 1>
              <#assign isShowVerifyItemButton = "false">
              <#list orderItems as orderItem>
                <#assign orderItemSeqId = orderItem.orderItemSeqId!>
                <#assign readyToVerify = verifyPickSession.getReadyToVerifyQuantity(orderId,orderItemSeqId)>
                <#assign orderItemQuantity = orderItem.getBigDecimal("quantity")>
                <#assign verifiedQuantity = 0.000000>
                <#assign shipments = delegator.findByAnd("Shipment", Static["org.ofbiz.base.util.UtilMisc"].toMap("primaryOrderId", orderItem.getString("orderId"), "statusId", "SHIPMENT_PICKED"), null, false)/>
                <#if (shipments?has_content)>
                  <#list shipments as shipment>
                    <#assign itemIssuances = delegator.findByAnd("ItemIssuance", Static["org.ofbiz.base.util.UtilMisc"].toMap("shipmentId", shipment.getString("shipmentId"), "orderItemSeqId", orderItemSeqId), null, false)/>
                    <#if itemIssuances?has_content>
                      <#list itemIssuances as itemIssuance>
                        <#assign verifiedQuantity = verifiedQuantity + itemIssuance.getBigDecimal("quantity")>
                      </#list>
                    </#if>
                  </#list>
                </#if>
                <#if verifiedQuantity == orderItemQuantity>
                  <#assign counter = counter +1>
                </#if>
                <#assign orderItemQuantity = orderItemQuantity.subtract(verifiedQuantity)>
                <#assign product = orderItem.getRelatedOne("Product", false)!/>
                <@tr>
                  <#if (orderItemQuantity.compareTo(readyToVerify) > 0) >
                    <@td><input type="checkbox" name="sel_${rowKey}" value="Y" checked=""/></@td>
                    <#assign isShowVerifyItemButton = "true">
                  <#else>
                    <@td>&nbsp;</@td>
                  </#if>
                  <@td>${orderItemSeqId!}</@td>
                  <@td>${product.productId?default("N/A")}</@td>
                  <@td>
                    <a href="/catalog/control/EditProduct?productId=${product.productId!}${StringUtil.wrapString(externalKeyParam)}" class="${styles.link_nav_record_name!}" target="_blank">${(product.internalName)!}</a>
                  </@td>
                  <@td>
                    <select name="geo_${rowKey}">
                      <#if product.originGeoId?has_content>
                        <#assign originGeoId = product.originGeoId>
                        <#assign geo = delegator.findOne("Geo", Static["org.ofbiz.base.util.UtilMisc"].toMap("geoId", originGeoId), true)>
                        <option value="${originGeoId}">${geo.geoName!}</option>
                        <option value="${originGeoId}">---</option>
                      </#if>
                      <option value=""></option>
                      ${screens.render("component://common/widget/CommonScreens.xml#countries")}
                    </select>
                  </@td>
                  <@td align="right">${orderItemQuantity!}</@td>
                  <@td align="right">${readyToVerify!}</@td>
                  <@td align="center">
                    <#if (orderItemQuantity.compareTo(readyToVerify) > 0)>
                      <#assign qtyToVerify = orderItemQuantity.subtract(readyToVerify) >
                      <input type="text" size="7" name="qty_${rowKey}" value="${qtyToVerify!}"/>
                    <#else>
                      0
                    </#if>
                  </@td>
                  <input type="hidden" name="prd_${rowKey}" value="${(orderItem.productId)!}"/>
                  <input type="hidden" name="ite_${rowKey}" value="${(orderItem.orderItemSeqId)!}"/>
                </@tr>
                <#assign workOrderItemFulfillments = orderItem.getRelated("WorkOrderItemFulfillment", null, null, false)/>
                <#if workOrderItemFulfillments?has_content>
                  <#assign workOrderItemFulfillment = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(workOrderItemFulfillments)/>
                  <#if workOrderItemFulfillment?has_content>
                    <#assign workEffort = workOrderItemFulfillment.getRelatedOne("WorkEffort", false)/>
                    <#if workEffort?has_content>
                      <#assign workEffortTask = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(delegator.findByAnd("WorkEffort", Static["org.ofbiz.base.util.UtilMisc"].toMap("workEffortParentId", workEffort.workEffortId), null, false))/>
                      <#if workEffortTask?has_content>
                        <#assign workEffortInventoryAssigns = workEffortTask.getRelated("WorkEffortInventoryAssign", null, null, false)/>
                        <#if workEffortInventoryAssigns?has_content>
                          <@tr>
                            <@th colspan="8">
                              ${uiLabelMap.OrderMarketingPackageComposedBy}
                            </@th>
                          </@tr>
                          <@tr type="util"><@td colspan="8"><hr /></@td></@tr>
                          <#list workEffortInventoryAssigns as workEffortInventoryAssign>
                            <#assign inventoryItem = workEffortInventoryAssign.getRelatedOne("InventoryItem", false)/>
                            <#assign product = inventoryItem.getRelatedOne("Product", false)/>
                            <@tr>
                              <@td colspan="2"></@td>
                              <@td>${product.productId?default("N/A")}</@td>
                              <@td>${product.internalName!}</@td>
                              <@td></@td>
                              <@td align="right">${workEffortInventoryAssign.quantity!}</@td>
                            </@tr>
                          </#list>
                          <@tr type="util"><@td colspan="8"><hr /></@td></@tr>
                        </#if>
                      </#if>
                    </#if>
                  </#if>
                </#if>
                <#assign rowKey = rowKey + 1>
              </#list>
            </#if>
            </@tbody>
            <@tfoot>
            <@tr>
              <@td colspan="12" align="right">
                <#if isShowVerifyItemButton == "true">
                  <input type="submit" value="${uiLabelMap.ProductVerify}&nbsp;${uiLabelMap.OrderItems}"/>
                </#if>
                &nbsp;
                <#if rowKey != counter>
                  <input type="button" value="${uiLabelMap.CommonCancel}" onclick="javascript:document.clearPickForm.submit();"/>
                </#if>
              </@td>
            </@tr>
            </@tfoot>
          </@table>
        </form>
        </@section>
    </@section>
    <#assign orderId = orderId! >
    <#assign pickRows = verifyPickSession.getPickRows(orderId)!>
    <form name="completePickForm" method="post" action="<@ofbizUrl>completeVerifiedPick</@ofbizUrl>">
      <input type="hidden" name="orderId" value="${orderId!}"/>
      <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}"/>
      <input type="hidden" name="facilityId" value="${facility.facilityId!}"/>
      <input type="hidden" name="userLoginId" value="${userLoginId!}"/>
      <#if pickRows?has_content>
        <@section title="${uiLabelMap.ProductVerified}&nbsp;${uiLabelMap.OrderItems} : ${pickRows.size()!}">
            <@table type="data-list" cellspacing="0"> <#-- orig: class="basic-table" -->
             <@thead>
              <@tr class="header-row">
                <@th>${uiLabelMap.ProductItem} ${uiLabelMap.CommonNbr}</@th>
                <@th>${uiLabelMap.ProductProductId}</@th>
                <@th>${uiLabelMap.ProductInventoryItem} ${uiLabelMap.CommonNbr}</@th>
                <@th align="right">${uiLabelMap.ProductVerified}&nbsp;${uiLabelMap.CommonQuantity}</@th>
                <@th>&nbsp;</@th>
              </@tr>
              </@thead>
              <#list pickRows as pickRow>
                <#if (pickRow.getOrderId()!).equals(orderId)>
                  <@tr>
                    <@td>${pickRow.getOrderItemSeqId()!}</@td>
                    <@td>${pickRow.getProductId()!}</@td>
                    <@td>${pickRow.getInventoryItemId()!}</@td>
                    <@td align="right">${pickRow.getReadyToVerifyQty()!}</@td>
                  </@tr>
                </#if>
              </#list>
            </@table>
            <div align="right">
              <a href="javascript:document.completePickForm.submit()" class="${styles.link_action_sys!} ${styles.action_complete!}">${uiLabelMap.ProductComplete}</a>
            </div>
        </@section>
      </#if>
    </form>
  </#if>
  <#if orderId?has_content>
    <@script>
      document.singlePickForm.productId.focus();
    </@script>
  <#else>
    <@script>
      document.selectOrderForm.orderId.focus();
    </@script>
  </#if>
  <#if shipmentId?has_content>
    <@script>
      document.selectOrderForm.orderId.focus();
    </@script>
  </#if>
<#else>
  <@alert type="error">${uiLabelMap.ProductFacilityViewPermissionError}</@alert>
</#if>
