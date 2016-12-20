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

<#if shipGroups?has_content && (!orderHeader.salesChannelEnumId?? || orderHeader.salesChannelEnumId != "POS_SALES_CHANNEL")>
  <#if parameters.view?has_content && parameters.view == "OISGA">
  <#-- New in Ofbiz 14.12 -->
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
       <@menuitem type="link" href=makeOfbizUrl("orderShipping?orderId=${orderId}") text=uiLabelMap.OrderShipmentInformationByOISG class="+${styles.action_nav!} ${styles.action_view!}" />
    </@menu>
  </#macro>
  <@section title=uiLabelMap.OrderShipmentInformation menuContent=menuContent>
    <@fields type="default-manual">
      <@table type="data-complex" role="grid"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
        <@thead>
          <@tr class="header-row">
              <@th width="15%">${uiLabelMap.OrderItemId}</@th>
              <@th width="25%">${uiLabelMap.ProductProduct}</@th>
              <@th width="10%">${uiLabelMap.CommonQuantity}</@th>
              <@th width="30%">${uiLabelMap.ProductQuantityNotAvailable}</@th>
              <@th width="20%">&nbsp;</@th>
          </@tr>
        </@thead>
        <@tbody>
          <#assign index = 0>
          <#list orderItemDatas as orderItemData>
              <#assign orderItem = orderItemData.orderItem>
              <#assign OISGAssContents = orderItemData.OISGAssContents>
              <#assign product = orderItemData.product!>
              <#assign quantityOrdered = orderItemData.quantityOrdered?default(0)>
              <#assign totalQuantityToPlan = orderItemData.totalQuantityToPlan>
              <#assign quantityNotAvailable = orderItemData.quantityNotAvailable>
              <#assign rowCount = 0>

              <#if index != 0>
          <@tr type="util"><@td colspan="4"><hr/></@td></@tr>
              </#if>
              <#if (quantityOrdered > 0) >
          <@tr>
              <@td><a name="orderItem${index}">${orderItem.orderItemSeqId}</a></@td>
              <@td><#if product.internalName?has_content>${product.internalName!}<br/></#if>[<a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${orderItem.productId!}</@ofbizInterWebappUrl>" class="link">${orderItem.productId!}</a>]</@td>
              <@td>${quantityOrdered}</@td>
              <@td>${quantityNotAvailable}</@td>
              <@td colspan="2">
                  <#if !orderItem.statusId?exists || orderItem.statusId == "ITEM_CREATED" || orderItem.statusId == "ITEM_APPROVED">
                  <div id="display${index}">
                      <a name="display${index}" href="javascript:showEdit('edit', '${index}');" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonEdit}</a>
                  </div>
                  <div id="edit${index}" style="display: none">
                      <a href="javascript:document.UpdateOrderItemShipGroupAssoc${index}.submit()" class="${styles.link_run_sys!} ${styles.action_verify!}">${uiLabelMap.CommonValidate}</a>
                      <a href="javascript:showEdit('display', '${index}'); restoreEditField('${index}');" class="${styles.link_run_local_cancel!}">${uiLabelMap.CommonCancel}</a>
                  </div>
                  </#if>
              </@td>
          </@tr>
          <@tr>
            <@td colspan="5">
              <form method="post" action="<@ofbizUrl>UpdateOrderItemShipGroupAssoc?view=OISGA</@ofbizUrl>" name="UpdateOrderItemShipGroupAssoc${index}"/>
                <input type="hidden" name="orderId" value="${orderId}"/>
                <input type="hidden" name="orderItemSeqId" value="${orderItem.orderItemSeqId}"/>
                
                <@table type="data-complex"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" --> <#-- orig: border="0" -->
                
              <#list OISGAssContents as OISGAssContent>
                <#assign OISG = OISGAssContent.getRelatedOne("OrderItemShipGroup")>
                <#assign orderShipments = OISGAssContent.getRelated("OrderShipment")>
                      
                <@tr>
                  <@td width="40%">&nbsp;</@td>
                  <@td width="40%">
                    <input name="_rowSubmit_o_${rowCount}" value="Y" type="hidden"/>
                    <input type="hidden" name="orderId_o_${rowCount}" value="${orderId}"/>
                    <input type="hidden" name="orderItemSeqId_o_${rowCount}" value="${orderItem.orderItemSeqId}"/>
                    <input type="hidden" name="shipGroupSeqId_o_${rowCount}" value="${OISG.shipGroupSeqId}"/>
                    <input type="hidden" name="rowCount_o_${rowCount}" value="${rowCount}"/>
                      <div> [${OISG.shipGroupSeqId}] <#if OISG.shipByDate?has_content>, ${uiLabelMap.OrderShipBeforeDate} : ${OISG.shipByDate?date}</#if></div>
                          <#if orderType == "SALES_ORDER">
                              <#list orderShipments as orderShipment>
                      <div>${uiLabelMap.OrderPlannedInShipment} : </b><a target="facility" href="<@ofbizInterWebappUrl>/facility/control/EditShipment?shipmentId=${orderShipment.shipmentId!}&externalLoginKey=${externalLoginKey}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}" style="font-size: xx-small;">${orderShipment.shipmentId!}</a>:${orderShipment.shipmentItemSeqId!} - ${orderShipment.quantity!}</div>
                              </#list>
                          <#elseif orderType == "PURCHASE_ORDER">
                              <#list orderShipments as orderShipment>
                                  <#if orderShipment.quantity?has_content & orderShipment.quantity != 0.0>
                      <div>${uiLabelMap.OrderPlannedInReceive} : </b><a target="facility" href="<@ofbizInterWebappUrl>/facility/control/ViewReceiveShipment?shipmentId=${orderShipment.shipmentId!}&externalLoginKey=${externalLoginKey}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}" style="font-size: xx-small;">${orderShipment.shipmentId!}</a>:${orderShipment.shipmentItemSeqId!} - ${orderShipment.quantity!}</div>
                                  <#else>
                                      <#assign shipmentItem = orderShipment.getShipmentItem()>
                      <div>${uiLabelMap.OrderPlannedRejected} : </b><a target="facility" href="<@ofbizInterWebappUrl>/facility/control/ViewReceiveShipment?shipmentId=${orderShipment.shipmentId!}&externalLoginKey=${externalLoginKey}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}" style="font-size: xx-small;">${orderShipment.shipmentId!}</a>:${orderShipment.shipmentItemSeqId!} - ${shipmentItem.quantity!}</div>
                                  </#if>
                              </#list>
                          </#if>
                  </@td>
                  <@td width="20%" class="${styles.text_right!}">
                      <div id="displayQuantity${index}${rowCount}">${OISGAssContent.quantity!}</div>
                          <#if (orderShipments.size()?default(0)) == 0>
                      <div id="editQuantity${index}${rowCount}" style="display: none;"><input id="edit${index}_o_${rowCount}" name="quantity_o_${rowCount}" size="5" value="${OISGAssContent.quantity!}" title="${OISGAssContent.quantity!}" /></div>
                          <#else>
                      <div id="editQuantity${index}${rowCount}" style="display: none;">${OISGAssContent.quantity!}</div>
                      <input type="hidden" name="quantity_o_${rowCount}" value="${OISGAssContent.quantity!}"/>
                          </#if>
                  </@td>
                </@tr>
                <#assign rowCount = rowCount + 1> 
              </#list>
                </@table> 
                
                <input type="hidden" name="_rowCount" value="${rowCount}"/>
              </form>
            
            </@td>
          </@tr>  
                  <#if !orderItem.statusId?exists || orderItem.statusId == "ITEM_CREATED" || orderItem.statusId == "ITEM_APPROVED" && (orderHeader.statusId != "ORDER_SENT" && orderHeader.statusId != "ORDER_COMPLETED" && orderHeader.statusId != "ORDER_REJECTED" && orderHeader.statusId != "ORDER_CANCELLED")>
          <@tr>
              
              <@td colspan="3">&nbsp;</@td>
              <@td colspan="2">
                <form method="post" action="<@ofbizUrl>AddOrderItemShipGroupAssoc?view=OISGA</@ofbizUrl>" name="addOISGForm${index}"/>
                <input type="hidden" name="editQuantity" value="edit"/>
                <input type="hidden" name="editQuantityIndex" value="${index}"/>
                <input type="hidden" name="orderId" value="${orderId}"/>
                <input type="hidden" name="orderItemSeqId" value="${orderItem.orderItemSeqId}"/>
                <input type="hidden" name="quantity" value="0"/>
                  <@table type="fields"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                      <@tr>
                          <@td>
                             <div class="label">${uiLabelMap.OrderAddToshipGroup} : </div>
                             <div>
                                 <@field type="select" name="shipGroupSeqId" onChange="showShipByDate(this, 'shipByDate${index}')">
                                    <#list shipGroups as shipGroup>
                                     <option value="${shipGroup.shipGroupSeqId}">[${shipGroup.shipGroupSeqId}]<#if shipGroup.shipByDate?has_content>, ${shipGroup.shipByDate?date}</#if></option>
                                    </#list>
                                     <option value="new">${uiLabelMap.CommonNew}</option>
                                 </@field>
                             </div>
                         </@td>
                     </@tr>
                     <@tr>
                         <@td>
                             <div style="display:none" id="shipByDate${index}">
                                 <div class="label">${uiLabelMap.OrderShipBeforeDate} : </div>
                                 <div><@field type="datetime" name="shipByDate" value=(requestParameters.maxDate!) size="25" maxlength="30" id="shipByDate_${index}" /></div>
                             </div>
                         </@td>
                     </@tr>
                     <@tr>
                         <@td>
                             <@field type="submit" submitType="link" href="javascript:document.addOISGForm${index}.submit()" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonAdd/>
                         </@td>
                     </@tr>
                 </@table>
               </form>
             </@td>
             
         </@tr>
                  </#if>
              </#if>
              <#assign index = index + 1>
          </#list>
        </@tbody>
      </@table>
    </@fields>
  </@section>
<#else>
<#list shipGroups as shipGroup>
  <#assign shipmentMethodType = shipGroup.getRelatedOne("ShipmentMethodType", false)!>
  <#assign shipGroupAddress = shipGroup.getRelatedOne("PostalAddress", false)!>
  <#assign shipGroup = shipGroup> <#-- SCIPIO: make this global so macro can access - ftl kludge -->
    <#macro menuContent menuArgs={}>
       <@menu args=menuArgs>
         <#--<@menuitem type="link" onclick="javascript:toggleScreenlet(this, 'ShipGroupScreenletBody_${shipGroup.shipGroupSeqId}', 'true', '${escapeVal(rawLabel('CommonExpand'), 'js')}', '${escapeVal(rawLabel('CommonCollapse'), 'js')}');" text=" " title="Collapse" class="+${styles.action_run_local!} ${styles.action_hide!}" />-->
         <@menuitem type="link" href=makeOfbizUrl("shipGroups.pdf?orderId=${orderId}&shipGroupSeqId=${shipGroup.shipGroupSeqId}") text="${rawLabel('OrderShipGroup')} PDF" target="_BLANK" class="+${styles.action_run_sys!} ${styles.action_export!}"/>
         <#-- Foundation: Button migrated from removed header to access OISGA -->
         <#if !parameters.view?has_content>
           <@menuitem type="link" href=makeOfbizUrl("orderShipping?orderId=${orderId}&view=OISGA") text=uiLabelMap.OrderShipmentInformationByOrderItem class="+${styles.action_run_sys!} ${styles.action_view!}" />
         </#if>
       </@menu>
    </#macro>
    <@section title="${rawLabel('OrderShipmentInformation')} - ${rawString(shipGroup.shipGroupSeqId)}" menuContent=menuContent>
        <@fields type="default-manual">
            <@table type="fields"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                    <form name="updateOrderItemShipGroup" method="post" action="<@ofbizUrl>updateShipGroupShipInfo</@ofbizUrl>">
                        <input type="hidden" name="orderId" value="${orderId!}"/>
                        <input type="hidden" name="shipGroupSeqId" value="${shipGroup.shipGroupSeqId!}"/>
                        <input type="hidden" name="contactMechPurposeTypeId" value="SHIPPING_LOCATION"/>
                        <input type="hidden" name="oldContactMechId" value="${shipGroup.contactMechId!}"/>            
                        
                        <#if orderHeader.orderTypeId == "SALES_ORDER">
                          <@tr>
                            <@td scope="row" class="${styles.grid_large!}3">
                                ${uiLabelMap.CommonMethod}
                            </@td>
                            <@td valign="top" colspan="3">
                                <#if orderHeader?has_content && orderHeader.statusId != "ORDER_CANCELLED" && orderHeader.statusId != "ORDER_COMPLETED" && orderHeader.statusId != "ORDER_REJECTED">
                                        <#-- passing the shipmentMethod value as the combination of three fields value
                                        i.e shipmentMethodTypeId & carrierPartyId & roleTypeId. Values are separated by
                                        "@" symbol.
                                        -->   
                                        <@row>
                                            <@cell columns=6>
                                                <select name="shipmentMethod">
                                                    <#if shipGroup.shipmentMethodTypeId?has_content>
                                                      <option value="${shipGroup.shipmentMethodTypeId}@${shipGroup.carrierPartyId!}@${shipGroup.carrierRoleTypeId!}"><#if shipGroup.carrierPartyId?? && shipGroup.carrierPartyId != "_NA_">${shipGroup.carrierPartyId!}</#if>&nbsp;${shipmentMethodType.get("description",locale)!}</option>
                                                    </#if>
                                                      <#list shipGroupShippingMethods[shipGroup.shipGroupSeqId] as productStoreShipmentMethod>
                                                      <#assign shipmentMethodTypeAndParty = productStoreShipmentMethod.shipmentMethodTypeId + "@" + productStoreShipmentMethod.partyId + "@" + productStoreShipmentMethod.roleTypeId>
                                                      <#if productStoreShipmentMethod.partyId?has_content || productStoreShipmentMethod?has_content>
                                                        <option value="${shipmentMethodTypeAndParty!}"><#if productStoreShipmentMethod.partyId != "_NA_">${productStoreShipmentMethod.partyId!}</#if>&nbsp;${productStoreShipmentMethod.get("description",locale)?default("")}</option>
                                                      </#if>
                                                    </#list>
                                                </select>
                                                
                                            </@cell>
                                            <@cell columns=6>
                                                <#if orderHeader?has_content && orderHeader.statusId != "ORDER_CANCELLED" && orderHeader.statusId != "ORDER_COMPLETED" && orderHeader.statusId != "ORDER_REJECTED">
                                                    <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                                                </#if>
                                            </@cell>
                                        </@row>
                                    <#else>
                                        <#if (shipGroup.carrierPartyId)?default("_NA_") != "_NA_">
                                            ${shipGroup.carrierPartyId!}
                                        </#if>
                                        <#if shipmentMethodType?has_content>
                                            ${shipmentMethodType.get("description",locale)?default("")}
                                        </#if>
                                    </#if>
                                </@td>
                          </@tr>
                        </#if>
                        <@tr>
                            <@td scope="row" class="${styles.grid_large!}3">
                                ${uiLabelMap.OrderAddress}
                            </@td>
                            <@td valign="top" colspan="3">
                                <#if orderHeader?has_content && orderHeader.statusId != "ORDER_CANCELLED" && orderHeader.statusId != "ORDER_COMPLETED" && orderHeader.statusId != "ORDER_REJECTED">
                                    <@row>
                                        <@cell columns=6>
                                            
                                              <select name="contactMechId">
                                                    <option selected="selected" value="${shipGroup.contactMechId!}">${(shipGroupAddress.address1)!""} - ${shipGroupAddress.city!""}</option>
                                                  <#if shippingContactMechList?has_content>
                                                    <option disabled="disabled" value=""></option>
                                                  <#list shippingContactMechList as shippingContactMech>
                                                    <#assign shippingPostalAddress = shippingContactMech.getRelatedOne("PostalAddress", false)!>
                                                    <#if shippingContactMech.contactMechId?has_content>
                                                      <option value="${shippingContactMech.contactMechId!}">${(shippingPostalAddress.address1)!""} - ${shippingPostalAddress.city!""}</option>
                                                    </#if>
                                                  </#list>
                                                  </#if>
                                                </select>
                                        </@cell>
                                        <@cell columns=6>
                                            <#if orderHeader?has_content && orderHeader.statusId != "ORDER_CANCELLED" && orderHeader.statusId != "ORDER_COMPLETED" && orderHeader.statusId != "ORDER_REJECTED">
                                                <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                                            </#if>
                                        </@cell>
                                    </@row>
                                <#else>
                                    ${(shipGroupAddress.address1)!""}
                                </#if>
                            </@td>
                        </@tr>
                        <#-- The usefulness of this information is limited. Uncomment in order to add these functions back in
                        <#if orderHeader?has_content && orderHeader.statusId != "ORDER_CANCELLED" && orderHeader.statusId != "ORDER_COMPLETED" && orderHeader.statusId != "ORDER_REJECTED">
                        <@tr>
                            <@td scope="row" class="${styles.grid_large!}3">&nbsp;</@td>
                            <@td valign="top" colspan="3">
                                <a class="${styles.link_nav!} ${styles.action_add!}" id="newShippingAddress" href="javascript:void(0);">${uiLabelMap.OrderNewShippingAddress}</a>
                                <@script>
                                    jQuery("#newShippingAddress").click(function(){jQuery("#newShippingAddressForm").dialog("open")});
                                </@script>
                            </@td>
                        </@tr>
                        </#if>
                      -->
                      <#if !shipGroup.contactMechId?has_content && !shipGroup.shipmentMethodTypeId?has_content>
                      <#assign noShipment = "true">
                        <@tr type="meta">
                            <@td colspan="2" align="center"><@commonMsg type="result">${uiLabelMap.OrderNotShipped}</@commonMsg></@td>
                        </@tr>
                      </#if>
                </form>
      <#-- Required for newShippingAddressFunction
      <div id="newShippingAddressForm" class="popup" style="display: none;">
        <form id="addShippingAddress" name="addShippingAddress" method="post" action="addShippingAddress">
          <@fields type="default">
          <input type="hidden" name="orderId" value="${orderId!}"/>
          <input type="hidden" name="partyId" value="${partyId!}"/>
          <input type="hidden" name="oldContactMechId" value="${shipGroup.contactMechId!}"/>
          <input type="hidden" name="shipGroupSeqId" value="${shipGroup.shipGroupSeqId!}"/>
          <input type="hidden" name="contactMechPurposeTypeId" value="SHIPPING_LOCATION"/>
          <@field type="input" required=true name="shipToAddress1" id="address1" value="" size="30" maxlength="30" label=uiLabelMap.PartyAddressLine1/>
          <@field type="input" name="shipToAddress2" id="address2" value="" size="30" maxlength="30" label=uiLabelMap.PartyAddressLine2/>
          <@field type="input" required=true name="shipToCity" id="city" value="" size="30" maxlength="30" label=uiLabelMap.PartyCity />
          <@field type="input" required=true class="+number" name="shipToPostalCode" id="postalCode" value="" size="30" maxlength="10" label=uiLabelMap.PartyZipCode/>
          <@field type="select" required=true name="shipToCountryGeoId" id="countryGeoId" label=uiLabelMap.CommonCountry>
            <#if countryGeoId??>
              <option value="${countryGeoId}">${countryGeoId}</option>
            </#if>
            <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"countriesPreselect":!countryGeoId??}/>
          </@field>
          <@field type="select" name="shipToStateProvinceGeoId" id="stateProvinceGeoId" label=uiLabelMap.PartyState>
            <#if stateProvinceGeoId?has_content>
              <option value="${stateProvinceGeoId}">${stateProvinceGeoId}</option>
            <#else>
              <option value="_NA_">${uiLabelMap.PartyNoState}</option>
            </#if>
          </@field>

          <@field type="submitarea">
            <@field type="submit" submitType="input-button" id="submitAddShippingAddress" text=uiLabelMap.CommonSubmit style="display:none"/>
            <form action="">
              <@field type="submit" submitType="input-button" class="+popup_closebox ${styles.link_run_local!} ${styles.action_close!}" text=uiLabelMap.CommonClose style="display:none"/>
            </form>
          </@field>
          </@fields>
        </form>
      </div>
      
      <@script>
       jQuery(document).ready( function() {
        jQuery("#newShippingAddressForm").dialog({autoOpen: false, modal: true,
                buttons: {
                '${uiLabelMap.CommonSubmit}': function() {
                    var addShippingAddress = jQuery("#addShippingAddress");
                    jQuery("<p>${uiLabelMap.CommonUpdatingData}</p>").insertBefore(addShippingAddress);
                    addShippingAddress.submit();
                },
                '${uiLabelMap.CommonClose}': function() {
                    jQuery(this).dialog('close');
                    }
                }
                });
       });
      </@script>
    -->


        <#if shipGroup.supplierPartyId?has_content>
           <#assign OISGAContent = shipGroup.getRelated("OrderItemShipGroupAssoc")>
           <#-- New in Ofbiz 14.12 -->
           <#if OISGAContent.size() == 0>
           <@tr>
              <@td colspan="2" valign="top" width="100%" align="center">
                   <a href="javascript:document.deleteOISG_${shipGroup.shipGroupSeqId}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.DeleteOrderItemShipGroup}</a>
                   <form name="deleteOISG_${shipGroup.shipGroupSeqId}" method="post" action="<@ofbizInterWebappUrl>/ordermgr/control/DeleteOrderItemShipGroup</@ofbizInterWebappUrl>">
                     <input type="hidden" name="orderId" value="${orderId}"/>
                     <input type="hidden" name="shipGroupSeqId" value="${shipGroup.shipGroupSeqId}"/>
                   </form>
              </@td>
           </@tr>
           </#if>
          <@tr>
            <@td scope="row" class="${styles.grid_large!}3">
              ${uiLabelMap.ProductDropShipment} - ${uiLabelMap.PartySupplier}
            </@td>
            <@td valign="top" colspan="3">
              ${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, shipGroup.supplierPartyId, false)!shipGroup.supplierPartyId}
            </@td>
          </@tr>
        </#if>

        <#-- This section appears when Shipment of order is in picked status and its items are packed,this case comes when new shipping estimates based on weight of packages are more than or less than default percentage (defined in shipment.properties) of original shipping estimate-->
        <#-- getShipGroupEstimate method of ShippingEvents class can be used for get shipping estimate from system, on the basis of new package's weight -->
        <#if shippingRateList?has_content>
          <#if orderReadHelper.getOrderTypeId() != "PURCHASE_ORDER">
  
            <@tr>
              <@td colspan="2">
                <@table type="fields">
                  <@tr>
                    <@td>
                      ${uiLabelMap.OrderOnlineUPSShippingEstimates}
                    </@td>
                  </@tr>
                  <form name="UpdateShippingMethod" method="post" action="<@ofbizUrl>updateShippingMethodAndCharges</@ofbizUrl>">
                    <#list shippingRateList as shippingRate>
                      <@tr>
                        <@td>
                          <#assign shipmentMethodAndAmount = shippingRate.shipmentMethodTypeId + "@" + "UPS" + "*" + shippingRate.rate>
                          <input type="radio" name="shipmentMethodAndAmount" value="${shipmentMethodAndAmount!}" />
                          UPS&nbsp;${shippingRate.shipmentMethodDescription!}
                          <#if (shippingRate.rate > -1)>
                            <@ofbizCurrency amount=shippingRate.rate isoCode=orderReadHelper.getCurrency()/>
                          <#else>
                            ${uiLabelMap.OrderCalculatedOffline}
                          </#if>
                        </@td>
                      </@tr>
                    </#list>
                    <input type="hidden" name="shipmentRouteSegmentId" value="${shipmentRouteSegmentId!}"/>
                    <input type="hidden" name="shipmentId" value="${pickedShipmentId!}"/>
                    <input type="hidden" name="orderAdjustmentId" value="${orderAdjustmentId!}"/>
                    <input type="hidden" name="orderId" value="${orderId!}"/>
                    <input type="hidden" name="shipGroupSeqId" value="${shipGroup.shipGroupSeqId!}"/>
                    <input type="hidden" name="contactMechPurposeTypeId" value="SHIPPING_LOCATION"/>
                    <input type="hidden" name="oldContactMechId" value="${shipGroup.contactMechId!}"/>
                    <input type="hidden" name="shippingAmount" value="${shippingAmount!}"/>
                    <@tr>
                      <@td valign="top" width="80%">
                        <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
                      </@td>
                    </@tr>
                  </form>
                </@table>
              </@td>
            </@tr>
          </#if>
        </#if>

        <#-- tracking number -->
        <#if shipGroup.trackingNumber?has_content || orderShipmentInfoSummaryList?has_content>

          <@tr>
            <@td scope="row" class="${styles.grid_large!}3">
              ${uiLabelMap.OrderTrackingNumber}
            </@td>
            <@td valign="top" colspan="3">
              <#-- TODO: add links to UPS/FEDEX/etc based on carrier partyId  -->
              <#if shipGroup.trackingNumber?has_content>
                ${shipGroup.trackingNumber}
              </#if>
              <#if orderShipmentInfoSummaryList?has_content>
                <#list orderShipmentInfoSummaryList as orderShipmentInfoSummary>
                  <#if (orderShipmentInfoSummary.shipGroupSeqId!) == (shipGroup.shipGroupSeqId!)>
                    <div>
                      <#if (orderShipmentInfoSummaryList?size > 1)>${orderShipmentInfoSummary.shipmentPackageSeqId}: </#if>
                      ${uiLabelMap.CommonIdCode}: ${orderShipmentInfoSummary.trackingCode?default("[${uiLabelMap.OrderNotYetKnown}]")}
                      <#if orderShipmentInfoSummary.boxNumber?has_content> ${uiLabelMap.ProductBox} #${orderShipmentInfoSummary.boxNumber}</#if>
                      <#if orderShipmentInfoSummary.carrierPartyId?has_content>(${uiLabelMap.ProductCarrier}: ${orderShipmentInfoSummary.carrierPartyId})</#if>
                    </div>
                  </#if>
                </#list>
              </#if>
            </@td>
          </@tr>
        </#if>
        <#--    
        <#if shipGroup.maySplit?has_content && noShipment?default("false") != "true">
          <@tr>
            <@td scope="row" class="${styles.grid_large!}3">
              ${uiLabelMap.OrderSplittingPreference}
            </@td>
            <@td valign="top" colspan="3">
                <#if shipGroup.maySplit?upper_case == "N">
                    <#if security.hasEntityPermission("ORDERMGR", "_UPDATE", session)>
                      <#if orderHeader.statusId != "ORDER_COMPLETED" && orderHeader.statusId != "ORDER_CANCELLED">
                        <form name="allowordersplit_${shipGroup.shipGroupSeqId}" method="post" action="<@ofbizUrl>allowordersplit</@ofbizUrl>">
                          <input type="hidden" name="orderId" value="${orderId}"/>
                          <input type="hidden" name="shipGroupSeqId" value="${shipGroup.shipGroupSeqId}"/>
                        </form>
                        <a href="javascript:document.allowordersplit_${shipGroup.shipGroupSeqId}.submit()" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.OrderAllowSplit}</a>
                      </#if>
                    </#if>
                <#else>
                    ${uiLabelMap.FacilityShipAvailable}
                </#if>
              </@td>
          </@tr>
        </#if>
        -->

        <@tr>
          <@td scope="row" class="${styles.grid_large!}3">
            ${uiLabelMap.OrderInstructions}
          </@td>
          <@td align="left" valign="top" width="80%">
            <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
              <form id="updateShippingInstructionsForm_${shipGroup.shipGroupSeqId}" name="updateShippingInstructionsForm" method="post" action="<@ofbizUrl>setShippingInstructions</@ofbizUrl>">
                <input type="hidden" name="orderId" value="${orderHeader.orderId}"/>
                <input type="hidden" name="shipGroupSeqId" value="${shipGroup.shipGroupSeqId}"/>
                <@row>
                    <@cell columns=6>
                        <@field type="textarea" name="shippingInstructions" id="shippingInstructions_${shipGroup.shipGroupSeqId}" style="display:none" rows="0" cols="0">${shipGroup.shippingInstructions!}</@field>
                    </@cell>
                    <@cell columns=6>
                        <@field type="submit" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys!} ${styles.action_update!}"/>                    
                    </@cell>
                </@row>
              </form>
            <#else>
              <#if shipGroup.shippingInstructions?has_content>
                ${shipGroup.shippingInstructions}
              <#else>
                ${uiLabelMap.OrderThisOrderDoesNotHaveShippingInstructions}
              </#if>
            </#if>
          </@td>
        </@tr>

        <#if shipGroup.isGift?has_content && noShipment?default("false") != "true">

        <@tr>
          <@td scope="row" class="${styles.grid_large!}3">
            ${uiLabelMap.OrderGiftMessage}
          </@td>
          <@td>
            <form id="setGiftMessageForm_${shipGroup.shipGroupSeqId}" name="setGiftMessageForm" method="post" action="<@ofbizUrl>setGiftMessage</@ofbizUrl>">
              <input type="hidden" name="orderId" value="${orderHeader.orderId}"/>
              <input type="hidden" name="shipGroupSeqId" value="${shipGroup.shipGroupSeqId}"/>
              <@row>
                    <@cell columns=6>
                        <@field type="textarea" name="giftMessage" id="giftMessage_${shipGroup.shipGroupSeqId}" style="display:none" rows="0" cols="0">${shipGroup.giftMessage!}</@field>
                    </@cell>
                    <@cell columns=6>
                        <@field type="submit" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys!} ${styles.action_update!}"/>                    
                    </@cell>
              </@row>
            </form>
          </@td>
        </@tr>
        </#if>

         <@tr>
            <@td scope="row" class="${styles.grid_large!}3">
              ${uiLabelMap.OrderShipBeforeDate}
            </@td>
            <@td>
                <form name="setShipGroupDates_${shipGroup.shipGroupSeqId}" method="post" action="<@ofbizUrl>updateOrderItemShipGroup</@ofbizUrl>">
                    <input type="hidden" name="orderId" value="${orderHeader.orderId}"/>
                    <input type="hidden" name="shipGroupSeqId" value="${shipGroup.shipGroupSeqId}"/>
                    <@row>
                        <@cell columns=6>
                            <@field type="datetime" dateType="date" name="shipByDate" value=(shipGroup.shipByDate!) size="25" maxlength="30" id="shipByDate_${shipGroup.shipGroupSeqId}" />
                        </@cell>
                        <@cell columns=6>
                            <@field type="submit" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys!} ${styles.action_update!}"/>                    
                        </@cell>
                    </@row>
                </form>
            </@td>
         </@tr>
         <@tr>
            <@td scope="row" class="${styles.grid_large!}3">
              ${uiLabelMap.OrderShipAfterDate}
            </@td>
            <@td >
                <form name="setShipGroupDates_${shipGroup.shipGroupSeqId}" method="post" action="<@ofbizUrl>updateOrderItemShipGroup</@ofbizUrl>">
                    <input type="hidden" name="orderId" value="${orderHeader.orderId}"/>
                    <input type="hidden" name="shipGroupSeqId" value="${shipGroup.shipGroupSeqId}"/>
                    <@row>
                        <@cell columns=6>
                            <@field type="datetime" dateType="date" name="shipAfterDate" value=(shipGroup.shipAfterDate!) size="25" maxlength="30" id="shipAfterDate_${shipGroup.shipGroupSeqId}" />
                        </@cell>
                        <@cell columns=6>
                            <@field type="submit" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys!} ${styles.action_update!}"/>                    
                        </@cell>
                    </@row>
                </form>
            </@td>
         </@tr>

       <#-- ToDo: These shipment options are somewhat useful, but we should consider adding them to the action menu instead-->
       <#if security.hasEntityPermission("ORDERMGR", "_UPDATE", session) && ((orderHeader.statusId == "ORDER_CREATED") || (orderHeader.statusId == "ORDER_APPROVED") || (orderHeader.statusId == "ORDER_SENT"))>
         <@tr>
            <@td colspan="4" valign="top" align="center">
             <#if orderHeader.orderTypeId == "SALES_ORDER">
               <#if !shipGroup.supplierPartyId?has_content>
                 <#if orderHeader.statusId == "ORDER_APPROVED">
                 <a href="<@ofbizInterWebappUrl>/facility/control/PackOrder?facilityId=${storeFacilityId!}&amp;orderId=${orderId}&amp;shipGroupSeqId=${shipGroup.shipGroupSeqId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.OrderPackShipmentForShipGroup}</a>
                 <br />
                 </#if>
                 <a href="javascript:document.createShipment_${shipGroup.shipGroupSeqId}.submit()" class="${styles.link_run_sys!} ${styles.action_add!}">${uiLabelMap.OrderNewShipmentForShipGroup}</a>
                 <form name="createShipment_${shipGroup.shipGroupSeqId}" method="post" action="<@ofbizInterWebappUrl>/facility/control/createShipment</@ofbizInterWebappUrl>">
                   <input type="hidden" name="primaryOrderId" value="${orderId}"/>
                   <input type="hidden" name="primaryShipGroupSeqId" value="${shipGroup.shipGroupSeqId}"/>
                   <input type="hidden" name="statusId" value="SHIPMENT_INPUT" />
                   <input type="hidden" name="facilityId" value="${storeFacilityId!}" />
                   <input type="hidden" name="estimatedShipDate" value="${shipGroup.shipByDate!}"/>
                 </form>
               </#if>
             <#else>
               <#assign facilities = facilitiesForShipGroup.get(shipGroup.shipGroupSeqId)>
               <#if facilities?has_content>
                   <div>
                    <form name="createShipment2_${shipGroup.shipGroupSeqId}" method="post" action="<@ofbizInterWebappUrl>/facility/control/createShipment</@ofbizInterWebappUrl>">
                       <input type="hidden" name="primaryOrderId" value="${orderId}"/>
                       <input type="hidden" name="primaryShipGroupSeqId" value="${shipGroup.shipGroupSeqId}"/>
                       <input type="hidden" name="shipmentTypeId" value="PURCHASE_SHIPMENT"/>
                       <input type="hidden" name="statusId" value="PURCH_SHIP_CREATED"/>
                       <input type="hidden" name="externalLoginKey" value="${externalLoginKey}"/>
                       <input type="hidden" name="estimatedShipDate" value="${shipGroup.estimatedShipDate!}"/>
                       <input type="hidden" name="estimatedArrivalDate" value="${shipGroup.estimatedDeliveryDate!}"/>
                       <select name="destinationFacilityId">
                         <#list facilities as facility>
                           <option value="${facility.facilityId}">${facility.facilityName}</option>
                         </#list>
                       </select>
                       <input type="submit" class="${styles.link_run_sys!} ${styles.action_add!}" value="${uiLabelMap.OrderNewShipmentForShipGroup} [${shipGroup.shipGroupSeqId}]"/>
                    </form>
                    </div>
               <#else>
                   <a href="javascript:document.quickDropShipOrder_${shipGroup_index}.submit();" class="${styles.link_run_sys!} ${styles.action_complete!}">${uiLabelMap.ProductShipmentQuickComplete}</a>
                   <a href="javascript:document.createShipment3_${shipGroup.shipGroupSeqId}.submit();" class="${styles.link_run_sys_long!} ${styles.action_add!}">${uiLabelMap.OrderNewDropShipmentForShipGroup} [${shipGroup.shipGroupSeqId}]</a>
                   <form name="quickDropShipOrder_${shipGroup_index}" method="post" action="<@ofbizUrl>quickDropShipOrder</@ofbizUrl>">
                        <input type="hidden" name="orderId" value="${orderId}"/>
                        <input type="hidden" name="shipGroupSeqId" value="${shipGroup.shipGroupSeqId}"/>
                        <input type="hidden" name="externalLoginKey" value="${externalLoginKey}" />
                    </form>
                    <form name="createShipment3_${shipGroup.shipGroupSeqId}" method="post" action="<@ofbizInterWebappUrl>/facility/control/createShipment</@ofbizInterWebappUrl>">
                        <input type="hidden" name="primaryOrderId" value="${orderId}"/>
                        <input type="hidden" name="primaryShipGroupSeqId" value="${shipGroup.shipGroupSeqId}"/>
                        <input type="hidden" name="shipmentTypeId" value="DROP_SHIPMENT" />
                        <input type="hidden" name="statusId" value="PURCH_SHIP_CREATED" />
                        <input type="hidden" name="externalLoginKey" value="${externalLoginKey}" />
                    </form>
               </#if>
             </#if>
            </@td>
         </@tr>
       </#if>
        
      </@table>
    </@fields>
    </@section>
</#list>
</#if>
</#if>
