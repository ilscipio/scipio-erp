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

<#-- SCIPIO: TODO: Uncomment return link once converted -->

<#-- NOTE: this template is used for the orderstatus screen in shop AND for order notification emails through the OrderNoticeEmail.ftl file -->
<#-- the "urlPrefix" value will be prepended to URLs by the ofbizUrl transform if/when there is no "request" object in the context -->
<#if baseEcommerceSecureUrl??><#assign urlPrefix = baseEcommerceSecureUrl/></#if>
<#if (orderHeader.externalId)?? && (orderHeader.externalId)?has_content >
  <#assign externalOrder = "(" + orderHeader.externalId + ")"/>
</#if>

<#-- SCIPIO: NOTE: This is used for checkout review and order summaries 
    Check orderHeader?has_content to see if is in-progress or placed order. -->

<#assign maySelect = ((maySelectItems!"N") == "Y")>
<#assign printable = printable!false>

<#if (orderHeader.orderId)??>
  <@heading>
    <#-- SCIPIO: This page doesn't actually show a full invoice - only the PDF is a full invoice (with tax information) - so "PDF" beside title is misleading -->
    ${getLabel("OrderOrderId")}: ${orderHeader.orderId}<#--<#if !maySelect && !printable> (<a href="<@ofbizUrl fullPath="true">order.pdf?orderId=${(orderHeader.orderId)!}</@ofbizUrl>" target="_BLANK" class="${styles.action_export!}">${uiLabelMap.CommonPdf} ${uiLabelMap.CommonInvoice}</a>)</#if>-->
  </@heading>
</#if>

<#macro menuContent menuArgs={}>
<#if (orderHeader.orderId)?has_content><#-- SCIPIO: Only if order not yet placed -->
  <#if !printable>
    <@menu args=menuArgs>
      <#-- SCIPIO: No reason to hide it: 
      <#if maySelect>-->
      <@menuitem type="link" href=makeOfbizUrl({"uri":"orderprint?orderId=" + (orderHeader.orderId)!, "fullPath":true}) target="_BLANK" class="+${styles.action_export!}" text=uiLabelMap.CommonPrintable />
      <#--</#if>-->
      <#-- above will be better
      <#if maySelect>
        <@menuitem type="link" href=makeOfbizUrl({"uri":"orderviewonly?orderId=" + (orderHeader.orderId)!, "fullPath":true}) target="_BLANK" class="+${styles.action_export!}" text=uiLabelMap.CommonPrintable />
      </#if>
      -->
      <#-- SCIPIO: Always show it here: <#if maySelect>-->
      <#-- NOTE: The order may actually have more than one invoice available. On this page, show only this one for now, because the
          others don't become available until after order is completed and stuff. -->
      <@menuitem type="link" href=makeOfbizUrl({"uri":"order.pdf?orderId=" + escapeVal((orderHeader.orderId)!, 'js'), "fullPath":true}) target="_BLANK" class="+${styles.action_export!}" text="${rawLabel('EcommerceOrderConfirmation')} (${rawLabel('CommonPdf')})" />
      <#--</#if>-->
      <#-- SCIPIO: TODO: Uncomment once converted/tested
      <#if maySelect && (returnLink!"N") == "Y" && ((orderHeader.statusId)!) == "ORDER_COMPLETED" && (roleTypeId!) == "PLACING_CUSTOMER">
        <@menuitem type="link" href=makeOfbizUrl("makeReturn?orderId=${orderHeader.orderId}") text=uiLabelMap.OrderRequestReturn />
      </#if>-->
    </@menu>
  </#if>
</#if>
</#macro>
<@section menuContent=menuContent>
    
    <#-- orderinfo -->
    <@row>
        <@cell columns=4>
            <@section title=uiLabelMap.CommonOverview containerClass="+${styles.email_callout_table!'callout'}" cellClass="+${styles.email_callout_table_cell!'callout-inner secondary'}">
                <@table type="fields">
                  <#if placingParty?has_content && orderDate?has_content>
                    <#-- SCIPIO: screen finds it -->
                    <#--<#assign displayParty = localOrderReadHelper.getPlacingParty()!/>-->
                    <#assign displayParty = placingParty/>
                    <#assign displayPartyNameResult = {}/>
                    <#if displayParty?has_content>
                        <#assign displayPartyNameResult = dispatcher.runSync("getPartyNameForDate", {"partyId":(displayParty.partyId!), "compareDate":(orderDate!), "userLogin":userLogin!})/>
                    </#if>
                    <#if displayPartyNameResult?has_content>
                        <@tr>
                          <@td class="${styles.grid_large!}2">${uiLabelMap.PartyName}</@td>
                          <@td colspan="3">${(displayPartyNameResult.fullName)!"[Name Not Found]"}</@td>
                        </@tr>
                    </#if>
                  </#if>
                    <#-- SCIPIO: Show the emails (from placing party + additional, combined due to schema) -->
                  <#if orderEmailList?has_content>
                    <@tr>
                      <@td class="${styles.grid_large!}2">${uiLabelMap.CommonEmail}</@td>
                      <@td colspan="3">
                        <#-- NOTE: Make sure to show a comma, because this is the accepted delimiter -->
                        <#list orderEmailList as email>
                          ${email}<#if email_has_next>, </#if>
                        </#list>
                      </@td>
                    </@tr>
                  </#if>
                    <@tr>
                      <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonStatus}</@td>
                      <@td colspan="3">
                        <#if orderHeader?has_content>
                            ${localOrderReadHelper.getStatusString(locale)}
                        <#else>
                            ${uiLabelMap.OrderNotYetOrdered}
                        </#if>
                      </@td>
                    </@tr>
                  <#if orderHeader?has_content><#-- SCIPIO: Show only for placed orders (for non-placed, context.orderDate is not the actual placing time) -->
                    <@tr>
                      <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderDateOrdered}</@td>
                      <@td colspan="3">
                          <#if orderHeader.orderDate?has_content><@formattedDateTime date=orderHeader.orderDate /></#if>
                      </@td>
                    </@tr>
                  </#if>
                  <#if distributorId??>
                    <@tr>
                      <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderDistributor}</@td>
                      <@td colspan="3">
                         <#assign distPartyNameResult = dispatcher.runSync("getPartyNameForDate", {"partyId":distributorId, "compareDate":orderHeader.orderDate, "userLogin":userLogin})/>
                         ${distPartyNameResult.fullName?default("[${uiLabelMap.OrderPartyNameNotFound}]")}
                      </@td>
                    </@tr>
                  </#if>
                
                  <#if affiliateId??>
                    <@tr>
                      <@td>${uiLabelMap.OrderAffiliate}</@td>
                      <@td colspan="3">
                        <#assign affPartyNameResult = dispatcher.runSync("getPartyNameForDate", {"partyId":affiliateId, "compareDate":orderHeader.orderDate, "userLogin":userLogin})/>
                        ${affPartyNameResult.fullName?default("[${uiLabelMap.OrderPartyNameNotFound}]")}
                      </@td>
                    </@tr>
                  </#if>
            
                </@table>
            </@section>
        </@cell>

        <#-- payment info -->
        <@cell columns=4>
            <#if paymentMethods?has_content || paymentMethodType?has_content || billingAccount?has_content>
                <@section title=uiLabelMap.AccountingPaymentInformation containerClass="+${styles.email_callout_table!'callout'}" cellClass="+${styles.email_callout_table_cell!'callout-inner secondary'}">
                    <@table type="fields">
                        <#macro paymentMethodAmount paymentMethodId>
                          <#if (paymentMethodAmountMap[paymentMethodId])?has_content>
                            <br/><strong><@ofbizCurrency amount=(paymentMethodAmountMap[paymentMethodId]!0) isoCode=((paymentMethodAmountMap[paymentMethodId].currencyUomId)!currencyUomId)/></strong>
                          </#if>
                        </#macro>

                        <#if !paymentMethod?has_content && paymentMethodType?has_content>
                        
                            <#-- offline payment -->
                            <#if paymentMethodType.paymentMethodTypeId == "EXT_OFFLINE">
                                <@tr>
                                      <#assign offPayTitle>${uiLabelMap.AccountingOfflinePayment}
                                        <@paymentMethodAmount paymentMethodId="EXT_OFFLINE" /></#assign>
                                      <#assign offPayDesc>
                                        <#if orderHeader?has_content && paymentAddress?has_content>
                                          <strong>${uiLabelMap.OrderSendPaymentTo}:</strong></br>
                                          <#if paymentAddress.toName?has_content>${paymentAddress.toName}<br/></#if>
                                          <#if paymentAddress.attnName?has_content>${uiLabelMap.PartyAddrAttnName}: ${paymentAddress.attnName}<br/></#if>
                                          ${paymentAddress.address1}<br/>
                                          <#if paymentAddress.address2?has_content>${paymentAddress.address2}<br/></#if>
                                          <#assign paymentStateGeo = (delegator.findOne("Geo", {"geoId", paymentAddress.stateProvinceGeoId!}, false))! />
                                          ${paymentAddress.city}<#if paymentStateGeo?has_content>, ${paymentStateGeo.geoName!}</#if> ${paymentAddress.postalCode!}<br/>
                                          <#assign paymentCountryGeo = (delegator.findOne("Geo", {"geoId", paymentAddress.countryGeoId!}, false))! />
                                          <#if paymentCountryGeo?has_content>${paymentCountryGeo.geoName!}<br/></#if>
                                          <br/>
                                          ${uiLabelMap.EcommerceBeSureToIncludeYourOrderNb}
                                        </#if>
                                      </#assign>

                                    <@td class="${styles.grid_large!}2">${offPayTitle}</@td>
                                    <@td colspan="3">
                                      <#-- SCIPIO: only show alert after placed and not printable -->
                                      <#if orderHeader?has_content && !printable>
                                        <@alert type="info" closable=false>
                                          ${offPayDesc}
                                        </@alert>
                                      <#else>
                                        ${offPayDesc}
                                      </#if>
                                    </@td>
                                </@tr>
                            <#elseif paymentMethodType.paymentMethodTypeId == "EXT_LIGHTNING">
                                <@tr>
                                  <#assign xbtAmount = Static["org.ofbiz.common.uom.UomWorker"].convertUom(orderGrandTotal!grandTotal!0, currencyUomId!,"XBT",dispatcher)>
                                  <#assign offPayTitle>${uiLabelMap.AccountingPayWithBitcoin}
                                  </#assign>
                                  <#assign offPayDesc>
                                    <br/><strong><@ofbizCurrency amount=xbtAmount?default(0.00) rounding="8" isoCode="XBT"/></p></strong> 
                                  </#assign>
                                <@td class="${styles.grid_large!}2">${offPayTitle}</@td>
                                <@td colspan="3">
                                  <#-- SCIPIO: only show alert after placed and not printable -->
                                  <#if orderHeader?has_content && !printable>
                                    <@alert type="info" closable=false>
                                      ${offPayDesc}
                                    </@alert>
                                  <#else>
                                    ${offPayDesc}
                                  </#if>
                                </@td>
                            </@tr>
                            <#else>
                                <#-- ${uiLabelMap.AccountingPaymentVia} -->
                                <@tr>
                                    <@td class="${styles.grid_large!}2">${paymentMethodType.get("description",locale)}
                                      <@paymentMethodAmount paymentMethodId=paymentMethodType.paymentMethodTypeId /></@td>
                                    <@td colspan="3"></@td>
                                </@tr>
                            </#if>
                        </#if>

                        <#-- SCIPIO: changed: <#elseif paymentMethods?has_content>-->
                        <#if paymentMethods?has_content>
                            <#list paymentMethods as paymentMethod>
                                <#if "CREDIT_CARD" == paymentMethod.paymentMethodTypeId>
                                  <#assign creditCard = paymentMethod.getRelatedOne("CreditCard", false)>
                                <#elseif "GIFT_CARD" == paymentMethod.paymentMethodTypeId>
                                  <#assign giftCard = paymentMethod.getRelatedOne("GiftCard", false)>
                                <#elseif "EFT_ACCOUNT" == paymentMethod.paymentMethodTypeId>
                                  <#assign eftAccount = paymentMethod.getRelatedOne("EftAccount", false)>
                                </#if>

                                <#-- Credit Card info -->
                                <#if "CREDIT_CARD" == paymentMethod.paymentMethodTypeId && creditCard?has_content>
                                    <#assign pmBillingAddress = creditCard.getRelatedOne("PostalAddress", false)!>
                                    <@tr>
                                        <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingCreditCard}
                                          <@paymentMethodAmount paymentMethodId=paymentMethod.paymentMethodId/></@td>
                                        <@td colspan="3">
                                          <@formattedCreditCardDetail creditCard=creditCard paymentMethod=paymentMethod />
                                        </@td>
                                    </@tr>
                                </#if>

                                <#-- Gift Card info -->
                                <#if "GIFT_CARD" == paymentMethod.paymentMethodTypeId && giftCard?has_content>
                                    <#-- SCIPIO: TODO: SHOW GIFT CARD BALANCE -->
                                    <@tr>
                                        <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingGiftCard}
                                          <@paymentMethodAmount paymentMethodId=paymentMethod.paymentMethodId/></@td>
                                        <@td colspan="3"><@formattedGiftCardDetail giftCard=giftCard paymentMethod=paymentMethod /></@td>
                                    </@tr>
                                </#if>

                                <#-- EFT account info -->
                                <#if "EFT_ACCOUNT" == paymentMethod.paymentMethodTypeId && eftAccount?has_content>
                                    <#assign pmBillingAddress = eftAccount.getRelatedOne("PostalAddress", false)!>
                                    <@tr>
                                        <@td class="${styles.grid_large!}2">
                                            ${uiLabelMap.AccountingEFTAccount}
                                            <@paymentMethodAmount paymentMethodId=paymentMethod.paymentMethodId/>
                                        </@td>
                                        <@td>
                                            <@formattedEftAccountDetail eftAccount=eftAccount paymentMethod=paymentMethod />
                                        </@td>
                                    </@tr>
                                </#if>
                                
                                <#if pmBillingAddress?has_content>
                                <@tr>
                                    <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingBillingAddress}</@td>
                                    <@td colspan="3">
                                        <@formattedAddress address=pmBillingAddress />
                                    </@td>
                                </@tr>
                              </#if>
                            </#list>
                        </#if>
                        <#-- billing account info -->
                        <#if paymentMethods?has_content || paymentMethodType?has_content || billingAccount?has_content>
                            <#if billingAccount?has_content>
                                <@tr>
                                    <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingBillingAccount}
                                      <@paymentMethodAmount paymentMethodId="EXT_BILLACT"/>
                                    </@td>
                                    <@td colspan="3">
                                      <@formattedBillingAccountDetail billingAccount=billingAccount />
                                    </@td>
                                </@tr>
                            </#if>
                        </#if>
                        <#-- extra payment information 
                            SCIPIO: This form separated out from billing account -->
                        <#if paymentMethods?has_content || paymentMethodType?has_content>
                          <#if customerPoNumberSet?has_content>
                            <@tr>
                                <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingPaymentInformation}</@td>
                                <@td colspan="3">
                                  ${uiLabelMap.OrderPurchaseOrderNumber}
                                  <#list customerPoNumberSet as customerPoNumber>
                                    ${customerPoNumber!}
                                  </#list>
                                </@td>
                            </@tr>
                          </#if>
                        </#if>
                    </@table>   
                </@section>
            </#if>
        </@cell>

        <#-- shipping info -->
        <@cell columns=4>
            <#if orderItemShipGroups?has_content>
                <@section title=uiLabelMap.OrderShippingInformation containerClass="+${styles.email_callout_table!'callout'}" cellClass="+${styles.email_callout_table_cell!'callout-inner secondary'}">
                    <#-- shipping address -->

                    <#if orderItemShipGroups?has_content>
                        <#assign groupIdx = 0>
                        <#list orderItemShipGroups as shipGroup>
                          <#if orderHeader?has_content>
                            <#assign shippingAddress = shipGroup.getRelatedOne("PostalAddress", false)!>
                            <#assign groupNumber = shipGroup.shipGroupSeqId!>
                          <#else>
                            <#assign shippingAddress = cart.getShippingAddress(groupIdx)!>
                            <#assign groupNumber = groupIdx + 1>
                          </#if>
                          <@table type="fields">
                            <#if shippingAddress?has_content>
                                <@tr>
                                    <@td class="${styles.grid_large!}2">${uiLabelMap.OrderDestination} ${groupNumber}</@td>
                                    <@td colspan="3">
                                        <#if shippingAddress.toName?has_content>${uiLabelMap.CommonTo}: ${shippingAddress.toName}<br/></#if>
                                        <#if shippingAddress.attnName?has_content>${uiLabelMap.PartyAddrAttnName}: ${shippingAddress.attnName}<br/></#if>
                                        ${shippingAddress.address1}<br/>
                                        <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br/></#if>
                                        <#assign shippingStateGeo = (delegator.findOne("Geo", {"geoId", shippingAddress.stateProvinceGeoId!}, false))! />
                                        ${shippingAddress.city}<#if shippingStateGeo?has_content>, ${shippingStateGeo.geoName!}</#if> ${shippingAddress.postalCode!}<br/>
                                        <#assign shippingCountryGeo = (delegator.findOne("Geo", {"geoId", shippingAddress.countryGeoId!}, false))! />
                                        <#if shippingCountryGeo?has_content>${shippingCountryGeo.geoName!}</#if>

                                    </@td>
                                </@tr>
                            </#if>
                            <@tr>
                                <@td class="${styles.grid_large!}2">${uiLabelMap.OrderMethod}</@td>
                                <@td colspan="3">
                                    <#if orderHeader?has_content>
                                        <#assign shipmentMethodType = shipGroup.getRelatedOne("ShipmentMethodType", false)!>
                                        <#assign carrierPartyId = shipGroup.carrierPartyId!>
                                  <#else>
                                        <#assign shipmentMethodType = cart.getShipmentMethodType(groupIdx)!>
                                        <#assign carrierPartyId = cart.getCarrierPartyId(groupIdx)!>
                                  </#if>
                                    <#if carrierPartyId?? && carrierPartyId != "_NA_">${carrierPartyId!}<br/></#if>
                                    ${(shipmentMethodType.description)!(uiLabelMap.CommonNA)}<br/>
                                    <#if shippingAccount??>${uiLabelMap.AccountingUseAccount}: ${shippingAccount}</#if>

                                </@td>
                            </@tr>
                              
                          <#-- SCIPIO: Make no sense if no shipping! -->    
                          <#if (shipmentMethodType.shipmentMethodTypeId)?has_content && shipmentMethodType.shipmentMethodTypeId != "NO_SHIPPING">
                            <#-- tracking number -->
                            <#if trackingNumber?has_content || orderShipmentInfoSummaryList?has_content>
                                <@tr>
                                    <@td class="${styles.grid_large!}2">${uiLabelMap.OrderTrackingNumber}</@td>
                                    <@td colspan="3">
                                        <#-- TODO: add links to UPS/FEDEX/etc based on carrier partyId  -->
                                        <#if shipGroup.trackingNumber?has_content>
                                          ${shipGroup.trackingNumber}<br/>
                                        </#if>
                                        <#if orderShipmentInfoSummaryList?has_content>
                                          <#list orderShipmentInfoSummaryList as orderShipmentInfoSummary>
                                            <#if (orderShipmentInfoSummaryList?size > 1)>${orderShipmentInfoSummary.shipmentPackageSeqId}: <br/></#if>
                                            Code: ${orderShipmentInfoSummary.trackingCode?default("[Not Yet Known]")}<br/>
                                            <#if orderShipmentInfoSummary.boxNumber?has_content>${uiLabelMap.OrderBoxNumber}${orderShipmentInfoSummary.boxNumber}<br/></#if>
                                            <#if orderShipmentInfoSummary.carrierPartyId?has_content>(${uiLabelMap.ProductCarrier}: ${orderShipmentInfoSummary.carrierPartyId})<br/></#if>
                                          </#list>
                                        </#if>
                                    </@td>
                                </@tr>
                              </#if>


                              <#-- splitting preference -->
                              <#if orderHeader?has_content>
                                <#assign maySplit = shipGroup.maySplit!"N">
                              <#else>
                                <#assign maySplit = cart.getMaySplit(groupIdx)!"N">
                              </#if>

                                <@tr>
                                    <@td class="${styles.grid_large!}2">${uiLabelMap.OrderSplittingPreference}</@td>
                                    <@td colspan="3">
                                        <#if (maySplit!"N") == "N">${uiLabelMap.OrderPleaseWaitUntilBeforeShipping}.</#if>
                                        <#if (maySplit!"N") == "Y">${uiLabelMap.OrderPleaseShipItemsBecomeAvailable}.</#if>
                                    </@td>
                                </@tr>
                             
                              <#-- shipping instructions -->
                              <#if orderHeader?has_content>
                                <#assign shippingInstructions = shipGroup.shippingInstructions!>
                              <#else>
                                <#assign shippingInstructions =  cart.getShippingInstructions(groupIdx)!>
                              </#if>
                              <#if shippingInstructions?has_content>
                                <@tr>
                                    <@td class="${styles.grid_large!}2">${uiLabelMap.OrderInstructions}</@td>
                                    <@td colspan="3">
                                        ${shippingInstructions}
                                    </@td>
                                </@tr>
                              </#if>
                          </#if>

                              <#-- gift settings -->
                              <#if orderHeader?has_content>
                                <#assign isGift = shipGroup.isGift!"N">
                                <#assign giftMessage = shipGroup.giftMessage!>
                              <#else>
                                <#assign isGift = cart.getIsGift(groupIdx)!"N">
                                <#assign giftMessage = cart.getGiftMessage(groupIdx)!>
                              </#if>
                              <#-- SCIPIO: Only show if gift -->
                              <#if (isGift!"N") == "Y">
                                <#if ((productStore.showCheckoutGiftOptions)!) != "N">
                                <@tr>
                                  <@td class="${styles.grid_large!}2">${uiLabelMap.OrderGift}</@td>
                                  <@td colspan="3">
                                      <#if (isGift!"N") == "N">${uiLabelMap.OrderThisIsNotGift}.</#if>
                                      <#if (isGift!"N") == "Y">${uiLabelMap.OrderThisIsGift}.</#if>
                                  </@td>
                                </@tr>
                                <#if giftMessage?has_content>
                                  <@tr>
                                    <@td class="${styles.grid_large!}2">${uiLabelMap.OrderGiftMessage}</@td>
                                    <@td colspan="3">
                                      ${giftMessage}
                                    </@td>
                                  </@tr>
                                </#if>
                              </#if>
                            </#if>

                            <#if shipGroup_has_next>
                            </#if>
                          </@table>
                          <#assign groupIdx = groupIdx + 1>
                        </#list>
                      </#if>
                       
                </@section>
            </#if>
        </@cell>
    </@row>

</@section>
