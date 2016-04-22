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

<#-- NOTE: this template is used for the orderstatus screen in shop AND for order notification emails through the OrderNoticeEmail.ftl file -->
<#-- the "urlPrefix" value will be prepended to URLs by the ofbizUrl transform if/when there is no "request" object in the context -->
<#if baseEcommerceSecureUrl??><#assign urlPrefix = baseEcommerceSecureUrl/></#if>
<#if (orderHeader.externalId)?? && (orderHeader.externalId)?has_content >
  <#assign externalOrder = "(" + orderHeader.externalId + ")"/>
</#if>

<#-- Cato: NOTE: This is used for checkout review and order summaries 
    Check orderHeader?has_content to see if is in-progress or placed order. -->

<#assign maySelect = ((maySelectItems!"N") == "Y")>
<#assign printable = printable!false>

<#if (orderHeader.orderId)??>
  <@heading level=1>
    <#-- Cato: This page doesn't actually show a full invoice - only the PDF is a full invoice (with tax information) - so "PDF" beside title is misleading -->
    ${orderHeader.orderId}<#--<#if !maySelect && !printable> (<a href="<@ofbizUrl fullPath="true">order.pdf?orderId=${(orderHeader.orderId)!}</@ofbizUrl>" target="_BLANK" class="${styles.action_export!}">${uiLabelMap.CommonPdf} ${uiLabelMap.CommonInvoice}</a>)</#if>-->
  </@heading>
</#if>

<#macro menuContent menuArgs={}>
<#if (orderHeader.orderId)?has_content><#-- Cato: Only if order not yet placed -->
  <#if !printable>
    <@menu args=menuArgs>
      <#if maySelect>
        <@menuitem type="link" href=makeOfbizUrl({"uri":"orderprint?orderId=" + (orderHeader.orderId)!, "fullPath":true}) target="_BLANK" class="+${styles.action_export!}" text=uiLabelMap.CommonPrintable />
      </#if>
      <#-- above will be better
      <#if maySelect>
        <@menuitem type="link" href=makeOfbizUrl({"uri":"orderviewonly?orderId=" + (orderHeader.orderId)!, "fullPath":true}) target="_BLANK" class="+${styles.action_export!}" text=uiLabelMap.CommonPrintable />
      </#if>
      -->
      <#-- Cato: Always show it here <#if maySelect>-->
      <@menuitem type="link" href=makeOfbizUrl({"uri":"order.pdf?orderId=" + (orderHeader.orderId)!, "fullPath":true}) target="_BLANK" class="+${styles.action_export!}" text="${uiLabelMap.CommonInvoice} (${uiLabelMap.CommonPdf})" />
      <#--</#if>-->
      <#if maySelect && (returnLink!"N") == "Y" && ((orderHeader.statusId)!) == "ORDER_COMPLETED" && (roleTypeId!) == "PLACING_CUSTOMER">
        <@menuitem type="link" href=makeOfbizUrl("makeReturn?orderId=${orderHeader.orderId}") text=uiLabelMap.OrderRequestReturn />
      </#if>
    </@menu>
  </#if>
</#if>
</#macro>
<@section menuContent=menuContent>
    
    <#-- orderinfo -->
    <@row>
        <@cell columns=4>
            <@section title=uiLabelMap.CommonOverview>
                <@table type="fields">
                  <#if localOrderReadHelper?? && orderHeader?has_content><#-- Cato: Show only for placed orders -->
                    <#assign displayParty = localOrderReadHelper.getPlacingParty()!/>
                    <#if displayParty?has_content>
                        <#assign displayPartyNameResult = dispatcher.runSync("getPartyNameForDate", {"partyId":(displayParty.partyId!), "compareDate":(orderHeader.orderDate!), "userLogin":userLogin})/>
                    </#if>
                    <#if displayPartyNameResult?has_content>
                        <@tr>
                          <@td class="${styles.grid_large!}2">${uiLabelMap.PartyName}</@td>
                          <@td colspan="3">${(displayPartyNameResult.fullName)!"[Name Not Found]"}</@td>
                        </@tr>
                    </#if>
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
                  <#if orderHeader?has_content><#-- Cato: Show only for placed orders -->
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
                <@section title=uiLabelMap.AccountingPaymentInformation>
                    <@table type="fields">
                        <#if !paymentMethod?has_content && paymentMethodType?has_content>
                        
                            <#-- offline payment -->
                            <#if paymentMethodType.paymentMethodTypeId == "EXT_OFFLINE">
                                <@tr>
                                    <@td colspan="4">
                                      <#assign offPayTitle><p>${uiLabelMap.AccountingOfflinePayment}</p></#assign>
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

                                      <#-- Cato: only show alert after placed and not printable -->
                                      <#if orderHeader?has_content && !printable>
                                        <@alert type="info" closable=false>
                                          ${offPayTitle}
                                          ${offPayDesc}
                                        </@alert>
                                      <#else>
                                        ${offPayTitle}
                                        ${offPayDesc}
                                      </#if>
                                    </@td>
                                </@tr>
                            <#else>
                                <@tr>
                                    <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingPaymentVia}</@td>
                                    <@td colspan="3">${paymentMethodType.get("description",locale)}</@td>
                                </@tr>
                            </#if>


                        <#elseif paymentMethods?has_content>
                            <#list paymentMethods as paymentMethod>
                                  <#if "CREDIT_CARD" == paymentMethod.paymentMethodTypeId>
                                    <#assign creditCard = paymentMethod.getRelatedOne("CreditCard", false)>
                                    <#assign formattedCardNumber = Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)>
                                  <#elseif "GIFT_CARD" == paymentMethod.paymentMethodTypeId>
                                    <#assign giftCard = paymentMethod.getRelatedOne("GiftCard", false)>
                                  <#elseif "EFT_ACCOUNT" == paymentMethod.paymentMethodTypeId>
                                    <#assign eftAccount = paymentMethod.getRelatedOne("EftAccount", false)>
                                  </#if>

                                  <#-- credit card info -->
                                <#if "CREDIT_CARD" == paymentMethod.paymentMethodTypeId && creditCard?has_content>
                                    <#assign pmBillingAddress = creditCard.getRelatedOne("PostalAddress", false)!>
                                    <@tr>
                                        <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingCreditCard}</@td>
                                        <@td colspan="3">${formattedCardNumber}<br/>
                                          <#if creditCard.companyNameOnCard?has_content>${creditCard.companyNameOnCard}><br/></#if>
                                          <#if creditCard.titleOnCard?has_content>${creditCard.titleOnCard}><br/></#if>
                                          ${creditCard.firstNameOnCard}<br/>
                                          <#if creditCard.middleNameOnCard?has_content>${creditCard.middleNameOnCard}><br/></#if>
                                          ${creditCard.lastNameOnCard}<br/>
                                          <#if creditCard.suffixOnCard?has_content>${creditCard.suffixOnCard}</#if>
                                        </@td>
                                    </@tr>
                                    
                                </#if>


                                <#-- Gift Card info -->
                                <#if "GIFT_CARD" == paymentMethod.paymentMethodTypeId && giftCard?has_content>
                                    <#if giftCard?has_content && giftCard.cardNumber?has_content>
                                      <#assign pmBillingAddress = giftCard.getRelatedOne("PostalAddress", false)!>
                                      <#assign giftCardNumber = "">
                                      <#assign pcardNumber = giftCard.cardNumber>
                                      <#if pcardNumber?has_content>
                                        <#assign psize = pcardNumber?length - 4>
                                        <#if 0 < psize>
                                          <#list 0 .. psize-1 as foo>
                                            <#assign giftCardNumber = giftCardNumber + "*">
                                          </#list>
                                          <#assign giftCardNumber = giftCardNumber + pcardNumber[psize .. psize + 3]>
                                        <#else>
                                          <#assign giftCardNumber = pcardNumber>
                                        </#if>
                                      </#if>
                                    </#if>
                                    <@tr>
                                        <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingGiftCard}</@td>
                                        <@td colspan="3">${giftCardNumber}</@td>
                                    </@tr>
                                </#if>

                                <#-- EFT account info -->
                                <#if "EFT_ACCOUNT" == paymentMethod.paymentMethodTypeId && eftAccount?has_content>
                                    <#assign pmBillingAddress = eftAccount.getRelatedOne("PostalAddress", false)!>
                                    <@tr>
                                        <@td class="${styles.grid_large!}2">
                                            ${uiLabelMap.AccountingEFTAccount}
                                            ${eftAccount.nameOnAccount!}
                                        </@td>
                                        <@td>
                                            ${uiLabelMap.AccountingAccount} #: ${eftAccount.accountNumber}
                                        </@td>
                                        <@td colspan="2">
                                            <#if eftAccount.companyNameOnAccount?has_content>${eftAccount.companyNameOnAccount}</#if>><br/>
                                            ${uiLabelMap.AccountingBank}: ${eftAccount.bankName}, ${eftAccount.routingNumber}

                                        </@td>
                                    </@tr>
                                </#if>
                                
                                <#if pmBillingAddress?has_content>
                                <@tr>
                                    <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingBillingAddress}</@td>
                                    <@td colspan="3">
                                        <#if pmBillingAddress.toName?has_content>${uiLabelMap.CommonTo}: ${pmBillingAddress.toName}<br/></#if>
                                        <#if pmBillingAddress.attnName?has_content>${uiLabelMap.CommonAttn}: ${pmBillingAddress.attnName}<br/></#if>
                                        ${pmBillingAddress.address1}<br/>
                                        <#if pmBillingAddress.address2?has_content>${pmBillingAddress.address2}<br/></#if>
                                        <#assign pmBillingStateGeo = (delegator.findOne("Geo", {"geoId", pmBillingAddress.stateProvinceGeoId!}, false))! />
                                        ${pmBillingAddress.city}<#if pmBillingStateGeo?has_content>, ${ pmBillingStateGeo.geoName!}</#if> ${pmBillingAddress.postalCode!}<br/>
                                        <#assign pmBillingCountryGeo = (delegator.findOne("Geo", {"geoId", pmBillingAddress.countryGeoId!}, false))! />
                                        <#if pmBillingCountryGeo?has_content>${pmBillingCountryGeo.geoName!}</#if>
                                    </@td>
                                </@tr>
                              </#if>
                            </#list>
                        </#if>
                        <#-- billing account info -->
                        <#if paymentMethods?has_content || paymentMethodType?has_content || billingAccount?has_content>
                            <#if billingAccount?has_content || customerPoNumberSet?has_content>>
                                <@tr>
                                    <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingPaymentInformation}</@td>
                                    <@td colspan="3">
                                        <#if billingAccount?has_content>
                                            ${uiLabelMap.AccountingBillingAccount}
                                            #${billingAccount.billingAccountId!} - ${billingAccount.description!}
                                      </#if>
                                      <#if (customerPoNumberSet?has_content)>
                                          ${uiLabelMap.OrderPurchaseOrderNumber}
                                          <#list customerPoNumberSet as customerPoNumber>
                                            ${customerPoNumber!}
                                          </#list>
                                      </#if>
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
                <@section title=uiLabelMap.OrderShippingInformation>
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

                              <#-- gift settings -->
                              <#if orderHeader?has_content>
                                <#assign isGift = shipGroup.isGift!"N">
                                <#assign giftMessage = shipGroup.giftMessage!>
                              <#else>
                                <#assign isGift = cart.getIsGift(groupIdx)!"N">
                                <#assign giftMessage = cart.getGiftMessage(groupIdx)!>
                              </#if>
                              <#-- Cato: Only show if gift -->
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
