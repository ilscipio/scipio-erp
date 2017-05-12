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

<#-- 
ToDo: Update menu with Authorize and Capture transaction actions 
<#if paymentMethodType.paymentMethodTypeId =="CREDIT_CARD" || paymentMethodType.paymentMethodTypeId =="FIN_ACCOUNT">
    <#if orderPaymentPreference.statusId != "PAYMENT_SETTLED">
      <a href="<@ofbizInterWebappUrl>/accounting/control/AuthorizeTransaction?orderId=${orderId!}&amp;orderPaymentPreferenceId=${orderPaymentPreference.orderPaymentPreferenceId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.AccountingAuthorize}</a>
    </#if>
    <#if orderPaymentPreference.statusId == "PAYMENT_AUTHORIZED">
      <a href="<@ofbizInterWebappUrl>/accounting/control/CaptureTransaction?orderId=${orderId!}&amp;orderPaymentPreferenceId=${orderPaymentPreference.orderPaymentPreferenceId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.AccountingCapture}</a>
    </#if>
</#if>


<#if paymentMethodType.paymentMethodTypeId =="EXT_BILLACT">
    <#if orderPaymentPreference.statusId != "PAYMENT_SETTLED" && orderPaymentPreference.statusId != "PAYMENT_RECEIVED">
        <a href="<@ofbizUrl>receivepayment?${rawString(paramString)}</@ofbizUrl>">${uiLabelMap.AccountingReceivePayment}</a>
    </#if>
</#if>
-->

<#macro maskSensitiveNumber cardNumber>
  <#assign cardNumberDisplay = "">
  <#if cardNumber?has_content>
    <#assign size = cardNumber?length - 4>
    <#if (size > 0)>
      <#list 0 .. size-1 as foo>
        <#assign cardNumberDisplay = cardNumberDisplay + "*">
      </#list>
      <#assign cardNumberDisplay = cardNumberDisplay + cardNumber[size .. size + 3]>
    <#else>
      <#-- but if the card number has less than four digits (ie, it was entered incorrectly), display it in full -->
      <#assign cardNumberDisplay = cardNumber>
    </#if>
  </#if>
  ${cardNumberDisplay!}
</#macro>

<@section title=uiLabelMap.AccountingPaymentInformation>
   <#assign orderTypeId = orderReadHelper.getOrderTypeId()>

  <#if orderTypeId == "PURCHASE_ORDER">
  <@table type="data-complex"> <#-- orig: class="basic-table" -->
    <#if orderPaymentPreferences?has_content || invoices?has_content>
      <@thead>
        <@tr>
          <@th>${uiLabelMap.AccountingPaymentID}</@th>
          <@th>${uiLabelMap.CommonTo}</@th>
          <@th>${uiLabelMap.CommonAmount}</@th>
          <@th>${uiLabelMap.CommonStatus}</@th>
        </@tr>
      </@thead>
       <#list orderPaymentPreferences as orderPaymentPreference>
         <#assign payments = orderPaymentPreference.getRelated("Payment", null, null, false)>
         <#list payments as payment>
           <#assign statusItem = payment.getRelatedOne("StatusItem", false)>
           <#assign partyName = delegator.findOne("PartyNameView", {"partyId" : payment.partyIdTo}, true)>
           <@tr>
             <#if security.hasEntityPermission("PAY_INFO", "_VIEW", session) || security.hasEntityPermission("ACCOUNTING", "_VIEW", session)>
               <@td scope="row" class="${styles.grid_large!}3"><a href="<@ofbizInterWebappUrl>/accounting/control/paymentOverview?paymentId=${payment.paymentId}</@ofbizInterWebappUrl>">${payment.paymentId}</a></@td>
             <#else>
               <@td scope="row" class="${styles.grid_large!}3">${payment.paymentId}</@td>
             </#if>
             <@td>${partyName.groupName!}${partyName.lastName!} ${partyName.firstName!} ${partyName.middleName!}
             <#if security.hasPermission("PARTYMGR_VIEW", session) || security.hasPermission("PARTYMGR_ADMIN", session)>
               [<a href="<@ofbizInterWebappUrl>/partymgr/control/viewprofile?partyId=${partyId!}</@ofbizInterWebappUrl>">${partyId!}</a>]
             <#else>
               [${partyId!}]
             </#if>
             </@td>
             <@td><@ofbizCurrency amount=(payment.amount!) isoCode=(currencyUomId!) /></@td>
             <@td>${statusItem.description}</@td>
           </@tr>
         </#list>
       </#list>
       <#-- invoices -->
       <#if invoices?has_content>
         <@tr>
           <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderInvoices}</@td>
           <@td>&nbsp;</@td>
           <@td>
             <#list invoices as invoice>
               <div>${uiLabelMap.CommonNbr} <a href="<@ofbizInterWebappUrl>/accounting/control/invoiceOverview?invoiceId=${invoice}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${invoice}</a>
               (<a target="_BLANK" href="<@ofbizInterWebappUrl>/accounting/control/invoice.pdf?invoiceId=${invoice}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">PDF</a>)</div>
             </#list>
           </@td>
           <@td>&nbsp;</@td>
         </@tr>
       </#if>

     <#else>
      <@tr>
        <@td colspan="4" align="center">${uiLabelMap.OrderNoOrderPaymentPreferences}</@td>
      </@tr>
     </#if>
  </@table>       
  <#else>
  <@table type="data-complex"> <#-- orig: class="basic-table" -->
     <#-- order payment status -->
    <#assign orderPaymentStatuses = orderReadHelper.getOrderPaymentStatuses()>
     <#if orderPaymentStatuses?has_content>
     <@tr>
       <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderStatusHistory}</@td>
       <@td colspan="3">
             <#assign currStatusItem = orderPaymentStatuses[0].getRelatedOne("StatusItem", false)!>
             <#if currStatusItem?has_content>${currStatusItem.get("description",locale)}</#if> 
             
              <@modal id="${orderId}_paymentstatus_" label="(${rawLabel('CommonDetail')})">
                 <ul class="no-bullet">
                   <#list orderPaymentStatuses as orderPaymentStatus>
                     <#assign statusItem = orderPaymentStatus.getRelatedOne("StatusItem", false)!>
                     <#if statusItem?has_content>
                                <li>
                          ${statusItem.get("description",locale)} <#if orderPaymentStatus.statusDatetime?has_content>- <@formattedDateTime date=orderPaymentStatus.statusDatetime /></#if>
                          &nbsp;
                          ${uiLabelMap.CommonBy} - [${orderPaymentStatus.statusUserLogin!}]
                                </li>
                     </#if>
                   </#list>
                </ul>
            </@modal>
         
       </@td>
     </@tr>
     </#if>
     <#if orderPaymentPreferences?has_content || billingAccount?has_content || invoices?has_content>
        <#list orderPaymentPreferences as orderPaymentPreference>
          <#assign paymentList = orderPaymentPreference.getRelated("Payment", null, null, false)>
          <#assign pmBillingAddress = {}>
          <#assign oppStatusItem = orderPaymentPreference.getRelatedOne("StatusItem", false)>
          <#if outputted?default("false") == "true">
            
          </#if>
          <#assign outputted = "true">
          <#-- try the paymentMethod first; if paymentMethodId is specified it overrides paymentMethodTypeId -->
          <#assign paymentMethod = orderPaymentPreference.getRelatedOne("PaymentMethod", false)!>
          <#if !paymentMethod?has_content>
            <#assign paymentMethodType = orderPaymentPreference.getRelatedOne("PaymentMethodType", false)>            
            <#if paymentMethodType.paymentMethodTypeId == "EXT_BILLACT">
                <#assign outputted = "false">
                <#-- billing account -->
                <#if billingAccount??>
                  <#if outputted?default("false") == "true">
                    
                  </#if>
                  <@tr>
                    <@td scope="row" class="${styles.grid_large!}3">
                      <#-- billing accounts require a special OrderPaymentPreference because it is skipped from above section of OPPs -->
                      ${uiLabelMap.AccountingBillingAccount}&nbsp;
                          <#if billingAccountMaxAmount?has_content>
                          <br />${uiLabelMap.OrderPaymentMaximumAmount}: <@ofbizCurrency amount=billingAccountMaxAmount?default(0.00) isoCode=currencyUomId/>
                          </#if>
                    </@td>
                    <@td colspan="3">
                        <@row>
                            <@cell columns=6>
                                ${uiLabelMap.CommonNbr} <a href="<@ofbizInterWebappUrl>/accounting/control/EditBillingAccount?billingAccountId=${billingAccount.billingAccountId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${billingAccount.billingAccountId}</a>  - ${billingAccount.description!}
                            </@cell>
                            <@cell columns=6>
                                <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
                                <#if orderPaymentPreference.statusId != "PAYMENT_SETTLED">                                
                                    <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                    <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@ofbizUrl>updateOrderPaymentPreference</@ofbizUrl>">
                                      <input type="hidden" name="orderId" value="${orderId}" />
                                      <input type="hidden" name="orderPaymentPreferenceId" value="${orderPaymentPreference.orderPaymentPreferenceId}" />
                                      <input type="hidden" name="statusId" value="PAYMENT_CANCELLED" />
                                      <input type="hidden" name="checkOutPaymentId" value="${paymentMethodType.paymentMethodTypeId!paymentMethod.paymentMethodTypeId!}" />
                                    </form>
                                </#if>
                            </#if>
                            </@cell>
                        </@row>
                    </@td>
                  </@tr>
                </#if>
            <#elseif paymentMethodType.paymentMethodTypeId == "FIN_ACCOUNT">
              <#assign finAccount = orderPaymentPreference.getRelatedOne("FinAccount", false)!/>
              <#if (finAccount?has_content)>
                <#assign gatewayResponses = orderPaymentPreference.getRelated("PaymentGatewayResponse", null, null, false)>
                <#assign finAccountType = finAccount.getRelatedOne("FinAccountType", false)!/>
                <@tr>
                  <@td scope="row" class="${styles.grid_large!}3">
                    ${uiLabelMap.AccountingFinAccount}
                  </@td>
                  <@td colspan="3">
                      <#if orderPaymentPreference.maxAmount?has_content>
                      ${uiLabelMap.OrderPaymentMaximumAmount}: <@ofbizCurrency amount=orderPaymentPreference.maxAmount?default(0.00) isoCode=currencyUomId/>
                    </#if>
                    <@row>
                        <@cell columns=6>
                            <#if (finAccountType?has_content)>
                                ${finAccountType.description!finAccountType.finAccountTypeId}&nbsp;
                              </#if>
                              #${finAccount.finAccountCode!finAccount.finAccountId} (<a href="<@ofbizInterWebappUrl>/accounting/control/EditFinAccount?finAccountId=${finAccount.finAccountId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${finAccount.finAccountId}</a>)
                              <br />
                              ${finAccount.finAccountName!}
                        </@cell>
                        <@cell columns=6>
                            <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
                            <#if orderPaymentPreference.statusId != "PAYMENT_SETTLED">                            
                                  <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                  <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@ofbizUrl>updateOrderPaymentPreference</@ofbizUrl>">
                                    <input type="hidden" name="orderId" value="${orderId}" />
                                    <input type="hidden" name="orderPaymentPreferenceId" value="${orderPaymentPreference.orderPaymentPreferenceId}" />
                                    <input type="hidden" name="statusId" value="PAYMENT_CANCELLED" />
                                    <input type="hidden" name="checkOutPaymentId" value="${paymentMethodType.paymentMethodTypeId!paymentMethod.paymentMethodTypeId!}" />
                                  </form>
                             </#if>
                            </#if>
                        </@cell>
                    </@row>
                    <#if gatewayResponses?has_content>
                        <@modal label="(${rawLabel('CommonDetail')})" id="modal_AccountingFinAccount">
                                <#list gatewayResponses as gatewayResponse>
                                  <#assign transactionCode = gatewayResponse.getRelatedOne("TranCodeEnumeration", false)>
                                  ${(transactionCode.get("description",locale))?default("Unknown")}:
                                  <#if gatewayResponse.transactionDate?has_content><@formattedDateTime date=gatewayResponse.transactionDate /></#if>
                                  <@ofbizCurrency amount=gatewayResponse.amount isoCode=currencyUomId/><br />
                                  (${uiLabelMap.OrderReference}&nbsp;${gatewayResponse.referenceNum!}
                                  ${uiLabelMap.OrderAvs}&nbsp;${gatewayResponse.gatewayAvsResult!(uiLabelMap.CommonNA)}
                                  ${uiLabelMap.OrderScore}&nbsp;${gatewayResponse.gatewayScoreResult!(uiLabelMap.CommonNA)})
                                  <a href="<@ofbizInterWebappUrl>/accounting/control/ViewGatewayResponse?paymentGatewayResponseId=${gatewayResponse.paymentGatewayResponseId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${uiLabelMap.CommonDetails}</a>
                                  <#if gatewayResponse_has_next><hr /></#if>
                                </#list>
                        </@modal>
                    </#if>
                  </@td>
                </@tr>
                <#if paymentList?has_content>
                    <@tr>
                      <@td>${uiLabelMap.AccountingInvoicePayments}</@td>
                      <@td colspan="3">
                            <#list paymentList as paymentMap>
                                <a href="<@ofbizInterWebappUrl>/accounting/control/paymentOverview?paymentId=${paymentMap.paymentId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${paymentMap.paymentId}</a><#if paymentMap_has_next><br /></#if>
                            </#list>
                      </@td>
                    </@tr>
                </#if>
              </#if>
            <#else>
              <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${paymentMethodType.get("description",locale)!}
                  
                </@td>
                <@td colspan="3">
                    <#if orderPaymentPreference.maxAmount?has_content>
                        ${uiLabelMap.OrderPaymentMaximumAmount}: <@ofbizCurrency amount=orderPaymentPreference.maxAmount?default(0.00) isoCode=currencyUomId/>
                      </#if>
                    <@row>
                        <@cell columns=6>
                            <#if paymentMethodType.paymentMethodTypeId != "EXT_OFFLINE" && paymentMethodType.paymentMethodTypeId != "EXT_PAYPAL" && paymentMethodType.paymentMethodTypeId != "EXT_COD">
                              <#if orderPaymentPreference.maxAmount?has_content>
                                 ${uiLabelMap.OrderPaymentMaximumAmount}: <@ofbizCurrency amount=orderPaymentPreference.maxAmount?default(0.00) isoCode=currencyUomId/>
                              </#if>
                              <br />&nbsp;[<#if oppStatusItem??>${oppStatusItem.get("description",locale)}<#else>${orderPaymentPreference.statusId}</#if>]
                            <#--
                            <div><@ofbizCurrency amount=orderPaymentPreference.maxAmount?default(0.00) isoCode=currencyUomId/>&nbsp;-&nbsp;${(orderPaymentPreference.authDate.toString())!}</div>
                            <div>&nbsp;<#if orderPaymentPreference.authRefNum??>(${uiLabelMap.OrderReference}: ${orderPaymentPreference.authRefNum})</#if></div>
                            -->
                        <#else>
                            <a href="<@ofbizUrl>receivepayment?${rawString(paramString)}</@ofbizUrl>">${uiLabelMap.AccountingReceivePayment}</a>
                        </#if>
                        </@cell>
                        <@cell columns=6>
                            <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
                                <#if orderPaymentPreference.statusId != "PAYMENT_SETTLED">                                
                                    <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                    <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@ofbizUrl>updateOrderPaymentPreference</@ofbizUrl>">
                                      <input type="hidden" name="orderId" value="${orderId}" />
                                      <input type="hidden" name="orderPaymentPreferenceId" value="${orderPaymentPreference.orderPaymentPreferenceId}" />
                                      <input type="hidden" name="statusId" value="PAYMENT_CANCELLED" />
                                      <input type="hidden" name="checkOutPaymentId" value="${paymentMethodType.paymentMethodTypeId!paymentMethod.paymentMethodTypeId!}" />
                                    </form>
                                </#if>
                           </#if>
                        </@cell>
                    </@row>
                    <#if paymentList?has_content>
                        <@row>
                              <@cell columns=6>${uiLabelMap.AccountingInvoicePayments}</@cell>
                              <@cell columns=6>
                                    <#list paymentList as paymentMap>
                                        <a href="<@ofbizInterWebappUrl>/accounting/control/paymentOverview?paymentId=${paymentMap.paymentId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${paymentMap.paymentId}</a><#if paymentMap_has_next><br /></#if>
                                    </#list>
                              </@cell>
                        </@row>
                    </#if>
                  </@td>
                </@tr>
                
            </#if>
          <#else>
            <#if (paymentMethod.paymentMethodTypeId!) == "CREDIT_CARD">
              <#assign gatewayResponses = orderPaymentPreference.getRelated("PaymentGatewayResponse", null, null, false)>
              <#assign creditCard = paymentMethod.getRelatedOne("CreditCard", false)!>
              <#if creditCard?has_content>
                <#assign pmBillingAddress = creditCard.getRelatedOne("PostalAddress", false)!>
              </#if>
              <@tr>
                <@td scope="row" class="${styles.grid_large!}3">
                    ${uiLabelMap.AccountingCreditCard}
                </@td>
                <@td colspan="3">
                    <#if orderPaymentPreference.maxAmount?has_content>
                     ${uiLabelMap.OrderPaymentMaximumAmount}: <@ofbizCurrency amount=orderPaymentPreference.maxAmount?default(0.00) isoCode=currencyUomId/>
                  </#if>
                  <@row>
                    <@cell columns=6>
                          <#if creditCard?has_content>
                              <#if creditCard.companyNameOnCard??>${creditCard.companyNameOnCard}<br /></#if>
                              <#if creditCard.titleOnCard?has_content>${creditCard.titleOnCard}&nbsp;</#if>
                              ${creditCard.firstNameOnCard!(uiLabelMap.CommonNA)}&nbsp;
                              <#if creditCard.middleNameOnCard?has_content>${creditCard.middleNameOnCard}&nbsp;</#if>
                              ${creditCard.lastNameOnCard!(uiLabelMap.CommonNA)}
                              <#if creditCard.suffixOnCard?has_content>&nbsp;${creditCard.suffixOnCard}</#if>
                              <br />
                              <#if security.hasEntityPermission("PAY_INFO", "_VIEW", session) || security.hasEntityPermission("ACCOUNTING", "_VIEW", session)>
                                ${creditCard.cardType}
                                <@maskSensitiveNumber cardNumber=(creditCard.cardNumber!)/>
                                ${creditCard.expireDate}
                                &nbsp;[<#if oppStatusItem??>${oppStatusItem.get("description",locale)}<#else>${orderPaymentPreference.statusId}</#if>]
                              <#else>
                                ${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}
                                &nbsp;[<#if oppStatusItem??>${oppStatusItem.get("description",locale)}<#else>${orderPaymentPreference.statusId}</#if>]
                              </#if>
                              <#else>
                              ${uiLabelMap.CommonInformation} ${uiLabelMap.CommonNot} ${uiLabelMap.CommonAvailable}
                            </#if>

                          </@cell>
                          <@cell columns=6>
                                
                                <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
                                   <#if orderPaymentPreference.statusId != "PAYMENT_SETTLED">                                   
                                      <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                      <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@ofbizUrl>updateOrderPaymentPreference</@ofbizUrl>">
                                        <input type="hidden" name="orderId" value="${orderId}" />
                                        <input type="hidden" name="orderPaymentPreferenceId" value="${orderPaymentPreference.orderPaymentPreferenceId}" />
                                        <input type="hidden" name="statusId" value="PAYMENT_CANCELLED" />
                                        <input type="hidden" name="checkOutPaymentId" value="${paymentMethodType.paymentMethodTypeId!paymentMethod.paymentMethodTypeId!}" />
                                      </form>
                                   </#if>
                              </#if>
                          </@cell>
                    </@row>
                    
                    
                    <#if gatewayResponses?has_content>
                        <@modal label="(${rawLabel('CommonDetail')})" id="modal_AccountingCC">
                                  <#list gatewayResponses as gatewayResponse>
                                    <#assign transactionCode = gatewayResponse.getRelatedOne("TranCodeEnumeration", false)>
                                    ${(transactionCode.get("description",locale))?default("Unknown")}:
                                    <#if gatewayResponse.transactionDate?has_content><@formattedDateTime date=gatewayResponse.transactionDate /></#if>
                                    <@ofbizCurrency amount=gatewayResponse.amount isoCode=currencyUomId/><br />
                                    (${uiLabelMap.OrderReference}&nbsp;${gatewayResponse.referenceNum!}
                                    ${uiLabelMap.OrderAvs}&nbsp;${gatewayResponse.gatewayAvsResult!(uiLabelMap.CommonNA)}
                                    ${uiLabelMap.OrderScore}&nbsp;${gatewayResponse.gatewayScoreResult!(uiLabelMap.CommonNA)})
                                    <a href="<@ofbizInterWebappUrl>/accounting/control/ViewGatewayResponse?paymentGatewayResponseId=${gatewayResponse.paymentGatewayResponseId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${uiLabelMap.CommonDetails}</a>
                                    <#if gatewayResponse_has_next><hr /></#if>
                                  </#list>
                        </@modal>
                  </#if>
                </@td>
              </@tr>
            <#elseif (paymentMethod.paymentMethodTypeId!) == "EFT_ACCOUNT">
              <#assign eftAccount = paymentMethod.getRelatedOne("EftAccount", false)>
              <#if eftAccount?has_content>
                <#assign pmBillingAddress = eftAccount.getRelatedOne("PostalAddress", false)!>
              </#if>
              <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.AccountingEFTAccount}
                  <#if orderPaymentPreference.maxAmount?has_content>
                  <br />${uiLabelMap.OrderPaymentMaximumAmount}: <@ofbizCurrency amount=orderPaymentPreference.maxAmount?default(0.00) isoCode=currencyUomId/>
                  </#if>
                </@td>
                <@td colspan="3">
                    <@row>
                        <@cell columns=6>
                            <#if eftAccount?has_content>
                              ${eftAccount.nameOnAccount!}<br />
                              <#if eftAccount.companyNameOnAccount??>${eftAccount.companyNameOnAccount}<br /></#if>
                              ${uiLabelMap.AccountingBankName}: ${eftAccount.bankName}, ${eftAccount.routingNumber}<br />
                              ${uiLabelMap.AccountingAccount}#: ${eftAccount.accountNumber}
                            <#else>
                              ${uiLabelMap.CommonInformation} ${uiLabelMap.CommonNot} ${uiLabelMap.CommonAvailable}
                            </#if>  
                        </@cell>
                        <@cell columns=6>
                            <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
                               <#if orderPaymentPreference.statusId != "PAYMENT_SETTLED">                               
                                  <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                  <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@ofbizUrl>updateOrderPaymentPreference</@ofbizUrl>">
                                    <input type="hidden" name="orderId" value="${orderId}" />
                                    <input type="hidden" name="orderPaymentPreferenceId" value="${orderPaymentPreference.orderPaymentPreferenceId}" />
                                    <input type="hidden" name="statusId" value="PAYMENT_CANCELLED" />
                                    <input type="hidden" name="checkOutPaymentId" value="${paymentMethodType.paymentMethodTypeId!paymentMethod.paymentMethodTypeId!}" />
                                  </form>
                               </#if>
                              </#if>
                        </@cell>
                    </@row>
                    <#if paymentList?has_content>
                    </#if>
                </@td>
              </@tr>
              <#if paymentList?has_content>
                <@row>
                    <@cell columns=6>
                        ${uiLabelMap.AccountingInvoicePayments}
                    </@cell>
                    <@cell columns=6>
                        <#list paymentList as paymentMap>
                            <a href="<@ofbizInterWebappUrl>/accounting/control/paymentOverview?paymentId=${paymentMap.paymentId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${paymentMap.paymentId}</a><#if paymentMap_has_next><br /></#if>
                        </#list>
                    </@cell>
                </@row>
              </#if>
            <#elseif (paymentMethod.paymentMethodTypeId!) == "GIFT_CARD">
              <#assign giftCard = paymentMethod.getRelatedOne("GiftCard", false)>
              <#if giftCard??>
                <#assign pmBillingAddress = giftCard.getRelatedOne("PostalAddress", false)!>
              </#if>
              <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderGiftCard}
                  <#if orderPaymentPreference.maxAmount?has_content>
                    <br />${uiLabelMap.OrderPaymentMaximumAmount}: <@ofbizCurrency amount=orderPaymentPreference.maxAmount?default(0.00) isoCode=currencyUomId/>
                  </#if>
                </@td>
                <@td colspan="3">
                    <@row>
                        <@cell columns=6>
                            <#if giftCard?has_content>
                              <#if security.hasEntityPermission("PAY_INFO", "_VIEW", session) || security.hasEntityPermission("ACCOUNTING", "_VIEW", session)>
                                ${giftCard.cardNumber!(uiLabelMap.CommonNA)} [${giftCard.pinNumber!(uiLabelMap.CommonNA)}]
                                &nbsp;[<#if oppStatusItem??>${oppStatusItem.get("description",locale)}<#else>${orderPaymentPreference.statusId}</#if>]
                              <#else>
                              <@maskSensitiveNumber cardNumber=(giftCard.cardNumber!)/>
                              <#if !cardNumberDisplay?has_content>${uiLabelMap.CommonNA}</#if>
                                &nbsp;[<#if oppStatusItem??>${oppStatusItem.get("description",locale)}<#else>${orderPaymentPreference.statusId}</#if>]
                              </#if>
                            <#else>
                              ${uiLabelMap.CommonInformation} ${uiLabelMap.CommonNot} ${uiLabelMap.CommonAvailable}
                            </#if>
                        </@cell>
                        <@cell columns=6>
                            <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
                               <#if orderPaymentPreference.statusId != "PAYMENT_SETTLED">
                               F
                                  <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                  <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@ofbizUrl>updateOrderPaymentPreference</@ofbizUrl>">
                                    <input type="hidden" name="orderId" value="${orderId}" />
                                    <input type="hidden" name="orderPaymentPreferenceId" value="${orderPaymentPreference.orderPaymentPreferenceId}" />
                                    <input type="hidden" name="statusId" value="PAYMENT_CANCELLED" />
                                    <input type="hidden" name="checkOutPaymentId" value="${paymentMethodType.paymentMethodTypeId!paymentMethod.paymentMethodTypeId!}" />
                                  </form>
                               </#if>
                              </#if>
                        </@cell>
                    </@row>
                    <#if paymentList?has_content>
                    <@row>
                      <@cell columns=6>${uiLabelMap.AccountingInvoicePayments}</@cell>
                      <@cell columns=6>
                            <#list paymentList as paymentMap>
                                <a href="<@ofbizInterWebappUrl>/accounting/control/paymentOverview?paymentId=${paymentMap.paymentId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${paymentMap.paymentId}</a><#if paymentMap_has_next><br /></#if>
                            </#list>
                      </@cell>
                    </@row>
                  </#if>
                    
                  
                </@td>
              </@tr>
              
            </#if>
          </#if>
          <#-- Duplicated from contact information
          <#if pmBillingAddress?has_content>
            <@tr>
              <@td>&nbsp;</@td>
              <@td colspan="3">
                  <#if pmBillingAddress.toName?has_content>${uiLabelMap.CommonTo}&nbsp;${pmBillingAddress.toName}<br /></#if>
                  <#if pmBillingAddress.attnName?has_content>${uiLabelMap.CommonAttn}&nbsp;${pmBillingAddress.attnName}<br /></#if>
                  ${pmBillingAddress.address1}<br />
                  <#if pmBillingAddress.address2?has_content>${pmBillingAddress.address2}<br /></#if>
                  ${pmBillingAddress.city}<#if pmBillingAddress.stateProvinceGeoId?has_content>, ${pmBillingAddress.stateProvinceGeoId} </#if>
                  ${pmBillingAddress.postalCode!}<br />
                  ${pmBillingAddress.countryGeoId!}
                </@td>
            </@tr>
            <#if paymentList?has_content>
            <@tr>
              <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.AccountingInvoicePayments}</@td>
              <@td colspan="3">
                <#list paymentList as paymentMap>
                    <a href="<@ofbizInterWebappUrl>/accounting/control/paymentOverview?paymentId=${paymentMap.paymentId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${paymentMap.paymentId}</a><#if paymentMap_has_next><br /></#if>
                </#list>
              </@td>
            </@tr>
            </#if>
          </#if>-->
        </#list>

        <#if customerPoNumber?has_content>
          <@tr>
            <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderPONumber}</@td>
            <@td colspan="3">${customerPoNumber!}</@td>
          </@tr>
        </#if>

        <#-- invoices -->
        <#if invoices?has_content>
          <@tr>
            <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderInvoices}</@td>
            <@td colspan="3">
              <#list invoices as invoice>
                <div>${uiLabelMap.CommonNbr} <a href="<@ofbizInterWebappUrl>/accounting/control/invoiceOverview?invoiceId=${invoice}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${invoice}</a>
                (<a target="_BLANK" href="<@ofbizInterWebappUrl>/accounting/control/invoice.pdf?invoiceId=${invoice}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">PDF</a>)</div>
              </#list>
            </@td>
          </@tr>
        </#if>
   <#else>
    <@tr>
      <@td colspan="4" align="center">${uiLabelMap.OrderNoOrderPaymentPreferences}</@td>
    </@tr>
   </#if>
   
  <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED")) && (paymentMethodValueMaps?has_content)>
             <@tr>
              <@td scope="row" class="${styles.grid_large!}3">
                 
              </@td>
              <@td colspan="3">
                   <@modal label=uiLabelMap.AccountingNewPayment id="modal_addPaymentMethodToOrder" class="${styles.link_nav!} ${styles.action_add!}">
                       <form name="addPaymentMethodToOrder" method="post" action="<@ofbizUrl>addPaymentMethodToOrder</@ofbizUrl>">
                           <input type="hidden" name="orderId" value="${orderId!}"/>
                             <@row>
                                <@cell columns=6>
                                    <#assign openAmount = orderReadHelper.getOrderOpenAmount()>
                                    <input type="text" name="maxAmount" value="${openAmount}"/>
                                </@cell>
                                <@cell columns=3>
                                    <select name="paymentMethodId">
                                       <#list paymentMethodValueMaps as paymentMethodValueMap>
                                         <#assign paymentMethod = paymentMethodValueMap.paymentMethod/>
                                         <option value="${paymentMethod.get("paymentMethodId")!}">
                                           <#if "CREDIT_CARD" == paymentMethod.paymentMethodTypeId>
                                             <#assign creditCard = paymentMethodValueMap.creditCard/>
                                             <#if (creditCard?has_content)>
                                               <#if security.hasEntityPermission("PAY_INFO", "_VIEW", session) || security.hasEntityPermission("ACCOUNTING", "_VIEW", session)>
                                                 ${creditCard.cardType!} <@maskSensitiveNumber cardNumber=(creditCard.cardNumber!)/> ${creditCard.expireDate!}
                                               <#else>
                                                 ${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}
                                               </#if>
                                             </#if>
                                           <#else>
                                             ${paymentMethod.paymentMethodTypeId!}
                                             <#if paymentMethod.description??>${paymentMethod.description}</#if>
                                               (${paymentMethod.paymentMethodId})
                                             </#if>
                                           </option>
                                       </#list>
                                     </select>            
                                </@cell>
                                <@cell columns=3>
                                    <@field type="submit" text=uiLabelMap.CommonAdd class="${styles.link_run_sys!} ${styles.action_update!}"/>                    
                                </@cell>
                            </@row>
                       </form>
                   </@modal>
              </@td>
           </@tr>
  </#if>
    
  </@table>    
    
  </#if>

</@section>
