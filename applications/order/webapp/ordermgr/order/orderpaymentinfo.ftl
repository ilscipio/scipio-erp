<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://order/webapp/ordermgr/common/common.ftl">
<#import "component://accounting/webapp/accounting/common/acctlib.ftl" as acctlib>

<#-- 
ToDo: Update menu with Authorize and Capture transaction actions 
<#if paymentMethodType.paymentMethodTypeId =="CREDIT_CARD" || paymentMethodType.paymentMethodTypeId =="FIN_ACCOUNT">
    <#if orderPaymentPreference.statusId != "PAYMENT_SETTLED">
      <a href="<@serverUrl>/accounting/control/AuthorizeTransaction?orderId=${orderId!}&amp;orderPaymentPreferenceId=${orderPaymentPreference.orderPaymentPreferenceId}${raw(externalKeyParam)}</@serverUrl>" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.AccountingAuthorize}</a>
    </#if>
    <#if orderPaymentPreference.statusId == "PAYMENT_AUTHORIZED">
      <a href="<@serverUrl>/accounting/control/CaptureTransaction?orderId=${orderId!}&amp;orderPaymentPreferenceId=${orderPaymentPreference.orderPaymentPreferenceId}${raw(externalKeyParam)}</@serverUrl>" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.AccountingCapture}</a>
    </#if>
</#if>


<#if paymentMethodType.paymentMethodTypeId =="EXT_BILLACT">
    <#if orderPaymentPreference.statusId != "PAYMENT_SETTLED" && orderPaymentPreference.statusId != "PAYMENT_RECEIVED">
        <a href="<@pageUrl>receivepayment?${raw(paramString)}</@pageUrl>">${uiLabelMap.AccountingReceivePayment}</a>
    </#if>
</#if>
-->

<#-- SCIPIO: MOVED TO: component://accounting/webapp/accounting/common/acctlib.ftl
<#macro maskSensitiveNumber cardNumber paymentMethod= cardNumberMask=>
</#macro>-->

<@section title=uiLabelMap.AccountingPaymentInformation>
   <#assign orderTypeId = orderReadHelper.getOrderTypeId()>

  <#if orderTypeId == "PURCHASE_ORDER">
  <@table type="data-complex">
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
             <#if security.hasEntityPermission("PAY_INFO", "_VIEW", request) || security.hasEntityPermission("ACCOUNTING", "_VIEW", request)>
               <@td scope="row" class="${styles.grid_large!}3"><a href="<@serverUrl>/accounting/control/paymentOverview?paymentId=${payment.paymentId}</@serverUrl>">${payment.paymentId}</a></@td>
             <#else>
               <@td scope="row" class="${styles.grid_large!}3">${payment.paymentId}</@td>
             </#if>
             <@td>${partyName.groupName!}${partyName.lastName!} ${partyName.firstName!} ${partyName.middleName!}
             <#if security.hasPermission("PARTYMGR_VIEW", request) || security.hasPermission("PARTYMGR_ADMIN", request)>
               [<a href="<@serverUrl>/partymgr/control/viewprofile?partyId=${partyId!}</@serverUrl>">${partyId!}</a>]
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
               <div>${uiLabelMap.CommonNbr} <a href="<@serverUrl>/accounting/control/invoiceOverview?invoiceId=${invoice}${raw(externalKeyParam)}</@serverUrl>">${invoice}</a>
               (<a target="_BLANK" href="<@serverUrl>/accounting/control/invoice.pdf?invoiceId=${invoice}${raw(externalKeyParam)}</@serverUrl>">PDF</a>)</div>
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
  <@table type="data-complex">
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
                                ${uiLabelMap.CommonNbr} <a href="<@serverUrl>/accounting/control/EditBillingAccount?billingAccountId=${billingAccount.billingAccountId}${raw(externalKeyParam)}</@serverUrl>">${billingAccount.billingAccountId}</a>  - ${billingAccount.description!}
                            </@cell>
                            <@cell columns=6>
                                <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
                                <#if orderPaymentPreference.statusId != "PAYMENT_RECEIVED">                                
                                    <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                    <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@pageUrl>updateOrderPaymentPreference</@pageUrl>">
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
                              #${finAccount.finAccountCode!finAccount.finAccountId} (<a href="<@serverUrl>/accounting/control/EditFinAccount?finAccountId=${finAccount.finAccountId}${raw(externalKeyParam)}</@serverUrl>">${finAccount.finAccountId}</a>)
                              <br />
                              ${finAccount.finAccountName!}
                        </@cell>
                        <@cell columns=6>
                            <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
                            <#if orderPaymentPreference.statusId != "PAYMENT_RECEIVED">                            
                                  <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                  <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@pageUrl>updateOrderPaymentPreference</@pageUrl>">
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
                                  <a href="<@serverUrl>/accounting/control/ViewGatewayResponse?paymentGatewayResponseId=${gatewayResponse.paymentGatewayResponseId}${raw(externalKeyParam)}</@serverUrl>">${uiLabelMap.CommonDetails}</a>
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
                                <a href="<@serverUrl>/accounting/control/paymentOverview?paymentId=${paymentMap.paymentId}${raw(externalKeyParam)}</@serverUrl>">${paymentMap.paymentId}</a><#if paymentMap_has_next><br /></#if>
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
                        <#if paymentMethodType.paymentMethodTypeId == "EXT_LIGHTNING">
                            <#assign orderDate = .now?iso>
                            <#if orderHeader?has_content><#assign orderDate = orderHeader.orderDate/></#if>
                            <#assign xbtAmount = Static["org.ofbiz.common.uom.UomWorker"].convertDatedUom(orderDate, orderPaymentPreference.maxAmount?default(0.00), currencyUomId,"XBT",dispatcher,true)>
                            ${uiLabelMap.OrderPaymentMaximumAmount}: <@ofbizCurrency amount=xbtAmount rounding="8" isoCode="XBT"/>
                        <#else>
                            ${uiLabelMap.OrderPaymentMaximumAmount}: <@ofbizCurrency amount=orderPaymentPreference.maxAmount?default(0.00) isoCode=currencyUomId/>
                        </#if>
                      </#if>
                    <@row>
                        <@cell columns=6>
                            <#if paymentMethodType.paymentMethodTypeId != "EXT_OFFLINE" && paymentMethodType.paymentMethodTypeId != "EXT_PAYPAL" && paymentMethodType.paymentMethodTypeId != "EXT_LIGHTNING" && paymentMethodType.paymentMethodTypeId != "EXT_COD">
                              <#if orderPaymentPreference.maxAmount?has_content>
                                 ${uiLabelMap.OrderPaymentMaximumAmount}: <@ofbizCurrency amount=orderPaymentPreference.maxAmount?default(0.00) isoCode=currencyUomId/>
                              </#if>
                              <br />&nbsp;[<#if oppStatusItem??>${oppStatusItem.get("description",locale)}<#else>${orderPaymentPreference.statusId}</#if>]
                            <#--
                            <div><@ofbizCurrency amount=orderPaymentPreference.maxAmount?default(0.00) isoCode=currencyUomId/>&nbsp;-&nbsp;${(orderPaymentPreference.authDate.toString())!}</div>
                            <div>&nbsp;<#if orderPaymentPreference.authRefNum??>(${uiLabelMap.OrderReference}: ${orderPaymentPreference.authRefNum})</#if></div>
                            -->
                        <#elseif paymentMethodType.paymentMethodTypeId == "EXT_LIGHTNING">
                            <#assign paymentTotal = 0.00 />
                            <#list paymentList as payment>
                                <#assign paymentTotal = paymentTotal + payment.amount />
                            </#list>
                            <#assign orderDate = .now?iso>
                            <#if orderHeader?has_content><#assign orderDate = orderHeader.orderDate/></#if>
                            <#assign xbtAmount = Static["org.ofbiz.common.uom.UomWorker"].convertDatedUom(orderDate, paymentTotal, currencyUomId,"XBT",dispatcher,true)>
                            <p>${uiLabelMap.CommonAmount}: <@ofbizCurrency amount=xbtAmount?default(0.00) rounding="8" isoCode="XBT"/></p>
                            <#if paymentTotal &lt; orderPaymentPreference.maxAmount?default(0.00)>
                                <a href="<@pageUrl>receivepayment?${raw(paramString)}</@pageUrl>">${uiLabelMap.AccountingReceivePayment}</a>
                            </#if>
                        <#else>
                            <#assign paymentTotal = 0.00 />
                            <#list paymentList as payment>
                                <#assign paymentTotal = paymentTotal + payment.amount />
                            </#list>
                            <#if paymentTotal &lt; orderPaymentPreference.maxAmount?default(0.00)>
                                <a href="<@pageUrl>receivepayment?${raw(paramString)}</@pageUrl>">${uiLabelMap.AccountingReceivePayment}</a>
                            </#if>
                        </#if>
                        </@cell>
                        <@cell columns=6>
                            <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
                                <#if orderPaymentPreference.statusId != "PAYMENT_RECEIVED">                                
                                    <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                    <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@pageUrl>updateOrderPaymentPreference</@pageUrl>">
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
                                        <a href="<@serverUrl>/accounting/control/paymentOverview?paymentId=${paymentMap.paymentId}${raw(externalKeyParam)}</@serverUrl>">${paymentMap.paymentId}</a><#if paymentMap_has_next><br /></#if>
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
                              <#-- SCIPIO: Here, re-inverted the permission logic the right way -->
                              <#if security.hasEntityPermission("PAY_INFO", "_VIEW", request) || security.hasEntityPermission("ACCOUNTING", "_VIEW", request)>
                                ${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}
                              <#else>
                                ${creditCard.cardType}
                                <@acctlib.maskSensitiveNumber cardNumber=creditCard paymentMethod=paymentMethod/><#-- SCIPIO: Pass payment method -->
                                ${creditCard.expireDate}
                              </#if>
                              &nbsp;[<#if oppStatusItem??>${oppStatusItem.get("description", locale)}<#else>${orderPaymentPreference.statusId!}</#if>]
                              <#else>
                              ${uiLabelMap.CommonInformation} ${uiLabelMap.CommonNot} ${uiLabelMap.CommonAvailable}
                            </#if>

                          </@cell>
                          <@cell columns=6>
                                
                                <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
                                   <#if orderPaymentPreference.statusId != "PAYMENT_RECEIVED">                                   
                                      <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                      <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@pageUrl>updateOrderPaymentPreference</@pageUrl>">
                                        <input type="hidden" name="orderId" value="${orderId}" />
                                        <input type="hidden" name="orderPaymentPreferenceId" value="${orderPaymentPreference.orderPaymentPreferenceId}" />
                                        <input type="hidden" name="statusId" value="PAYMENT_CANCELLED" />
                                        <#-- SCIPIO: paymentMethodType is not set here - crashes + dangerous code in loop
                                        <input type="hidden" name="checkOutPaymentId" value="${paymentMethodType.paymentMethodTypeId!paymentMethod.paymentMethodTypeId!}" />-->
                                        <input type="hidden" name="checkOutPaymentId" value="${paymentMethod.paymentMethodTypeId!}" />
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
                                    <a href="<@serverUrl>/accounting/control/ViewGatewayResponse?paymentGatewayResponseId=${gatewayResponse.paymentGatewayResponseId}${raw(externalKeyParam)}</@serverUrl>">${uiLabelMap.CommonDetails}</a>
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
                               <#if orderPaymentPreference.statusId != "PAYMENT_RECEIVED">                               
                                  <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                  <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@pageUrl>updateOrderPaymentPreference</@pageUrl>">
                                    <input type="hidden" name="orderId" value="${orderId}" />
                                    <input type="hidden" name="orderPaymentPreferenceId" value="${orderPaymentPreference.orderPaymentPreferenceId}" />
                                    <input type="hidden" name="statusId" value="PAYMENT_CANCELLED" />
                                    <#-- SCIPIO: paymentMethodType is not set here - crashes + dangerous code in loop
                                    <input type="hidden" name="checkOutPaymentId" value="${paymentMethodType.paymentMethodTypeId!paymentMethod.paymentMethodTypeId!}" />-->
                                    <input type="hidden" name="checkOutPaymentId" value="${paymentMethod.paymentMethodTypeId!}" />
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
                            <a href="<@serverUrl>/accounting/control/paymentOverview?paymentId=${paymentMap.paymentId}${raw(externalKeyParam)}</@serverUrl>">${paymentMap.paymentId}</a><#if paymentMap_has_next><br /></#if>
                        </#list>
                    </@cell>
                </@row>
              </#if>
            <#elseif (paymentMethod.paymentMethodTypeId!) == "GIFT_CARD">
              <#assign giftCard = paymentMethod.getRelatedOne("GiftCard", false)!>
              <#if giftCard?has_content>
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
                              <#if security.hasEntityPermission("PAY_INFO", "_VIEW", request) || security.hasEntityPermission("ACCOUNTING", "_VIEW", request)>
                                ${giftCard.cardNumber!(uiLabelMap.CommonNA)} [${giftCard.pinNumber!(uiLabelMap.CommonNA)}]
                                &nbsp;[<#if oppStatusItem??>${oppStatusItem.get("description",locale)}<#else>${orderPaymentPreference.statusId}</#if>]
                              <#else>
                              <@acctlib.maskSensitiveNumber cardNumber=giftCard paymentMethod=paymentMethod/><#-- SCIPIO: Pass payment method -->
                              <#if !cardNumberDisplay?has_content>${uiLabelMap.CommonNA}</#if>
                                &nbsp;[<#if oppStatusItem??>${oppStatusItem.get("description",locale)}<#else>${orderPaymentPreference.statusId}</#if>]
                              </#if>
                            <#else>
                              ${uiLabelMap.CommonInformation} ${uiLabelMap.CommonNot} ${uiLabelMap.CommonAvailable}
                            </#if>
                        </@cell>
                        <@cell columns=6>
                            <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
                               <#if orderPaymentPreference.statusId != "PAYMENT_RECEIVED">
                                  <a href="javascript:document.CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonCancel}</a>
                                  <form name="CancelOrderPaymentPreference_${orderPaymentPreference.orderPaymentPreferenceId}" method="post" action="<@pageUrl>updateOrderPaymentPreference</@pageUrl>">
                                    <input type="hidden" name="orderId" value="${orderId}" />
                                    <input type="hidden" name="orderPaymentPreferenceId" value="${orderPaymentPreference.orderPaymentPreferenceId}" />
                                    <input type="hidden" name="statusId" value="PAYMENT_CANCELLED" />
                                    <#-- SCIPIO: paymentMethodType is not set here - crashes + dangerous code in loop
                                    <input type="hidden" name="checkOutPaymentId" value="${paymentMethodType.paymentMethodTypeId!paymentMethod.paymentMethodTypeId!}" />-->
                                    <input type="hidden" name="checkOutPaymentId" value="${paymentMethod.paymentMethodTypeId!}" />
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
                                <a href="<@serverUrl>/accounting/control/paymentOverview?paymentId=${paymentMap.paymentId}${raw(externalKeyParam)}</@serverUrl>">${paymentMap.paymentId}</a><#if paymentMap_has_next><br /></#if>
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
                    <a href="<@serverUrl>/accounting/control/paymentOverview?paymentId=${paymentMap.paymentId}${raw(externalKeyParam)}</@serverUrl>">${paymentMap.paymentId}</a><#if paymentMap_has_next><br /></#if>
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
                <div>${uiLabelMap.CommonNbr} <a href="<@serverUrl>/accounting/control/invoiceOverview?invoiceId=${invoice}${raw(externalKeyParam)}</@serverUrl>">${invoice}</a>
                (<a target="_BLANK" href="<@serverUrl>/accounting/control/invoice.pdf?invoiceId=${invoice}${raw(externalKeyParam)}</@serverUrl>">PDF</a>)</div>
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
                   <@modal label=uiLabelMap.AccountingNewPayment id="modal_addPaymentMethodToOrder" linkClass="${styles.link_nav!} ${styles.action_add!}">
                       <form name="addPaymentMethodToOrder" method="post" action="<@pageUrl>addPaymentMethodToOrder</@pageUrl>">
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
                                               <#-- SCIPIO: Here, re-inverted the permission logic the right way -->
                                               <#if security.hasEntityPermission("PAY_INFO", "_VIEW", request) || security.hasEntityPermission("ACCOUNTING", "_VIEW", request)>
                                                 ${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}
                                               <#else>
                                                 ${creditCard.cardType!} <@acctlib.maskSensitiveNumber cardNumber=creditCard paymentMethod=paymentMethod/> ${creditCard.expireDate!}<#-- SCIPIO: Pass payment method -->
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
