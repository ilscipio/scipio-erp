<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#include "component://party/webapp/partymgr/common/common.ftl">
<#import "component://accounting/webapp/accounting/common/acctlib.ftl" as acctlib>

<#-- SCIPIO: MOVED TO: component://accounting/webapp/accounting/common/acctlib.ftl
<#macro maskSensitiveNumber cardNumber paymentMethod= cardNumberMask=>
</#macro>-->

  <#-- SCIPIO: Removed
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
    <#if security.hasEntityPermission("PAY_INFO", "_CREATE", request) || security.hasEntityPermission("ACCOUNTING", "_CREATE", request)>
      <@menuitem type="link" href=makeOfbizUrl("editeftaccount?partyId=${partyId}") text=uiLabelMap.AccountingCreateNewEftAccount class="+${styles.action_nav!} ${styles.action_add!}"/>
      <@menuitem type="link" href=makeOfbizUrl("editgiftcard?partyId=${partyId}") text=uiLabelMap.AccountingCreateNewGiftCard class="+${styles.action_nav!} ${styles.action_add!}"/>
      <@menuitem type="link" href=makeOfbizUrl("editcreditcard?partyId=${partyId}") text=uiLabelMap.AccountingCreateNewCreditCard class="+${styles.action_nav!} ${styles.action_add!}"/>
    </#if>  
    </@menu>
  </#macro>
  -->
  <@section id="partyPaymentMethod" title=uiLabelMap.PartyPaymentMethodInformation>
      <#if paymentMethodValueMaps?has_content || billingAccounts?has_content>
        <@table type="data-complex">
        <@tbody>
        <#if paymentMethodValueMaps?has_content>
          <#list paymentMethodValueMaps as paymentMethodValueMap>
            <#assign paymentMethod = paymentMethodValueMap.paymentMethod/>
            <@tr>
              <#macro deleteButton>
                <#if security.hasEntityPermission("PAY_INFO", "_DELETE", request) || security.hasEntityPermission("ACCOUNTING", "_DELETE", request)>
                  <a href="<@ofbizUrl>deletePaymentMethod/viewprofile?partyId=${partyId}&amp;paymentMethodId=${paymentMethod.paymentMethodId}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonExpire}</a>
                <#else>
                  &nbsp;
                </#if>
              </#macro>
              <#if "CREDIT_CARD" == paymentMethod.paymentMethodTypeId && paymentMethodValueMap.creditCard?has_content>
                <#assign creditCard = paymentMethodValueMap.creditCard/>
                <@td>
                  ${uiLabelMap.AccountingCreditCard}
                </@td>
                <@td>
                  <#if creditCard.companyNameOnCard?has_content>${creditCard.companyNameOnCard}&nbsp;</#if>
                  <#if creditCard.titleOnCard?has_content>${creditCard.titleOnCard}&nbsp;</#if>
                  ${creditCard.firstNameOnCard}&nbsp;
                  <#if creditCard.middleNameOnCard?has_content>${creditCard.middleNameOnCard}&nbsp;</#if>
                  ${creditCard.lastNameOnCard}
                  <#if creditCard.suffixOnCard?has_content>&nbsp;${creditCard.suffixOnCard}</#if>
                  &nbsp;-&nbsp;
                  <#if security.hasEntityPermission("PAY_INFO", "_VIEW", request) || security.hasEntityPermission("ACCOUNTING", "_VIEW", request)>
                    ${creditCard.cardType}
                    <@acctlib.maskSensitiveNumber cardNumber=creditCard paymentMethod=paymentMethod/><#-- SCIPIO: Pass payment method -->
                    ${creditCard.expireDate}
                  <#else>
                    ${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}
                  </#if>
                  <#if paymentMethod.description?has_content>(${paymentMethod.description})</#if>
                  <#if paymentMethod.glAccountId?has_content>(for GL Account ${paymentMethod.glAccountId})</#if>
                  <#if paymentMethod.fromDate?has_content>(${uiLabelMap.CommonUpdated}:&nbsp;${paymentMethod.fromDate!})</#if>
                  <#if paymentMethod.thruDate?has_content><b>(${uiLabelMap.PartyContactEffectiveThru}:&nbsp;${paymentMethod.thruDate})</#if>
                </@td>
                <@td class="button-col">
                  <#if security.hasEntityPermission("MANUAL", "_PAYMENT", request)>
                    <a href="<@ofbizInterWebappUrl>/accounting/control/manualETx?paymentMethodId=${paymentMethod.paymentMethodId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${uiLabelMap.PartyManualTx}</a>
                  </#if>
                  <#if security.hasEntityPermission("PAY_INFO", "_UPDATE", request) || security.hasEntityPermission("ACCOUNTING", "_UPDATE", request)>
                    <a href="<@ofbizUrl>editcreditcard?partyId=${partyId}&amp;paymentMethodId=${paymentMethod.paymentMethodId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                  </#if>
                  <@deleteButton />
                </@td>
              <#elseif "GIFT_CARD" == paymentMethod.paymentMethodTypeId>
                <#assign giftCard = paymentMethodValueMap.giftCard>
                <@td>
                  ${uiLabelMap.AccountingGiftCard}
                </@td>
                <@td>
                  <#if security.hasEntityPermission("PAY_INFO", "_VIEW", request) || security.hasEntityPermission("ACCOUNTING", "_VIEW", request)>
                    ${giftCard.cardNumber!(uiLabelMap.CommonNA)} [${giftCard.pinNumber!(uiLabelMap.CommonNA)}]
                  <#else>
                    <@acctlib.maskSensitiveNumber cardNumber=giftCard paymentMethod=paymentMethod/><#-- SCIPIO: Pass payment method -->
                    <#if !cardNumberDisplay?has_content>${uiLabelMap.CommonNA}</#if>
                  </#if>
                  <#if paymentMethod.description?has_content>(${paymentMethod.description})</#if>
                  <#if paymentMethod.glAccountId?has_content>(for GL Account ${paymentMethod.glAccountId})</#if>
                  <#if paymentMethod.fromDate?has_content>(${uiLabelMap.CommonUpdated}:&nbsp;${paymentMethod.fromDate!})</#if>
                  <#if paymentMethod.thruDate?has_content><b>(${uiLabelMap.PartyContactEffectiveThru}:&nbsp;${paymentMethod.thruDate.toString()}</b></#if>
                </@td>
                <@td class="button-col">
                  <#if security.hasEntityPermission("PAY_INFO", "_UPDATE", request) || security.hasEntityPermission("ACCOUNTING", "_UPDATE", request)>
                    <a href="<@ofbizUrl>editgiftcard?partyId=${partyId}&amp;paymentMethodId=${paymentMethod.paymentMethodId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                  </#if>
                  <@deleteButton />
                </@td>
              <#elseif "EFT_ACCOUNT" == paymentMethod.paymentMethodTypeId>
                <#assign eftAccount = paymentMethodValueMap.eftAccount>
                <@td>
                    ${uiLabelMap.PartyEftAccount}
                </@td>
                <@td>
                  ${eftAccount.nameOnAccount} - <#if eftAccount.bankName?has_content>${uiLabelMap.PartyBank}: ${eftAccount.bankName}</#if> <#if eftAccount.accountNumber?has_content>${uiLabelMap.PartyAccount} #: ${eftAccount.accountNumber}</#if>                  <#if paymentMethod.description?has_content>(${paymentMethod.description})</#if>
                  <#if paymentMethod.glAccountId?has_content>(for GL Account ${paymentMethod.glAccountId})</#if>
                  <#if paymentMethod.fromDate?has_content>(${uiLabelMap.CommonUpdated}:&nbsp;${paymentMethod.fromDate!})</#if>
                  <#if paymentMethod.thruDate?has_content><b>(${uiLabelMap.PartyContactEffectiveThru}:&nbsp;${paymentMethod.thruDate.toString()}</#if>
                </@td>
                <@td class="button-col">
                  <#if security.hasEntityPermission("PAY_INFO", "_UPDATE", request) || security.hasEntityPermission("ACCOUNTING", "_UPDATE", request)>
                    <a href="<@ofbizUrl>editeftaccount?partyId=${partyId}&amp;paymentMethodId=${paymentMethod.paymentMethodId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                  </#if>
                  <@deleteButton />
                </@td>
              <#elseif "COMPANY_CHECK" == paymentMethod.paymentMethodTypeId>
                <@td>
                  <#-- TODO: Convert hard-coded text to UI label properties -->
                  Company Check
                </@td>
                <@td>
                  <#if paymentMethod.description?has_content>(${paymentMethod.description})</#if>
                  <#if paymentMethod.glAccountId?has_content>(for GL Account ${paymentMethod.glAccountId})</#if>
                  <#if paymentMethod.fromDate?has_content>(${uiLabelMap.CommonUpdated}:&nbsp;${paymentMethod.fromDate!})</#if>
                  <#if paymentMethod.thruDate?has_content>(${uiLabelMap.PartyContactEffectiveThru}:&nbsp;${paymentMethod.thruDate.toString()}</#if>
                </@td>
                <@td class="button-col">
                  &nbsp;
                  <@deleteButton />
                </@td>
              <#else>
                <@td class="button-col">
                  &nbsp;
                  <@deleteButton />
                </@td>
              </#if>
            </@tr>
          </#list>
        </#if>
        <#-- Billing list-->
        <#if billingAccounts?has_content>
            <#list billingAccounts as billing>
            <@tr>
              <@td>${uiLabelMap.AccountingBilling}</@td>
              <@td>
                  <#if billing.billingAccountId?has_content>${billing.billingAccountId}</#if>
                  <#if billing.description?has_content>(${billing.description})</#if>
                  <#if billing.accountLimit?has_content>(${uiLabelMap.AccountingAccountLimit} $${billing.accountLimit})</#if>
                  <#if billing.accountBalance?has_content>(${uiLabelMap.AccountingBillingAvailableBalance} $${billing.accountBalance})</#if>
                  <#if billing.fromDate?has_content>(${uiLabelMap.CommonUpdated}:&nbsp;${billing.fromDate!})</#if>
                  <#if billing.thruDate?has_content><b>(${uiLabelMap.PartyContactEffectiveThru}:&nbsp;${billing.thruDate.toString()}</b></#if>
              </@td>
              <@td class="button-col">
                <a href="<@ofbizUrl>EditBillingAccount?billingAccountId=${billing.billingAccountId}&amp;partyId=${partyId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                <a href="<@ofbizUrl>deleteBillingAccount?partyId=${partyId}&amp;billingAccountId=${billing.billingAccountId}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonExpire}</a>
              </@td>
          </@tr>
          </#list>
        </#if>
        </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.PartyNoPaymentMethodInformation}</@commonMsg>
      </#if>
  </@section>
