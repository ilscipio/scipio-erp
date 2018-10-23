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

<#if paymentMethod?has_content || paymentMethodType?has_content || billingAccount?has_content>
<@section title=uiLabelMap.AccountingPaymentInformation>
      <#-- order payment info -->
      <@table type="fields" class="+${styles.table_spacing_tiny_hint!}" width="100%"> <#-- orig: cellpadding="1" --> <#-- orig: border="0" -->
        <#-- offline payment address infomation :: change this to use Company's address -->
        <#if !paymentMethod?has_content && paymentMethodType?has_content>
          <@tr>
            <#if paymentMethodType.paymentMethodTypeId == "EXT_OFFLINE">
              <@td colspan="2" valign="top">
                <div align="center"><b>${uiLabelMap.AccountingOfflinePayment}</b></div>
                <#if orderHeader?has_content && paymentAddress?has_content>
                  <div align="center"><hr /></div>
                  <div align="center"><b>${uiLabelMap.AccountingPleaseSendPaymentTo}:</b></div>
                  <#if paymentAddress.toName?has_content><div align="center">${paymentAddress.toName}</div></#if>
                  <#if paymentAddress.attnName?has_content><div align="center"><b>${uiLabelMap.CommonAttn}:</b> ${paymentAddress.attnName}</div></#if>
                  <div align="center">${paymentAddress.address1}</div>
                  <#if paymentAddress.address2?has_content><div align="center">${paymentAddress.address2}</div></#if>
                  <div align="center">${paymentAddress.city}<#if paymentAddress.stateProvinceGeoId?has_content>, ${paymentAddress.stateProvinceGeoId}</#if> ${paymentAddress.postalCode}
                  <div align="center">${paymentAddress.countryGeoId}</div>
                  <div align="center"><hr /></div>
                  <div align="center"><b>${uiLabelMap.OrderBeSureIncludeOrder} ${uiLabelMap.CommonNbr}</b></div>
                </#if>
              </@td>
            <#else>
              <#assign outputted = true>
              <@td colspan="2" valign="top">
                <div align="center"><b>${uiLabelMap.AccountingPaymentVia} ${paymentMethodType.get("description",locale)}</b></div>
              </@td>
            </#if>
          </@tr>
        </#if>
        <#if paymentMethod?has_content>
          <#assign outputted = true>
          <#-- credit card info -->
          <#if creditCard?has_content>
            <@tr>
              <@td align="right" valign="top" width="15%">&nbsp;<b>${uiLabelMap.AccountingCreditCard}</b></@td>
              <@td valign="top" width="80%">
                  <#if creditCard.companyNameOnCard?has_content>${creditCard.companyNameOnCard}<br /></#if>
                  <#if creditCard.titleOnCard?has_content>${creditCard.titleOnCard}&nbsp;</#if>
                  ${creditCard.firstNameOnCard}&nbsp;
                  <#if creditCard.middleNameOnCard?has_content>${creditCard.middleNameOnCard}&nbsp;</#if>
                  ${creditCard.lastNameOnCard}
                  <#if creditCard.suffixOnCard?has_content>&nbsp;${creditCard.suffixOnCard}</#if>
                  <br />
                  ${formattedCardNumber}
                </@td>
            </@tr>
          <#-- EFT account info -->
          <#elseif eftAccount?has_content>
            <@tr>
              <@td align="right" valign="top" width="15%">&nbsp;<b>${uiLabelMap.AccountingEFTAccount}</b></@td>
              <@td valign="top" width="80%">
                  ${eftAccount.nameOnAccount}<br />
                  <#if eftAccount.companyNameOnAccount?has_content>${eftAccount.companyNameOnAccount}<br /></#if>
                  Bank: ${eftAccount.bankName}, ${eftAccount.routingNumber}<br />
                  Account #: ${eftAccount.accountNumber}
                </@td>
            </@tr>
          </#if>
        </#if>
        <#-- billing account info -->
        <#if billingAccount?has_content>
          <#if outputted?default(false)>
            <@tr type="util"><@td colspan="2"><hr /></@td></@tr>
          </#if>
          <#assign outputted = true/>
          <@tr>
            <@td align="right" valign="top" width="15%">&nbsp;<b>${uiLabelMap.AccountingBillingAccount}</b></@td>
            <@td valign="top" width="80%">
                #${billingAccount.billingAccountId!} - ${billingAccount.description!}
              </@td>
          </@tr>
        </#if>
      </@table>
    </@section>
</#if>
