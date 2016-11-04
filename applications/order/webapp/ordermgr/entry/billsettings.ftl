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
<#include "ordercommon.ftl">
<@script>
function shipBillAddr() {
    if (document.checkoutsetupform.useShipAddr.checked) {
        window.location = "<@ofbizUrl>setBilling?createNew=Y&finalizeMode=payment&paymentMethodType=${paymentMethodType!}&useShipAddr=Y</@ofbizUrl>";
    } else {
        window.location = "<@ofbizUrl>setBilling?createNew=Y&finalizeMode=payment&paymentMethodType=${paymentMethodType!}</@ofbizUrl>";
    }
}

function makeExpDate() {
    document.checkoutsetupform.expireDate.value = document.checkoutsetupform.expMonth.options[document.checkoutsetupform.expMonth.selectedIndex].value + "/" + document.checkoutsetupform.expYear.options[document.checkoutsetupform.expYear.selectedIndex].value;
}
</@script>

<#if security.hasEntityPermission("ORDERMGR", "_CREATE", session) || security.hasEntityPermission("ORDERMGR", "_PURCHASE_CREATE", session)>
<@section>
    <@row>
        <@cell columns=6>
        <#if request.getAttribute("paymentMethodId")?? || ( (paymentMethodList?has_content || billingAccountList?has_content) && !requestParameters.createNew??)>
         <@menu type="button">
           <@menuitem type="link" href=makeOfbizUrl("setBilling?createNew=Y") text=uiLabelMap.CommonNew class="+${styles.action_nav!} ${styles.action_add!}"/>
         </@menu>
        
          <#-- initial screen when we have a associated party -->
          <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="checkoutsetupform">
            <input type="hidden" name="finalizeMode" value="payment"/>
             
              <#if billingAccountList?has_content>
                <#assign labelContent>${uiLabelMap.FormFieldTitle_billingAccountId}</#assign>
                <#assign postfixContent></#assign>
                <@checkoutInvField type="generic" labelContent=labelContent postfixContent=postfixContent>
                    <select name="billingAccountId">
                      <option value=""></option>
                        <#list billingAccountList as billingAccount>
                          <#assign availableAmount = billingAccount.accountBalance?double>
                          <#if (billingAccount.accountLimit)??>
                              <#assign accountLimit = billingAccount.accountLimit?double />
                          <#else>
                              <#assign accountLimit = 0.00 />
                          </#if> 
                          <option value="${billingAccount.billingAccountId}" <#if billingAccount.billingAccountId == (selectedBillingAccountId!"")>selected="selected"</#if>>${billingAccount.description!""} [${billingAccount.billingAccountId}] Available: <@ofbizCurrency amount=availableAmount isoCode=billingAccount.accountCurrencyUomId/> Limit: <@ofbizCurrency amount=accountLimit isoCode=billingAccount.accountCurrencyUomId/></option>
                        </#list>
                    </select>
                </@checkoutInvField>
                <#assign labelContent>${uiLabelMap.OrderBillUpTo}</#assign>
                <#assign postfixContent></#assign>
                <@checkoutInvField type="generic" labelContent=labelContent postfixContent=postfixContent>
                    <input type="text" size="5" name="billingAccountAmount" value=""/>
                </@checkoutInvField>
                
              </#if>
              <#assign labelContent><label for="checkOutPaymentId_EXT_OFFLINE">${uiLabelMap.OrderPaymentOfflineCheckMoney}</label></#assign>
              <#assign postfixContent></#assign>
              <@checkoutInvField type="generic" labelContent=labelContent postfixContent=postfixContent>
                  <input type="radio" id="checkOutPaymentId_EXT_OFFLINE" name="checkOutPaymentId" value="EXT_OFFLINE" <#if checkOutPaymentId?? && checkOutPaymentId == "EXT_OFFLINE">checked="checked"</#if>/>
              </@checkoutInvField>
             
              <#assign labelContent><label for="checkOutPaymentId_EXT_COD">${uiLabelMap.OrderCOD}</label></#assign>
              <#assign postfixContent></#assign>
              <@checkoutInvField type="generic" labelContent=labelContent postfixContent=postfixContent>
                  <input type="radio" id="checkOutPaymentId_EXT_COD" name="checkOutPaymentId" value="EXT_COD" <#if checkOutPaymentId?? && checkOutPaymentId == "EXT_COD">checked="checked"</#if>/>
              </@checkoutInvField>
             
              <#if paymentMethodList?has_content>
                <#list paymentMethodList as paymentMethod>
                  <#if paymentMethod.paymentMethodTypeId == "CREDIT_CARD">
                    <#assign creditCard = paymentMethod.getRelatedOne("CreditCard", false)>
                    <#assign labelContent>
                        <label for="checkOutPaymentId_CREDIT_CARD_${paymentMethod.paymentMethodId}">
                          CC:&nbsp;${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}
                          <#if paymentMethod.description?has_content>(${paymentMethod.description})</#if>
                        </label><br/>
                        <@field type="input" size="5" maxlength="10" name="securityCode_${paymentMethod.paymentMethodId}" value="" label="CSC" collapse=true tooltip=uiLabelMap.OrderCardSecurityCode/>
                    </#assign>
                    <#assign postfixContent><a href="<@ofbizInterWebappUrl>/partymgr/control/editcreditcard?party_id=${orderParty.partyId}&amp;paymentMethodId=${paymentMethod.paymentMethodId}</@ofbizInterWebappUrl>" target="_blank" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a></#assign>
                    <@checkoutInvField type="generic" labelContent=labelContent postfixContent=postfixContent>
                        <input type="radio" id="checkOutPaymentId_CREDIT_CARD_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value="${paymentMethod.paymentMethodId}" <#if checkOutPaymentId?? && paymentMethod.paymentMethodId == checkOutPaymentId>checked="checked"</#if>/>
                    </@checkoutInvField>
                  <#elseif paymentMethod.paymentMethodTypeId == "EFT_ACCOUNT">
                    <#assign eftAccount = paymentMethod.getRelatedOne("EftAccount", false)>
                    <#assign labelContent><label for="checkOutPaymentId_EFT_ACCOUNT_${paymentMethod.paymentMethodId}">
                          EFT:&nbsp;${eftAccount.bankName!}: ${eftAccount.accountNumber!}
                          <#if paymentMethod.description?has_content>(${paymentMethod.description})</#if>
                        </label></#assign>
                    <#assign postfixContent><a href="<@ofbizInterWebappUrl>/partymgr/control/editeftaccount?party_id=${orderParty.partyId}&amp;paymentMethodId=${paymentMethod.paymentMethodId}</@ofbizInterWebappUrl>" target="_blank" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a></#assign>
                    <@checkoutInvField type="generic" labelContent=labelContent postfixContent=postfixContent>
                        <input type="radio" id="checkOutPaymentId_EFT_ACCOUNT_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value="${paymentMethod.paymentMethodId}" <#if checkOutPaymentId?? && paymentMethod.paymentMethodId == checkOutPaymentId>checked="checked"</#if>/>
                    </@checkoutInvField>
                    
                  </#if>
                </#list>
              <#else>
                <@commonMsg type="result-norecord">${uiLabelMap.AccountingNoPaymentMethods}</@commonMsg>
              </#if>
          </form>
        <#elseif paymentMethodType?? || finalizeMode?default("") == "payment">
          <#-- after initial screen; show detailed screens for selected type -->
          <#if paymentMethodType == "CC">
            <#if postalAddress?has_content>
              <form method="post" action="<@ofbizUrl>updateCreditCardAndPostalAddress</@ofbizUrl>" name="checkoutsetupform">
                <input type="hidden" name="paymentMethodId" value="${creditCard.paymentMethodId!}"/>
                <input type="hidden" name="contactMechId" value="${postalAddress.contactMechId!}"/>
            <#elseif requestParameters.useShipAddr??>
              <form method="post" action="<@ofbizUrl>createCreditCardOrderEntry</@ofbizUrl>" name="checkoutsetupform">
            <#else>
              <form method="post" action="<@ofbizUrl>createCreditCardAndPostalAddress</@ofbizUrl>" name="checkoutsetupform">
            </#if>
          </#if>
          <#if paymentMethodType == "EFT">
            <#if postalAddress?has_content>
              <form method="post" action="<@ofbizUrl>updateEftAndPostalAddress</@ofbizUrl>" name="checkoutsetupform">
                <input type="hidden" name="paymentMethodId" value="${eftAccount.paymentMethodId!}"/>
                <input type="hidden" name="contactMechId" value="${postalAddress.contactMechId!}"/>
            <#elseif requestParameters.useShipAddr??>
              <form method="post" action="<@ofbizUrl>createEftAccount</@ofbizUrl>" name="checkoutsetupform">
            <#else>
              <form method="post" action="<@ofbizUrl>createEftAndPostalAddress</@ofbizUrl>" name="checkoutsetupform">
            </#if>
          </#if>

          <input type="hidden" name="contactMechTypeId" value="POSTAL_ADDRESS"/>
          <input type="hidden" name="partyId" value="${cart.getPartyId()}"/>
          <input type="hidden" name="paymentMethodType" value="${paymentMethodType}"/>
          <input type="hidden" name="finalizeMode" value="payment"/>
          <input type="hidden" name="createNew" value="Y"/>
          <#if requestParameters.useShipAddr??>
            <input type="hidden" name="contactMechId" value="${postalFields.contactMechId}"/>
          </#if>

            <#if cart.getShippingContactMechId()??>
            <#assign labelContent>${uiLabelMap.FacilityBillingAddressSameShipping}</#assign>
            <#assign postfixContent></#assign>
            <@checkoutInvField type="generic" labelContent=labelContent postfixContent=postfixContent>
                <input type="checkbox" name="useShipAddr" value="Y" onclick="javascript:shipBillAddr();" <#if requestParameters.useShipAddr??>checked="checked"</#if>/>
            </@checkoutInvField>

              <hr />

            </#if>

            <#if orderPerson?has_content>
              <#assign toName = "">
              <#if orderPerson.personalTitle?has_content><#assign toName = orderPerson.personalTitle + " "></#if>
              <#assign toName = toName + orderPerson.firstName + " ">
              <#if orderPerson.middleName?has_content><#assign toName = toName + orderPerson.middleName + " "></#if>
              <#assign toName = toName + orderPerson.lastName>
              <#if orderPerson.suffix?has_content><#assign toName = toName + " " + orderPerson.suffix></#if>
            <#else>
              <#assign toName = postalFields.toName?default("")>
            </#if>

            <#-- generic address information -->
            <#assign fieldDisabled = requestParameters.useShipAddr??>
            <@field type="input" label=uiLabelMap.CommonToName size="30" maxlength="60" name="toName" value=toName disabled=fieldDisabled />
            <@field type="input" label=uiLabelMap.CommonAttentionName size="30" maxlength="60" name="attnName" value=(postalFields.attnName!) disabled=fieldDisabled />
            <@field type="input" label="${rawLabel('CommonAddressLine')} 1" required=true size="30" maxlength="30" name="address1" value=(postalFields.address1!) disabled=fieldDisabled />
            <@field type="input" label="${rawLabel('CommonAddressLine')} 2" size="30" maxlength="30" name="address2" value=(postalFields.address2!) disabled=fieldDisabled />
            <@field type="input" label=uiLabelMap.CommonCity required=true size="30" maxlength="30" name="city" value=(postalFields.city!) disabled=fieldDisabled />
            <@field type="select" label=uiLabelMap.CommonStateProvince name="stateProvinceGeoId" disabled=fieldDisabled>
              <#if postalFields.stateProvinceGeoId??>
                <option>${postalFields.stateProvinceGeoId}</option>
                <option value="${postalFields.stateProvinceGeoId}">---</option>
              </#if>
                <option value=""></option>
                <@render resource="component://common/widget/CommonScreens.xml#states" ctxVars={"statesPreselect":!(postalFields.stateProvinceGeoId??)}/>
            </@field>
            <@field type="input" label=uiLabelMap.CommonZipPostalCode required=true size="12" maxlength="10" name="postalCode" value=(postalFields.postalCode!) disabled=fieldDisabled />
            <@field type="select" label=uiLabelMap.CommonCountry required=true name="countryGeoId" disabled=fieldDisabled>
              <#if postalFields.countryGeoId??>
                <option>${postalFields.countryGeoId}</option>
                <option value="${postalFields.countryGeoId}">---</option>
              </#if>
                <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"countriesPreselect":!(postalFields.countryGeoId??)}/>
            </@field>

            <#-- credit card fields -->
            <#if paymentMethodType == "CC">
              <#if !creditCard?has_content>
                <#assign creditCard = requestParameters>
              </#if>
              <input type="hidden" name="expireDate" value="${creditCard.expireDate!}"/>
              
              <hr />

              <@field type="input" label=uiLabelMap.AccountingCompanyNameCard size="30" maxlength="60" name="companyNameOnCard" value=(creditCard.companyNameOnCard!)/>
              <@field type="select" label=uiLabelMap.AccountingPrefixCard name="titleOnCard">
                  <option value="">${uiLabelMap.CommonSelectOne}</option>
                  <option<#if ((creditCard.titleOnCard)?default("") == "Mr.")> checked="checked"</#if>>${uiLabelMap.CommonTitleMr}</option>
                  <option<#if ((creditCard.titleOnCard)?default("") == "Mrs.")> checked="checked"</#if>>${uiLabelMap.CommonTitleMrs}</option>
                  <option<#if ((creditCard.titleOnCard)?default("") == "Ms.")> checked="checked"</#if>>${uiLabelMap.CommonTitleMs}</option>
                  <option<#if ((creditCard.titleOnCard)?default("") == "Dr.")> checked="checked"</#if>>${uiLabelMap.CommonTitleDr}</option>
              </@field>
              <@field type="input" label=uiLabelMap.AccountingFirstNameCard required=true size="20" maxlength="60" name="firstNameOnCard" value=((creditCard.firstNameOnCard)!)/>
              <@field type="input" label=uiLabelMap.AccountingMiddleNameCard size="15" maxlength="60" name="middleNameOnCard" value=((creditCard.middleNameOnCard)!)/>
              <@field type="input" label=uiLabelMap.AccountingLastNameCard required=true size="20" maxlength="60" name="lastNameOnCard" value=((creditCard.lastNameOnCard)!)/>
              <@field type="select" label=uiLabelMap.AccountingSuffixCard name="suffixOnCard">
                  <option value="">${uiLabelMap.CommonSelectOne}</option>
                  <option<#if ((creditCard.suffixOnCard)?default("") == "Jr.")> checked="checked"</#if>>Jr.</option>
                  <option<#if ((creditCard.suffixOnCard)?default("") == "Sr.")> checked="checked"</#if>>Sr.</option>
                  <option<#if ((creditCard.suffixOnCard)?default("") == "I")> checked="checked"</#if>>I</option>
                  <option<#if ((creditCard.suffixOnCard)?default("") == "II")> checked="checked"</#if>>II</option>
                  <option<#if ((creditCard.suffixOnCard)?default("") == "III")> checked="checked"</#if>>III</option>
                  <option<#if ((creditCard.suffixOnCard)?default("") == "IV")> checked="checked"</#if>>IV</option>
                  <option<#if ((creditCard.suffixOnCard)?default("") == "V")> checked="checked"</#if>>V</option>
              </@field>

              <@field type="select" label=uiLabelMap.AccountingCardType required=true name="cardType">
                <#if creditCard.cartType??>
                  <option>${creditCard.cardType}</option>
                  <option value="${creditCard.cardType}">---</option>
                </#if>
                  <@render resource="component://common/widget/CommonScreens.xml#cctypes" />
              </@field>
              <@field type="input" label=uiLabelMap.AccountingCardNumber required=true size="20" maxlength="30" name="cardNumber" value=(creditCard.cardNumber!)/>

              <@field type="generic" label=uiLabelMap.AccountingExpirationDate required=true>
                  <#assign expMonth = "">
                  <#assign expYear = "">
                  <#if creditCard?? && creditCard.expDate??>
                    <#assign expDate = creditCard.expireDate>
                    <#if (expDate?? && expDate.indexOf("/") > 0)>
                      <#assign expMonth = expDate.substring(0,expDate.indexOf("/"))>
                      <#assign expYear = expDate.substring(expDate.indexOf("/")+1)>
                    </#if>
                  </#if>
                  <@field type="select" inline=true name="expMonth" onChange="javascript:makeExpDate();">
                    <#if creditCard?has_content && expMonth?has_content><#assign ccExprMonth = expMonth><#else><#assign ccExprMonth = requestParameters.expMonth!></#if>
                    <#if ccExprMonth?has_content>
                      <option value="${ccExprMonth!}">${ccExprMonth!}</option>
                    </#if>
                    <@render resource="component://common/widget/CommonScreens.xml#ccmonths" />
                  </@field>
                  <@field type="select" inline=true name="expYear" onChange="javascript:makeExpDate();">
                    <#if creditCard?has_content && expYear?has_content><#assign ccExprYear = expYear><#else><#assign ccExprYear = requestParameters.expYear!></#if>
                    <#if ccExprYear?has_content>
                      <option value="${ccExprYear!}">${ccExprYear!}</option>
                    </#if>
                    <@render resource="component://common/widget/CommonScreens.xml#ccyears" />
                  </@field>
              </@field>
              <@field type="input" label=uiLabelMap.CommonDescription size="20" maxlength="30" name="description" value=(creditCard.description!)/>
                </#if>

                <#-- eft fields -->
                <#if paymentMethodType =="EFT">
                  <#if !eftAccount?has_content>
                    <#assign eftAccount = requestParameters>
                  </#if>
             
              <hr />
           
              <@field type="input" label=uiLabelMap.AccountingNameAccount required=true size="30" maxlength="60" name="nameOnAccount" value=(eftAccount.nameOnAccount!)/>
              <@field type="input" label=uiLabelMap.AccountingCompanyNameAccount size="30" maxlength="60" name="companyNameOnAccount" value=(eftAccount.companyNameOnAccount!)/>
              <@field type="input" label=uiLabelMap.AccountingBankName required=true size="30" maxlength="60" name="bankName" value=(eftAccount.bankName!)/>
              <@field type="input" label=uiLabelMap.AccountingRoutingNumber required=true size="10" maxlength="30" name="routingNumber" value=(eftAccount.routingNumber!)/>
              <@field type="select" label=uiLabelMap.AccountingAccountType required=true name="accountType">
                  <option>${eftAccount.accountType!}</option>
                  <option></option>
                  <option>Checking</option>
                  <option>Savings</option>
              </@field>
              <@field type="input" label=uiLabelMap.AccountingAccountNumber required=true size="20" maxlength="40" name="accountNumber" value=(eftAccount.accountNumber!)/>
              <@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="60" name="description" value=(eftAccount.description!)/>
            </#if>

        <#else>
          <#-- initial screen show a list of options -->

          <@script>
              function setCheckoutPaymentId( selectedValue ) {
                  checkoutForm = document.getElementById('checkoutsetupform');
                  if( selectedValue.match('^EXT_.*') ) {
                      checkoutForm.action = '<@ofbizUrl>finalizeOrder</@ofbizUrl>?checkOutPaymentId=' + selectedValue ;
                  } else {
                      checkoutForm.action = '<@ofbizUrl>setBilling</@ofbizUrl>?paymentMethodType=' + selectedValue ;
                  }
              }
          </@script>

          <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="checkoutsetupform" id="checkoutsetupform">
            <input type="hidden" name="finalizeMode" value="payment"/>
            <input type="hidden" name="createNew" value="${(requestParameters.createNew)!}"/>
            
            <#if "Y" != requestParameters.createNew?default("")>
              <#assign labelContent>${uiLabelMap.OrderPaymentOfflineCheckMoney}</#assign>
              <#assign postfixContent></#assign>
              <@checkoutInvField type="generic" labelContent=labelContent postfixContent=postfixContent>
                  <input type="radio" name="paymentMethodTypeAndId" value="EXT_OFFLINE" <#if checkOutPaymentId?? && checkOutPaymentId == "EXT_OFFLINE">checked="checked"</#if> onchange="setCheckoutPaymentId(this.value)" onclick="setCheckoutPaymentId(this.value)"/>
              </@checkoutInvField>
              
              <#assign labelContent>${uiLabelMap.OrderCOD}</#assign>
              <#assign postfixContent></#assign>
              <@checkoutInvField type="generic" labelContent=labelContent postfixContent=postfixContent>
                  <input type="radio" name="paymentMethodTypeAndId" value="EXT_COD" <#if checkOutPaymentId?? && checkOutPaymentId == "EXT_COD">checked="checked"</#if> onchange="setCheckoutPaymentId(this.value)" onclick="setCheckoutPaymentId(this.value)"/>
              </@checkoutInvField>
            </#if>

              <#assign labelContent>${uiLabelMap.AccountingVisaMastercardAmexDiscover}</#assign>
              <#assign postfixContent></#assign>
              <@checkoutInvField type="generic" labelContent=labelContent postfixContent=postfixContent>
                  <input type="radio" name="paymentMethodTypeAndId" value="CC" onchange="setCheckoutPaymentId(this.value)" onclick="setCheckoutPaymentId(this.value)"/>
              </@checkoutInvField>
              
              <#assign labelContent>${uiLabelMap.AccountingAHCElectronicCheck}</#assign>
              <#assign postfixContent></#assign>
              <@checkoutInvField type="generic" labelContent=labelContent postfixContent=postfixContent>
                  <input type="radio" name="paymentMethodTypeAndId" value="EFT" onchange="setCheckoutPaymentId(this.value)" onclick="setCheckoutPaymentId(this.value)"/>
              </@checkoutInvField>
          </form>
        </#if>
        </@cell>
      </@row>
    </@section>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>
