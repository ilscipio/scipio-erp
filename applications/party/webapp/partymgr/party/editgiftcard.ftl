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

<!-- begin editgiftcard.ftl -->
<#if !giftCard??>
  <#assign sectionTitle = "${uiLabelMap.AccountingCreateNewGiftCard}">
<#else>
  <#assign sectionTitle = "${uiLabelMap.AccountingEditGiftCard}">
</#if>
<@section title=sectionTitle>
    <#macro saveCancelMenu>
      <@menu type="button">
        <@menuitem type="link" href=makeOfbizUrl("${donePage}?partyId=${partyId}") text="${uiLabelMap.CommonCancelDone}" />
        <@menuitem type="link" href="javascript:document.editgiftcardform.submit()" text="${uiLabelMap.CommonSave}" />
      </@menu>
    </#macro>
        
    <@saveCancelMenu />
     
    <#if !giftCard??>
      <form method="post" action="<@ofbizUrl>createGiftCard?DONE_PAGE=${donePage}</@ofbizUrl>" name="editgiftcardform">
    <#else>
      <form method="post" action="<@ofbizUrl>updateGiftCard?DONE_PAGE=${donePage}</@ofbizUrl>" name="editgiftcardform">
        <input type="hidden" name="paymentMethodId" value="${paymentMethodId}" />
    </#if>
        <input type="hidden" name="partyId" value="${partyId}"/>
        
        <@field type="generic" label="${uiLabelMap.AccountingCardNumber}">
            <input type="text" size="20" maxlength="60" name="cardNumber" value="${giftCardData.cardNumber!}" />
        </@field>
        <@field type="generic" label="${uiLabelMap.AccountingPinNumber}">
            <input type="text" size="10" maxlength="60" name="pinNumber" value="${giftCardData.pinNumber!}" />
        </@field>
        <@field type="generic" label="${uiLabelMap.CommonExpireDate}">
            <#assign expMonth = "">
            <#assign expYear = "">
            <#if giftCardData?? && giftCardData.expireDate??>
              <#assign expDate = giftCard.expireDate>
              <#if (expDate?? && expDate.indexOf("/") > 0)>
                <#assign expMonth = expDate.substring(0,expDate.indexOf("/"))>
                <#assign expYear = expDate.substring(expDate.indexOf("/")+1)>
              </#if>
            </#if>
            <select name="expMonth" onchange="javascript:makeExpDate();">
              <#if giftCardData?has_content && expMonth?has_content>
                <#assign ccExprMonth = expMonth>
              <#else>
                <#assign ccExprMonth = requestParameters.expMonth!>
              </#if>
              <#if ccExprMonth?has_content>
                <option value="${ccExprMonth!}">${ccExprMonth!}</option>
              </#if>
              ${screens.render("component://common/widget/CommonScreens.xml#ccmonths")}
            </select>
            <select name="expYear" onchange="javascript:makeExpDate();">
              <#if giftCard?has_content && expYear?has_content>
                <#assign ccExprYear = expYear>
              <#else>
                <#assign ccExprYear = requestParameters.expYear!>
              </#if>
              <#if ccExprYear?has_content>
                <option value="${ccExprYear!}">${ccExprYear!}</option>
              </#if>
              ${screens.render("component://common/widget/CommonScreens.xml#ccyears")}
            </select>
        </@field>
        <@field type="generic" label="${uiLabelMap.CommonDescription}">
            <input type="text" size="30" maxlength="60" name="description" value="${paymentMethodData.description!}" />
        </@field>

      </form>
      
      <@saveCancelMenu />
</@section>
<!-- end editgiftcard.ftl -->