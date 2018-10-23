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
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#-- SCIPIO: TODO: SHOW GIFT CARD BALANCE -->

<#if canNotView>
  <@commonMsg type="error-perm">${uiLabelMap.AccountingCardInfoNotBelongToYou}.</@commonMsg>
  <@menu type="button">
    <@menuitem type="link" href=makeOfbizUrl(donePage) class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
  </@menu>
<#else>

<#-- SCIPIO: This was a message to explain to "Go Back" kludge; however I have now recoded controller and screen
    to redirect automatically.
<@commonMsg type="info-important">${uiLabelMap.ShopSaveGoBackExplanation}</@commonMsg>-->

<#-- SCIPIO: FIXME -->
<@commonMsg type="warning">${uiLabelMap.CommonWarning}: This form currently bypasses gift card product store settings and validation (for testing purposes).</@commonMsg>

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl(donePage) class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
    <@menuitem type="link" href="javascript:document.editgiftcardform.submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
  </@menu>
</#macro>

<#if !giftCard??>
  <#assign sectionTitle = uiLabelMap.AccountingAddNewGiftCard/>
<#else>
  <#-- SCIPIO: duplicate: <#assign sectionTitle = uiLabelMap.AccountingEditGiftCard/> -->
  <#assign sectionTitle = ""/>
</#if>
<@section title=sectionTitle menuContent=menuContent menuLayoutGeneral="bottom">
  <form method="post" action="<@ofbizUrl><#if !giftCard??>createGiftCard?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done<#else>updateGiftCard?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done</#if></@ofbizUrl>" name="editgiftcardform">
    <#if giftCard??>
      <input type="hidden" name="paymentMethodId" value="${paymentMethodId}" />
    </#if>

    <#if giftCardData?has_content && giftCardData.cardNumber?has_content>
      <#assign pcardNumberDisplay = "">
      <#assign pcardNumber = giftCardData.cardNumber!>
      <#if pcardNumber?has_content>
        <#assign psize = pcardNumber?length - 4>
        <#if (0 < psize)>
          <#list 0..(psize-1) as foo>
            <#assign pcardNumberDisplay = pcardNumberDisplay + "*">
          </#list>
          <#assign pcardNumberDisplay = pcardNumberDisplay + pcardNumber[psize .. psize + 3]>
        <#else>
          <#assign pcardNumberDisplay = pcardNumber>
        </#if>
      </#if>
    </#if>
    <@field type="input" label=uiLabelMap.AccountingCardNumber size="20" maxlength="60" name="cardNumber" value=(pcardNumberDisplay!) />
    <@field type="password" label=uiLabelMap.AccountingPINNumber size="10" maxlength="60" name="pinNumber" value=((giftCardData.pinNumber)!) />

    <@field type="generic" label=uiLabelMap.AccountingExpirationDate>
        <#assign expMonth = "">
        <#assign expYear = "">
        <#if giftCardData?? && giftCardData.expireDate??>
          <#assign expDate = giftCard.expireDate!?string>
          <#if (expDate?? && (expDate?index_of("/") > 0))>
            <#assign expMonth = expDate?substring(0, expDate?index_of("/"))>
            <#assign expYear = expDate?substring(expDate?index_of("/")+1)>
          </#if>
        </#if>
        <@field type="select" inline=true name="expMonth" onChange="javascript:makeExpDate();" tooltip=uiLabelMap.CommonMonth>
          <#if giftCardData?has_content && expMonth?has_content>
            <#assign ccExprMonth = expMonth>
          <#else>
            <#assign ccExprMonth = requestParameters.expMonth!>
          </#if>
          <#if ccExprMonth?has_content>
            <option value="${ccExprMonth!}">${ccExprMonth!}</option>
          </#if>
          <@render resource="component://common/widget/CommonScreens.xml#ccmonths" />
        </@field>
        <@field type="select" inline=true name="expYear" onChange="javascript:makeExpDate();" tooltip=uiLabelMap.CommonYear>
          <#if giftCard?has_content && expYear?has_content>
            <#assign ccExprYear = expYear>
          <#else>
            <#assign ccExprYear = requestParameters.expYear!>
          </#if>
          <#if ccExprYear?has_content>
            <option value="${ccExprYear!}">${ccExprYear!}</option>
          </#if>
          <@render resource="component://common/widget/CommonScreens.xml#ccyears" />
        </@field>
    </@field>
    <@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="60" name="description" value=(paymentMethodData.description!) />
  </form>  
</@section>
 
</#if>
