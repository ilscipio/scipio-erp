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

<form id="addGiftCertificate" action="<@ofbizUrl>addGiftCertificateSurvey</@ofbizUrl>" method="post">
    <#if surveyId?has_content>
      <input type="hidden" name="quantity" value="1" />
      <input type="hidden" name="surveyId" value="${surveyId!}" />
      <#if giftCardProductList?has_content>
        <@heading relLevel=+1>${uiLabelMap.OrderSelectGiftAmount}</@heading>
        <#list giftCardProductList?sort_by("price") as giftCardProduct>
          <@field type="generic" label="${giftCardProduct.productId!}&nbsp;:&nbsp;${giftCardProduct.productName!}" id="productId_${giftCardProduct.price!}">
            <input type="radio" name="add_product_id" id="productId_${giftCardProduct.price!}" value="${giftCardProduct.productId!}" checked="checked" />
          </@field>
        </#list>
        <@field type="generic" label="${uiLabelMap.OrderRecipientEmailAdd}" id="emailAddress">
          <input type="text" id="emailAddress" name="answers_1002" value="" />
        </@field>
        <@field type="generic" label="${uiLabelMap.OrderRecipientName}" id="recipientName">
          <input type="text" id="recipientName" name="answers_1001" value="" />
        </@field>
        <@field type="generic" label="${uiLabelMap.OrderSenderName}" id="senderName">
          <input type="text" id="senderName" name="answers_1000" value="" />
        </@field>
        <@field type="generic" label="${uiLabelMap.OrderGiftMessage}" id="message">
          <textarea id="message" name="answers_1003"></textarea>
        </@field>
        <@field type="submitarea">
          <input type="submit" value="${uiLabelMap.CommonSubmit}" class="${styles.link_run_sys!} ${styles.action_add!}"/>
        </@field>
      <#else>
        <@commonMsg type="result">${uiLabelMap.OrderNoGiftCertificatesFound}</@commonMsg>
      </#if>
    <#else>
      <@commonMsg type="result">${uiLabelMap.OrderNoProductStoreFinAccountSettingsFound}.</@commonMsg>
    </#if>
</form>

