<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<form id="addGiftCertificate" action="<@ofbizUrl>addGiftCertificateSurvey</@ofbizUrl>" method="post">
    <#if surveyId?has_content>
      <input type="hidden" name="quantity" value="1" />
      <input type="hidden" name="surveyId" value="${surveyId!}" />
      <#if giftCardProductList?has_content>
        <@heading relLevel=+1>${uiLabelMap.OrderSelectGiftAmount}</@heading>
        <#list giftCardProductList?sort_by("price") as giftCardProduct>
          <@field type="input" name="add_product_id" label="${rawString(giftCardProduct.productId!)} : ${rawString(giftCardProduct.productName!)}" id="productId_${giftCardProduct.price!}" value=(giftCardProduct.productId!) checked=true />
        </#list>
        <@field type="input" label=uiLabelMap.OrderRecipientEmailAdd id="emailAddress" name="answers_1002" value="" />
        <@field type="input" label=uiLabelMap.OrderRecipientName id="recipientName" name="answers_1001" value="" />
        <@field type="input" label=uiLabelMap.OrderSenderName id="senderName" name="answers_1000" value="" />
        <@field type="textarea" label=uiLabelMap.OrderGiftMessage id="message" name="answers_1003"></@field>
        <@field type="submit" text=uiLabelMap.CommonSubmit class="+${styles.link_run_sys!} ${styles.action_add!}"/>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.OrderNoGiftCertificatesFound}</@commonMsg>
      </#if>
    <#else>
      <@commonMsg type="result-norecord">${uiLabelMap.OrderNoProductStoreFinAccountSettingsFound}.</@commonMsg>
    </#if>
</form>

