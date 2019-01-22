<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#-- Three standard fields cardNumber, pinNumber and amount are available from the activation
     All other fields in this tempalte are designed to work with the values (responses)
     from surveyId 1000 - The gift card purchase survey.
 -->

<#if recipientName??>${recipientName},</#if>
<br />

<#-- SCIPIO: Rewrote and unhardcoded
${uiLabelMap.EcommerceYouHaveBeenSent} MyCompany.com (FIXME) <#if senderName??> ${uiLabelMap.EcommerceGiftCardFrom} ${senderName}</#if>! -->
${getLabel((senderName??)?then('EcommerceYouHaveBeenSentGiftCardFrom', 'EcommerceYouHaveBeenSentGiftCard'),
    {'storeName': rawString((productStore.storeName)!), 'senderName': senderName!})}
<br /><br />
<#if giftMessage?has_content>
  ${getLabel('OrderGiftMessage')}:
  <br /><br />
  "${giftMessage}"
  <br /><br />
</#if>

<pre>
  ${uiLabelMap.EcommerceYourCardNumber}: ${cardNumber!}
  ${uiLabelMap.EcommerceYourPinNumber}: ${pinNumber!}
  ${uiLabelMap.EcommerceGiftAmount}: <@ofbizCurrency amount=(amount!) isoCode=(currencyUomId!)/>
</pre>
