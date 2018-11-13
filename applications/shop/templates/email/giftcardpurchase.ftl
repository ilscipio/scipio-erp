<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#-- Three standard fields cardNumber, pinNumber and amount are available from the activation
     All other fields in this tempalte are designed to work with the values (responses)
     from surveyId 1000 - The gift card purchase survey.
 -->

<#if recipientName??>${recipientName},</#if>
<br />

<#-- MyCompany.com (not a variable why?) must be adapted - JLR 1/6/5 -->
${uiLabelMap.EcommerceYouHaveBeenSent} MyCompany.com <#if senderName??> ${uiLabelMap.EcommerceGiftCardFrom} ${senderName}</#if>!
<br /><br />
<#if giftMessage?has_content>
  ${uiLabelMap.OrderGiftMessage}
  <br /><br />
  ${giftMessage}
  <br /><br />
</#if>

<pre>
  ${uiLabelMap.EcommerceYourCardNumber} ${cardNumber!}
  ${uiLabelMap.EcommerceYourPinNumber} ${pinNumber!}
  ${uiLabelMap.EcommerceGiftAmount} ${amount!}
</pre>
