<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#--
     Standard fields for this template are: cardNumber, pinNumber, amount, previousAmount, processResult, responseCode
     All other fields in this template are designed to work with the values (responses) from surveyId 1001
-->

<#if giftCardNumber?has_content>
  <#assign displayNumber = "">
  <#assign numSize = giftCardNumber?length - 4>
  <#if 0 < numSize>
    <#list 0 .. numSize-1 as foo>
      <#assign displayNumber = displayNumber + "*">
    </#list>
    <#assign displayNumber = displayNumber + giftCardNumber[numSize .. numSize + 3]>
  <#else>
    <#assign displayNumber = giftCardNumber>
  </#if>
</#if>

<#if processResult>
  <#-- success -->
  <br />
  ${uiLabelMap.EcommerceYourGiftCard} ${displayNumber} ${uiLabelMap.EcommerceYourGiftCardReloaded}
  <br />
  ${uiLabelMap.EcommerceGiftCardNewBalance} ${amount} ${uiLabelMap.CommonFrom} ${previousAmount}
  <br />
<#else>
  <#-- fail -->
  <br />
  ${uiLabelMap.EcommerceGiftCardReloadFailed} ${responseCode}
  <br />
  ${uiLabelMap.EcommerceGiftCardRefunded}
  <br />
</#if>
