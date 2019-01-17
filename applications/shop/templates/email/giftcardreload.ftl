<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#--
     Standard fields for this template are: cardNumber, pinNumber, amount, previousAmount, processResult, responseCode
     All other fields in this template are designed to work with the values (responses) from surveyId 1001
-->

<#include "component://shop/webapp/shop/common/common.ftl">

<#if cardNumber?has_content><#-- SCIPIO: Cross-support with giftcardpurchase.ftl -->
  <#assign giftCardNumber = cardNumber>
</#if>
<#if giftCardNumber?has_content>
  <#assign displayNumber = getGiftCardDisplayNumber(giftCardNumber)><#-- SCIPIO: Refactored -->
</#if>

<#if processResult>
  <#-- success -->
  <br />
  <#-- SCIPIO: Doubled words and bad localization
  ${uiLabelMap.EcommerceYourGiftCard} ${displayNumber} ${uiLabelMap.EcommerceYourGiftCardReloaded}-->
  ${getLabel('EcommerceYourGiftCardHasBeenReloaded', {'cardNumber': rawString(displayNumber!)})}
  <br />
  ${uiLabelMap.EcommerceGiftCardNewBalance}: <@ofbizCurrency amount=(amount!) isoCode=(currencyUomId!)/><#rt/>
    <#lt/> (${uiLabelMap.CommonFrom}: <@ofbizCurrency amount=(previousAmount!) isoCode=(currencyUomId!)/>)
  <br />
<#else>
  <#-- fail -->
  <br />
  ${uiLabelMap.EcommerceGiftCardReloadFailed} [${responseCode!}]
  <br />
  ${uiLabelMap.EcommerceGiftCardRefunded}
  <br />
</#if>
