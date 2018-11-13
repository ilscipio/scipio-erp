<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#-- gift card fields -->
  <input type="hidden" name="addGiftCard" value="Y"/>
  <#assign giftCard = giftCard!>
  <#if (paymentMethodTypeId!) != "GIFT_CARD">
    <hr />
  </#if>
  <@heading>${uiLabelMap.AccountingGiftCardInformation}</@heading>
  <@field type="input" label=uiLabelMap.AccountingGiftCardNumber required=true size="20" maxlength="60" name="giftCardNumber" value=(giftCard.cardNumber!)/>
  <@field type="input" label=uiLabelMap.AccountingPINNumber required=true size="10" maxlength="60" name="giftCardPin" value=(giftCard.pinNumber!)/>
  <@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="60" name="description" value=(giftCard.description!)/>
  <#if (paymentMethodTypeId!) != "GIFT_CARD">
    <@field type="input" label=uiLabelMap.AccountingAmountToUse required=true size="5" maxlength="10" name="giftCardAmount" value=(giftCard.pinNumber!)/>
  </#if>
