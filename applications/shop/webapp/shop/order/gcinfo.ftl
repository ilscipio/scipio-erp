<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
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
