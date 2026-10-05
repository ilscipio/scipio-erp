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
  ${getLabel('EcommerceYourGiftCardHasBeenReloaded', {'cardNumber': raw(displayNumber!)})}
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
