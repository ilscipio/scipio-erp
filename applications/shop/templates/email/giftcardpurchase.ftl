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

<#-- Three standard fields cardNumber, pinNumber and amount are available from the activation
     All other fields in this tempalte are designed to work with the values (responses)
     from surveyId 1000 - The gift card purchase survey.
 -->

<#if recipientName??>${recipientName},</#if>
<br />

<#-- SCIPIO: Rewrote and unhardcoded
${uiLabelMap.EcommerceYouHaveBeenSent} MyCompany.com (FIXME) <#if senderName??> ${uiLabelMap.EcommerceGiftCardFrom} ${senderName}</#if>! -->
${getLabel((senderName??)?then('EcommerceYouHaveBeenSentGiftCardFrom', 'EcommerceYouHaveBeenSentGiftCard'),
    {'storeName': raw((productStore.storeName)!), 'senderName': senderName!})}
<br /><br />
<#if giftMessage?has_content>
  ${getLabel('OrderGiftMessage')}:
  <br /><br />
  "${giftMessage}"
  <br /><br />
</#if>

<pre>
  ${uiLabelMap.EcommerceYourCardNumber}: ${cardNumber!}
  <#if pinNumber?has_content>${uiLabelMap.EcommerceYourPinNumber}: ${pinNumber!}</#if>
  ${uiLabelMap.EcommerceGiftAmount}: <@ofbizCurrency amount=(amount!) isoCode=(currencyUomId!)/>
</pre>
