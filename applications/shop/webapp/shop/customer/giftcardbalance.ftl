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
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@heading>${uiLabelMap.AccountingGiftCardBalance}</@heading>

<p>${uiLabelMap.AccountingEnterGiftCardNumber}</p>

<@table type="fields">
  <#if requestAttributes.processResult??>
    <@tr>
      <@td colspan="2">
        <div align="center">
          ${uiLabelMap.AccountingCurrentBalance}
        </div>
      </@td>
    </@tr>
    <@tr>
      <@td colspan="2">
        <div class="graybox">
          <#if ((requestAttributes.balance!0) > 0)>
            ${requestAttributes.balance}
          <#else>
            ${uiLabelMap.AccountingCurrentBalanceProblem}
          </#if>
        </div>
      </@td>
    </@tr>
    <@tr><@td colspan="2">&nbsp;</@td></@tr>
  </#if>
  <form method="post" action="<@pageUrl>querygcbalance</@pageUrl>">
    <input type="hidden" name="currency" value="USD" />
    <#-- SCIPIO: Security: Server-side code must set the paymentConfig
    <input type="hidden" name="paymentConfig" value="${paymentProperties!"payment.properties"}" />-->
    <@tr>
      <@td>${uiLabelMap.AccountingCardNumber}</@td>
      <@td><input type="text" name="cardNumber" size="20" value="${(requestParameters.cardNumber)!}" /></@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.AccountingPINNumber}</@td>
      <@td><input type="text" name="pin" size="15" value="${(requestParameters.pin)!}" /></@td>
    </@tr>
    <@tr><@td colspan="2">&nbsp;</@td></@tr>
    <@tr>
      <@td colspan="2" align="center"><input type="submit" class="${styles.link_run_sys!} ${styles.action_verify!}" value="${uiLabelMap.EcommerceCheckBalance}" /></@td>
    </@tr>
  </form>
</@table>
<br />
