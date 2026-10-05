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

<@heading>${uiLabelMap.AccountingGiftCardLink}</@heading>

<p>${uiLabelMap.AccountingEnterGiftCardLink}.</p>

<form name="gclink" method="post" action="<@pageUrl>linkgiftcard</@pageUrl>">
  <#-- SCIPIO: Security: Server-side code must set the paymentConfig
  <input type="hidden" name="paymentConfig" value="${paymentProperties!"payment.properties"}" />-->
  <#if userLogin?has_content>
    <input type="hidden" name="partyId" value="${userLogin.partyId}" />
  </#if>
  <@table type="fields">
    <@tr>
      <@td colspan="2" align="center">
        <div class="tableheadtext">${uiLabelMap.AccountingPhysicalCard}</div>
      </@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.AccountingCardNumber}</@td>
      <@td><input type="text" name="physicalCard" size="20" /></@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.AccountingPINNumber}</@td>
      <@td><input type="text" name="physicalPin" size="20" /></@td>
    </@tr>
    <@tr>
      <@td colspan="2">&nbsp;</@td>
    </@tr>
    <@tr>
      <@td colspan="2" align="center">
        <div class="tableheadtext">${uiLabelMap.AccountingVirtualCard}</div>
      </@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.AccountingCardNumber}</@td>
      <@td><input type="text" name="virtualCard" size="20" /></@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.AccountingPINNumber}</@td>
      <@td><input type="text" name="virtualPin" size="20" /></@td>
    </@tr>
    <@tr>
      <@td colspan="2">&nbsp;</@td>
    </@tr>
    <@tr>
      <@td colspan="2" align="center"><input type="submit" class="${styles.link_run_sys!} ${styles.action_update!}" value="${uiLabelMap.EcommerceLinkCards}" /></@td>
    </@tr>
  </@table>
</form>
<br />
