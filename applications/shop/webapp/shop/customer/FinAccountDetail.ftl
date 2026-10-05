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

<@table type="fields">
  <@tr>
      <@td width="25%">Account Number</@td>
      <@td>${ownedFinAccount.finAccountId}</@td>
  </@tr>
  <@tr>
      <@td>Currency</@td>
      <@td>${(accountCurrencyUom.description)!} [${ownedFinAccount.currencyUomId!}]</@td>
  </@tr>
  <@tr>
      <@td>Date Opened</@td>
      <@td>${ownedFinAccount.fromDate!}</@td>
  </@tr>
  <@tr>
      <@td>Status</@td>
      <@td>${(finAccountStatusItem.description)!"Active"}</@td>
  </@tr>
  <#if ownedFinAccount.replenishLevel??>
  <@tr>
      <@td>Replenish Level</@td>
      <@td>${ownedFinAccount.replenishLevel}</@td>
  </@tr>
  </#if>
</@table>

<@table type="data-list">
<@thead>
  <@tr>
    <@th>Transaction ${uiLabelMap.CommonDate}</@th>
    <@th>ID</@th>
    <@th>Order Item</@th>
    <@th>Payment</@th>
    <@th>Type</@th>
    <@th>Amount</@th>
  </@tr>
</@thead>
<@tbody>
  <#list ownedFinAccountTransList as ownedFinAccountTrans>
    <#assign finAccountTransType = ownedFinAccountTrans.getRelatedOne("FinAccountTransType", false)/>
    <#assign displayAmount = ownedFinAccountTrans.amount/>
    <#if ownedFinAccountTrans.finAccountTransTypeId == "WITHDRAWAL">
      <#assign displayAmount = -displayAmount/>
    </#if>
  <@tr>
    <@td>${ownedFinAccountTrans.transactionDate!}</@td>
    <@td>${ownedFinAccountTrans.finAccountTransId}</@td>
    <@td><#if ownedFinAccountTrans.orderId?has_content>${ownedFinAccountTrans.orderId!}:${ownedFinAccountTrans.orderItemSeqId!}<#else>&nbsp;</#if></@td>
    <@td><#if ownedFinAccountTrans.paymentId?has_content>${ownedFinAccountTrans.paymentId!}<#else>&nbsp;</#if></@td>
    <@td>${finAccountTransType.description?default(ownedFinAccountTrans.finAccountTransTypeId)!}</@td>
    <@td><@ofbizCurrency amount=displayAmount isoCode=ownedFinAccount.currencyUomId/></@td>
  </@tr>
  </#list>
</@tbody>
<@tfoot>
  <@tr>
    <@th>Actual Balance</@th>
    <@td>&nbsp;</@td>
    <@td>&nbsp;</@td>
    <@th><@ofbizCurrency amount=ownedFinAccount.actualBalance isoCode=ownedFinAccount.currencyUomId/></@th>
  </@tr>
</@tfoot>
</@table>

<#if ownedFinAccountAuthList?has_content>
<@table type="data-list">
<@thead>
  <@tr>
    <@th>Authorization ${uiLabelMap.CommonDate}</@th>
    <@th>ID</@th>
    <@th>Expires</@th>
    <@th>Amount</@th>
  </@tr>
</@thead>
<@tbody>
  <@tr>
    <@td>Actual Balance</@td>
    <@td>&nbsp;</@td>
    <@td>&nbsp;</@td>
    <@td><@ofbizCurrency amount=ownedFinAccount.actualBalance isoCode=ownedFinAccount.currencyUomId/></@td>
  </@tr>
  <#list ownedFinAccountAuthList as ownedFinAccountAuth>
  <@tr>
    <@td>${ownedFinAccountAuth.authorizationDate!}</@td>
    <@td>${ownedFinAccountAuth.finAccountAuthId}</@td>
    <@td>${ownedFinAccountAuth.thruDate!}</@td>
    <@td><@ofbizCurrency amount=-ownedFinAccountAuth.amount isoCode=ownedFinAccount.currencyUomId/></@td>
  </@tr>
  </#list>
</@tbody>
<@tfoot>
  <@tr>
    <@th>Available Balance</@th>
    <@td>&nbsp;</@td>
    <@td>&nbsp;</@td>
    <@th><@ofbizCurrency amount=ownedFinAccount.availableBalance isoCode=ownedFinAccount.currencyUomId/></@th>
  </@tr>
</@tfoot>
</@table>
</#if>
