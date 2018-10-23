<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@table type="fields"> <#-- orig: border="0" cellpadding="1" -->
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

<@table type="data-list"> <#-- orig: border="0" cellpadding="1" border="2" -->
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
<@table type="data-list"> <#-- orig: border="0" cellpadding="1" -->
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
