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

<#if security.hasEntityPermission("ORDERMGR", "_VIEW", session)>
  <@section>
    <@table type="summary"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
        <@thead>
          <@tr>
            <@th>&nbsp;</@th>
            <@th>&nbsp;</@th>
            <@th scope="column">${uiLabelMap.CommonToday}</@th>
            <@th scope="column">${uiLabelMap.OrderWTD}</@th>
            <@th scope="column">${uiLabelMap.OrderMTD}</@th>
            <@th scope="column">${uiLabelMap.OrderYTD}</@th>
          </@tr>
        </@thead>
        <@tbody>
          <@tr>
            <@td scope="row">${uiLabelMap.OrderOrdersTotals}</@td>
            <@td scope="row">${uiLabelMap.OrderGrossDollarAmountsIncludesAdjustmentsAndPendingOrders}</@td>
            <@td align="right">${dayItemTotal}</@td>
            <@td align="right">${weekItemTotal}</@td>
            <@td align="right">${monthItemTotal}</@td>
            <@td align="right">${yearItemTotal}</@td>
          </@tr>
          <@tr>
            <@td scope="row">&nbsp;</@td>
            <@td scope="row">${uiLabelMap.OrderPaidDollarAmountsIncludesAdjustments}</@td>
            <@td align="right">${dayItemTotalPaid}</@td>
            <@td align="right">${weekItemTotalPaid}</@td>
            <@td align="right">${monthItemTotalPaid}</@td>
            <@td align="right">${yearItemTotalPaid}</@td>
          </@tr>
          <@tr>
            <@td scope="row">&nbsp;</@td>
            <@td scope="row">${uiLabelMap.OrderPendingPaymentDollarAmountsIncludesAdjustments}</@td>
            <@td align="right">${dayItemTotalPending}</@td>
            <@td align="right">${weekItemTotalPending}</@td>
            <@td align="right">${monthItemTotalPending}</@td>
            <@td align="right">${yearItemTotalPending}</@td>
          </@tr>
          <@tr>
            <@td scope="row">${uiLabelMap.OrderOrdersItemCounts}</@td>
            <@td scope="row">${uiLabelMap.OrderGrossItemsSoldIncludesPromotionsAndPendingOrders}</@td>
            <@td align="right">${dayItemCount?string.number}</@td>
            <@td align="right">${weekItemCount?string.number}</@td>
            <@td align="right">${monthItemCount?string.number}</@td>
            <@td align="right">${yearItemCount?string.number}</@td>
          </@tr>
          <@tr>
            <@td scope="row">&nbsp;</@td>
            <@td scope="row">${uiLabelMap.OrderPaidItemsSoldIncludesPromotions}</@td>
            <@td align="right">${dayItemCountPaid?string.number}</@td>
            <@td align="right">${weekItemCountPaid?string.number}</@td>
            <@td align="right">${monthItemCountPaid?string.number}</@td>
            <@td align="right">${yearItemCountPaid?string.number}</@td>
          </@tr>
          <@tr>
            <@td scope="row">&nbsp;</@td>
            <@td scope="row">${uiLabelMap.OrderPendingPaymentItemsSoldIncludesPromotions}</@td>
            <@td align="right">${dayItemCountPending?string.number}</@td>
            <@td align="right">${weekItemCountPending?string.number}</@td>
            <@td align="right">${monthItemCountPending?string.number}</@td>
            <@td align="right">${yearItemCountPending?string.number}</@td>
          </@tr>
          
          <@tr>
            <@td scope="row">${uiLabelMap.OrderOrdersPending}</@td>
            <@td scope="row">${uiLabelMap.OrderWaitingPayment}</@td>
            <@td align="right">${waitingPayment?default(0)?string.number}</@td>
            <@td align="right">--</@td>
            <@td align="right">--</@td>
            <@td align="right">--</@td>
          </@tr>
          <@tr>
            <@td scope="row">&nbsp;</@td>
            <@td scope="row">${uiLabelMap.OrderWaitingApproval}</@td>
            <@td align="right">${waitingApproval?default(0)?string.number}</@td>
            <@td align="right">--</@td>
            <@td align="right">--</@td>
            <@td align="right">--</@td>
          </@tr>
          <@tr>
            <@td scope="row">&nbsp;</@td>
            <@td scope="row">${uiLabelMap.OrderWaitingCompletion}</@td>
            <@td align="right">${waitingComplete?default(0)?string.number}</@td>
            <@td align="right">--</@td>
            <@td align="right">--</@td>
            <@td align="right">--</@td>
          </@tr>
          <@tr>
            <@td scope="row">${uiLabelMap.OrderStatusChanges}</@td>
            <@td scope="row">${uiLabelMap.OrderCreated}</@td>
            <@td align="right">${dayOrder?size?default(0)?string.number}</@td>
            <@td align="right">${weekOrder?size?default(0)?string.number}</@td>
            <@td align="right">${monthOrder?size?default(0)?string.number}</@td>
            <@td align="right">${yearOrder?size?default(0)?string.number}</@td>
          </@tr>
          <@tr>
            <@td scope="row">&nbsp;</@td>
            <@td scope="row">${uiLabelMap.OrderApproved}</@td>
            <@td align="right">${dayApprove?size?default(0)?string.number}</@td>
            <@td align="right">${weekApprove?size?default(0)?string.number}</@td>
            <@td align="right">${monthApprove?size?default(0)?string.number}</@td>
            <@td align="right">${yearApprove?size?default(0)?string.number}</@td>
          </@tr>
          <@tr>
            <@td scope="row">&nbsp;</@td>
            <@td scope="row">${uiLabelMap.OrderCompleted}</@td>
            <@td align="right">${dayComplete?size?default(0)?string.number}</@td>
            <@td align="right">${weekComplete?size?default(0)?string.number}</@td>
            <@td align="right">${monthComplete?size?default(0)?string.number}</@td>
            <@td align="right">${yearComplete?size?default(0)?string.number}</@td>
          </@tr>
          <@tr>
            <@td scope="row">&nbsp;</@td>
            <@td scope="row">${uiLabelMap.OrderCancelled}</@td>
            <@td align="right">${dayCancelled?size?default(0)?string.number}</@td>
            <@td align="right">${weekCancelled?size?default(0)?string.number}</@td>
            <@td align="right">${monthCancelled?size?default(0)?string.number}</@td>
            <@td align="right">${yearCancelled?size?default(0)?string.number}</@td>
          </@tr>
          <@tr>
            <@td scope="row">&nbsp;</@td>
            <@td scope="row">${uiLabelMap.OrderRejected}</@td>
            <@td align="right">${dayRejected?size?default(0)?string.number}</@td>
            <@td align="right">${weekRejected?size?default(0)?string.number}</@td>
            <@td align="right">${monthRejected?size?default(0)?string.number}</@td>
            <@td align="right">${yearRejected?size?default(0)?string.number}</@td>
          </@tr>
        </@tbody>
    </@table>
  </@section>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>
