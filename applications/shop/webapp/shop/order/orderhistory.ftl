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
<#include "ordercommon.ftl">

<@section title=uiLabelMap.OrderSalesHistory>
  <#if orderHeaderList?has_content>
    <@table type="data-list" id="orderSalesHistory" summary="This table display order sales history.">
      <@thead>
        <@tr>
          <@th>${uiLabelMap.CommonDate}</@th>
          <@th>${uiLabelMap.OrderOrder} ${uiLabelMap.CommonNbr}</@th>
          <@th>${uiLabelMap.CommonAmount}</@th>
          <@th>${uiLabelMap.CommonStatus}</@th>
          <@th>${uiLabelMap.OrderInvoices}</@th>
          <@th></@th>
        </@tr>
      </@thead>
      <@tbody>
      <#list orderHeaderList as orderHeader>
        <#assign status = orderHeader.getRelatedOne("StatusItem", true) />
        <@tr>
          <@td>${orderHeader.orderDate.toString()}</@td>
          <@td>${orderHeader.orderId}</@td>
          <@td><@ofbizCurrency amount=orderHeader.grandTotal isoCode=orderHeader.currencyUom /></@td>
          <@td>${status.get("description",locale)}</@td>
          <#-- invoices -->
          <#assign invoices = delegator.findByAnd("OrderItemBilling", {"orderId":"${orderHeader.orderId}"}, Static["org.ofbiz.base.util.UtilMisc"].toList("invoiceId"), false) />
          <#assign distinctInvoiceIds = Static["org.ofbiz.entity.util.EntityUtil"].getFieldListFromEntityList(invoices, "invoiceId", true)>
          <#if distinctInvoiceIds?has_content>
            <@td>
              <#list distinctInvoiceIds as invoiceId>
                 <a href="<@ofbizUrl>invoice.pdf?invoiceId=${invoiceId}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_export!}">(${invoiceId} PDF) </a>
              </#list>
            </@td>
          <#else>
            <@td></@td>
          </#if>
          <@td><a href="<@ofbizUrl>orderstatus?orderId=${orderHeader.orderId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a></@td>
        </@tr>
      </#list>
      </@tbody>
    </@table>
  <#else>
    <@commonMsg type="result-norecord">(${uiLabelMap.OrderNoOrderFound})</@commonMsg>
  </#if>
</@section>

<@section title=uiLabelMap.OrderPurchaseHistory>
  <#if porderHeaderList?has_content>
    <@table type="data-list" id="orderPurchaseHistory" summary="This table display order purchase history.">
      <@thead>
        <@tr>
          <@th>${uiLabelMap.CommonDate}</@th>
          <@th>${uiLabelMap.OrderOrder} ${uiLabelMap.CommonNbr}</@th>
          <@th>${uiLabelMap.CommonAmount}</@th>
          <@th>${uiLabelMap.CommonStatus}</@th>
          <@th></@th>
        </@tr>
      </@thead>
      <@tbody>
          <#list porderHeaderList as porderHeader>
            <#assign pstatus = porderHeader.getRelatedOne("StatusItem", true) />
            <@tr>
              <@td>${porderHeader.orderDate.toString()}</@td>
              <@td>${porderHeader.orderId}</@td>
              <@td><@ofbizCurrency amount=porderHeader.grandTotal isoCode=porderHeader.currencyUom /></@td>
              <@td>${pstatus.get("description",locale)}</@td>
              <@td><a href="<@ofbizUrl>orderstatus?orderId=${porderHeader.orderId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a></@td>
            </@tr>
          </#list>
      </@tbody>
    </@table>
  <#else>
    <@commonMsg type="result-norecord">(${uiLabelMap.OrderNoOrderFound})</@commonMsg>
  </#if>
</@section>

<#assign sectionTitle><a href="<@ofbizUrl uri="orderdownloads"/>" class="${styles.link_nav_info!}">${uiLabelMap.EcommerceDownloadsAvailableTitle}</a></#assign>
<@section title=sectionTitle>
    <@render resource="component://shop/widget/OrderScreens.xml#orderdownloadswidget" />
</@section>
