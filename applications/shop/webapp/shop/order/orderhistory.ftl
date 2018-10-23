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
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

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
          <@td>
            <#-- SCIPIO: NOTE: There is more than one kind of invoice, the PDF accessible upon creation, and additional invoices
                created upon order completion. Just show it all for now (final invoice may have more information). -->
            <a href="<@ofbizUrl>order.pdf?orderId=${orderHeader.orderId}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_export!}">${orderHeader.orderId} (${uiLabelMap.CommonPdf})</a>
            <#if distinctInvoiceIds?has_content>
              <#list distinctInvoiceIds as invoiceId>
                <a href="<@ofbizUrl>invoice.pdf?invoiceId=${invoiceId}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_export!}">${invoiceId} (${uiLabelMap.CommonPdf})</a>
              </#list>
            </#if>
          </@td>
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

<#-- show it for now due to the order completion notice
<#if hasOrderDownloads>-->
  <#assign sectionTitle = uiLabelMap.EcommerceDownloadsAvailableTitle/>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href=makeOfbizUrl("orderdownloads") class="+${styles.action_nav!} ${styles.action_export!}" text=uiLabelMap.EcommerceViewAll />
    </@menu>
  </#macro>
  <@section title=sectionTitle menuContent=menuContent menuLayoutGeneral="bottom">
    <#-- SCIPIO: NOTE: Here we currently render the full widget. 
        Alternatively, we could show a smaller summary here and leave full details to the dedicated page. -->
    <@render resource="component://shop/widget/OrderScreens.xml#orderdownloadscontent" />
  </@section>
<#--
</#if>-->
