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
          <#assign invoices = delegator.findByAnd("OrderItemBilling", {"orderId":raw(orderHeader.orderId)}, UtilMisc.toList("invoiceId"), false) />
          <#assign distinctInvoiceIds = Static["org.ofbiz.entity.util.EntityUtil"].getFieldListFromEntityList(invoices, "invoiceId", true)>
          <@td>
            <#-- SCIPIO: NOTE: There is more than one kind of invoice, the PDF accessible upon creation, and additional invoices
                created upon order completion. Just show it all for now (final invoice may have more information). -->
            <a href="<@pageUrl>order.pdf?orderId=${orderHeader.orderId}</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_export!}">${orderHeader.orderId} (${uiLabelMap.CommonPdf})</a>
            <#if distinctInvoiceIds?has_content>
              <#list distinctInvoiceIds as invoiceId>
                <a href="<@pageUrl>invoice.pdf?invoiceId=${invoiceId}</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_export!}">${invoiceId} (${uiLabelMap.CommonPdf})</a>
              </#list>
            </#if>
          </@td>
          <@td><a href="<@pageUrl>orderstatus?orderId=${orderHeader.orderId}</@pageUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a></@td>
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
              <@td><a href="<@pageUrl>orderstatus?orderId=${porderHeader.orderId}</@pageUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a></@td>
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
      <@menuitem type="link" href=makePageUrl("orderdownloads") class="+${styles.action_nav!} ${styles.action_export!}" text=uiLabelMap.EcommerceViewAll />
    </@menu>
  </#macro>
  <@section title=sectionTitle menuContent=menuContent menuLayoutGeneral="bottom">
    <#-- SCIPIO: NOTE: Here we currently render the full widget. 
        Alternatively, we could show a smaller summary here and leave full details to the dedicated page. -->
    <@render resource="component://shop/widget/OrderScreens.xml#orderdownloadscontent" />
  </@section>
<#--
</#if>-->
