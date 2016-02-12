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

<@section title="${uiLabelMap.OrderOrderReceivedOn} ${Static[\"org.ofbiz.base.util.UtilDateTime\"].toDateString(filterDate)}">
      <#assign listSize = state.getSize()>
      <#if (listSize > 10)>
        <a href="/ordermgr/control/orderlist?viewIndex=${state.getViewIndex() + 1}&amp;viewSize=${state.getViewSize()}&amp;filterDate=${filterDate!}">${uiLabelMap.CommonMore}</a>
      </#if>

    <#if orderHeaderList?has_content>
      <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
        <@thead>
        <@tr class="header-row">
          <@th width="10%">${uiLabelMap.OrderOrder} ${uiLabelMap.CommonNbr}</@th>
          <@th width="15%">${uiLabelMap.OrderOrderBillToParty}</@th>
          <@th width="25%">${uiLabelMap.OrderProductStore}</@th>
          <@th width="10%">${uiLabelMap.CommonAmount}</@th>
          <@th width="20%">${uiLabelMap.OrderTrackingCode}</@th>
          <@th width="20%">${uiLabelMap.CommonStatus}</@th>
        </@tr>
        </@thead>
        <#list orderHeaderList as orderHeader>
          <#assign status = orderHeader.getRelatedOne("StatusItem", true)>
          <#assign orh = Static["org.ofbiz.order.order.OrderReadHelper"].getHelper(orderHeader)>
          <#assign billToParty = orh.getBillToParty()!>
          <#if billToParty?has_content>
            <#assign billToPartyNameResult = dispatcher.runSync("getPartyNameForDate", Static["org.ofbiz.base.util.UtilMisc"].toMap("partyId", billToParty.partyId, "compareDate", orderHeader.orderDate, "userLogin", userLogin))/>
            <#assign billTo = billToPartyNameResult.fullName?default("[${uiLabelMap.OrderPartyNameNotFound}]")/>
          </#if>
          <#assign productStore = orderHeader.getRelatedOne("ProductStore", true)! />
          <@tr>
            <@td><a href="/ordermgr/control/orderview?orderId=${orderHeader.orderId}" class="${styles.link_nav_info_id!}">${orderHeader.orderId}</a></@td>
            <@td>${billTo!}</@td>
            <@td><#if productStore?has_content>${productStore.storeName?default(productStore.productStoreId)}</#if></@td>
            <@td><@ofbizCurrency amount=orderHeader.grandTotal isoCode=orderHeader.currencyUom/></@td>
            <@td>
              <#assign trackingCodes = orderHeader.getRelated("TrackingCodeOrder", null, null, false)>
              <#list trackingCodes as trackingCode>
                <#if trackingCode?has_content>
                  <a href="/marketing/control/FindTrackingCodeOrders?trackingCodeId=${trackingCode.trackingCodeId}&amp;externalLoginKey=${requestAttributes.externalLoginKey!}">${trackingCode.trackingCodeId}</a><br />
                </#if>
              </#list>
            </@td>
            <@td>${orderHeader.getRelatedOne("StatusItem", true).get("description",locale)}</@td>
          </@tr>
        </#list>
      </@table>
      <@row>
        <@cell class="+${styles.text_right}">
          <span>1-${orderHeaderList.size()} ${uiLabelMap.CommonOf} ${state.getSize()}</span>
        </@cell>
      </@row>
    <#else>
      <@commonMsg type="result-norecord">${uiLabelMap.OrderNoOrderFound}.</@commonMsg>
    </#if>
  </@section>
