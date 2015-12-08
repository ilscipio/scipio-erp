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

<@script>
    var checkBoxNameStart = "view";
    var formName = "findorder";


    function setCheckboxes() {
        // This would be clearer with camelCase variable names
        var allCheckbox = document.forms[formName].elements[checkBoxNameStart + "all"];
        for(i = 0;i < document.forms[formName].elements.length;i++) {
            var elem = document.forms[formName].elements[i];
            if (elem.name.indexOf(checkBoxNameStart) == 0 && elem.name.indexOf("_") < 0 && elem.type == "checkbox") {
                elem.checked = allCheckbox.checked;
            }
        }
    }

</@script>

<#-- order list -->
<@section>
      <form method="post" name="findorder" action="<@ofbizUrl>orderlist</@ofbizUrl>">
        <input type="hidden" name="changeStatusAndTypeState" value="Y" />
       <@row>
        <@cell>
          <@row class="+prefix-radius">
            <@cell columns=3>
              ${uiLabelMap.CommonStatus}
            </@cell>
            <@cell columns=9>
              <span class="${styles.text_left!}">
                <input type="checkbox" name="viewall" value="Y" onclick="javascript:setCheckboxes()" <#if state.hasAllStatus()>checked="checked"</#if> /><label>${uiLabelMap.CommonAll}</label>
                <input type="checkbox" name="viewcreated" value="Y" <#if state.hasStatus('viewcreated')>checked="checked"</#if> /><label>${uiLabelMap.CommonCreated}</label>
                <input type="checkbox" name="viewprocessing" value="Y" <#if state.hasStatus('viewprocessing')>checked="checked"</#if> /><label>${uiLabelMap.CommonProcessing}</label>
                <input type="checkbox" name="viewapproved" value="Y" <#if state.hasStatus('viewapproved')>checked="checked"</#if> /><label>${uiLabelMap.CommonApproved}</label>
                <input type="checkbox" name="viewhold" value="Y" <#if state.hasStatus('viewhold')>checked="checked"</#if> /><label>${uiLabelMap.CommonHeld}</label>
                <input type="checkbox" name="viewcompleted" value="Y" <#if state.hasStatus('viewcompleted')>checked="checked"</#if> /><label>${uiLabelMap.CommonCompleted}
                <#--input type="checkbox" name="viewsent" value="Y" <#if state.hasStatus('viewsent')>checked="checked"</#if> /><label>${uiLabelMap.CommonSent}</label>-->
                <input type="checkbox" name="viewrejected" value="Y" <#if state.hasStatus('viewrejected')>checked="checked"</#if> /><label>${uiLabelMap.CommonRejected}</label>
                <input type="checkbox" name="viewcancelled" value="Y" <#if state.hasStatus('viewcancelled')>checked="checked"</#if> /><label>${uiLabelMap.CommonCancelled}</label>
              </span>
            </@cell>
          </@row>
        </@cell>
      </@row>
      <@row>
        <@cell>
          <@row class="+prefix-radius">
            <@cell columns=3>
              <span class="">${uiLabelMap.CommonType}</span>
            </@cell>
            <@cell columns=9>
                <span class="${styles.text_left!}">
                  <input type="checkbox" name="view_SALES_ORDER" value="Y" <#if state.hasType('view_SALES_ORDER')>checked="checked"</#if>/>
                  <label>${descr_SALES_ORDER}</label>
                  <input type="checkbox" name="view_PURCHASE_ORDER" value="Y" <#if state.hasType('view_PURCHASE_ORDER')>checked="checked"</#if>/>
                  <label>${descr_PURCHASE_ORDER}</label>
                </span>
            </@cell>
          </@row>
        </@cell>
       </@row>
       <@row>
        <@cell>
          <@row class="+prefix-radius">
            <@cell columns=3>
              <span class="">${uiLabelMap.CommonFilter}</span>
            </@cell>
            <@cell columns=9>
              <span class="${styles.text_left!}">
                <input type="checkbox" name="filterInventoryProblems" value="Y"
                    <#if state.hasFilter('filterInventoryProblems')>checked="checked"</#if>/>
                <label>${uiLabelMap.OrderFilterInventoryProblems}</label>
                <input type="checkbox" name="filterAuthProblems" value="Y"
                    <#if state.hasFilter('filterAuthProblems')>checked="checked"</#if>/>
                <label>${uiLabelMap.OrderFilterAuthProblems}</label>
              </span>
            </@cell>
          </@row>
        </@cell>
       </@row>
       <@row>
        <@cell>
          <@row class="+prefix-radius">
            <@cell columns=3>
              <span class="">${uiLabelMap.CommonFilter} (${uiLabelMap.OrderFilterPOs})</span>
            </@cell>
            <@cell columns=9>
                <span class="${styles.text_left!}">
                    <input type="checkbox" name="filterPartiallyReceivedPOs" value="Y"
                        <#if state.hasFilter('filterPartiallyReceivedPOs')>checked="checked"</#if>/>
                    <label>${uiLabelMap.OrderFilterPartiallyReceivedPOs}</label>
                    <input type="checkbox" name="filterPOsOpenPastTheirETA" value="Y"
                        <#if state.hasFilter('filterPOsOpenPastTheirETA')>checked="checked"</#if>/>
                    <label>${uiLabelMap.OrderFilterPOsOpenPastTheirETA}</label>
                    <input type="checkbox" name="filterPOsWithRejectedItems" value="Y"
                        <#if state.hasFilter('filterPOsWithRejectedItems')>checked="checked"</#if>/>
                    <label>${uiLabelMap.OrderFilterPOsWithRejectedItems}</label>
                 </span>
              </@cell>
            </@row>
         </@cell>
       </@row>
       <@row>
        <@cell>
          <@row class="+prefix-radius">
            <@cell>
              <input type="submit" value="${uiLabelMap.CommonFind}" class="${styles.link_action!}"/>
            </@cell>
          </@row>
        </@cell>
       </@row>
      </form>
</@section>
 

<#if hasPermission>
  <@section title="${uiLabelMap.OrderOrderList}" id="findOrderList">
        <#assign url><@ofbizUrl>orderlist</@ofbizUrl></#assign>
        
        <#macro paginateOrders>
          <@paginate url=url viewSize=state.getViewSize() viewIndex=state.getViewIndex() listSize=state.getSize() altParam=true/>
        </#macro>
        <#assign paginated = true>
        
        <#if paginated>
          <@paginateOrders />
        </#if>
        
        <@table type="data-list" autoAltRows=true cellspacing="0">
          <@thead>
          <@tr>
            <@th>${uiLabelMap.OrderOrder} ${uiLabelMap.CommonNbr}</@th>
            <@th>${uiLabelMap.CommonDate}</@th>
            <#--<@th>${uiLabelMap.OrderOrderName}</@th>-->
            <#--<@th>${uiLabelMap.OrderOrderType}</@th>-->
            <#--<@th>${uiLabelMap.OrderOrderBillFromParty}</@th>-->
            <@th>${uiLabelMap.OrderOrderBillToParty}</@th>
            <@th>${uiLabelMap.OrderProductStore}</@th>
            <@th>${uiLabelMap.CommonAmount}</@th>
            <#if state.hasFilter('filterInventoryProblems') || state.hasFilter('filterAuthProblems') || state.hasFilter('filterPOsOpenPastTheirETA') || state.hasFilter('filterPOsWithRejectedItems') || state.hasFilter('filterPartiallyReceivedPOs')>
                <@th>${uiLabelMap.CommonStatus}</@th>
                <@th>${uiLabelMap.CommonFilter}</@th>
            <#else>
                <@th>${uiLabelMap.CommonStatus}</@th>
            </#if>
            <@th>${uiLabelMap.OrderTrackingCode}</@th>
          </@tr>
          </@thead>
          <#list orderHeaderList as orderHeader>
            <#assign status = orderHeader.getRelatedOne("StatusItem", true)>
            <#assign orh = Static["org.ofbiz.order.order.OrderReadHelper"].getHelper(orderHeader)>
            <#assign billToParty = orh.getBillToParty()!>
            <#assign billFromParty = orh.getBillFromParty()!>
            <#if billToParty?has_content>
                <#assign billToPartyNameResult = dispatcher.runSync("getPartyNameForDate", Static["org.ofbiz.base.util.UtilMisc"].toMap("partyId", billToParty.partyId, "compareDate", orderHeader.orderDate, "userLogin", userLogin))/>
                <#assign billTo = billToPartyNameResult.fullName?default("[${uiLabelMap.OrderPartyNameNotFound}]")/>
                <#-- <#assign billTo = Static["org.ofbiz.party.party.PartyHelper"].getPartyName(billToParty, true)!> -->
            <#else>
              <#assign billTo = ''/>
            </#if>
            <#if billFromParty?has_content>
              <#assign billFrom = Static["org.ofbiz.party.party.PartyHelper"].getPartyName(billFromParty, true)!>
            <#else>
              <#assign billFrom = ''/>
            </#if>
            <#assign productStore = orderHeader.getRelatedOne("ProductStore", true)! />
            <@tr>
              <@td>
                <a href="<@ofbizUrl>orderview?orderId=${orderHeader.orderId}</@ofbizUrl>">${orderHeader.orderId}</a>
              </@td>
              <@td><#if orderHeader.orderDate?has_content>${Static["org.ofbiz.base.util.UtilFormatOut"].formatDateTime(orderHeader.orderDate, "", locale, timeZone)!}</#if></@td>
              <#--<@td>${orderHeader.orderName!}</@td>-->
              <#--<@td>${orderHeader.getRelatedOne("OrderType", true).get("description",locale)}</@td>-->
              <#--<@td>${billFrom!}</@td>-->
              <@td>${billTo!}</@td>
              <@td><#if productStore?has_content>${productStore.storeName?default(productStore.productStoreId)}</#if></@td>
              <@td><@ofbizCurrency amount=orderHeader.grandTotal isoCode=orderHeader.currencyUom/></@td>
              <@td>${orderHeader.getRelatedOne("StatusItem", true).get("description",locale)}</@td>
              <@td>
                <#assign trackingCodes = orderHeader.getRelated("TrackingCodeOrder", null, null, false)>
                <#list trackingCodes as trackingCode>
                    <#if trackingCode?has_content>
                        <a href="/marketing/control/FindTrackingCodeOrders?trackingCodeId=${trackingCode.trackingCodeId}&amp;externalLoginKey=${requestAttributes.externalLoginKey!}">${trackingCode.trackingCodeId}</a><br />
                    </#if>
                </#list>
              </@td>
              <#if state.hasFilter('filterInventoryProblems') || state.hasFilter('filterAuthProblems') || state.hasFilter('filterPOsOpenPastTheirETA') || state.hasFilter('filterPOsWithRejectedItems') || state.hasFilter('filterPartiallyReceivedPOs')>
              <@td>
                  <#if filterInventoryProblems.contains(orderHeader.orderId)>
                    Inv&nbsp;
                  </#if>
                  <#if filterAuthProblems.contains(orderHeader.orderId)>
                   Aut&nbsp;
                  </#if>
                  <#if filterPOsOpenPastTheirETA.contains(orderHeader.orderId)>
                    ETA&nbsp;
                  </#if>
                  <#if filterPOsWithRejectedItems.contains(orderHeader.orderId)>
                    Rej&nbsp;
                  </#if>
                  <#if filterPartiallyReceivedPOs.contains(orderHeader.orderId)>
                    Part&nbsp;
                  </#if>
              </@td>
              <#else>
              </#if>
            </@tr>
          </#list>
          <#if !orderHeaderList?has_content>
            <@tr type="meta"><@td colspan="9"><@resultMsg>${uiLabelMap.OrderNoOrderFound}</@resultMsg></@td></@tr>
          </#if>
        </@table>
        
        <#if paginated>
          <@paginateOrders />
        </#if>
        
  </@section>
<#else>
  <@alert type="error">${uiLabelMap.OrderViewPermissionError}</@alert>
</#if>
