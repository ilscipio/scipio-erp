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
      <#-- Scipio: Use alt/simple checkboxes, currently implied by default-alt1
        FIXME: here, manually override checkboxType for now to preserve the old look (because default-alt1 is slightly non-standard) but really it should be left to styles hash! 
            Needs to be sorted out globally... -->
      <@fields type="default-alt1" checkboxType="simple-standard"> 
        <input type="hidden" name="changeStatusAndTypeState" value="Y" />
        <@field type="generic" label=uiLabelMap.CommonStatus>
            <@field type="checkbox" name="viewall" value="Y" onClick="javascript:setCheckboxes()" checked=(state.hasAllStatus()) label=uiLabelMap.CommonAll/>
            <@field type="checkbox" name="viewcreated" value="Y" checked=(state.hasStatus('viewcreated')) label=uiLabelMap.CommonCreated/>
            <@field type="checkbox" name="viewprocessing" value="Y" checked=(state.hasStatus('viewprocessing')) label=uiLabelMap.CommonProcessing/>
            <@field type="checkbox" name="viewapproved" value="Y" checked=(state.hasStatus('viewapproved')) label=uiLabelMap.CommonApproved/>
            <@field type="checkbox" name="viewhold" value="Y" checked=(state.hasStatus('viewhold')) label=uiLabelMap.CommonHeld/>
            <@field type="checkbox" name="viewcompleted" value="Y" checked=(state.hasStatus('viewcompleted')) label=uiLabelMap.CommonCompleted/>
            <#--@field type="checkbox" name="viewsent" value="Y" checked=(state.hasStatus('viewsent')) label=uiLabelMap.CommonSent/>-->
            <@field type="checkbox" name="viewrejected" value="Y" checked=(state.hasStatus('viewrejected')) label=uiLabelMap.CommonRejected/>
            <@field type="checkbox" name="viewcancelled" value="Y" checked=(state.hasStatus('viewcancelled')) label=uiLabelMap.CommonCancelled/>
        </@field>
        <@field type="generic" label=uiLabelMap.CommonType>
            <@field type="checkbox" name="view_SALES_ORDER" value="Y" checked=(state.hasType('view_SALES_ORDER')) label=(descr_SALES_ORDER)/>
            <@field type="checkbox" name="view_PURCHASE_ORDER" value="Y" checked=(state.hasType('view_PURCHASE_ORDER')) label=(descr_PURCHASE_ORDER)/>
        </@field>
        <@field type="generic" label=uiLabelMap.CommonFilter>
            <@field type="checkbox" name="filterInventoryProblems" value="Y" checked=(state.hasFilter('filterInventoryProblems')) label=uiLabelMap.OrderFilterInventoryProblems/>
            <@field type="checkbox" name="filterAuthProblems" value="Y" checked=(state.hasFilter('filterAuthProblems')) label=uiLabelMap.OrderFilterAuthProblems/>
        </@field>
        <@field type="generic" label="${rawLabel('CommonFilter')} (${rawLabel('OrderFilterPOs')})">
            <@field type="checkbox" name="filterPartiallyReceivedPOs" value="Y" checked=(state.hasFilter('filterPartiallyReceivedPOs')) label=uiLabelMap.OrderFilterPartiallyReceivedPOs/>
            <@field type="checkbox" name="filterPOsOpenPastTheirETA" value="Y" checked=(state.hasFilter('filterPOsOpenPastTheirETA')) label=uiLabelMap.OrderFilterPOsOpenPastTheirETA/>
            <@field type="checkbox" name="filterPOsWithRejectedItems" value="Y" checked=(state.hasFilter('filterPOsWithRejectedItems')) label=uiLabelMap.OrderFilterPOsWithRejectedItems/>
        </@field>

        <@field type="submit" text=uiLabelMap.CommonFind class="${styles.link_run_sys!} ${styles.action_find!}"/>
      </@fields>
      </form>
</@section>
 

<#if hasPermission>
  <@section title=uiLabelMap.OrderOrderList id="findOrderList">        
      <@paginate mode="content" url=makeOfbizUrl("orderlist") viewSize=state.getViewSize() viewIndex=state.getViewIndex() listSize=state.getSize() altParam=true>
        <@table type="data-list" autoAltRows=true> <#-- orig: cellspacing="0" -->
          <@thead>
          <@tr>
            <@th width="10%">${uiLabelMap.OrderOrder} ${uiLabelMap.CommonNbr}</@th>
            <@th width="15%">${uiLabelMap.CommonDate}</@th>
            <#--<@th width="10%">${uiLabelMap.OrderOrderName}</@th>-->
            <#--<@th width="10%">${uiLabelMap.OrderOrderType}</@th>-->
            <#--<@th width="10%">${uiLabelMap.OrderOrderBillFromParty}</@th>-->
            <@th width="25%">${uiLabelMap.OrderOrderBillToParty}</@th>
            <@th width="20%">${uiLabelMap.OrderProductStore}</@th>
            <@th width="8%">${uiLabelMap.CommonAmount}</@th>
            <#if state.hasFilter('filterInventoryProblems') || state.hasFilter('filterAuthProblems') || state.hasFilter('filterPOsOpenPastTheirETA') || state.hasFilter('filterPOsWithRejectedItems') || state.hasFilter('filterPartiallyReceivedPOs')>
                <@th width="10%">${uiLabelMap.CommonStatus}</@th>
                <@th width="5%">${uiLabelMap.CommonFilter}</@th>
            <#else>
                <@th width="15%">${uiLabelMap.CommonStatus}</@th>
            </#if>
            <@th width="7%">${uiLabelMap.OrderTrackingCode}</@th>
          </@tr>
          </@thead>
          <#list orderHeaderList as orderHeader>
            <#assign status = orderHeader.getRelatedOne("StatusItem", true)>
            <#assign orh = Static["org.ofbiz.order.order.OrderReadHelper"].getHelper(orderHeader)>
            <#assign billToParty = orh.getBillToParty()!>
            <#assign billFromParty = orh.getBillFromParty()!>
            <#if billToParty?has_content>
                <#assign billToPartyNameResult = dispatcher.runSync("getPartyNameForDate", {"partyId":billToParty.partyId, "compareDate":orderHeader.orderDate, "userLogin":userLogin})/>
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
              <@td><#if orderHeader.orderDate?has_content><@formattedDateTime date=orderHeader.orderDate /></#if></@td>
              <#--<@td>${orderHeader.orderName!}</@td>-->
              <#--<@td>${orderHeader.getRelatedOne("OrderType", true).get("description",locale)}</@td>-->
              <#--<@td>${billFrom!}</@td>-->
              <@td>${billTo!}</@td>
              <@td><#if productStore?has_content>${productStore.storeName!productStore.productStoreId}</#if></@td>
              <@td><@ofbizCurrency amount=orderHeader.grandTotal isoCode=orderHeader.currencyUom/></@td>
              <@td>${orderHeader.getRelatedOne("StatusItem", true).get("description",locale)}</@td>
              <@td>
                <#assign trackingCodes = orderHeader.getRelated("TrackingCodeOrder", null, null, false)>
                <#list trackingCodes as trackingCode>
                    <#if trackingCode?has_content>
                        <a href="<@ofbizInterWebappUrl>/marketing/control/FindTrackingCodeOrders?trackingCodeId=${trackingCode.trackingCodeId}&amp;externalLoginKey=${requestAttributes.externalLoginKey!}</@ofbizInterWebappUrl>">${trackingCode.trackingCodeId}</a><br />
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
            <@tr type="meta"><@td colspan="9"><@commonMsg type="result-norecord">${uiLabelMap.OrderNoOrderFound}</@commonMsg></@td></@tr>
          </#if>
        </@table>
      </@paginate>
  </@section>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>
