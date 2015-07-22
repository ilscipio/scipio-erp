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

<script language="JavaScript" type="text/javascript">
<!-- //
function lookupOrders(click) {
    orderIdValue = document.lookuporder.orderId.value;
    if (orderIdValue.length > 1) {
        document.lookuporder.action = "<@ofbizUrl>orderview</@ofbizUrl>";
        document.lookuporder.method = "get";
    } else {
        document.lookuporder.action = "<@ofbizUrl>searchorders</@ofbizUrl>";
    }

    if (click) {
        document.lookuporder.submit();
    }
    return true;
}
function toggleOrderId(master) {
    var form = document.massOrderChangeForm;
    var orders = form.elements.length;
    for (var i = 0; i < orders; i++) {
        var element = form.elements[i];
        if (element.name == "orderIdList") {
            element.checked = master.checked;
        }
    }
}
function setServiceName(selection) {
    document.massOrderChangeForm.action = selection.value;
}
function runAction() {
    var form = document.massOrderChangeForm;
    form.submit();
}

function toggleOrderIdList() {
    var form = document.massOrderChangeForm;
    var orders = form.elements.length;
    var isAllSelected = true;
    for (var i = 0; i < orders; i++) {
        var element = form.elements[i];
        if (element.name == "orderIdList" && !element.checked)
            isAllSelected = false;
    }
    jQuery('#checkAllOrders').attr("checked", isAllSelected);
}

function submitFindForm(val){
    document.massOrderChangeForm.action = val;
    var form = document.massOrderChangeForm;
    form.submit();
}

// -->
</script>

<#if security.hasEntityPermission("ORDERMGR", "_VIEW", session)>
<#if parameters.hideFields?has_content>
<form name='lookupandhidefields${requestParameters.hideFields!"Y"}' method="post" action="<@ofbizUrl>searchorders</@ofbizUrl>">
  <#if (parameters.hideFields!"N")=='Y'>
    <input type="hidden" name="hideFields" value="N"/>
  <#else>
    <input type='hidden' name='hideFields' value='Y'/>
  </#if>
  <input type="hidden" name="viewSize" value="${viewSize}"/>
  <input type="hidden" name="viewIndex" value="${viewIndex}"/>
  <input type='hidden' name='correspondingPoId' value='${requestParameters.correspondingPoId!}'/>
  <input type='hidden' name='internalCode' value='${requestParameters.internalCode!}'/>
  <input type='hidden' name='productId' value='${requestParameters.productId!}'/>
  <input type='hidden' name='goodIdentificationTypeId' value='${requestParameters.goodIdentificationTypeId!}'/>
  <input type='hidden' name='goodIdentificationIdValue' value='${requestParameters.goodIdentificationIdValue!}'/>
  <input type='hidden' name='inventoryItemId' value='${requestParameters.inventoryItemId!}'/>
  <input type='hidden' name='serialNumber' value='${requestParameters.serialNumber!}'/>
  <input type='hidden' name='softIdentifier' value='${requestParameters.softIdentifier!}'/>
  <input type='hidden' name='partyId' value='${requestParameters.partyId!}'/>
  <input type='hidden' name='userLoginId' value='${requestParameters.userLoginId!}'/>
  <input type='hidden' name='billingAccountId' value='${requestParameters.billingAccountId!}'/>
  <input type='hidden' name='createdBy' value='${requestParameters.createdBy!}'/>
  <input type='hidden' name='minDate' value='${requestParameters.minDate!}'/>
  <input type='hidden' name='maxDate' value='${requestParameters.maxDate!}'/>
  <input type='hidden' name='roleTypeId' value="${requestParameters.roleTypeId!}"/>
  <input type='hidden' name='orderTypeId' value='${requestParameters.orderTypeId!}'/>
  <input type='hidden' name='salesChannelEnumId' value='${requestParameters.salesChannelEnumId!}'/>
  <input type='hidden' name='productStoreId' value='${requestParameters.productStoreId!}'/>
  <input type='hidden' name='orderWebSiteId' value='${requestParameters.orderWebSiteId!}'/>
  <input type='hidden' name='orderStatusId' value='${requestParameters.orderStatusId!}'/>
  <input type='hidden' name='hasBackOrders' value='${requestParameters.hasBackOrders!}'/>
  <input type='hidden' name='filterInventoryProblems' value='${requestParameters.filterInventoryProblems!}'/>
  <input type='hidden' name='filterPartiallyReceivedPOs' value='${requestParameters.filterPartiallyReceivedPOs!}'/>
  <input type='hidden' name='filterPOsOpenPastTheirETA' value='${requestParameters.filterPOsOpenPastTheirETA!}'/>
  <input type='hidden' name='filterPOsWithRejectedItems' value='${requestParameters.filterPOsWithRejectedItems!}'/>
  <input type='hidden' name='countryGeoId' value='${requestParameters.countryGeoId!}'/>
  <input type='hidden' name='includeCountry' value='${requestParameters.includeCountry!}'/>
  <input type='hidden' name='isViewed' value='${requestParameters.isViewed!}'/>
  <input type='hidden' name='shipmentMethod' value='${requestParameters.shipmentMethod!}'/>
  <input type='hidden' name='gatewayAvsResult' value='${requestParameters.gatewayAvsResult!}'/>
  <input type='hidden' name='gatewayScoreResult' value='${requestParameters.gatewayScoreResult!}'/>
</form>
</#if>

<form method="post" name="lookuporder" id="lookuporder" action="<@ofbizUrl>searchorders</@ofbizUrl>" onsubmit="javascript:lookupOrders();">
<input type="hidden" name="lookupFlag" value="Y"/>
<input type="hidden" name="hideFields" value="Y"/>
<input type="hidden" name="viewSize" value="${viewSize}"/>
<input type="hidden" name="viewIndex" value="${viewIndex}"/>

<@section>
   
    <ul class="button-group">
      <#if (requestParameters.hideFields!"N") == "Y">
        <li><a href="javascript:document.lookupandhidefields${requestParameters.hideFields}.submit()" class="tiny button">${uiLabelMap.CommonShowLookupFields}</a></li>
      <#else>
        <#if orderList??><li><a href="javascript:document.lookupandhidefields${requestParameters.hideFields!"Y"}.submit()"  class="tiny button">${uiLabelMap.CommonHideFields}</a></li></#if>
        <li><a href="/partymgr/control/findparty?externalLoginKey=${requestAttributes.externalLoginKey!}"  class="tiny button">${uiLabelMap.PartyLookupParty}</a></li>
        <li><a href="javascript:lookupOrders(true);"  class="tiny button">${uiLabelMap.OrderLookupOrder}</a></li>
      </#if>
    </ul>

  <#if (parameters.hideFields!"N") != "Y">
    <@row>
    <@cell class="${style_grid_large!}9 columns">

      <@field type="input" label="${uiLabelMap.OrderOrderId}" name="orderId"/>
      
      <@row collapse=true>
                <@cell class="${style_grid_large!}2 ${style_grid_small!}3"> <label>${uiLabelMap.CommonDateFilter}</label></@cell>
                <@cell class="${style_grid_large!}10 ${style_grid_small!}9">
                    <@field type="datetime" dateType="datetime" label="${uiLabelMap.CommonFrom}" name="minDate" value="${requestParameters.minDate!}" size="25" maxlength="30" id="minDate1" collapse=true/>
                    <@field type="datetime" dateType="datetime" label="${uiLabelMap.CommonThru}" name="maxDate" value="${requestParameters.maxDate!}" size="25" maxlength="30" id="maxDate" collapse=true/>
                </@cell>
        </@row>
      
      <@fieldset title="${uiLabelMap.CommonAdvancedSearch}" collapsed=true>
          <@field type="input" label="${uiLabelMap.OrderExternalId}" name="externalId"/>
          <@field type="input" label="${uiLabelMap.OrderCustomerPo}" name="correspondingPoId" value="${requestParameters.correspondingPoId!}"/>
          <@field type="input" label="${uiLabelMap.OrderInternalCode}" name="internalCode" value="${requestParameters.internalCode!}"/>
          <@field type="input" label="${uiLabelMap.ProductProductId}" name="productId" value="${requestParameters.productId!}"/>
              <#if goodIdentificationTypes?has_content>
            <@field type="select" label="${uiLabelMap.ProductGoodIdentificationType}" name="goodIdentificationTypeId">
                          <#if currentGoodIdentificationType?has_content>
                              <option value="${currentGoodIdentificationType.goodIdentificationTypeId}">${currentGoodIdentificationType.get("description", locale)}</option>
                              <option value="${currentGoodIdentificationType.goodIdentificationTypeId}">---</option>
                          </#if>
                          <option value="">${uiLabelMap.ProductAnyGoodIdentification}</option>
                          <#list goodIdentificationTypes as goodIdentificationType>
                              <option value="${goodIdentificationType.goodIdentificationTypeId}">${goodIdentificationType.get("description", locale)}</option>
                          </#list>
            </@field>
          <@field type="input" label="${uiLabelMap.ProductGoodIdentification}" name="goodIdentificationIdValue" value="${requestParameters.goodIdentificationIdValue!}"/>        
              </#if>
          <@field type="input" label="${uiLabelMap.ProductInventoryItemId}" name="inventoryItemId" value="${requestParameters.inventoryItemId!}"/>
          <@field type="input" label="${uiLabelMap.ProductSerialNumber}" name="serialNumber" value="${requestParameters.serialNumber!}"/>
          <@field type="input" label="${uiLabelMap.ProductSoftIdentifier}" name="softIdentifier" value="${requestParameters.softIdentifier!}"/>
          <@field type="select" label="${uiLabelMap.PartyRoleType}" name="roleTypeId" id="roleTypeId" multiple="multiple">
                    <#if currentRole?has_content>
                    <option value="${currentRole.roleTypeId}">${currentRole.get("description", locale)}</option>
                    </#if>
                    <option value="">${uiLabelMap.CommonAnyRoleType}</option>
                    <#list roleTypes as roleType>
                      <option value="${roleType.roleTypeId}">${roleType.get("description", locale)}</option>
                    </#list>
          </@field>     
          <@row collapse=true>
                <@cell class="${style_grid_small!}3 ${style_grid_large!}2">
                    <label class="inline">${uiLabelMap.PartyPartyId}</label>
                </@cell>
                <@cell class="${style_grid_small!}9 ${style_grid_large!}10">
                  <@htmlTemplate.lookupField value='${requestParameters.partyId!}' formName="lookuporder" name="partyId" id="partyId" fieldFormName="LookupPartyName"/>
                </@cell>
           </@row>
           <@field type="input" label="${uiLabelMap.CommonUserLoginId}" name="userLoginId" value="${requestParameters.userLoginId!}"/>
           <@field type="select" label="${uiLabelMap.OrderOrderType}" name="orderTypeId">
                    <#if currentType?has_content>
                    <option value="${currentType.orderTypeId}">${currentType.get("description", locale)}</option>
                    <option value="${currentType.orderTypeId}">---</option>
                    </#if>
                    <option value="">${uiLabelMap.OrderAnyOrderType}</option>
                    <#list orderTypes as orderType>
                      <option value="${orderType.orderTypeId}">${orderType.get("description", locale)}</option>
                    </#list>
            </@field>
            <@field type="input" label="${uiLabelMap.AccountingBillingAccount}" name="billingAccountId" value="${requestParameters.billingAccountId!}"/>
            <@field type="input" label="${uiLabelMap.CommonCreatedBy}" name="createdBy" value="${requestParameters.createdBy!}"/>
            <@field type="select" label="${uiLabelMap.OrderSalesChannel}" name="salesChannelEnumId">
                    <#if currentSalesChannel?has_content>
                    <option value="${currentSalesChannel.enumId}">${currentSalesChannel.get("description", locale)}</option>
                    <option value="${currentSalesChannel.enumId}">---</option>
                    </#if>
                    <option value="">${uiLabelMap.CommonAnySalesChannel}</option>
                    <#list salesChannels as channel>
                      <option value="${channel.enumId}">${channel.get("description", locale)}</option>
                    </#list>
            </@field>
            <@field type="select" label="${uiLabelMap.ProductProductStore}" name="productStoreId">
                    <#if currentProductStore?has_content>
                    <option value="${currentProductStore.productStoreId}">${currentProductStore.storeName!}</option>
                    <option value="${currentProductStore.productStoreId}">---</option>
                    </#if>
                    <option value="">${uiLabelMap.CommonAnyStore}</option>
                    <#list productStores as store>
                      <option value="${store.productStoreId}">${store.storeName!}</option>
                    </#list>
             </@field>
             <@field type="select" label="${uiLabelMap.ProductWebSite}" name="orderWebSiteId">
    
                    <#if currentWebSite?has_content>
                    <option value="${currentWebSite.webSiteId}">${currentWebSite.siteName}</option>
                    <option value="${currentWebSite.webSiteId}">---</option>
                    </#if>
                    <option value="">${uiLabelMap.CommonAnyWebSite}</option>
                    <#list webSites as webSite>
                      <option value="${webSite.webSiteId}">${webSite.siteName!}</option>
                    </#list>
            </@field>
            <@field type="select" label="${uiLabelMap.CommonStatus}" name="orderStatusId">
    
                    <#if currentStatus?has_content>
                    <option value="${currentStatus.statusId}">${currentStatus.get("description", locale)}</option>
                    <option value="${currentStatus.statusId}">---</option>
                    </#if>
                    <option value="">${uiLabelMap.OrderAnyOrderStatus}</option>
                    <#list orderStatuses as orderStatus>
                      <option value="${orderStatus.statusId}">${orderStatus.get("description", locale)}</option>
                    </#list>
             </@field>
             <@field type="select" label="${uiLabelMap.OrderContainsBackOrders}" name="hasBackOrders">
                    <#if requestParameters.hasBackOrders?has_content>
                    <option value="Y">${uiLabelMap.OrderBackOrders}</option>
                    <option value="Y">---</option>
                    </#if>
                    <option value="">${uiLabelMap.CommonShowAll}</option>
                    <option value="Y">${uiLabelMap.CommonOnly}</option>
             </@field>
             <@field type="select" label="${uiLabelMap.OrderSelectShippingMethod}" name="shipmentMethod">
                    <#if currentCarrierShipmentMethod?has_content>
                      <#assign currentShipmentMethodType = currentCarrierShipmentMethod.getRelatedOne("ShipmentMethodType", false)>
                      <option value="${currentCarrierShipmentMethod.partyId}@${currentCarrierShipmentMethod.shipmentMethodTypeId}">${currentCarrierShipmentMethod.partyId!} ${currentShipmentMethodType.description!}</option>
                      <option value="${currentCarrierShipmentMethod.partyId}@${currentCarrierShipmentMethod.shipmentMethodTypeId}">---</option>
                    </#if>
                    <option value="">${uiLabelMap.OrderSelectShippingMethod}</option>
                    <#list carrierShipmentMethods as carrierShipmentMethod>
                      <#assign shipmentMethodType = carrierShipmentMethod.getRelatedOne("ShipmentMethodType", false)>
                      <option value="${carrierShipmentMethod.partyId}@${carrierShipmentMethod.shipmentMethodTypeId}">${carrierShipmentMethod.partyId!} ${shipmentMethodType.description!}</option>
                    </#list>
             </@field>
             <@field type="select" label="${uiLabelMap.OrderViewed}" name="isViewed">
                    <#if requestParameters.isViewed?has_content>
                      <#assign isViewed = requestParameters.isViewed>
                      <option value="${isViewed}"><#if "Y" == isViewed>${uiLabelMap.CommonYes}<#elseif "N" == isViewed>${uiLabelMap.CommonNo}</#if></option>
                    </#if>
                    <option value=""></option>
                    <option value="Y">${uiLabelMap.CommonYes}</option>
                    <option value="N">${uiLabelMap.CommonNo}</option>
            </@field>
    
            <@field type="input" label="${uiLabelMap.OrderAddressVerification}" name="gatewayAvsResult" value="${requestParameters.gatewayAvsResult!}"/>
            <@field type="input" label="${uiLabelMap.OrderScore}" name="gatewayScoreResult" value="${requestParameters.gatewayScoreResult!}"/>
            
            
            <@row collapse=false>
                <@cell class="${style_grid_large!}4">
                    <@renderCheckBox name="filterInventoryProblems" currentValue="Y" checked=requestParameters.filterInventoryProblems!"N"/> ${uiLabelMap.OrderFilterOn} ${uiLabelMap.OrderFilterInventoryProblems}
                </@cell>
                <@cell class="${style_grid_large!}4">
                    <@renderCheckBox name="filterPOsOpenPastTheirETA" currentValue="Y" checked=requestParameters.filterPOsOpenPastTheirETA!"N" /> ${uiLabelMap.OrderFilterOn} ${uiLabelMap.OrderFilterPOs} ${uiLabelMap.OrderFilterPOsOpenPastTheirETA}
                </@cell>
    
                <@cell class="${style_grid_large!}4">
                    <@renderCheckBox name="filterPOsWithRejectedItems" currentValue="Y" checked=requestParameters.filterPOsWithRejectedItems!"N"/> ${uiLabelMap.OrderFilterOn} ${uiLabelMap.OrderFilterPOs} ${uiLabelMap.OrderFilterPOsWithRejectedItems}      
                </@cell>
            </@row>
            <@field type="select" label="${uiLabelMap.OrderShipToCountry}" name="countryGeoId">
                    <#if requestParameters.countryGeoId?has_content>
                        <#assign countryGeoId = requestParameters.countryGeoId>
                        <#assign geo = delegator.findOne("Geo", Static["org.ofbiz.base.util.UtilMisc"].toMap("geoId", countryGeoId), true)>
                        <option value="${countryGeoId}">${geo.geoName!}</option>
                        <option value="${countryGeoId}">---</option>
                    <#else>
                        <option value="">---</option>
                    </#if>
                    ${screens.render("component://common/widget/CommonScreens.xml#countries")}
            </@field>
            <@field type="select" name="includeCountry" label="${uiLabelMap.OrderIncludeCountry}">
                    <option value="">${uiLabelMap.CommonAny}</option>
                    <#if requestParameters.includeCountry?has_content>
                       <#assign includeCountry = requestParameters.includeCountry>
                       <option value="${includeCountry}"><#if "Y" == includeCountry>${uiLabelMap.OrderOnlyInclude}<#elseif "N" == includeCountry>${uiLabelMap.OrderDoNotInclude}</#if></option>
                       <option value="${includeCountry}">---</option>
                    </#if>
                    <option value="Y">${uiLabelMap.OrderOnlyInclude}</option>
                    <option value="N">${uiLabelMap.OrderDoNotInclude}</option>
            </@field>
            <@field type="select" label="${uiLabelMap.AccountingPaymentStatus}" name="paymentStatusId">
                        <option value="">${uiLabelMap.CommonAll}</option>
                        <#list paymentStatusList as paymentStatus>
                            <option value="${paymentStatus.statusId}">${paymentStatus.get("description", locale)}</option>
                        </#list>
            </@field>
        </@fieldset>
        <input type="hidden" name="showAll" value="Y"/>
        <input type="submit" value='${uiLabelMap.CommonFind}'/>
      </@cell>
  </@row>    
      </#if>
</@section>
<input type="image" src="<@ofbizContentUrl>/images/spacer.gif</@ofbizContentUrl>" onclick="javascript:lookupOrders(true);"/>
</form>
<#if (requestParameters.hideFields!"N") != "Y">
<script language="JavaScript" type="text/javascript">
<!--//
document.lookuporder.orderId.focus();
//-->
</script>
</#if>

<br />

<#if (searchPerformed!false)==true>
<@section title="${uiLabelMap.CommonSearchResults}">
  <@row>
    <@cell>

  <#if lookupErrorMessage?has_content>
    <@alert type="alert">
        ${lookupErrorMessage}
    </@alert>
  </#if>    

  <#-- note: added this check here for simplicity but haven't removed old code inside; no harm, maybe reuse-->
  <#if orderList?has_content>
  
    <#assign paginated = false>
    <#if (orderList?has_content && (0 < orderList?size))>
        <#assign url><@ofbizUrl>searchorders</@ofbizUrl></#assign>
        <#assign paramStr = addParamsToStr(StringUtil.wrapString(paramList!""), {"showAll": showAll!"", "hideFields": requestParameters.hideFields!"N"}, "&amp;", false)>
        <#-- forcePost required because search done from service event with https="true" -->
        <@paginate url=url viewSize=viewSize viewIndex=viewIndex listSize=orderListSize altParam=true paramStr=paramStr forcePost=true viewIndexFirst=1 />
        <#assign paginated = true>
    </#if>
    
    <form name="massOrderChangeForm" method="post" action="javascript:void(0);">
        <input type="hidden" name="screenLocation" value="component://order/widget/ordermgr/OrderPrintScreens.xml#OrderPDF"/>
        
        <#--
        <select name="serviceName" onchange="javascript:setServiceName(this);">
           <option value="javascript:void(0);">&nbsp;</option>
           <option value="<@ofbizUrl>massApproveOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>">${uiLabelMap.OrderApproveOrder}</option>
           <option value="<@ofbizUrl>massHoldOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>">${uiLabelMap.OrderHold}</option>
           <option value="<@ofbizUrl>massProcessOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>">${uiLabelMap.OrderProcessOrder}</option>
           <option value="<@ofbizUrl>massCancelOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>">${uiLabelMap.OrderCancelOrder}</option>
           <option value="<@ofbizUrl>massCancelRemainingPurchaseOrderItems?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>">${uiLabelMap.OrderCancelRemainingPOItems}</option>
           <option value="<@ofbizUrl>massRejectOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>">${uiLabelMap.OrderRejectOrder}</option>
           <option value="<@ofbizUrl>massPickOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>">${uiLabelMap.OrderPickOrders}</option>
           <option value="<@ofbizUrl>massQuickShipOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>">${uiLabelMap.OrderQuickShipEntireOrder}</option>
           <option value="<@ofbizUrl>massPrintOrders?hideFields=${requestParameters.hideFields!'N'}${paramList}</@ofbizUrl>">${uiLabelMap.CommonPrint}</option>
           <option value="<@ofbizUrl>massCreateFileForOrders?hideFields=${requestParameters.hideFields!'N'}${paramList}</@ofbizUrl>">${uiLabelMap.ContentCreateFile}</option>
        </select>-->
        <#--
        <select name="printerName">
           <option value="javascript:void(0);">&nbsp;</option>
           <#list printers as printer>
           <option value="${printer}">${printer}</option>
           </#list>
        </select>
        <a href="javascript:runAction();" class="button tiny">${uiLabelMap.OrderRunAction}</a>
        -->
      <#macro massOrderChangeButton id="1">  
          <@row>
            <@cell>
                <button href="#" data-dropdown="drop1" aria-controls="drop_${id!"1"}" aria-expanded="false" class="button small secondary dropdown">${uiLabelMap.OrderRunAction}</button><br>
                <ul id="drop${id!"1"}" data-dropdown-content class="f-dropdown" aria-hidden="true" tabindex="-1">
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massApproveOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>')">${uiLabelMap.OrderApproveOrder}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massHoldOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>')">${uiLabelMap.OrderHold}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massProcessOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>')">${uiLabelMap.OrderProcessOrder}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massCancelOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>')">${uiLabelMap.OrderCancelOrder}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massCancelRemainingPurchaseOrderItems?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>')">${uiLabelMap.OrderCancelRemainingPOItems}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massRejectOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>')">${uiLabelMap.OrderRejectOrder}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massPickOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>')">${uiLabelMap.OrderPickOrders}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massQuickShipOrders?hideFields=${requestParameters.hideFields!"N"}${paramList}</@ofbizUrl>')">${uiLabelMap.OrderQuickShipEntireOrder}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massPrintOrders?hideFields=${requestParameters.hideFields!'N'}${paramList}</@ofbizUrl>')">${uiLabelMap.CommonPrint}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massCreateFileForOrders?hideFields=${requestParameters.hideFields!'N'}${paramList}</@ofbizUrl>')">${uiLabelMap.ContentCreateFile}</a></li>
                </ul>
            </@cell>  
          </@row>
      </#macro>
      <@massOrderChangeButton id="1"/>
      <table class="hover-bar" cellspacing='0'>
       <thead>
        <tr class="header-row">
          <th width="5%">
            <input type="checkbox" id="checkAllOrders" name="checkAllOrders" value="1" onchange="javascript:toggleOrderId(this);"/>
          </th>
          <th width="10%">${uiLabelMap.OrderOrderType}</th>
          <th width="15%">${uiLabelMap.OrderOrderId}</th>
          <th width="20%">${uiLabelMap.PartyName}</th>
          <#--<th width="5%" align="right">${uiLabelMap.OrderSurvey}</th>-->
          <th width="10%" align="right">${uiLabelMap.OrderItemsOrdered}</th>
          <#--<th width="5%" align="right">${uiLabelMap.OrderItemsBackOrdered}</th>
          <th width="5%" align="right">${uiLabelMap.OrderItemsReturned}</th>-->
          <th width="10%" align="right">${uiLabelMap.OrderRemainingSubTotal}</th>
          <th width="10%" align="right">${uiLabelMap.OrderOrderTotal}</th>
          <th width="5%">&nbsp;</th>
            <#if ((requestParameters.filterInventoryProblems!"N") == "Y") || ((requestParameters.filterPOsOpenPastTheirETA!"N") == "Y") || ((requestParameters.filterPOsWithRejectedItems!"N") == "Y") || ((requestParameters.filterPartiallyReceivedPOs!"N") == "Y")>
              <th width="10%">${uiLabelMap.CommonStatus}</th>
              <th width="5%">${uiLabelMap.CommonFilter}</th>
            <#else>
              <th width="15%">${uiLabelMap.CommonStatus}</th>
            </#if>
          <th width="10%">${uiLabelMap.OrderDate}</th>
          <th width="10%">${uiLabelMap.PartyPartyId}</th>
          <th width="10%">&nbsp;</th>
        </tr>
        </thead>
        <#if orderList?has_content>
          <#assign alt_row = false>
          <#list orderList as orderHeader>
            <#assign orh = Static["org.ofbiz.order.order.OrderReadHelper"].getHelper(orderHeader)>
            <#assign statusItem = orderHeader.getRelatedOne("StatusItem", true)!>
            <#assign orderType = orderHeader.getRelatedOne("OrderType", true)!>
            <#if (orderType.orderTypeId)! == "PURCHASE_ORDER">
              <#assign displayParty = orh.getSupplierAgent()!>
            <#else>
              <#assign displayParty = orh.getPlacingParty()!>
            </#if>
            <#assign partyId = displayParty.partyId!("_NA_")>
            <tr valign="middle"<#if alt_row> class="alternate-row"</#if>>
              <td>
                 <input type="checkbox" name="orderIdList" value="${orderHeader.orderId}" onchange="javascript:toggleOrderIdList();"/>
              </td>
              <td>${(orderType.get("description",locale)!(orderType.orderTypeId!""))!""}</td>
              <td><a href="<@ofbizUrl>orderview?orderId=${orderHeader.orderId}</@ofbizUrl>" class="">${orderHeader.orderId}</a></td>
              <td>
                <div>
                  <#if displayParty?has_content>
                      <#assign displayPartyNameResult = dispatcher.runSync("getPartyNameForDate", Static["org.ofbiz.base.util.UtilMisc"].toMap("partyId", displayParty.partyId, "compareDate", orderHeader.orderDate!, "userLogin", userLogin))/>
                      ${displayPartyNameResult.fullName!"[${uiLabelMap.OrderPartyNameNotFound}]"}
                  <#else>
                    ${uiLabelMap.CommonNA}
                  </#if>
                </div>
                <#--
                <div>
                <#if placingParty?has_content>
                  <#assign partyId = placingParty.partyId>
                  <#if placingParty.getEntityName() == "Person">
                    <#if placingParty.lastName??>
                      ${placingParty.lastName}<#if placingParty.firstName??>, ${placingParty.firstName}</#if>
                    <#else>
                      ${uiLabelMap.CommonNA}
                    </#if>
                  <#else>
                    <#if placingParty.groupName??>
                      ${placingParty.groupName}
                    <#else>
                      ${uiLabelMap.CommonNA}
                    </#if>
                  </#if>
                <#else>
                  ${uiLabelMap.CommonNA}
                </#if>
                </div>
                -->
              </td>
              <#--<td align="right">${orh.hasSurvey()?string.number}</td>-->
              <td align="right">${orh.getTotalOrderItemsQuantity()?string.number}</td>
              <#--<td align="right">${orh.getOrderBackorderQuantity()?string.number}</td>
              <td align="right">${orh.getOrderReturnedQuantity()?string.number}</td>-->
              <td align="right"><#if orderHeader.remainingSubTotal?has_content><@ofbizCurrency amount=orderHeader.remainingSubTotal isoCode=orh.getCurrency()/></#if></td>
              <td align="right"><#if orderHeader.grandTotal?has_content><@ofbizCurrency amount=orderHeader.grandTotal isoCode=orh.getCurrency()/></#if></td>

              <td>&nbsp;</td>
              <td>${(statusItem.get("description",locale)!(statusItem.statusId!("N/A")))!""}</td>
              </td>
              <#if ((requestParameters.filterInventoryProblems!"N") == "Y") || ((requestParameters.filterPOsOpenPastTheirETA!"N") == "Y") || ((requestParameters.filterPOsWithRejectedItems!"N") == "Y") || ((requestParameters.filterPartiallyReceivedPOs!"N") == "Y")>
                  <td>
                      <#if filterInventoryProblems.contains(orderHeader.orderId)>
                        Inv&nbsp;
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
                  </td>
              </#if>
              <td>${orderHeader.getString("orderDate")!}</td>
              <td>
                <#if partyId != "_NA_">
                  <a href="${customerDetailLink}${partyId}" class="">${partyId}</a>
                <#else>
                  ${uiLabelMap.CommonNA}
                </#if>
              </td>
              <td align='right'>
                <a href="<@ofbizUrl>orderview?orderId=${orderHeader.orderId}</@ofbizUrl>" class="button tiny">${uiLabelMap.CommonView}</a>
              </td>
            </tr>
            <#-- toggle the row color -->
            <#assign alt_row = !alt_row>
          </#list>
        <#else>
          <tr>
            <td colspan='4'><h3>${uiLabelMap.OrderNoOrderFound}</h3></td>
          </tr>
        </#if>
      </table>
      <@massOrderChangeButton id="2"/>
      
    </form>
    
    <#if paginated>
        <@paginate url=url viewSize=viewSize viewIndex=viewIndex listSize=orderListSize altParam=true paramStr=paramStr forcePost=true viewIndexFirst=1 />
    </#if>
    
  <#else>
    <@alert type="info">
       ${uiLabelMap.OrderNoOrderFound}
    </@alert>
  </#if>
    
    </@cell>
  </@row>
</@section>
</#if>

<#else>
  <@section title="${uiLabelMap.OrderViewPermissionError!}" />
</#if>
