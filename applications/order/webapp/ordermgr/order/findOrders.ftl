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

<#-- SCIPIO: use parameters map as source -->
<#assign findParams = parameters>

<@script>
function lookupOrders(click) {
    orderIdValue = document.lookuporder.orderId.value;
    <#-- Scipio: don't lookup if ID contains search wildcards -->
    if (orderIdValue.length > 1 && !orderIdValue.match(/[%*]/)) {
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
    <#-- SCIPIO: only submit if at least one order checked -->
    var hasChecked = false;
    jQuery('form[name=massOrderChangeForm] input[name=orderIdList]').each(function(i, e) {
        if (jQuery(e).is(":checked")) {
            hasChecked = true;
        }
    });
    if (hasChecked) {
        document.massOrderChangeForm.action = val;
        var form = document.massOrderChangeForm;
        form.submit();
    }
}

</@script>

<#if security.hasEntityPermission("ORDERMGR", "_VIEW", session)>
<#if parameters.hideFields?has_content>
<form name="lookupandhidefields${findParams.hideFields!"Y"}" method="post" action="<@ofbizUrl>searchorders</@ofbizUrl>">

<#-- SCIPIO: new flag -->
<input type="hidden" name="doFindQuery" value="Y" />

<#assign prevQueryInputParamsCore>
  <#if paramIdList?has_content>
    <#list paramIdList as paramIds>
      <#assign paramId = paramIds.split("=")/>
      <input type="hidden" name="${paramId[0]}" value="${paramId[1]}"/>
    </#list>
  </#if>
</#assign>
<#assign prevQueryInputParamsWithView>
  <input type="hidden" name="showAll" value="${showAll!}"/>
  <input type="hidden" name="viewSize" value="${viewSize}"/>
  <input type="hidden" name="viewIndex" value="${viewIndex}"/>
  ${prevQueryInputParamsCore}
</#assign>
<#assign prevQueryInputParams>
  <#if (parameters.hideFields!"N")=='Y'>
    <input type="hidden" name="hideFields" value="N"/>
  <#else>
    <input type="hidden" name="hideFields" value="Y"/>
  </#if>
  ${prevQueryInputParamsWithView}
</#assign>
  ${prevQueryInputParams}
</form>
</#if>

<form method="post" name="lookuporder" id="lookuporder" action="<@ofbizUrl>searchorders</@ofbizUrl>" onsubmit="javascript:lookupOrders();">
<input type="hidden" name="lookupFlag" value="Y"/>
<input type="hidden" name="hideFields" value="Y"/>
<input type="hidden" name="viewSize" value="${viewSize}"/>
<input type="hidden" name="viewIndex" value="${viewIndex}"/>

<#-- SCIPIO: new flag -->
<input type="hidden" name="doFindQuery" value="Y" />

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if (findParams.hideFields!"N") == "Y">
    <@menuitem type="link" href="javascript:document.lookupandhidefields${findParams.hideFields}.submit()" text=uiLabelMap.CommonShowLookupFields class="+${styles.action_run_sys!} ${styles.action_show!}" />
  <#else>
    <#if orderList??>
      <@menuitem type="link" href="javascript:document.lookupandhidefields${findParams.hideFields!'Y'}.submit()" text=uiLabelMap.CommonHideFields class="+${styles.action_run_sys!} ${styles.action_hide!}" />
    </#if>
    <@menuitem type="link" href=makeOfbizInterWebappUrl("/partymgr/control/findparty?externalLoginKey=${requestAttributes.externalLoginKey!}") text=uiLabelMap.PartyLookupParty class="+${styles.action_nav!} ${styles.action_find!}" />
    <@menuitem type="link" href="javascript:lookupOrders(true);" text=uiLabelMap.OrderLookupOrder class="+${styles.action_run_sys!} ${styles.action_find!}" />
  </#if>
  </@menu>
</#macro>
<#assign showFields = ((parameters.hideFields!"N") != "Y")>
<@section menuContent=menuContent hasContent=showFields>
<#if showFields>
  <@row>
    <@cell columns=9>

          <@field type="input" label=uiLabelMap.OrderOrderId name="orderId" value=(findParams.orderId!)/>
      
          <@field type="generic" label=uiLabelMap.CommonDateFilter>
              <@field type="datetime" dateType="datetime" label=uiLabelMap.CommonFrom name="minDate" value=(findParams.minDate!) size="25" maxlength="30" id="minDate1" />
              <@field type="datetime" dateType="datetime" label=uiLabelMap.CommonThru name="maxDate" value=(findParams.maxDate!) size="25" maxlength="30" id="maxDate" />
          </@field>
      
      <@fieldset title=uiLabelMap.CommonAdvancedSearch collapsed=true>
          <@field type="input" label=uiLabelMap.OrderExternalId name="externalId" value=(findParams.externalId!)/>
          <@field type="input" label=uiLabelMap.OrderCustomerPo name="correspondingPoId" value=(findParams.correspondingPoId!)/>
          <@field type="input" label=uiLabelMap.OrderInternalCode name="internalCode" value=(findParams.internalCode!)/>
          <@field type="lookup" label=uiLabelMap.ProductProductId name="productId" value=(findParams.productId!) formName="lookuporder" id="productId" fieldFormName="LookupProduct"/>
          <#if goodIdentificationTypes?has_content>
            <@field type="select" label=uiLabelMap.ProductGoodIdentificationType name="goodIdentificationTypeId">
                <#if currentGoodIdentificationType?has_content>
                  <@field type="option" value=currentGoodIdentificationType.goodIdentificationTypeId>${currentGoodIdentificationType.get("description", locale)}</@field>
                  <@field type="option" value=currentGoodIdentificationType.goodIdentificationTypeId>---</@field>
                </#if>
                <@field type="option" value="">${uiLabelMap.ProductAnyGoodIdentification}</@field>
                <#list goodIdentificationTypes as goodIdentificationType>
                  <@field type="option" value=goodIdentificationType.goodIdentificationTypeId>${goodIdentificationType.get("description", locale)}</@field>
                </#list>
            </@field>
            <@field type="input" label=uiLabelMap.ProductGoodIdentification name="goodIdentificationIdValue" value=(findParams.goodIdentificationIdValue!)/>        
          </#if>
          <@field type="input" label=uiLabelMap.ProductInventoryItemId name="inventoryItemId" value=(findParams.inventoryItemId!)/>
          <@field type="input" label=uiLabelMap.ProductSerialNumber name="serialNumber" value=(findParams.serialNumber!)/>
          <@field type="input" label=uiLabelMap.ProductSoftIdentifier name="softIdentifier" value=(findParams.softIdentifier!)/>
          <@field type="select" label=uiLabelMap.PartyRoleType name="roleTypeId" id="roleTypeId" multiple=true>
              <@field type="option" value="">${uiLabelMap.CommonAnyRoleType}</@field>
              <#list roleTypes as roleType>
                <#assign optSelected = (currentRoleTypeIdList![])?seq_contains(roleType.roleTypeId)>
                <@field type="option" value=roleType.roleTypeId selected=optSelected>${roleType.get("description", locale)}</@field>
              </#list>
          </@field>  
          <@field type="lookup" label=uiLabelMap.PartyPartyId value=(findParams.partyId!) formName="lookuporder" name="partyId" id="partyId" fieldFormName="LookupPartyName"/>
          <@field type="input" label=uiLabelMap.CommonUserLoginId name="userLoginId" value=(findParams.userLoginId!)/>
          <@field type="select" label=uiLabelMap.OrderOrderType name="orderTypeId">
              <#if currentType?has_content>
                <@field type="option" value=currentType.orderTypeId>${currentType.get("description", locale)}</@field>
                <@field type="option" value=currentType.orderTypeId>---</@field>
              </#if>
              <@field type="option" value="">${uiLabelMap.OrderAnyOrderType}</@field>
              <#list orderTypes as orderType>
                <@field type="option" value=orderType.orderTypeId>${orderType.get("description", locale)}</@field>
              </#list>
          </@field>
          <@field type="input" label=uiLabelMap.AccountingBillingAccount name="billingAccountId" value=(findParams.billingAccountId!)/>
          <@field type="input" label=uiLabelMap.CommonCreatedBy name="createdBy" value=(findParams.createdBy!)/>
          <@field type="select" label=uiLabelMap.OrderSalesChannel name="salesChannelEnumId">
              <#if currentSalesChannel?has_content>
                <@field type="option" value=currentSalesChannel.enumId>${currentSalesChannel.get("description", locale)}</@field>
                <@field type="option" value=currentSalesChannel.enumId>---</@field>
              </#if>
              <@field type="option" value="">${uiLabelMap.CommonAnySalesChannel}</@field>
              <#list salesChannels as channel>
                <@field type="option" value=channel.enumId>${channel.get("description", locale)}</@field>
              </#list>
          </@field>
          <@field type="select" label=uiLabelMap.ProductProductStore name="productStoreId">
              <#if currentProductStore?has_content>
                <@field type="option" value=currentProductStore.productStoreId>${currentProductStore.storeName!(currentProductStore.productStoreId!)}</@field>
                <@field type="option" value=currentProductStore.productStoreId>---</@field>
              </#if>
              <@field type="option" value="">${uiLabelMap.CommonAnyStore}</@field>
              <#list productStores as store>
                <@field type="option" value=store.productStoreId>${store.storeName!(store.productStoreId!)}</@field>
              </#list>
          </@field>
          <@field type="select" label=uiLabelMap.ProductWebSite name="orderWebSiteId">
              <#if currentWebSite?has_content>
                <@field type="option" value=currentWebSite.webSiteId>${currentWebSite.siteName}</@field>
                <@field type="option" value=currentWebSite.webSiteId>---</@field>
              </#if>
              <@field type="option" value="">${uiLabelMap.CommonAnyWebSite}</@field>
              <#list webSites as webSite>
                <@field type="option" value=webSite.webSiteId>${webSite.siteName!}</@field>
              </#list>
          </@field>
          <@field type="select" label=uiLabelMap.CommonStatus name="orderStatusId">
              <#if currentStatus?has_content>
                <@field type="option" value=currentStatus.statusId>${currentStatus.get("description", locale)}</@field>
                <@field type="option" value=currentStatus.statusId>---</@field>
              </#if>
              <@field type="option" value="">${uiLabelMap.OrderAnyOrderStatus}</@field>
              <#list orderStatuses as orderStatus>
                <@field type="option" value=orderStatus.statusId>${orderStatus.get("description", locale)}</@field>
              </#list>
          </@field>
          <@field type="select" label=uiLabelMap.OrderContainsBackOrders name="hasBackOrders">
              <#if findParams.hasBackOrders?has_content>
                <@field type="option" value="Y">${uiLabelMap.OrderBackOrders}</@field>
                <@field type="option" value="Y">---</@field>
              </#if>
              <@field type="option" value="">${uiLabelMap.CommonShowAll}</@field>
              <@field type="option" value="Y">${uiLabelMap.CommonOnly}</@field>
          </@field>
          <@field type="select" label=uiLabelMap.OrderSelectShippingMethod name="shipmentMethod">
              <#if currentCarrierShipmentMethod?has_content>
                <#assign currentShipmentMethodType = currentCarrierShipmentMethod.getRelatedOne("ShipmentMethodType", false)>
                <@field type="option" value="${rawString(currentCarrierShipmentMethod.partyId)}@${rawString(currentCarrierShipmentMethod.shipmentMethodTypeId)}">${currentCarrierShipmentMethod.partyId!} ${currentShipmentMethodType.description!}</@field>
                <@field type="option" value="${rawString(currentCarrierShipmentMethod.partyId)}@${rawString(currentCarrierShipmentMethod.shipmentMethodTypeId)}">---</@field>
              </#if>
              <@field type="option" value="">${uiLabelMap.OrderSelectShippingMethod}</@field>
              <#list carrierShipmentMethods as carrierShipmentMethod>
                <#assign shipmentMethodType = carrierShipmentMethod.getRelatedOne("ShipmentMethodType", false)>
                <@field type="option" value="${rawString(carrierShipmentMethod.partyId)}@${rawString(carrierShipmentMethod.shipmentMethodTypeId)}">${carrierShipmentMethod.partyId!} ${shipmentMethodType.description!}</@field>
              </#list>
          </@field>
          <@field type="select" label=uiLabelMap.OrderViewed name="isViewed">
              <#if findParams.isViewed?has_content>
                <#assign isViewed = findParams.isViewed>
                <@field type="option" value=isViewed><#if "Y" == isViewed>${uiLabelMap.CommonYes}<#elseif "N" == isViewed>${uiLabelMap.CommonNo}</#if></@field>
              </#if>
              <@field type="option" value=""></@field>
              <@field type="option" value="Y">${uiLabelMap.CommonYes}</@field>
              <@field type="option" value="N">${uiLabelMap.CommonNo}</@field>
          </@field>
  
          <@field type="input" label=uiLabelMap.OrderAddressVerification name="gatewayAvsResult" value=(findParams.gatewayAvsResult!)/>
          <@field type="input" label=uiLabelMap.OrderScore name="gatewayScoreResult" value=(findParams.gatewayScoreResult!)/>
          
          <@field type="generic" label=uiLabelMap.CommonFilter inlineItems=false> <#-- NOTE: inlineItems setting propagates to child elements here -->
              <@field type="checkbox" name="filterInventoryProblems" value="Y" checked=((findParams.filterInventoryProblems!"N")=="Y") label="${rawString(uiLabelMap.OrderFilterOn)} ${rawString(uiLabelMap.OrderFilterInventoryProblems)}" /> 
              <@field type="checkbox" name="filterPOsOpenPastTheirETA" value="Y" checked=((findParams.filterPOsOpenPastTheirETA!"N")=="Y") label="${rawString(uiLabelMap.OrderFilterOn)} ${rawString(uiLabelMap.OrderFilterPOs)} ${rawString(uiLabelMap.OrderFilterPOsOpenPastTheirETA)}" /> 
              <@field type="checkbox" name="filterPOsWithRejectedItems" value="Y" checked=((findParams.filterPOsWithRejectedItems!"N")=="Y") label="${rawString(uiLabelMap.OrderFilterOn)} ${rawString(uiLabelMap.OrderFilterPOs)} ${rawString(uiLabelMap.OrderFilterPOsWithRejectedItems)}" /> 
          </@field>

          <@field type="select" label=uiLabelMap.OrderShipToCountry name="countryGeoId">
              <#if findParams.countryGeoId?has_content>
                  <#assign countryGeoId = findParams.countryGeoId>
                  <#assign geo = delegator.findOne("Geo", {"geoId":countryGeoId}, true)>
                  <@field type="option" value=countryGeoId selected=true>${geo.get("geoName",locale)!}</@field>
                  <@field type="option" value=countryGeoId>---</@field>
              </#if>
              <@field type="option" value="" selected=(findParams.countryGeoId?? && findParams.countryGeoId == "")></@field>
              <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"countriesPreselect":!(findParams.countryGeoId??)}/>
          </@field>
          <@field type="select" name="includeCountry" label=uiLabelMap.OrderIncludeCountry>
              <#if findParams.includeCountry?has_content>
                 <#assign includeCountry = findParams.includeCountry>
                 <@field type="option" value=includeCountry selected=true><#if "Y" == includeCountry>${uiLabelMap.OrderOnlyInclude}<#elseif "N" == includeCountry>${uiLabelMap.OrderDoNotInclude}</#if></@field>
                 <@field type="option" value=includeCountry>---</@field>
              </#if>
              <@field type="option" value="">${uiLabelMap.CommonAny}</@field>
              <@field type="option" value="Y">${uiLabelMap.OrderOnlyInclude}</@field>
              <@field type="option" value="N">${uiLabelMap.OrderDoNotInclude}</@field>
          </@field>
          <@field type="select" label=uiLabelMap.AccountingPaymentStatus name="paymentStatusId">
              <#if findParams.paymentStatusId?has_content>
                  <#assign paymentStatusId = findParams.paymentStatusId>
                  <#assign currentPaymentStatus = delegator.findOne("StatusItem", {"statusId":paymentStatusId}, true)>
                  <@field type="option" value=paymentStatusId selected=true>${currentPaymentStatus.get("description", locale)}</@field>
                  <@field type="option" value=paymentStatusId>---</@field>
              </#if>
              <@field type="option" value="" selected=(!findParams.paymentStatusId?has_content)>${uiLabelMap.CommonAll}</@field>
              <#list paymentStatusList as paymentStatus>
                  <@field type="option" value=paymentStatus.statusId>${paymentStatus.get("description", locale)}</@field>
              </#list>
          </@field>
      </@fieldset>

        <input type="hidden" name="showAll" value="Y"/>
        <@field type="submit" text=uiLabelMap.CommonFind/>
    </@cell>
  </@row>    
</#if>
</@section>
<#--<input type="image" src="<@ofbizContentUrl>/images/spacer.gif</@ofbizContentUrl>" onclick="javascript:lookupOrders(true);"/>-->
</form>
<#if (findParams.hideFields!"N") != "Y">
<@script>
document.lookuporder.orderId.focus();
</@script>
</#if>

<#if (searchPerformed!false)==true>
  <@section title=uiLabelMap.CommonSearchResults>

  <#if lookupErrorMessage?has_content>
    <@alert type="alert">
      ${lookupErrorMessage}
    </@alert>
  </#if>    

  <#-- note: added this check here for simplicity but haven't removed old code inside; no harm, maybe reuse-->
  <#if orderList?has_content>
  
    <#assign paramStr = addParamsToStr(rawString(paramList!""), {"showAll": showAll!"", "hideFields": findParams.hideFields!"N", "doFindQuery": "Y"}, "&amp;", false)>
    <#-- forcePost required because search done from service event with https="true" -->
    <@paginate mode="content" url=makeOfbizUrl("searchorders") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!1 listSize=orderListSize!0 altParam=true forcePost=true viewIndexFirst=1>
   
    <form name="massOrderChangeForm" method="post" action="javascript:void(0);">
        <input type="hidden" name="screenLocation" value="component://order/widget/ordermgr/OrderPrintScreens.xml#OrderPDF"/>
        <#-- SCIPIO: new flag -->
        <input type="hidden" name="massOrderChangeSubmitted" value="Y" />
        
        <#-- SCIPIO: new flag -->
        <input type="hidden" name="doFindQuery" value="Y" />
        
      <#-- SCIPIO: WARN: here we make sure to pass the params through ?html and ?js_string
          In order to do this we must UNDO the ofbiz manual and screen encoding and then reencode. -->
      
      <#-- FIXME: temporarily forcing hide fields to false; there is an issue where
          the redirect after mass approve is unable to automatically retrigger the find;
          therefore we have to rely on the user to click "find" again until some sort of 
          workaround is found (controller limitation) -->
      <#--
      <#assign massParamList = "hideFields=" + rawString(findParams.hideFields!"N")>
      -->

      <#-- 2016-08-11: these are now included in form as POST
      <#assign massParamList = "hideFields=" + "N">
      <#if paramList?has_content>
        <#assign massParamList = massParamList + "&" + rawString(paramList)?replace("&amp;","&")>
      </#if>
      -->
      <#assign massParamList = "">
      <#assign massParamListJsHtml = massParamList?js_string?html>

        <#--
        <select name="serviceName" onchange="javascript:setServiceName(this);">
           <option value="javascript:void(0);">&nbsp;</option>
           <option value="<@ofbizUrl>massApproveOrders?${massParamListJsHtml}</@ofbizUrl>">${uiLabelMap.OrderApproveOrder}</option>
           <option value="<@ofbizUrl>massHoldOrders?${massParamListJsHtml}</@ofbizUrl>">${uiLabelMap.OrderHold}</option>
           <option value="<@ofbizUrl>massProcessOrders?${massParamListJsHtml}</@ofbizUrl>">${uiLabelMap.OrderProcessOrder}</option>
           <option value="<@ofbizUrl>massCancelOrders?${massParamListJsHtml}</@ofbizUrl>">${uiLabelMap.OrderCancelOrder}</option>
           <option value="<@ofbizUrl>massCancelRemainingPurchaseOrderItems?${massParamListJsHtml}</@ofbizUrl>">${uiLabelMap.OrderCancelRemainingPOItems}</option>
           <option value="<@ofbizUrl>massRejectOrders?${massParamListJsHtml}</@ofbizUrl>">${uiLabelMap.OrderRejectOrder}</option>
           <option value="<@ofbizUrl>massPickOrders?${massParamListJsHtml}</@ofbizUrl>">${uiLabelMap.OrderPickOrders}</option>
           <option value="<@ofbizUrl>massQuickShipOrders?${massParamListJsHtml}</@ofbizUrl>">${uiLabelMap.OrderQuickShipEntireOrder}</option>
           <option value="<@ofbizUrl>massPrintOrders?${massParamListJsHtml}</@ofbizUrl>">${uiLabelMap.CommonPrint}</option>
           <option value="<@ofbizUrl>massCreateFileForOrders?${massParamListJsHtml}</@ofbizUrl>">${uiLabelMap.ContentCreateFile}</option>
        </select>-->
        <#--
        <select name="printerName">
           <option value="javascript:void(0);">&nbsp;</option>
           <#list printers as printer>
           <option value="${printer}">${printer}</option>
           </#list>
        </select>
        <a href="javascript:runAction();" class="${styles.link_run_sys!} ${styles.action_modify!}">${uiLabelMap.OrderRunAction}</a>
        -->
      <#macro massOrderChangeButton id="1">  
          <@row>
            <@cell>
                <button href="#" data-dropdown="drop1" aria-controls="drop_${id!"1"}" aria-expanded="false" class="${styles.button!} ${styles.small!} ${styles.button_color_secondary!} ${styles.dropdown!}">${uiLabelMap.OrderRunAction}</button><br>
                <ul id="drop${id!"1"}" data-dropdown-content class="f-dropdown" aria-hidden="true" tabindex="-1">
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massApproveOrders?${massParamListJsHtml}</@ofbizUrl>')">${uiLabelMap.OrderApproveOrder}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massHoldOrders?${massParamListJsHtml}</@ofbizUrl>')">${uiLabelMap.OrderHold}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massProcessOrders?${massParamListJsHtml}</@ofbizUrl>')">${uiLabelMap.OrderProcessOrder}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massCancelOrders?${massParamListJsHtml}</@ofbizUrl>')">${uiLabelMap.OrderCancelOrder}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massCancelRemainingPurchaseOrderItems?${massParamListJsHtml}</@ofbizUrl>')">${uiLabelMap.OrderCancelRemainingPOItems}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massRejectOrders?${massParamListJsHtml}</@ofbizUrl>')">${uiLabelMap.OrderRejectOrder}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massPickOrders?${massParamListJsHtml}</@ofbizUrl>')">${uiLabelMap.OrderPickOrders}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massQuickShipOrders?${massParamListJsHtml}</@ofbizUrl>')">${uiLabelMap.OrderQuickShipEntireOrder}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massPrintOrders?${massParamListJsHtml}</@ofbizUrl>')">${uiLabelMap.CommonPrint}</a></li>
                   <li><a href="javascript:submitFindForm('<@ofbizUrl>massCreateFileForOrders?${massParamListJsHtml}</@ofbizUrl>')">${uiLabelMap.ContentCreateFile}</a></li>
                </ul>
            </@cell>  
          </@row>
      </#macro>
      <@massOrderChangeButton id="1"/>
      <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
       <@thead>
        <@tr class="header-row">
          <@th width="5%">
            <input type="checkbox" id="checkAllOrders" name="checkAllOrders" value="1" onchange="javascript:toggleOrderId(this);"/>
          </@th>
          <@th width="10%">${uiLabelMap.OrderOrderType}</@th>
          <@th width="15%">${uiLabelMap.OrderOrderId}</@th>
          <@th width="20%">${uiLabelMap.PartyName}</@th>
          <#--<@th width="5%" align="right">${uiLabelMap.OrderSurvey}</@th>-->
          <@th width="10%" align="right">${uiLabelMap.OrderItemsOrdered}</@th>
          <#--<@th width="5%" align="right">${uiLabelMap.OrderItemsBackOrdered}</@th>
          <@th width="5%" align="right">${uiLabelMap.OrderItemsReturned}</@th>-->
          <@th width="10%" align="right">${uiLabelMap.OrderRemainingSubTotal}</@th>
          <@th width="10%" align="right">${uiLabelMap.OrderOrderTotal}</@th>
          <@th width="5%">&nbsp;</@th>
            <#if ((findParams.filterInventoryProblems!"N") == "Y") || ((findParams.filterPOsOpenPastTheirETA!"N") == "Y") || ((findParams.filterPOsWithRejectedItems!"N") == "Y") || ((findParams.filterPartiallyReceivedPOs!"N") == "Y")>
              <@th width="10%">${uiLabelMap.CommonStatus}</@th>
              <@th width="5%">${uiLabelMap.CommonFilter}</@th>
            <#else>
              <@th width="15%">${uiLabelMap.CommonStatus}</@th>
            </#if>
          <@th width="10%">${uiLabelMap.OrderDate}</@th>
          <@th width="10%">${uiLabelMap.PartyPartyId}</@th>
          <@th width="10%">&nbsp;</@th>
        </@tr>
        </@thead>
        <@tbody>
          <#list orderList as orderHeader>
            <#assign orh = Static["org.ofbiz.order.order.OrderReadHelper"].getHelper(orderHeader)>
            <#assign statusItem = orderHeader.getRelatedOne("StatusItem", true)!>
            <#assign orderType = orderHeader.getRelatedOne("OrderType", true)!>
            <#if ((orderType.orderTypeId)!) == "PURCHASE_ORDER">
              <#assign displayParty = orh.getSupplierAgent()!>
            <#else>
              <#assign displayParty = orh.getPlacingParty()!>
            </#if>
            <#assign partyId = displayParty.partyId!("_NA_")>
            <@tr valign="middle">
              <@td>
                 <input type="checkbox" name="orderIdList" value="${orderHeader.orderId}" onchange="javascript:toggleOrderIdList();"/>
              </@td>
              <@td>${(orderType.get("description",locale)!(orderType.orderTypeId!""))!""}</@td>
              <@td><a href="<@ofbizUrl>orderview?orderId=${orderHeader.orderId}</@ofbizUrl>">${orderHeader.orderId}</a></@td>
              <@td>
                <div>
                  <#if displayParty?has_content>
                      <#assign displayPartyNameResult = dispatcher.runSync("getPartyNameForDate", {"partyId":displayParty.partyId, "compareDate":orderHeader.orderDate!, "userLogin":userLogin})/>
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
              </@td>
              <#--<@td align="right">${orh.hasSurvey()?string.number}</@td>-->
              <@td align="right">${orh.getTotalOrderItemsQuantity()?string.number}</@td>
              <#--<@td align="right">${orh.getOrderBackorderQuantity()?string.number}</@td>
              <@td align="right">${orh.getOrderReturnedQuantity()?string.number}</@td>-->
              <@td align="right"><#if orderHeader.remainingSubTotal?has_content><@ofbizCurrency amount=orderHeader.remainingSubTotal isoCode=orh.getCurrency()/></#if></@td>
              <@td align="right"><#if orderHeader.grandTotal?has_content><@ofbizCurrency amount=orderHeader.grandTotal isoCode=orh.getCurrency()/></#if></@td>

              <@td>&nbsp;</@td>
              <@td>${(statusItem.get("description",locale)!(statusItem.statusId!(uiLabelMap.CommonNA)))!""}</@td>
              <#if ((findParams.filterInventoryProblems!"N") == "Y") || ((findParams.filterPOsOpenPastTheirETA!"N") == "Y") || ((findParams.filterPOsWithRejectedItems!"N") == "Y") || ((findParams.filterPartiallyReceivedPOs!"N") == "Y")>
                  <@td>
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
                  </@td>
              </#if>
              <@td>${orderHeader.getString("orderDate")!}</@td>
              <@td>
                <#if partyId != "_NA_">
                  <a href="${customerDetailLink}${partyId}" class="${styles.link_nav_info_id!}">${partyId}</a>
                <#else>
                  ${uiLabelMap.CommonNA}
                </#if>
              </@td>
              <@td align='right'>
                <a href="<@ofbizUrl>orderview?orderId=${orderHeader.orderId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a>
              </@td>
            </@tr>
          </#list>
        </@tbody>
      </@table>
      <@massOrderChangeButton id="2"/>
      
      <#-- SCIPIO: add missing saving of prev query params -->
      ${prevQueryInputParams}
      <#--<input type="hidden" name="showAll" value="Y"/>-->
    </form>
    
    </@paginate>
    
  <#else>
    <@commonMsg type="result-norecord">${uiLabelMap.OrderNoOrderFound}.</@commonMsg>
  </#if>
    
  </@section>
</#if>

<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>
