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
function lookupShipments() {
    shipmentIdValue = document.lookupShipmentForm.shipmentId.value;
    if (shipmentIdValue.length > 1) {
        document.lookupShipmentForm.action = "<@ofbizUrl>EditShipment</@ofbizUrl>";
    } else {
        document.lookupShipmentForm.action = "<@ofbizUrl>FindShipment</@ofbizUrl>";
    }
    document.lookupShipmentForm.submit();
}
</@script>
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if requestParameters.facilityId?has_content>
    <@menuitem type="link" href=makeOfbizUrl("quickShipOrder?facilityId=${requestParameters.facilityId}") text=uiLabelMap.ProductQuickShipOrder class="+${styles.action_nav!} ${styles.action_send!}" />
  </#if>
    <@menuitem type="link" href=makeOfbizUrl("EditShipment") text=uiLabelMap.ProductNewShipment class="+${styles.action_nav!} ${styles.action_add!}" />
    <#--<@menuitem type="link" href="javascript:lookupShipments();" text=uiLabelMap.ProductFindShipment class="+${styles.action_nav!} ${styles.action_find!}" />-->
  </@menu>
</#macro>
<@section id="findOrders" menuContent=menuContent> <#-- title=uiLabelMap.ProductFindShipmentTitle -->
        <form method="post" name="lookupShipmentForm" action="<@ofbizUrl>FindShipment</@ofbizUrl>">
            <input type="hidden" name="lookupFlag" value="Y" />
              <@field type="input" label=uiLabelMap.ProductShipmentId name="shipmentId" value=(shipmentId!) />
              <@field type="select" label=uiLabelMap.ProductShipmentType name="shipmentTypeId">
                    <#if currentShipmentType?has_content>
                    <option value="${currentShipmentType.shipmentTypeId}">${currentShipmentType.get("description",locale)}</option>
                    <option value="${currentShipmentType.shipmentTypeId}">---</option>
                    </#if>
                    <option value="">${uiLabelMap.ProductAnyShipmentType}</option>
                    <#list shipmentTypes as shipmentType>
                      <option value="${shipmentType.shipmentTypeId}">${shipmentType.get("description",locale)}</option>
                    </#list>
              </@field>
              <@field type="select" label=uiLabelMap.ProductOriginFacility name="originFacilityId">
                    <#if currentOriginFacility?has_content>
                    <option value="${currentOriginFacility.facilityId}">${currentOriginFacility.facilityName} [${currentOriginFacility.facilityId}]</option>
                    <option value="${currentOriginFacility.facilityId}">---</option>
                    </#if>
                    <option value="">${uiLabelMap.ProductAnyFacility}</option>
                    <#list facilities as facility>
                      <option value="${facility.facilityId}">${facility.facilityName} [${facility.facilityId}]</option>
                    </#list>
              </@field>
              <@field type="select" label=uiLabelMap.ProductDestinationFacility name="destinationFacilityId">
                    <#if currentDestinationFacility?has_content>
                    <option value="${currentDestinationFacility.facilityId}">${currentDestinationFacility.facilityName} [${currentDestinationFacility.facilityId}]</option>
                    <option value="${currentDestinationFacility.facilityId}">---</option>
                    </#if>
                    <option value="">${uiLabelMap.ProductAnyFacility}</option>
                    <#list facilities as facility>
                      <option value="${facility.facilityId}">${facility.facilityName} [${facility.facilityId}]</option>
                    </#list>
              </@field>
              <@field type="select" label=uiLabelMap.CommonStatus name="statusId">
                    <#if currentStatus?has_content>
                    <option value="${currentStatus.statusId}">${currentStatus.get("description",locale)}</option>
                    <option value="${currentStatus.statusId}">---</option>
                    </#if>
                    <option value="">${uiLabelMap.ProductSalesShipmentStatus}</option>
                    <#list shipmentStatuses as shipmentStatus>
                      <option value="${shipmentStatus.statusId}">${shipmentStatus.get("description",locale)}</option>
                    </#list>
                    <option value="">---</option>
                    <option value="">${uiLabelMap.ProductPurchaseShipmentStatus}</option>
                    <#list purchaseShipmentStatuses as shipmentStatus>
                      <option value="${shipmentStatus.statusId}">${shipmentStatus.get("description",locale)}</option>
                    </#list>
                    <option value="">---</option>
                    <option value="">${uiLabelMap.ProductOrderReturnStatus}</option>
                    <#list returnStatuses as returnStatus>
                      <#if returnStatus.statusId != "RETURN_REQUESTED">
                        <option value="${returnStatus.statusId}">${returnStatus.get("description",locale)}</option>
                      </#if>
                    </#list>
              </@field>

              <@field type="generic" label=uiLabelMap.ProductDateFilter>
                  <@field type="datetime" label=uiLabelMap.CommonFrom name="minDate" value=(requestParameters.minDate!) size="25" maxlength="30" id="minDate1"/>
                  <@field type="datetime" label=uiLabelMap.CommonThru name="maxDate" value=(requestParameters.maxDate!) size="25" maxlength="30" id="maxDate1"/>
              </@field>
              
              <@field type="submit" submitType="link" href="javascript:lookupShipments();" class="+${styles.link_run_sys!} ${styles.action_find!}" text=uiLabelMap.ProductFindShipment />
        </form>
</@section>

<#if shipmentList??>
  <@section id="findOrders_2" title=uiLabelMap.ProductShipments>
    <#if shipmentList?has_content>  
      <#assign paramStr = addParamsToStr(rawString(paramList!""), {"lookupFlag": "Y"}, "&amp;", false)>
      <@paginate mode="content" url=makeOfbizUrl("FindShipment") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=shipmentList?size>
   
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" -->
        <@thead>
          <@tr class="header-row">
            <@th width="5%">${uiLabelMap.ProductShipmentId}</@th>
            <@th width="15%">${uiLabelMap.ProductShipmentType}</@th>
            <@th width="10%">${uiLabelMap.CommonStatus}</@th>
            <@th width="25%">${uiLabelMap.ProductOriginFacility}</@th>
            <@th width="25%">${uiLabelMap.ProductDestFacility}</@th>
            <@th width="15%">${uiLabelMap.ProductShipDate}</@th>
            <@th width="5%">&nbsp;</@th>
          </@tr>
        </@thead>
          <#list shipmentList as shipment>
            <#assign originFacility = delegator.findOne("Facility", {"facilityId":shipment.originFacilityId}, true)! />
            <#assign destinationFacility = delegator.findOne("Facility", {"facilityId":shipment.destinationFacilityId}, true)! />
            <#assign statusItem = delegator.findOne("StatusItem", {"statusId":shipment.statusId}, true)!/>
            <#assign shipmentType = delegator.findOne("ShipmentType", {"shipmentTypeId":shipment.shipmentTypeId}, true)!/>
            <@tr>
              <@td><a href="<@ofbizUrl>EditShipment?shipmentId=${shipment.shipmentId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${shipment.shipmentId}</a></@td>
              <@td>${(shipmentType.get("description",locale))?default(shipmentType.shipmentTypeId?default(""))}</@td>
              <@td>${(statusItem.get("description",locale))?default(statusItem.statusId!(uiLabelMap.CommonNA))}</@td>
              <@td>${(originFacility.facilityName)!} [${shipment.originFacilityId!}]</@td>
              <@td>${(destinationFacility.facilityName)!} [${shipment.destinationFacilityId!}]</@td>
              <@td>${(shipment.estimatedShipDate.toString())!}</@td>
              <@td>
                <a href="<@ofbizUrl>EditShipment?shipmentId=${shipment.shipmentId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a>
              </@td>
            </@tr>
          </#list>
        </@table>
      </@paginate>
    <#else>
      <@commonMsg type="result-norecord">${uiLabelMap.ProductNoShipmentsFound}.</@commonMsg>
    </#if>  
  </@section>
</#if>
