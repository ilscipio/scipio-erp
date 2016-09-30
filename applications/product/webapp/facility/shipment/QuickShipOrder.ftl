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
// function called from ShipmentScaleApplet when a weight is read
function setWeight(weight) {
  document.weightForm.weight.value = weight;
}
</@script>

<#if security.hasEntityPermission("FACILITY", "_VIEW", session)>
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl("quickShipOrder?facilityId=${facilityId}") text=uiLabelMap.ProductNextShipment class="+${styles.action_run_sys!} ${styles.action_continue!}"/>
    <#if shipment?has_content>
      <@menuitem type="link" href=makeOfbizUrl("EditShipment?shipmentId=${shipmentId}") text=uiLabelMap.ProductEditShipment class="+${styles.action_nav!} ${styles.action_update!}"/>
    </#if>
  </@menu>
</#macro>
<@section title="${rawString(uiLabelMap.ProductQuickShipOrderFrom)} ${rawString(facility.facilityName!)} [${rawString(uiLabelMap.CommonId)}:${rawString(facilityId!)}]">

  <#if shipment??>
    <#if 1 < shipmentPackages.size()>
      <#-- multiple packages -->
      <@alert type="info">${uiLabelMap.ProductMorePackageFoundShipment}.</@alert>
    <#else>
      <#-- single package -->
      <#assign shipmentPackage = (Static["org.ofbiz.entity.util.EntityUtil"].getFirst(shipmentPackages))!>
      <#if shipmentPackage?has_content>
        <#assign weight = (shipmentPackage.weight)?default(0.00)>
        <#if (0 < weight?double) && !requestParameters.reweigh??>
          <@section>
          <#if (1 < shipmentRoutes.size())>
            <#-- multiple routes -->
            <@alert type="info">${uiLabelMap.ProductMoreRouteSegmentFound}.</@alert>
          <#elseif !requestParameters.shipmentRouteSegmentId?? || requestAttributes._ERROR_MESSAGE_??>
            <form name="routeForm" method="post" action="<@ofbizUrl>setQuickRouteInfo</@ofbizUrl>">
            <@fields type="default-manual">
              <#assign shipmentRoute = (Static["org.ofbiz.entity.util.EntityUtil"].getFirst(shipmentRoutes))!>
              <#assign carrierPerson = (shipmentRoute.getRelatedOne("CarrierPerson", false))!>
              <#assign carrierPartyGroup = (shipmentRoute.getRelatedOne("CarrierPartyGroup", false))!>
              <#assign shipmentMethodType = (shipmentRoute.getRelatedOne("ShipmentMethodType", false))!>
              <input type="hidden" name="facilityId" value="${facilityId!}"/>
              <input type="hidden" name="shipmentId" value="${shipmentRoute.shipmentId}"/>
              <input type="hidden" name="shipmentRouteSegmentId" value="${shipmentRoute.shipmentRouteSegmentId}"/>
            <@row>
              <@cell columns=9>
                <@field type="select" label=uiLabelMap.ProductCarrier name="carrierPartyId">
                      <#if shipmentRoute.carrierPartyId?has_content>
                        <option value="${shipmentRoute.carrierPartyId}">${(carrierPerson.firstName)!} ${(carrierPerson.middleName)!} ${(carrierPerson.lastName)!} ${(carrierPartyGroup.groupName)!} [${shipmentRoute.carrierPartyId}]</option>
                        <option value="${shipmentRoute.carrierPartyId}">---</option>
                      <#else>
                        <option value="">&nbsp;</option>
                      </#if>
                      <#list carrierPartyDatas as carrierPartyData>
                        <option value="${carrierPartyData.party.partyId}">${(carrierPartyData.person.firstName)!} ${(carrierPartyData.person.middleName)!} ${(carrierPartyData.person.lastName)!} ${(carrierPartyData.partyGroup.groupName)!} [${carrierPartyData.party.partyId}]</option>
                      </#list>
                </@field>
              </@cell>
              <@cell columns=3>
                <@field type="submit" submitType="link" href="javascript:document.routeForm.submit();" class="+${styles.link_run_sys!} ${styles.action_updatestatus!}" text=uiLabelMap.ProductConfirmShipmentUps />
              </@cell>
            </@row>
            <@row>
              <@cell columns=9>
                <@field type="select" label=uiLabelMap.ProductShipMethod name="shipmentMethodTypeId">
                      <#if shipmentMethodType?has_content>
                        <option value="${shipmentMethodType.shipmentMethodTypeId}">${shipmentMethodType.get("description",locale)}</option>
                        <option value="${shipmentMethodType.shipmentMethodTypeId}">---</option>
                      <#else>
                        <option value="">&nbsp;</option>
                      </#if>
                      <#list shipmentMethodTypes as shipmentMethodTypeOption>
                        <option value="${shipmentMethodTypeOption.shipmentMethodTypeId}">${shipmentMethodTypeOption.get("description",locale)}</option>
                      </#list>
                </@field>
              </@cell>
              <@cell columns=3>
                <@field type="submit" submitType="link" href=makeOfbizUrl("quickShipOrder?facilityId=${facilityId}&amp;shipmentId=${shipmentId}&amp;reweigh=Y") class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.ProductReWeighPackage />
              </@cell>
            </@row>
            <@row>
              <@cell columns=9>
                &nbsp;
              </@cell>
              <@cell columns=3>
                <@field type="submit" submitType="image" src=makeOfbizContentUrl("/images/spacer.gif") onClick="javascript:document.routeForm.submit();" />
              </@cell>
            </@row>
            </@fields>
            </form>
            <@script>
              document.routeForm.carrierPartyId.focus();
            </@script>
          <#else>
            <#-- display the links for label/packing slip -->
            <#assign allDone = "yes">
            <center>
              <a href="<@ofbizUrl>viewShipmentPackageRouteSegLabelImage?shipmentId=${requestParameters.shipmentId}&amp;shipmentRouteSegmentId=${requestParameters.shipmentRouteSegmentId}&amp;shipmentPackageSeqId=00001</@ofbizUrl>" target="_blank" class="${styles.link_run_sys!} ${styles.action_export!}">${uiLabelMap.ProductShippingLabel}</a><br />
              <a href="<@ofbizUrl>ShipmentManifest.pdf?shipmentId=${requestParameters.shipmentId}&amp;shipmentRouteSegmentId=${requestParameters.shipmentRouteSegmentId}</@ofbizUrl>" target="_blank" class="${styles.link_run_sys!} ${styles.action_export!}">${uiLabelMap.ProductPackingSlip}</a>
            </center>
          </#if>
          </@section>
        <#else>
          <@section>
          <form name="weightForm" method="post" action="<@ofbizUrl>setQuickPackageWeight</@ofbizUrl>">
            <#assign weightUom = shipmentPackage.getRelatedOne("WeightUom", false)!>
            <input type="hidden" name="facilityId" value="${facilityId!}"/>
            <input type="hidden" name="shipmentId" value="${shipmentPackage.shipmentId}"/>
            <input type="hidden" name="shipmentPackageSeqId" value="${shipmentPackage.shipmentPackageSeqId}"/>
              <@field type="generic" label="${rawString(uiLabelMap.ProductPackage)} ${rawString(shipmentPackage.shipmentPackageSeqId)} ${rawString(uiLabelMap.ProductWeight)}">
                  <@field type="input" name="weight" />
                  <@field type="select" name="weightUomId">
                    <#if weightUom?has_content>
                      <option value="${weightUom.uomId}">${weightUom.get("description",locale)}</option>
                      <option value="${weightUom.uomId}">---</option>
                    </#if>
                    <#list weightUomList as weightUomOption>
                      <option value="${weightUomOption.uomId}">${weightUomOption.get("description",locale)} [${weightUomOption.abbreviation}]</option>
                    </#list>
                  </@field>
              </@field>
              <@field type="submitarea">
                  <@field type="submit" submitType="image" src=makeOfbizContentUrl("/images/spacer.gif") onClick="javascript:document.weightForm.submit();"/>
                  <@field type="submit" submitType="link" href="javascript:document.weightForm.submit();" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.ProductSetWeight />
              </@field>
          </form>
          <@script>
            document.weightForm.weight.focus();
          </@script>
          <#-- todo embed the applet
          <applet code="ShipmentScaleApplet.class" codebase="/images/" name="Package Weight Reader" width="0" height="0" MAYSCRIPT>
            <param name="serialPort" value="com1">
            <param name="fakeWeight" value="22">
          </applet>
          -->
          </@section>
        </#if>
      <#else>
        <div class="alert">${uiLabelMap.ProductErrorNoPackagesFoundForShipment} !</div>
      </#if>
      <hr />
      ${pages.get("/shipment/ViewShipmentInfo.ftl")}
      <br />${pages.get("/shipment/ViewShipmentItemInfo.ftl")}
      <br />${pages.get("/shipment/ViewShipmentPackageInfo.ftl")}
      <#if allDone?default("no") == "yes">
        <br />${pages.get("/shipment/ViewShipmentRouteInfo.ftl")}
      </#if>
    </#if>
  <#else>
    <@section>
    <form name="selectOrderForm" method="post" action="<@ofbizUrl>createQuickShipment</@ofbizUrl>">
      <input type="hidden" name="facilityId" value="${facilityId!}" />
      <input type="hidden" name="originFacilityId" value="${facilityId!}" />
      <input type="hidden" name="setPackedOnly" value="Y" />
        <@field type="input" label=uiLabelMap.ProductOrderNumber name="orderId" size="20" maxlength="20" value=(requestParameters.orderId!) />
        <@field type="submitarea">
            <@field type="submit" submitType="image" src=makeOfbizContentUrl("/images/spacer.gif") onClick="javascript:document.selectOrderForm.submit();" />
            <@field type="submit" submitType="link" href="javascript:document.selectOrderForm.submit();" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.ProductShipOrder />
        </@field>
    </form>
    <@script>
        document.selectOrderForm.orderId.focus();
    </@script>
    </@section>
  </#if>
</@section>
</#if>
