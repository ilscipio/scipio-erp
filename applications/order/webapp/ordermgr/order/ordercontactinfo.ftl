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

<#macro updateOrderContactMech orderHeader contactMechTypeId contactMechList contactMechPurposeTypeId contactMechAddress>
  <#if (!orderHeader.statusId.equals("ORDER_COMPLETED")) && !(orderHeader.statusId.equals("ORDER_REJECTED")) && !(orderHeader.statusId.equals("ORDER_CANCELLED"))>
    <@modal label="(${uiLabelMap.CommonEdit})" id="modal_updateOrderContactMech_${contactMechTypeId}">
        <form name="updateOrderContactMech" method="post" action="<@ofbizUrl>updateOrderContactMech</@ofbizUrl>">
          <input type="hidden" name="orderId" value="${orderId!}" />
          <input type="hidden" name="contactMechPurposeTypeId" value="${contactMechPurpose.contactMechPurposeTypeId!}" />
          <input type="hidden" name="oldContactMechId" value="${contactMech.contactMechId!}" />
          <@row>
                <@cell columns=6>
                      <select name="contactMechId">
                        <#if contactMech.contactMechTypeId == "POSTAL_ADDRESS">
                          <option value="${contactMechAddress.contactMechId}">${(contactMechAddress.address1)!""} - ${contactMechAddress.city!""}</option>
                          <option value="${contactMechAddress.contactMechId}"></option>
                          <#list contactMechList as contactMech>
                            <#assign postalAddress = contactMech.getRelatedOne("PostalAddress", false)! />
                            <#assign partyContactPurposes = postalAddress.getRelated("PartyContactMechPurpose", null, null, false)! />
                            <#list partyContactPurposes as partyContactPurpose>
                              <#if contactMech.contactMechId?has_content && partyContactPurpose.contactMechPurposeTypeId == contactMechPurposeTypeId>
                                <option value="${contactMech.contactMechId!}">${(postalAddress.address1)!""} - ${postalAddress.city!""}</option>
                              </#if>
                            </#list>
                          </#list>
                        <#elseif contactMech.contactMechTypeId == "TELECOM_NUMBER">
                          <option value="${contactMechAddress.contactMechId}">${contactMechAddress.countryCode!} <#if contactMechAddress.areaCode??>${contactMechAddress.areaCode}-</#if>${contactMechAddress.contactNumber}</option>
                          <option value="${contactMechAddress.contactMechId}"></option>
                          <#list contactMechList as contactMech>
                             <#assign telecomNumber = contactMech.getRelatedOne("TelecomNumber", false)! />
                             <#assign partyContactPurposes = telecomNumber.getRelated("PartyContactMechPurpose", null, null, false)! />
                             <#list partyContactPurposes as partyContactPurpose>
                               <#if contactMech.contactMechId?has_content && partyContactPurpose.contactMechPurposeTypeId == contactMechPurposeTypeId>
                                  <option value="${contactMech.contactMechId!}">${telecomNumber.countryCode!} <#if telecomNumber.areaCode??>${telecomNumber.areaCode}-</#if>${telecomNumber.contactNumber}</option>
                               </#if>
                             </#list>
                          </#list>
                        <#elseif contactMech.contactMechTypeId == "EMAIL_ADDRESS">
                          <option value="${contactMechAddress.contactMechId}">${(contactMechAddress.infoString)!""}</option>
                          <option value="${contactMechAddress.contactMechId}"></option>
                          <#list contactMechList as contactMech>
                             <#assign partyContactPurposes = contactMech.getRelated("PartyContactMechPurpose", null, null, false)! />
                             <#list partyContactPurposes as partyContactPurpose>
                               <#if contactMech.contactMechId?has_content && partyContactPurpose.contactMechPurposeTypeId == contactMechPurposeTypeId>
                                  <option value="${contactMech.contactMechId!}">${contactMech.infoString!}</option>
                               </#if>
                             </#list>
                          </#list>
                        </#if>
                      </select>
                </@cell>
                <@cell columns=6>
                    <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}" /> 
                </@cell>
            </@row>  
        </form>
    </@modal>
  </#if>
</#macro>

<#if displayParty?has_content || orderContactMechValueMaps?has_content>
 <#-- The usefulness of this information is limited. Uncomment and add as menuContent to section in order to add these functions back in
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("orderentry?partyId=${partyId}&orderTypeId=${orderHeader.orderTypeId}") text=uiLabelMap.OrderNewOrder class="+${styles.action_nav!} ${styles.action_view!}" />
        <@menuitem type="link" href="javascript:document.searchOtherOrders.submit()" text=uiLabelMap.OrderOtherOrders class="+${styles.action_nav!} ${styles.action_find!}" />
    </@menu>
  </#macro>
  -->  
  <@section title=uiLabelMap.OrderContactInformation>
      <@table type="fields"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
        <#-- the setting of shipping method is only supported for sales orders at this time -->
        <@tr>
          <@td class="${styles.grid_large!}3">${uiLabelMap.CommonName}</@td>
          <@td colspan="3">
                <#if partyId?? && displayParty?has_content>
                    <#assign displayPartyNameResult = dispatcher.runSync("getPartyNameForDate", {"partyId":displayParty.partyId, "compareDate":orderHeader.orderDate, "userLogin":userLogin})/>
                    <a href="${customerDetailLink}${partyId}${rawString(externalKeyParam)}" target="partymgr" class="">${displayPartyNameResult.fullName?default("[${uiLabelMap.OrderPartyNameNotFound}]")}</a>
                    <#else>
                    <a href="${customerDetailLink}${partyId}${rawString(externalKeyParam)}" target="partymgr" class="">${partyId}</a>
                </#if>
                <#--
                <#if (orderHeader.salesChannelEnumId)?? && orderHeader.salesChannelEnumId != "POS_SALES_CHANNEL">
                  <form name="searchOtherOrders" method="post" action="<@ofbizUrl>searchorders</@ofbizUrl>">
                    <input type="hidden" name="lookupFlag" value="Y"/>
                    <input type="hidden" name="hideFields" value="Y"/>
                    <input type="hidden" name="partyId" value="${partyId}" />
                    <input type="hidden" name="viewIndex" value="1"/>
                    <input type="hidden" name="viewSize" value="20"/>
                  </form>
                </#if>-->
          </@td>
        </@tr>
        <#list shipGroups as shipGroup>
        <#assign shipGroupShipments = shipGroup.getRelated("PrimaryShipment", null, null, false)>
           <#if shipGroupShipments?has_content>
              <@tr>
                <@td scope="row" class="${styles.grid_large!}3">
                  ${uiLabelMap.FacilityShipments}
                </@td>
                <@td>
                    <#list shipGroupShipments as shipment>
                          <@row>
                            <@cell columns=6>
                          ${uiLabelMap.CommonNbr} <a href="<@ofbizInterWebappUrl>/facility/control/EditShipment?shipmentId=${shipment.shipmentId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${shipment.shipmentId}</a>
                                                          (<a target="_BLANK" href="<@ofbizInterWebappUrl>/facility/control/PackingSlip.pdf?shipmentId=${shipment.shipmentId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!} ${styles.action_export!}">${uiLabelMap.ProductPackingSlip}</a>)
                          </@cell>
                        </@row>
                        <#if "SALES_ORDER" == orderHeader.orderTypeId && "ORDER_COMPLETED" == orderHeader.statusId>
                            <#assign shipmentRouteSegments = delegator.findByAnd("ShipmentRouteSegment", {"shipmentId" : shipment.shipmentId}, null, false)>
                            <#if shipmentRouteSegments?has_content>
                            <@row>
                            <@cell columns=6>
                              <hr/>
                              <#assign shipmentRouteSegment = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(shipmentRouteSegments)>
                              <#if "UPS" == ((shipmentRouteSegment.carrierPartyId)!)>
                                <a href="javascript:document.upsEmailReturnLabel${shipment_index}.submit();" class="${styles.link_nav_info_id!} ${styles.action_send!}">${uiLabelMap.ProductEmailReturnShippingLabelUPS}</a>
                              </#if>
                              <form name="upsEmailReturnLabel${shipment_index}" method="post" action="<@ofbizUrl>upsEmailReturnLabelOrder</@ofbizUrl>">
                                <input type="hidden" name="orderId" value="${orderId}"/>
                                <input type="hidden" name="shipmentId" value="${shipment.shipmentId}"/>
                                <input type="hidden" name="shipmentRouteSegmentId" value="${shipmentRouteSegment.shipmentRouteSegmentId}" />
                              </form>
                              </@cell>
                            </@row>
                            </#if>
                          </#if>
                    </#list>
                </@td>
              </@tr>
           </#if>
        </#list> 
        <#list orderContactMechValueMaps as orderContactMechValueMap>
          <#assign contactMech = orderContactMechValueMap.contactMech>
          <#assign contactMechPurpose = orderContactMechValueMap.contactMechPurposeType>
          <@tr>
            <@td class="${styles.grid_large!}3">
              ${contactMechPurpose.get("description",locale)}
            </@td>
            <@td colspan="3">
              <#if contactMech.contactMechTypeId == "POSTAL_ADDRESS">
                <#assign postalAddress = orderContactMechValueMap.postalAddress>
                <#if postalAddress?has_content>
                     <#assign dummy = setContextField("postalAddress", postalAddress)>
                     <@render resource="component://party/widget/partymgr/PartyScreens.xml#postalAddressHtmlFormatter" />
                  <@updateOrderContactMech orderHeader=(orderHeader!) contactMechTypeId=contactMech.contactMechTypeId contactMechList=(postalContactMechList!) contactMechPurposeTypeId=(contactMechPurpose.contactMechPurposeTypeId!) contactMechAddress=(postalAddress!) />
                </#if>
              <#elseif contactMech.contactMechTypeId == "TELECOM_NUMBER">
                <#assign telecomNumber = orderContactMechValueMap.telecomNumber>
                  ${telecomNumber.countryCode!}
                  <#if telecomNumber.areaCode??>${telecomNumber.areaCode}-</#if>${telecomNumber.contactNumber}
                  <#--<#if partyContactMech.extension??>ext&nbsp;${partyContactMech.extension}</#if>-->
                  <#if !telecomNumber.countryCode?? || telecomNumber.countryCode == "011" || telecomNumber.countryCode == "1">
                    <a target="_blank" href="${uiLabelMap.CommonLookupAnywhoLink}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupAnywho}</a>
                   <a target="_blank" href="${uiLabelMap.CommonLookupWhitepagesTelNumberLink}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupWhitepages}</a>
                  </#if>
                <@updateOrderContactMech orderHeader=(orderHeader!) contactMechTypeId=contactMech.contactMechTypeId contactMechList=(telecomContactMechList!) contactMechPurposeTypeId=(contactMechPurpose.contactMechPurposeTypeId!) contactMechAddress=(telecomNumber!) />
              <#elseif contactMech.contactMechTypeId == "EMAIL_ADDRESS">
                ${contactMech.infoString} <@updateOrderContactMech orderHeader=(orderHeader!) contactMechTypeId=contactMech.contactMechTypeId contactMechList=(emailContactMechList!) contactMechPurposeTypeId=(contactMechPurpose.contactMechPurposeTypeId!) contactMechAddress=(contactMech!) />
                  
                  <#-- ToDo: Validate usefulness
                      <#if security.hasEntityPermission("ORDERMGR", "_SEND_CONFIRMATION", session)>
                         <a href="<@ofbizUrl>confirmationmailedit?orderId=${orderId}&amp;partyId=${partyId}&amp;sendTo=${contactMech.infoString}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_update!}" >${uiLabelMap.OrderSendConfirmationEmail}</a>
                      <#else>
                         <a href="mailto:${contactMech.infoString}" class="${styles.link_run_sys!} ${styles.action_send!} ${styles.action_external!}">(${uiLabelMap.OrderSendEmail})</a>
                      </#if>
                  -->
              <#elseif contactMech.contactMechTypeId == "WEB_ADDRESS">
                  ${contactMech.infoString}
                  <#assign openString = contactMech.infoString>
                  <#if !openString?starts_with("http") && !openString?starts_with("HTTP")>
                    <#assign openString = "http://" + openString>
                  </#if>
                  <a target="_blank" href="${openString}" class="${styles.link_nav!} ${styles.action_view!} ${styles.action_external}">(open&nbsp;page&nbsp;in&nbsp;new&nbsp;window)</a>
              <#else>
                  ${contactMech.infoString!}
              </#if>
            </@td>
          </@tr>
        </#list>
      </@table>
  </@section>
</#if>
