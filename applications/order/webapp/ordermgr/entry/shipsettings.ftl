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
      jQuery(document).ready(function(){
        jQuery('#shipToSameParty, #shipToOtherParty').change(function(){
            if(jQuery('#shipToSameParty').is(':checked')){
                jQuery('#shipToParty').val("");
            }
            if(jQuery('#shipToOtherParty').is(':checked')){
                jQuery('#shipToParty').val("${shipToPartyId!}");
            }
        })
      });
</script>
<#if security.hasEntityPermission("ORDERMGR", "_CREATE", session) || security.hasEntityPermission("ORDERMGR", "_PURCHASE_CREATE", session)>

<#-- Purchase Orders -->
<@section>
<#if facilityMaps??>
            <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="checkoutsetupform">
            <input type="hidden" name="finalizeMode" value="ship"/>
            <input type="hidden" name="shipToPartyId" id="shipToParty"/>
            <#if (cart.getShipGroupSize() > 1)>
            <input type="hidden" name="finalizeReqShipGroups" value="true"/>
            </#if>
          
        <@menu type="button">
          <@menuitem type="link" href=makeOfbizUrl("setShipping?createNewShipGroup=Y") text=uiLabelMap.OrderCreateShipGroup class="+${styles.action_run_session!} ${styles.action_add!}" />
        </@menu>         

<#list 1..cart.getShipGroupSize() as currIndex>
<#assign shipGroupIndex = currIndex - 1>
    <@section title="${rawLabel('OrderShipGroup')} ${rawLabel('CommonNbr')} ${currIndex}">
        <@row>
            <@cell columns=6>
            <@table type="data-complex"> <#-- orig: class="basic-table" -->
                <#assign i = 0>
                <#assign shipGroup = cart.getShipInfo(shipGroupIndex)>
                <#list facilityMaps as facilityMap>
                <#assign facility = facilityMap.facility>
                <#assign facilityContactMechList = facilityMap.facilityContactMechList>
                <@tr>
                  <@td colspan="3">${uiLabelMap.FacilityFacility}: ${facility.facilityName!} [${facility.facilityId}]</@td>
                </@tr>
                <#-- company postal addresses -->

                <#if facilityContactMechList?has_content>
                <#list facilityContactMechList as shippingContactMech>
                  <#if shippingContactMech.postalAddress??>
                  <#assign shippingAddress = shippingContactMech.postalAddress>
                  <@tr>
                    <@td class="${styles.grid_large!}3">
                      <#assign checked='' />
                      <#if shipGroup?has_content && (shipGroup.getFacilityId()?has_content && shipGroup.getFacilityId() == facility.facilityId) && (shipGroup.getContactMechId()?has_content && shipGroup.getContactMechId() == shippingAddress.contactMechId) && !shipToPartyShippingContactMechList?has_content>
                          <#assign checked='checked' />
                      <#elseif i == 0 && !shipToPartyShippingContactMechList?has_content>
                          <#assign checked='checked' />
                      </#if>
                      <input type="radio" id="shipToSameParty" name="${shipGroupIndex!"0"}_shipping_contact_mech_id" value="${shippingAddress.contactMechId}_@_${facility.facilityId}" ${checked} />
                    </@td>
                    <@td>
                        <#if shippingAddress.toName?has_content><b>${uiLabelMap.CommonTo}:</b>&nbsp;${shippingAddress.toName}<br /></#if>
                        <#if shippingAddress.attnName?has_content><b>${uiLabelMap.CommonAttn}:</b>&nbsp;${shippingAddress.attnName}<br /></#if>
                        <#if shippingAddress.address1?has_content>${shippingAddress.address1}<br /></#if>
                        <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br /></#if>
                        <#if shippingAddress.city?has_content>${shippingAddress.city}</#if>
                        <#if shippingAddress.stateProvinceGeoId?has_content><br />${shippingAddress.stateProvinceGeoId}</#if>
                        <#if shippingAddress.postalCode?has_content><br />${shippingAddress.postalCode}</#if>
                        <#if shippingAddress.countryGeoId?has_content><br />${shippingAddress.countryGeoId}</#if>
                    </@td>
                    <@td><a href="<@ofbizInterWebappUrl>/facility/control/EditContactMech?facilityId=${facility.facilityId}&amp;contactMechId=${shippingAddress.contactMechId}</@ofbizInterWebappUrl>" target="_blank" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a></@td>
                  </@tr>
                  <#if shippingContactMech_has_next>
                  <@tr type="util"><@td colspan="4"><hr /></@td></@tr>
                  </#if>
                  </#if>
                  <#assign i = i + 1>
                </#list>
                <#else>
                  <@tr>
                    <@td>${uiLabelMap.CommonNoContactInformationOnFile}
                    </@td>
                    <@td></@td>
                    <@td>
                        <a href="<@ofbizInterWebappUrl>/facility/control/EditContactMech?facilityId=${facility.facilityId}&amp;preContactMechTypeId=POSTAL_ADDRESS</@ofbizInterWebappUrl>" target="_blank" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.CommonNew}</a>
                    </@td>
                  </@tr>
                </#if>
                </#list>
            </@table>
            </@cell>
        </@row>
    </@section>
</#list>

<#-- Foundation: New in OFbiz 14.12 branch (was outside list, as extra rows in a table; now need new table) -->
<#if shipToPartyShippingContactMechList?has_content>  
  <@section title=uiLabelMap.OrderShipToAnotherParty>
    <@row>
      <@cell columns=6>
        <@table type="data-complex"> <#-- orig: class="basic-table" -->
        
          <@tr><@td colspan="3">${uiLabelMap.OrderShipToAnotherParty}: <b>${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(shipToParty)}</b></@td></@tr>
          <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
          <#list shipToPartyShippingContactMechList as shippingContactMech>
            <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
            <@tr>
              <@td class="${styles.grid_large!}3">
                <input type="radio" id="shipToOtherParty" name="${shipGroupIndex!"0"}_shipping_contact_mech_id" value="${shippingAddress.contactMechId}"s/>
              </@td>
              <@td class="${styles.grid_large!}6">
                  <#if shippingAddress.toName?has_content><b>${uiLabelMap.CommonTo}:</b>&nbsp;${shippingAddress.toName}<br /></#if>
                  <#if shippingAddress.attnName?has_content><b>${uiLabelMap.CommonAttn}:</b>&nbsp;${shippingAddress.attnName}<br /></#if>
                  <#if shippingAddress.address1?has_content>${shippingAddress.address1}<br /></#if>
                  <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br /></#if>
                  <#if shippingAddress.city?has_content>${shippingAddress.city}</#if>
                  <#if shippingAddress.stateProvinceGeoId?has_content><br />${shippingAddress.stateProvinceGeoId}</#if>
                  <#if shippingAddress.postalCode?has_content><br />${shippingAddress.postalCode}</#if>
                  <#if shippingAddress.countryGeoId?has_content><br />${shippingAddress.countryGeoId}</#if>
                </@td>
              <@td><a href="<@ofbizInterWebappUrl>/partymgr/control/editcontactmech?partyId=${orderParty.partyId}&amp;contactMechId=${shippingContactMech.contactMechId}</@ofbizInterWebappUrl>" target="_blank" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a></@td>
            </@tr>
            <#if shippingContactMech_has_next>
              <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            </#if>
          </#list>
          
        </@table>
      </@cell>
    </@row>
  </@section> 
</#if>



            </form>

<#else>

<#-- Sales Orders -->

            <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="checkoutsetupform">
            <input type="hidden" name="finalizeMode" value="ship"/>
            <input type="hidden" name="shipToPartyId" id="shipToParty"/>
            <#if (cart.getShipGroupSize() > 1)>
            <input type="hidden" name="finalizeReqShipGroups" value="true"/>
            </#if>
            
    <@menu type="button">
        <@menuitem type="link" href=makeOfbizUrl("setShipping?createNewShipGroup=Y") text="${rawLabel('CommonNew')} ${rawLabel('OrderShipGroup')}" class="+${styles.action_run_session!} ${styles.action_add!}" />
        <@menuitem type="link" href=makeOfbizUrl("EditShipAddress") text=uiLabelMap.OrderCreateShippingAddress class="+${styles.action_nav!} ${styles.action_add!}" />
    </@menu> 
    
<#list 1..cart.getShipGroupSize() as currIndex>
<#assign shipGroupIndex = currIndex - 1>

<#assign currShipContactMechId = cart.getShippingContactMechId(shipGroupIndex)!>
<#assign supplierPartyId = cart.getSupplierPartyId(shipGroupIndex)!>
<#assign facilityId = cart.getShipGroupFacilityId(shipGroupIndex)!>
    <@section title="${rawLabel('OrderShipGroup')} ${rawLabel('CommonNbr')} ${currIndex}">
        <@row>
        <@cell columns=6>
            <@table type="data-complex"> <#-- orig: class="basic-table" -->
              <@tr>
                <@td class="${styles.grid_large!}3">${uiLabelMap.PartySupplier}</@td>
                <@td class="${styles.grid_large!}6">
                      <select name="${shipGroupIndex!"0"}_supplierPartyId">
                        <option value=""></option>
                        <#list suppliers as supplier>
                          <option value="${supplier.partyId}"<#if supplierPartyId??><#if supplier.partyId == supplierPartyId> selected="selected"</#if></#if>>${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(supplier, true)}</option>
                        </#list>
                      </select>
                 </@td>
                 <@td></@td>
              </@tr>
              <@tr>
                <@td class="${styles.grid_large!}3">${uiLabelMap.ProductReserveInventoryFromFacility}</@td>
                <@td class="${styles.grid_large!}6">
                      <select name="${shipGroupIndex!"0"}_shipGroupFacilityId">
                        <option value=""></option>
                        <#list productStoreFacilities as productStoreFacility>
                          <#assign facility = productStoreFacility.getRelatedOne("Facility", false)>
                          <option value="${productStoreFacility.facilityId}"<#if facilityId??><#if productStoreFacility.facilityId == facilityId> selected="selected"</#if></#if>>${facility.facilityName!} </option>
                        </#list>
                      </select>
                </@td>
                <@td></@td>              
              </@tr>
            <#if shippingContactMechList?has_content>
                <#assign i = 0>
                <#list shippingContactMechList as shippingContactMech>
                  <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
                  <#if currShipContactMechId?? && currShipContactMechId?has_content>
                      <#if currShipContactMechId == shippingContactMech.contactMechId>
                        <#assign checkedValue = "checked=\"checked\"">
                      <#else>
                        <#assign checkedValue = "">
                      </#if>
                  <#else>
                      <#if i == 0>
                          <#assign checkedValue = "checked=\"checked\"">
                      <#else>
                          <#assign checkedValue = "">
                      </#if>
                  </#if>
                  <@tr>
                    <@td class="${styles.grid_large!}3">
                      <input type="radio" id="shipToSameParty" name="${shipGroupIndex!"0"}_shipping_contact_mech_id" value="${shippingAddress.contactMechId}" ${checkedValue} />
                    </@td>
                    <@td class="${styles.grid_large!}6">
                        <#if shippingAddress.toName?has_content><b>${uiLabelMap.CommonTo}:</b>&nbsp;${shippingAddress.toName}<br /></#if>
                        <#if shippingAddress.attnName?has_content><b>${uiLabelMap.CommonAttn}:</b>&nbsp;${shippingAddress.attnName}<br /></#if>
                        <#if shippingAddress.address1?has_content>${shippingAddress.address1}<br /></#if>
                        <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br /></#if>
                        <#if shippingAddress.city?has_content>${shippingAddress.city}</#if>
                        <#if shippingAddress.stateProvinceGeoId?has_content><br />${shippingAddress.stateProvinceGeoId}</#if>
                        <#if shippingAddress.postalCode?has_content><br />${shippingAddress.postalCode}</#if>
                        <#if shippingAddress.countryGeoId?has_content><br />${shippingAddress.countryGeoId}</#if>
                    </@td>
                    <@td>
                      <a href="<@ofbizInterWebappUrl>/partymgr/control/editcontactmech?partyId=${orderParty.partyId}&amp;contactMechId=${shippingContactMech.contactMechId}</@ofbizInterWebappUrl>" target="_blank" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                    </@td>
                  </@tr>
                  <#if shippingContactMech_has_next>
                  <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
                  </#if>
                  <#assign i = i + 1>
                </#list>
            </#if>
            <#if shipToPartyShippingContactMechList?has_content>
                <@tr><@td colspan="3">${uiLabelMap.OrderShipToAnotherParty}: <b>${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(shipToParty)}</b></@td></@tr>
                <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
                <#list shipToPartyShippingContactMechList as shippingContactMech>
                  <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
                  <@tr>
                    <@td class="${styles.grid_large!}3">
                      <input type="radio" id="shipToOtherParty" name="${shipGroupIndex!"0"}_shipping_contact_mech_id" value="${shippingAddress.contactMechId}"/>
                    </@td>
                    <@td class="${styles.grid_large!}6">
                        <#if shippingAddress.toName?has_content><b>${uiLabelMap.CommonTo}:</b>&nbsp;${shippingAddress.toName}<br /></#if>
                        <#if shippingAddress.attnName?has_content><b>${uiLabelMap.CommonAttn}:</b>&nbsp;${shippingAddress.attnName}<br /></#if>
                        <#if shippingAddress.address1?has_content>${shippingAddress.address1}<br /></#if>
                        <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br /></#if>
                        <#if shippingAddress.city?has_content>${shippingAddress.city}</#if>
                        <#if shippingAddress.stateProvinceGeoId?has_content><br />${shippingAddress.stateProvinceGeoId}</#if>
                        <#if shippingAddress.postalCode?has_content><br />${shippingAddress.postalCode}</#if>
                        <#if shippingAddress.countryGeoId?has_content><br />${shippingAddress.countryGeoId}</#if>
                      </@td>
                    <@td><a href="<@ofbizInterWebappUrl>/partymgr/control/editcontactmech?partyId=${orderParty.partyId}&amp;contactMechId=${shippingContactMech.contactMechId}</@ofbizInterWebappUrl>" target="_blank" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a></@td>
                  </@tr>
                  <#if shippingContactMech_has_next>
                  <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
                  </#if>
                </#list>
            </#if>
            </@table>
            </@cell>
        </@row>
       </@section>
</#list>

            </form>
</#if>

    <#-- select a party id to ship to instead -->
    <@section title=uiLabelMap.OrderShipToAnotherParty>
      <form method="post" action="setShipping" name="partyshipform">
        <@fields type="default-manual">
          <@row>
            <@cell columns=6>
              <@row>
                <@cell columns=9>
                    <@field type="lookup" label=uiLabelMap.PartyPartyId value='${thisPartyId!}' formName="partyshipform" name="shipToPartyId" id="shipToPartyId" fieldFormName="LookupPartyName"/>
                </@cell>
                <@cell columns=3>                 
                    <@field type="submit" class="+${styles.link_run_session!} ${styles.action_update!}" text=uiLabelMap.CommonContinue />
                </@cell>
              </@row>
            </@cell>
          </@row>
        </@fields>
      </form>
    </@section> 
 </@section>
<#else>
 <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>
