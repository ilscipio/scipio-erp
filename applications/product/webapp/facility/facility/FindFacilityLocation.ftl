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

<#assign menuHtml>
  <@menu type="section" inlineItems=true>
    <@menuitem type="link" href=makeOfbizUrl("EditFacility") text="${uiLabelMap.ProductNewFacility}" contentClass="+create" />
    <@menuitem type="link" href=makeOfbizUrl("EditFacilityLocation?facilityId=${facilityId!}") text="${uiLabelMap.ProductNewFacilityLocation}" contentClass="+ create" />
  </@menu>
</#assign>
<@section menuHtml=menuHtml>
    <form action="<@ofbizUrl>FindFacilityLocation</@ofbizUrl>" method="get" name="findFacilityLocation" class="basic-form">
        <#if (facilityId??)>
            <input type="hidden" name="facilityId" value="${facilityId}" />
        </#if>        
        <#if !(facilityId??)>
            <@field type="generic" label="${uiLabelMap.ProductFacility}">
                <input type="text" value="" size="19" maxlength="20" />
            </@field>
        </#if>
        <@field type="generic" label="${uiLabelMap.ProductLocationSeqId}">
            <#if parameters.facilityId??>
                    <#assign LookupFacilityLocationView="LookupFacilityLocation?facilityId=${facilityId}">
                <#else>
                    <#assign LookupFacilityLocationView="LookupFacilityLocation">
                </#if>
                <@htmlTemplate.lookupField formName="findFacilityLocation" name="locationSeqId" id="locationSeqId" fieldFormName="${LookupFacilityLocationView}"/>
        </@field>
        <@field type="generic" label="${uiLabelMap.CommonArea}">
            <input type="text" name="areaId" value="" size="19" maxlength="20" />
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductAisle}">
            <input type="text" name="aisleId" value="" size="19" maxlength="20" />
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductSection}">
            <input type="text" name="sectionId" value="" size="19" maxlength="20" />
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductLevel}">
            <input type="text" name="levelId" value="" size="19" maxlength="20" />
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductPosition}">
            <input type="text" name="positionId" value="" size="19" maxlength="20" />
        </@field>
        <@field type="submitarea">
            <input type="submit" name="look_up" value="${uiLabelMap.CommonFind}" />
        </@field>
    </form>

    <#if foundLocations??>
      <#assign sectionTitle>${uiLabelMap.CommonFound}:&nbsp;${foundLocations.size()}&nbsp;${uiLabelMap.ProductLocationsFor}&nbsp;<#if facility??>${(facility.facilityName)!}</#if> [ID:${facilityId!}]</#assign>
      <@section title=sectionTitle>
        <@table type="data-list" autoAltRows=true class="+hover-bar" cellspacing="0"> <#-- orig: class="basic-table hover-bar" -->
        <@thead>
        <@tr class="header-row-2">
            <@th>${uiLabelMap.ProductFacility}</@th>
            <@th>${uiLabelMap.ProductLocationSeqId}</@th>
            <@th>${uiLabelMap.ProductType}</@th>
            <@th>${uiLabelMap.CommonArea}</@th>
            <@th>${uiLabelMap.ProductAisle}</@th>
            <@th>${uiLabelMap.ProductSection}</@th>
            <@th>${uiLabelMap.ProductLevel}</@th>
            <@th>${uiLabelMap.ProductPosition}</@th>
            <@th>&nbsp;</@th>
        </@tr>
        </@thead>
        <#list foundLocations as location>
        <#assign locationTypeEnum = location.getRelatedOne("TypeEnumeration", true)!>
        <@tr valign="middle">
            <@td><a href="<@ofbizUrl>EditFacility?facilityId=${(location.facilityId)!}</@ofbizUrl>" class="${styles.button_default!}">${(location.facilityId)!}</a></@td>
            <@td><a href="<@ofbizUrl>EditFacilityLocation?facilityId=${facilityId}&locationSeqId=${(location.locationSeqId)!}</@ofbizUrl>" class="${styles.button_default!}">${(location.locationSeqId)!}</a></@td>
            <@td>${(locationTypeEnum.get("description",locale))?default(location.locationTypeEnumId!)}</@td>
            <@td>${(location.areaId)!}</@td>
            <@td>${(location.aisleId)!}</@td>
            <@td>${(location.sectionId)!}</@td>
            <@td>${(location.levelId)!}</@td>
            <@td>${(location.positionId)!}</@td>
            <@td class="button-col">
              <a href="<@ofbizUrl>EditInventoryItem?facilityId=${(location.facilityId)!}&locationSeqId=${(location.locationSeqId)!}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.ProductNewInventoryItem}</a>
              <#if itemId??>
                <a href="<@ofbizUrl>UpdateInventoryItem?inventoryItemId=${itemId}&facilityId=${facilityId}&locationSeqId=${(location.locationSeqId)!}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.ProductSetItem} ${itemId}</a>
              </#if>
              <a href="<@ofbizUrl>EditFacilityLocation?facilityId=${(location.facilityId)!}&locationSeqId=${(location.locationSeqId)!}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonEdit}</a>
            </@td>
        </@tr>
        </#list>
        </@table>
      </@section>
    </#if>
</@section>