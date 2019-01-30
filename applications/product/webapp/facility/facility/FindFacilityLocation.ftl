<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makePageUrl("EditFacilityLocation?facilityId=${facilityId!}") text=uiLabelMap.ProductNewFacilityLocation class="+${styles.action_nav!} ${styles.action_add!}" />
  </@menu>
</#macro>
<@section menuContent=menuContent>
    <form action="<@pageUrl>FindFacilityLocation</@pageUrl>" method="get" name="findFacilityLocation" class="basic-form">
        <#if (facilityId??)>
            <input type="hidden" name="facilityId" value="${facilityId}" />
        </#if>        
        <#if !(facilityId??)>
            <@field type="input" label=uiLabelMap.ProductFacility value="" size="19" maxlength="20" />
        </#if>
        <#if parameters.facilityId??>
            <#assign LookupFacilityLocationView="LookupFacilityLocation?facilityId=${facilityId}">
        <#else>
            <#assign LookupFacilityLocationView="LookupFacilityLocation">
        </#if>
        <@field type="lookup" label=uiLabelMap.ProductLocationSeqId formName="findFacilityLocation" name="locationSeqId" id="locationSeqId" fieldFormName=LookupFacilityLocationView/>
        <@field type="input" label=uiLabelMap.CommonArea name="areaId" value="" size="19" maxlength="20" />
        <@field type="input" label=uiLabelMap.ProductAisle name="aisleId" value="" size="19" maxlength="20" />
        <@field type="input" label=uiLabelMap.ProductSection name="sectionId" value="" size="19" maxlength="20" />
        <@field type="input" label=uiLabelMap.ProductLevel name="levelId" value="" size="19" maxlength="20" />
        <@field type="input" label=uiLabelMap.ProductPosition name="positionId" value="" size="19" maxlength="20" />
        <@field type="submit" name="look_up" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}" />
    </form>

    <#if foundLocations??>
      <#assign sectionTitle>${rawLabel('CommonFound')}: ${foundLocations.size()} ${rawLabel('ProductLocationsFor')} <#if facility??>${rawString((facility.facilityName)!)}</#if> [${rawString(facilityId!)}]</#assign>
      <@section title=sectionTitle>
        <@table type="data-list" autoAltRows=true>
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
            <@td><a href="<@pageUrl>EditFacility?facilityId=${(location.facilityId)!}</@pageUrl>" class="${styles.link_nav_info_id!}">${(location.facilityId)!}</a></@td>
            <@td><a href="<@pageUrl>EditFacilityLocation?facilityId=${facilityId}&locationSeqId=${(location.locationSeqId)!}</@pageUrl>" class="${styles.link_nav_info_id!}">${(location.locationSeqId)!}</a></@td>
            <@td>${(locationTypeEnum.get("description",locale))?default(location.locationTypeEnumId!)}</@td>
            <@td>${(location.areaId)!}</@td>
            <@td>${(location.aisleId)!}</@td>
            <@td>${(location.sectionId)!}</@td>
            <@td>${(location.levelId)!}</@td>
            <@td>${(location.positionId)!}</@td>
            <@td class="button-col">
              <a href="<@pageUrl>EditInventoryItem?facilityId=${(location.facilityId)!}&locationSeqId=${(location.locationSeqId)!}</@pageUrl>" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.ProductNewInventoryItem}</a>
              <#if itemId??>
                <a href="<@pageUrl>UpdateInventoryItem?inventoryItemId=${itemId}&facilityId=${facilityId}&locationSeqId=${(location.locationSeqId)!}</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.ProductSetItem} ${itemId}</a>
              </#if>
              <a href="<@pageUrl>EditFacilityLocation?facilityId=${(location.facilityId)!}&locationSeqId=${(location.locationSeqId)!}</@pageUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonEdit}</a>
            </@td>
        </@tr>
        </#list>
        </@table>
      </@section>
    </#if>
</@section>