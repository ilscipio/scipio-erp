<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makePageUrl("returnMain") text=uiLabelMap.OrderCreateReturn class="+${styles.action_nav!} ${styles.action_add!}" />
  </@menu>
</#macro>
<@section title=uiLabelMap.OrderReturnsCurrent menuContent=menuContent>
  <#if returnList?has_content>
    <@table type="data-list">
      <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.OrderReturnId} ${uiLabelMap.CommonNbr}</@th>
        <@th>${uiLabelMap.FormFieldTitle_entryDate}</@th>
        <@th>${uiLabelMap.PartyParty}</@th>
        <@th>${uiLabelMap.FacilityFacility}</@th>
        <@th>${uiLabelMap.CommonStatus}</@th>
      </@tr>
      </@thead>
      <@tbody>
      <#list returnList as returnHeader>
      <#assign statusItem = returnHeader.getRelatedOne("StatusItem", false)>
      <#if returnHeader.destinationFacilityId??>
        <#assign facility = returnHeader.getRelatedOne("Facility", false)>
      </#if>
      <@tr>
        <@td><a href="<@pageUrl>returnMain?returnId=${returnHeader.returnId}</@pageUrl>" class="${styles.link_nav_info_id!}">${returnHeader.returnId}</a></@td>
        <@td>${returnHeader.entryDate.toString()}</@td>
        <@td>
          <#if returnHeader.fromPartyId??>
            <a href="${customerDetailLink}${returnHeader.fromPartyId}${rawString(externalKeyParam!)}" class="${styles.link_nav_info_id!}">${returnHeader.fromPartyId}</a>
          <#else>
            <span>${uiLabelMap.CommonNA}</span>
          </#if>
        </@td>
        <@td><#if facility??>${facility.facilityName!facility.facilityId}<#else>${uiLabelMap.CommonNone}</#if></@td>
        <@td>${statusItem.get("description",locale)}</@td>
      </@tr>
      </#list>
      </@tbody>
    </@table>
  </#if>
</@section>