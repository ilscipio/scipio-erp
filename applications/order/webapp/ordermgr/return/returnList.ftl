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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
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
            <a href="${customerDetailLink}${returnHeader.fromPartyId}${raw(externalKeyParam!)}" class="${styles.link_nav_info_id!}">${returnHeader.fromPartyId}</a>
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