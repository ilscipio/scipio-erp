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

<#if partyId??>
  <#assign sectionTitleParty = uiLabelMap.PartyParty>
<#else>
  <#assign sectionTitleParty = uiLabelMap.PartyActive>
</#if>
  
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if !partyId?? && showAll?lower_case == "true">
    <@menuitem type="link" href=makeOfbizUrl("showvisits?showAll=false") text=uiLabelMap.PartyShowActive class="+${styles.action_run_sys!} ${styles.action_find!}" />
  <#elseif !partyId??>
    <@menuitem type="link" href=makeOfbizUrl("showvisits?showAll=true") text=uiLabelMap.PartyShowAll class="+${styles.action_run_sys!} ${styles.action_find!}" />
  </#if>
  </@menu>
</#macro>
<@section title="${rawString(sectionTitleParty)} ${rawString(uiLabelMap.PartyVisitListing)}" menuContent=menuContent>
  <#if visitList?has_content>
    
    <#assign paramStr = addParamsToStr("", {"sort": sort!, "partyId": partyId!, "showAll": showAll!}, "&amp;", false)>
    <@paginate mode="content" url=makeOfbizUrl("showvisits") viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=visitSize!0 altParam=false paramStr=paramStr viewIndexFirst=1>
    
      <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
       <@thead>
        <@tr class="header-row">
          <@th><a href="<@ofbizUrl>showvisits?sort=visitId&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.PartyVisitId}</a></@th>
          <@th><a href="<@ofbizUrl>showvisits?sort=visitorId&amp;showAll=${showAll}<#if visitorId?has_content>&amp;visitorId=${visitorId}</#if></@ofbizUrl>">${uiLabelMap.PartyVisitorId}</a></@th>
          <@th><a href="<@ofbizUrl>showvisits?sort=partyId&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.PartyPartyId}</a></@th>
          <@th><a href="<@ofbizUrl>showvisits?sort=userLoginId&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.CommonUserLoginId}</a></@th>
          <@th><a href="<@ofbizUrl>showvisits?sort=-userCreated&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.PartyNewUser}</a></@th>
          <@th><a href="<@ofbizUrl>showvisits?sort=webappName&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.PartyWebApp}</a></@th>
          <@th><a href="<@ofbizUrl>showvisits?sort=clientIpAddress&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.PartyClientIP}</a></@th>
          <@th><a href="<@ofbizUrl>showvisits?sort=fromDate&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.CommonFromDate}</a></@th>
          <@th><a href="<@ofbizUrl>showvisits?sort=thruDate&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.CommonThruDate}</a></@th>
        </@tr>
        </@thead>
        <@tbody>
        <#list visitList as visitObj>
          <@tr>
            <@td class="button-col"><a href="<@ofbizUrl>visitdetail?visitId=${visitObj.visitId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${visitObj.visitId}</a></@td>
            <@td>${visitObj.visitorId!}</@td>
            <@td class="button-col"><a href="<@ofbizUrl>viewprofile?partyId=${visitObj.partyId!}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${visitObj.partyId!}</a></@td>
            <@td>${visitObj.userLoginId!}</@td>
            <@td>${visitObj.userCreated!}</@td>
            <@td>${visitObj.webappName!}</@td>
            <@td>${visitObj.clientIpAddress!}</@td>
            <@td>${(visitObj.fromDate?string)!}</@td>
            <@td>${(visitObj.thruDate?string)!}</@td>
          </@tr>
        </#list>
        </@tbody>
      </@table>
    </@paginate>
    
  <#else>
    <@commonMsg type="result-norecord"/>
  </#if>
      
</@section>
