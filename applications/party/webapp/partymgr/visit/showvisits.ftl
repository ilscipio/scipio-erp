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
    <#assign title = uiLabelMap.PartyParty>
  <#else>
    <#assign title = uiLabelMap.PartyActive>
  </#if>
  
<#assign menuHtml>
      <#if !partyId?? && showAll?lower_case == "true">
        <li><a href="<@ofbizUrl>showvisits?showAll=false</@ofbizUrl>">${uiLabelMap.PartyShowActive}</a></li>
      <#elseif !partyId??>
        <li><a href="<@ofbizUrl>showvisits?showAll=true</@ofbizUrl>">${uiLabelMap.PartyShowAll}</a></li>
      </#if>
</#assign>  

<@section title="${title}&nbsp;${uiLabelMap.PartyVisitListing}" menuHtml=menuHtml>
  <#if visitList?has_content>
    
    <#assign paginated = false>
    <#if (visitSize > 0)>
        <#assign url><@ofbizUrl>showvisits</@ofbizUrl></#assign>
        <#assign paramStr = addParamsToStr("", {"sort": sort!, "partyId": partyId!, "showAll": showAll!}, "&amp;", false)>
        <#-- forcePost required because search done from service event with https="true" -->
        <#macro paginateVisits>
          <@paginate url=url viewSize=viewSize viewIndex=viewIndex listSize=visitSize altParam=false paramStr=paramStr viewIndexFirst=1 />
        </#macro>
        <#assign paginated = true>
    </#if>
    
    <#if paginated>
      <@paginateVisits />
    </#if>
    
      <table class="basic-table hover-bar" cellspacing="0">
       <thead>
        <tr class="header-row">
          <th><a href="<@ofbizUrl>showvisits?sort=visitId&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.PartyVisitId}</a></th>
          <th><a href="<@ofbizUrl>showvisits?sort=visitorId&amp;showAll=${showAll}<#if visitorId?has_content>&amp;visitorId=${visitorId}</#if></@ofbizUrl>">${uiLabelMap.PartyVisitorId}</a></th>
          <th><a href="<@ofbizUrl>showvisits?sort=partyId&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.PartyPartyId}</a></th>
          <th><a href="<@ofbizUrl>showvisits?sort=userLoginId&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.CommonUserLoginId}</a></th>
          <th><a href="<@ofbizUrl>showvisits?sort=-userCreated&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.PartyNewUser}</a></th>
          <th><a href="<@ofbizUrl>showvisits?sort=webappName&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.PartyWebApp}</a></th>
          <th><a href="<@ofbizUrl>showvisits?sort=clientIpAddress&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.PartyClientIP}</a></th>
          <th><a href="<@ofbizUrl>showvisits?sort=fromDate&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.CommonFromDate}</a></th>
          <th><a href="<@ofbizUrl>showvisits?sort=thruDate&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@ofbizUrl>">${uiLabelMap.CommonThruDate}</a></th>
        </tr>
        </thead>
        <#assign alt_row = false>
        <#list visitList as visitObj>
          <tr<@dataRowClassStr alt=alt_row />>
            <td class="button-col"><a href="<@ofbizUrl>visitdetail?visitId=${visitObj.visitId}</@ofbizUrl>" class="${styles.button_default!}">${visitObj.visitId}</a></td>
            <td>${visitObj.visitorId!}</td>
            <td class="button-col"><a href="<@ofbizUrl>viewprofile?partyId=${visitObj.partyId!}</@ofbizUrl>" class="${styles.button_default!}">${visitObj.partyId!}</a></td>
            <td>${visitObj.userLoginId!}</td>
            <td>${visitObj.userCreated!}</td>
            <td>${visitObj.webappName!}</td>
            <td>${visitObj.clientIpAddress!}</td>
            <td>${(visitObj.fromDate?string)!}</td>
            <td>${(visitObj.thruDate?string)!}</td>
          </tr>
          <#assign alt_row = !alt_row>
        </#list>
      </table>
    
    <#if paginated>
      <@paginateVisits />
    </#if>
    
  <#else>
    <p>${uiLabelMap.CommonNoRecordFound}.</p>
  </#if>
      
</@section>
