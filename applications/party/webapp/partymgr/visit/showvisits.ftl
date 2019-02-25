<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if partyId??>
  <#assign sectionTitleParty = uiLabelMap.PartyParty>
<#else>
  <#assign sectionTitleParty = uiLabelMap.PartyActive>
</#if>
  
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if !partyId?? && showAll?lower_case == "true">
    <@menuitem type="link" href=makePageUrl("showvisits?showAll=false") text=uiLabelMap.PartyShowActive class="+${styles.action_run_sys!} ${styles.action_find!}" />
  <#elseif !partyId??>
    <@menuitem type="link" href=makePageUrl("showvisits?showAll=true") text=uiLabelMap.PartyShowAll class="+${styles.action_run_sys!} ${styles.action_find!}" />
  </#if>
  </@menu>
</#macro>
<@section title="${raw(sectionTitleParty)} ${rawLabel('PartyVisitListing')}" menuContent=menuContent>
  <#if visitList?has_content>
    
    <#assign paramStr = addParamsToStr("", {"sort": sort!, "partyId": partyId!, "showAll": showAll!}, "&amp;", false)>
    <@paginate mode="content" url=makePageUrl("showvisits") viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=visitSize!0 altParam=false paramStr=paramStr viewIndexFirst=1>
    
      <@table type="data-list" autoAltRows=true>
       <@thead>
        <@tr class="header-row">
          <@th><a href="<@pageUrl>showvisits?sort=visitId&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@pageUrl>">${uiLabelMap.PartyVisitId}</a></@th>
          <@th><a href="<@pageUrl>showvisits?sort=visitorId&amp;showAll=${showAll}<#if visitorId?has_content>&amp;visitorId=${visitorId}</#if></@pageUrl>">${uiLabelMap.PartyVisitorId}</a></@th>
          <@th><a href="<@pageUrl>showvisits?sort=partyId&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@pageUrl>">${uiLabelMap.PartyPartyId}</a></@th>
          <@th><a href="<@pageUrl>showvisits?sort=userLoginId&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@pageUrl>">${uiLabelMap.CommonUserLoginId}</a></@th>
          <@th><a href="<@pageUrl>showvisits?sort=-userCreated&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@pageUrl>">${uiLabelMap.PartyNewUser}</a></@th>
          <@th><a href="<@pageUrl>showvisits?sort=webappName&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@pageUrl>">${uiLabelMap.PartyWebApp}</a></@th>
          <@th><a href="<@pageUrl>showvisits?sort=clientIpAddress&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@pageUrl>">${uiLabelMap.PartyClientIP}</a></@th>
          <@th><a href="<@pageUrl>showvisits?sort=fromDate&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@pageUrl>">${uiLabelMap.CommonFromDate}</a></@th>
          <@th><a href="<@pageUrl>showvisits?sort=thruDate&amp;showAll=${showAll}<#if partyId?has_content>&amp;partyId=${partyId}</#if></@pageUrl>">${uiLabelMap.CommonThruDate}</a></@th>
        </@tr>
        </@thead>
        <@tbody>
        <#list visitList as visitObj>
          <@tr>
            <@td class="button-col"><a href="<@pageUrl>visitdetail?visitId=${visitObj.visitId}</@pageUrl>" class="${styles.link_nav_info_id!}">${visitObj.visitId}</a></@td>
            <@td>${visitObj.visitorId!}</@td>
            <@td class="button-col"><a href="<@pageUrl>viewprofile?partyId=${visitObj.partyId!}</@pageUrl>" class="${styles.link_nav_info_id!}">${visitObj.partyId!}</a></@td>
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
