<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

  <#-- SCIPIO: Removed
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href=makeOfbizUrl("findVisits?partyId=${partyId}") text=uiLabelMap.CommonListAll class="+${styles.action_run_sys!} ${styles.action_find!}" />
    </@menu>
  </#macro>-->
  <@section id="partyVisits" title=uiLabelMap.PartyVisits>
      <#if visits?has_content>
        <@table type="data-list">
         <@thead>
          <@tr class="header-row">
            <@th>${uiLabelMap.PartyVisitId}</@th>
            <@th>${uiLabelMap.PartyUserLogin}</@th>
            <@th>${uiLabelMap.PartyNewUser}</@th>
            <@th>${uiLabelMap.PartyWebApp}</@th>
            <@th>${uiLabelMap.PartyClientIP}</@th>
            <@th>${uiLabelMap.CommonFromDate}</@th>
            <@th>${uiLabelMap.CommonThruDate}</@th>
          </@tr>
          </@thead>
          <@tbody>
          <#list visits as visitObj>
            <#if (visitObj_index > 4)><#break></#if>
              <@tr>
                <@td class="button-col">
                  <a href="<@ofbizUrl>visitdetail?visitId=${visitObj.visitId!}</@ofbizUrl>">${visitObj.visitId!}</a>
                </@td>
                <@td>${visitObj.userLoginId!}</@td>
                <@td>${visitObj.userCreated!}</@td>
                <@td>${visitObj.webappName!}</@td>
                <@td>${visitObj.clientIpAddress!}</@td>
                <@td>${(visitObj.fromDate.toString())!}</@td>
                <@td>${(visitObj.thruDate.toString())!}</@td>
              </@tr>
          </#list>
          </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.PartyNoVisitFound}</@commonMsg>
      </#if>
  </@section>