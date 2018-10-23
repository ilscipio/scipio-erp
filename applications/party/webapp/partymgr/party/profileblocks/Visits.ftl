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

  <#-- SCIPIO: Removed
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href=makeOfbizUrl("findVisits?partyId=${partyId}") text=uiLabelMap.CommonListAll class="+${styles.action_run_sys!} ${styles.action_find!}" />
    </@menu>
  </#macro>-->
  <@section id="partyVisits" title=uiLabelMap.PartyVisits>
      <#if visits?has_content>
        <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
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