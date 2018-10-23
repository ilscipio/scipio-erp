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

  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
    <#if security.hasEntityPermission("PARTYMGR", "_NOTE", session)>
      <@menuitem type="link" href=makeOfbizUrl("AddPartyNote?partyId=${partyId}") text=uiLabelMap.CommonNew class="+${styles.action_nav!} ${styles.action_add!}" />
    </#if>
    </@menu>
  </#macro>
  <@section id="partyNotes" title=uiLabelMap.CommonNotes menuContent=menuContent>
      <#if notes?has_content>
        <@table type="data-complex" autoAltRows=false>
        <@tbody>
          <#list notes as noteRef>
            <@tr>
              <@td style="min-width:10em;">
                <div><strong>${uiLabelMap.FormFieldTitle_noteName}: </strong>${noteRef.noteName!}</div>
                <#if noteRef.noteParty?has_content>
                  <div><strong>${uiLabelMap.CommonBy}: </strong>${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, noteRef.noteParty, true)}</div>
                </#if>
                <div><strong>${uiLabelMap.CommonAt}: </strong>${noteRef.noteDateTime.toString()}</div>
              </@td>
              <@td>
                ${noteRef.noteInfo}
              </@td>
            </@tr>
            <#if noteRef_has_next>
              <@tr type="util"><@td colspan="2"><hr/></@td></@tr>
            </#if>
          </#list>
        </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.PartyNoNotesForParty}</@commonMsg>
      </#if>
  </@section>
