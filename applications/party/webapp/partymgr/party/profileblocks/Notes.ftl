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

  <@section id="partyNotes" title="${uiLabelMap.CommonNotes}">
      <ul class="${style_button_group!}">
        <#if security.hasEntityPermission("PARTYMGR", "_NOTE", session)>
          <li><a href="<@ofbizUrl>AddPartyNote?partyId=${partyId}</@ofbizUrl>" class="button tiny">${uiLabelMap.CommonCreateNew}</a></li>
        </#if>
      </ul>
     
    <@row>
      <@cell>
      <#if notes?has_content>
        <table>
          <#list notes as noteRef>
            <tr>
              <td>
                <div><strong>${uiLabelMap.FormFieldTitle_noteName}: </strong>${noteRef.noteName!}</div>
                <#if noteRef.noteParty?has_content>
                  <div><strong>${uiLabelMap.CommonBy}: </strong>${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, noteRef.noteParty, true)}</div>
                </#if>
                <div><strong>${uiLabelMap.CommonAt}: </strong>${noteRef.noteDateTime.toString()}</div>
              </td>
              <td>
                ${noteRef.noteInfo}
              </td>
            </tr>
            <#if noteRef_has_next>
              <tr><td colspan="2"><hr/></td></tr>
            </#if>
          </#list>
        </table>
      <#else>
        ${uiLabelMap.PartyNoNotesForParty}
      </#if>
      </@cell>
    </@row>
  </@section>
