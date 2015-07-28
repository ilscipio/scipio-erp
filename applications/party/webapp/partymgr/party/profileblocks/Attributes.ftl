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

  <#assign menuHtml>
    <#if security.hasEntityPermission("PARTYMGR", "_CREATE", session)>
      <li><a href="<@ofbizUrl>editPartyAttribute?partyId=${party.partyId!}</@ofbizUrl>" class="button tiny">${uiLabelMap.CommonCreateNew}</a></li>
    </#if>
  </#assign>

  <@section id="partyAttributes" title="${uiLabelMap.PartyAttributes}" menuHtml=menuHtml>

    <@row>
      <@cell>
      <#if attributes?has_content>
        <table class="basic-table hover-bar" cellspacing="0">
          <thead>
            <tr class="header-row">
              <th>${uiLabelMap.CommonName}</th>
              <th>${uiLabelMap.CommonValue}</th>
              <th>&nbsp;</th>
            </tr>
          </thead>
          <#assign alt_row = false>
          <#list attributes as attr>
            <tr class="<#if alt_row>odd<#else>even</#if>">
              <td>
                ${attr.attrName!}
              </td>
              <td>
                ${attr.attrValue!}
              </td>
              <td class="button-col">
                <a href="<@ofbizUrl>editPartyAttribute?partyId=${partyId!}&attrName=${attr.attrName!}</@ofbizUrl>" class="button tiny">${uiLabelMap.CommonEdit}</a>
              </td>
            </tr>
            <#-- toggle the row color -->
            <#assign alt_row = !alt_row>
          </#list>
        </table>
      <#else>
        ${uiLabelMap.PartyNoPartyAttributesFound}
      </#if>
      </@cell>
    </@row>
  </@section>