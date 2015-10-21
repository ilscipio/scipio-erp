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

  <#assign menuContent>
    <@menu type="section" inlineItems=true>
    <#if security.hasEntityPermission("PARTYMGR", "_CREATE", session)>
      <@menuitem type="link" href=makeOfbizUrl("editPartyAttribute?partyId=${party.partyId!}") text="${uiLabelMap.CommonNew}" />
    </#if>
    </@menu>
  </#assign>
  <@section id="partyAttributes" title="${uiLabelMap.PartyAttributes}" menuContent=menuContent>
      <#if attributes?has_content>
        <@table type="data-list" autoAltRows=true class="+hover-bar" cellspacing="0"> <#-- orig: class="basic-table hover-bar" -->
          <@thead>
            <@tr class="header-row">
              <@th>${uiLabelMap.CommonName}</@th>
              <@th>${uiLabelMap.CommonValue}</@th>
              <@th>&nbsp;</@th>
            </@tr>
          </@thead>
          <@tbody>
          <#list attributes as attr>
            <@tr>
              <@td>
                ${attr.attrName!}
              </@td>
              <@td>
                ${attr.attrValue!}
              </@td>
              <@td class="button-col">
                <a href="<@ofbizUrl>editPartyAttribute?partyId=${partyId!}&attrName=${attr.attrName!}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonEdit}</a>
              </@td>
            </@tr>
          </#list>
          </@tbody>
        </@table>
      <#else>
        <@resultMsg>${uiLabelMap.PartyNoPartyAttributesFound}</@resultMsg>
      </#if>
  </@section>