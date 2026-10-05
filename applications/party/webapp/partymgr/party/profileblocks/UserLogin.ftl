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

  <@section id="partyUserLogins" title=uiLabelMap.PartyUserName>
      <#if userLogins?has_content>
        <@table type="data-list">
          <@tbody>
          <#list userLogins as userUserLogin>
            <@tr>
              <@td>${uiLabelMap.PartyUserLogin}</@td>
              <@td>
                  <#if security.hasEntityPermission("PARTYMGR", "_CREATE", request)>
                    <a href="<@pageUrl>ProfileEditUserLogin?partyId=${party.partyId}&amp;userLoginId=${userUserLogin.userLoginId}</@pageUrl>" class="${styles.action_update!}">${userUserLogin.userLoginId}</a>
                <#else>
                    ${userUserLogin.userLoginId}
                </#if>
              </@td>
              <@td>
                <#assign enabled = uiLabelMap.PartyEnabled>
                <#if (userUserLogin.enabled)?default("Y") == "N">
                  <#if userUserLogin.disabledDateTime??>
                    <#assign disabledTime = userUserLogin.disabledDateTime.toString()>
                  <#else>
                    <#assign disabledTime = "??">
                  </#if>
                  <#assign enabled = uiLabelMap.PartyDisabled + " - " + disabledTime>
                </#if>
                ${enabled}
              </@td>
              <@td class="button-col">
                <#if security.hasEntityPermission("SECURITY", "_VIEW", request)>
                  <a href="<@pageUrl>ProfileEditUserLoginSecurityGroups?partyId=${party.partyId}&amp;userLoginId=${userUserLogin.userLoginId}</@pageUrl>" class="${styles.action_view!}">${uiLabelMap.SecurityGroups}</a>
                </#if>
              </@td>
            </@tr>
          </#list>
          </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.PartyNoUserLogin}</@commonMsg>
      </#if>
  </@section>