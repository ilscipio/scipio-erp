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

<#-- ==================== Party Listing dialog box ========================= -->
<#if additionalPartyRoleMap?has_content>
<@section title=uiLabelMap.PartyAdditionalPartyListing>
      <@table type="data-complex" width="100%"> <#-- orig: class="" --> <#-- orig: cellpadding="0" --> <#-- orig: border="0" -->
        <#list roleList as role>
          <@tr>
            <@td valign="bottom">${roleData[role].get("description", locale)}</@td>
          </@tr>
          <@tr type="util">
            <@td colspan="4"><hr /></@td>
          </@tr>
          <#list additionalPartyRoleMap[role] as party>
            <@tr>
              <@td>${party}</@td>
              <@td>
                  <#if partyData[party].type == "person">
                    ${partyData[party].firstName!}
                  <#else>
                    ${partyData[party].groupName!}
                  </#if>
              </@td>
              <@td>
                  <#if partyData[party].type == "person">
                    ${partyData[party].lastName!}
                  </#if>
              </@td>
              <@td class="+${styles.text_right!}">
                <a href="<@ofbizUrl>removeAdditionalParty?additionalRoleTypeId=${role}&additionalPartyId=${party}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
              </@td>
            </@tr>
          </#list>
          <@tr><@td>&nbsp;</@td></@tr>
        </#list>
      </@table>
    </@section>
</#if>
