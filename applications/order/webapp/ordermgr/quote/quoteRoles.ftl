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
<@section title=uiLabelMap.OrderOrderQuoteRoles>
      <#if quoteRoles?has_content>
        <@table type="data-complex"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <#assign row = 1>
            <#list quoteRoles as quoteRole>
                <#assign roleType = quoteRole.getRelatedOne("RoleType", false)>
                <#assign party = quoteRole.getRelatedOne("Party", false)>
                <#assign rolePartyNameResult = dispatcher.runSync("getPartyNameForDate", {"partyId":quoteRole.partyId, "compareDate":quote.issueDate, "userLogin":userLogin})/>
                <@tr>
                    <@td align="right" valign="top" width="15%">
                        &nbsp;${roleType.get("description",locale)!}
                    </@td>
                    <@td width="5%">&nbsp;</@td>
                    <@td valign="top" width="80%">
                        ${rolePartyNameResult.fullName!"Name Not Found"}
                    </@td>
                </@tr>
            <#if quoteRoles.size() != row>
                <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            </#if>
            <#assign row = row + 1>
            </#list>
        </@table>
      </#if>
</@section>
