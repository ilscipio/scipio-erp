<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->

<#if requestParties?has_content>
    <@section title=uiLabelMap.OrderRequestRoles>
        <@table type="generic" class="${styles.table_basic!}" cellspacing="0">
         <#assign rowCount = 1>
         <#list requestParties as requestParty>
            <#assign roleType = requestParty.getRelatedOne("RoleType", false)>
            <#assign party = requestParty.getRelatedOne("Party", false)>
              <@tr>
                  <@td align="right" valign="top" width="15%" class="label">
                      &nbsp;${roleType.get("description", locale)!}
                  </@td>
                  <@td width="5%">&nbsp;</@td>
                  <@td valign="top" width="80%">
                      ${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(party)}
                  </@td>
              </@tr>
              <#if requestParties.size() != rowCount>
                <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
              </#if>
              <#assign rowCount = rowCount + 1>
          </#list>
        </@table>
    </@section>
</#if>