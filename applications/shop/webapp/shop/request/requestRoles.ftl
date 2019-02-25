<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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