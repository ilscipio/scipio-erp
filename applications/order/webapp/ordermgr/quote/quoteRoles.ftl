<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
