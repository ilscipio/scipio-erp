<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
