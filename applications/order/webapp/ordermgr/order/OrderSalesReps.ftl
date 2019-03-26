<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if salesReps?has_content>
<@section title=uiLabelMap.OrderSalesReps>
      <@table type="data-list">
      <@thead>
      <@tr class="header-row">
        <@th width="50%">${uiLabelMap.PartyLastName}</@th>
        <@th width="50%">${uiLabelMap.PartyFirstName}</@th>
      </@tr>
      </@thead>
    <#list salesReps as salesRep>
      <#assign party = salesRep.getRelatedOne("Party", false)!/>
      <#assign person = party.getRelatedOne("Person", false)!/>
      <#if person?? && person?has_content>
      <@tr>
        <@td width="50%">${person.lastName}</@td>
        <@td width="50%">${person.firstName}</@td>
      </@tr>
      </#if>
    </#list>
      </@table>
    </@section>
</#if>
