<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if orderTerms?has_content>
  <@section title=uiLabelMap.OrderOrderTerms>
    <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
      <@thead> 
        <@tr class="header-row">
          <@th width="35%">${uiLabelMap.OrderOrderTermType}</@th>
          <@th width="10%" align="center">${uiLabelMap.OrderOrderTermValue}</@th>
          <@th width="10%" align="center">${uiLabelMap.OrderOrderTermDays}</@th>
          <@th width="10%" align="center">${uiLabelMap.OrderOrderTextValue}</@th>        
          <@th width="35%" align="center">${uiLabelMap.CommonDescription}</@th>
        </@tr>
      </@thead>
      <@tbody>
      <#list orderTerms as orderTerm>
        <@tr>
          <@td width="35%">${orderTerm.getRelatedOne("TermType", false).get("description", locale)}</@td>
          <@td width="10%" align="center">${orderTerm.termValue!""}</@td>
          <@td width="10%" align="center">${orderTerm.termDays!""}</@td>
          <@td width="10%" align="center">${orderTerm.textValue!""}</@td>
          <@td width="35%" align="center">${orderTerm.description!""}</@td>
        </@tr>
      </#list>
      </@tbody>
    </@table>
  </@section>
</#if>