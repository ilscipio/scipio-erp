<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section>
  <#if parameters.searchLabels??>
  <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" -->
    <@thead>
      <@tr class="header-row">
          <@th>${uiLabelMap.WebtoolsLabelManagerRow}</@th>
          <@th>${uiLabelMap.WebtoolsLabelManagerKey}</@th>
          <@th>${uiLabelMap.WebtoolsLabelManagerReferences}</@th>
      </@tr>
    </@thead>
    <#assign rowNumber = 1>
    <#list referencesList as reference>
      <#assign labelFound = 'N'>
      <#assign refNum = factory.getLabelReferenceFile(reference)>
      <#if (refNum > 0)>
        <@tr>
          <@td>${rowNumber}</@td>
          <@td>${reference}</@td>
          <@td align="center"><#if (refNum > 0)><a href="<@ofbizUrl>ViewReferences?sourceKey=${reference}</@ofbizUrl>">${refNum}</a><#else>${refNum}</#if></@td>
        </@tr>
        <#assign rowNumber = rowNumber + 1>
      </#if>
    </#list>
  </@table>
  </#if>
</@section>
