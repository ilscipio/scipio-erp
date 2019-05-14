<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if resultPartialList?has_content>
  <#--WARN: TODO: REVIEW for security issues-->
  <#assign paramStr = "${raw(curFindString)}&amp;searchOptions_collapsed=${(parameters.searchOptions_collapsed)!(\"false\")}"/>
  <@paginate mode="content" url=makePageUrl("FindGeneric") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=arraySize!0>
    <@table type="data-list" autoAltRows=true scrollable=true fixedColumnsLeft=1 fixedColumnsRight=1>
      <@thead>
        <@tr class="header-row-2">
          <@th>&nbsp;</@th>
          <#list fieldList as field>
            <@th>${field.name}</@th>
          </#list>
          <@th>&nbsp;</@th>
        </@tr>
      </@thead>
        <#if resultPartialList?has_content>
            <#list records as record>
                <@tr>
                    <@td>
                        <a href="<@pageUrl>ViewGeneric?${record.findString}</@pageUrl>">${uiLabelMap.CommonView}</a>
                        <a href="<@pageUrl>ViewGeneric?${record.findString}&amp;enableEdit=true</@pageUrl>">${uiLabelMap.CommonEdit}</a>
                    </@td>
                    <#list fieldList as field>
                        <@td>
                            <#-- FIXME?: Maybe this may cause unexpected issue so let's keep an eye on it 
                               2017-05-25: TODO?: REVIEW: if this is an issue, shouldn't it occur on other pages as well? -->
                            <#assign fieldValue = (record.fields[raw(field.name)])! />
                            <#if !fieldValue?is_method && fieldValue?has_content>
                                <#if (isObjectType("string", fieldValue) || fieldValue?is_date || fieldValue?is_number || fieldValue?is_boolean)>
                                    ${fieldValue?string}
                                <#else>
                                    <em>(${uiLabelMap.WebtoolsNoStringRepr})</em>
                                </#if>
                            </#if>
                        </@td>
                    </#list>
                    <@td>
                    <#if hasDeletePermission == 'Y'>
                       <a href="<@pageUrl>UpdateGeneric?${record.findString}&amp;UPDATE_MODE=DELETE</@pageUrl>">${uiLabelMap.CommonDelete}</a>
                    </#if>
                    </@td>
                </@tr>
            </#list>
        </#if>
    </@table>
  </@paginate>
<#else>
   <@panel><@commonMsg type="result-norecord">${uiLabelMap.WebtoolsNoEntityRecordsFound} ${entityName}.</@commonMsg></@panel>
</#if>
    

