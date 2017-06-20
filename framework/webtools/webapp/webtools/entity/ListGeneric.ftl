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

<#if resultPartialList?has_content>
  <#--WARN: TODO: REVIEW for security issues-->
  <#assign paramStr = "${rawString(curFindString)}&amp;searchOptions_collapsed=${(parameters.searchOptions_collapsed)!(\"false\")}"/>
  <@paginate mode="content" url=makeOfbizUrl("FindGeneric") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=arraySize!0>
    <@table type="data-list" autoAltRows=true scrollable=true fixedColumnsLeft=1 fixedColumnsRight=1> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
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
                        <a href="<@ofbizUrl>ViewGeneric?${record.findString}</@ofbizUrl>">${uiLabelMap.CommonView}</a>
                    </@td>
                    <#list fieldList as field>
                        <@td>
                            <#-- FIXME?: Maybe this may cause unexpected issue so let's keep an eye on it 
                               2017-05-25: TODO?: REVIEW: if this is an issue, shouldn't it occur on other pages as well? -->
                            <#assign fieldValue = (record.fields[rawString(field.name)])! />
                            <#if fieldValue?has_content>
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
                       <a href="<@ofbizUrl>UpdateGeneric?${record.findString}&amp;UPDATE_MODE=DELETE</@ofbizUrl>">${uiLabelMap.CommonDelete}</a>
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
    

