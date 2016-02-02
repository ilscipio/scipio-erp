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
        <#if (arraySize > 0)>
            <#assign commonUrl="FindGeneric?${curFindString}&amp;searchOptions_collapsed=${(parameters.searchOptions_collapsed)?default(\"false\")}&amp;"/>
            <@htmlTemplate.nextPrev position="top" commonUrl=commonUrl listSize=arraySize viewSize=viewSize viewIndex=viewIndex highIndex=highIndex commonDisplaying=commonDisplaying/>
        </#if>
        <#if resultPartialList?has_content>
          <@table type="data-list" autoAltRows=true cellspacing="0" scrollable=true fixedColumnsLeft=1 fixedColumnsRight=1> <#-- orig: class="basic-table responsive" -->
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
                            <@td>${record.fields.get(field.name)!?string}</@td>
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
      <#else>
         <@panel><@commonMsg type="result">${uiLabelMap.WebtoolsNoEntityRecordsFound} ${entityName}.</@commonMsg></@panel>
      </#if>
    
        <#if (arraySize > 0)>
            <@htmlTemplate.nextPrev position="bottom" commonUrl=commonUrl listSize=arraySize viewSize=viewSize viewIndex=viewIndex highIndex=highIndex commonDisplaying=commonDisplaying />
        </#if>
