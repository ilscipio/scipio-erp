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
            <@htmlTemplate.nextPrev commonUrl=commonUrl listSize=arraySize viewSize=viewSize viewIndex=viewIndex highIndex=highIndex commonDisplaying=commonDisplaying/>
        </#if>
        <#if resultPartialList?has_content>
          <table class="responsive" cellspacing="0">
           <thead>
                <tr class="header-row-2">
                    <th>&nbsp;</th>
                    <#list fieldList as field>
                        <th>${field.name}</th>
                    </#list>
                </tr>
            </thead>
            <#if resultPartialList?has_content>
                <#assign alt_row = false>
                <#list records as record>
                    <tr<#if alt_row> class="alternate-row"</#if>>
                        <td>
                            <ul class="button-group">
                            <li><a href="<@ofbizUrl>ViewGeneric?${record.findString}</@ofbizUrl>" class="button tiny">${uiLabelMap.CommonView}</a></li>
                        <#if hasDeletePermission == 'Y'>
                            <li><a href="<@ofbizUrl>UpdateGeneric?${record.findString}&amp;UPDATE_MODE=DELETE</@ofbizUrl>" class="button tiny alert">${uiLabelMap.CommonDelete}</a></li>
                        </#if>
                            </ul>
                        </td>
                        <#list fieldList as field>
                            <td>${record.fields.get(field.name)!?string}</td>
                        </#list>
                    </tr>
                    <#assign alt_row = !alt_row>
                </#list>
            </#if>
        </table>
        <#else>
         <@panel>${uiLabelMap.WebtoolsNoEntityRecordsFound} ${entityName}.</@panel>
        </#if>
        <#if (arraySize > 0)>
            <@htmlTemplate.nextPrev commonUrl=commonUrl listSize=arraySize viewSize=viewSize viewIndex=viewIndex  highIndex=highIndex />
        </#if>
