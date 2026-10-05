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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <#if modelEntity??>
      <@menuitem type="link" href=makePageUrl("FindGeneric?entityName=${entityName}&find=true&VIEW_SIZE=${getPropertyValue('webtools', 'webtools.record.paginate.defaultViewSize')!50}&VIEW_INDEX=0") text=uiLabelMap.WebtoolsBackToFindScreen class="+${styles.action_nav!} ${styles.action_cancel!}" />
    <#else>
      <@menuitem type="link" href=makePageUrl("entitymaint") text=uiLabelMap.WebtoolsBackToEntityList class="+${styles.action_nav!} ${styles.action_cancel!}" />
    </#if>
  </@menu>
</#macro>
<@section menuContent=menuContent><#-- title=uiLabelMap.WebtoolsRelations --><#-- SCIPIO: already in title -->
  <#if modelEntity??>
      <#if hasViewPermission>
        <#--<@heading>${uiLabelMap.WebtoolsForEntity}: ${entityName}</@heading>-->

        <@table type="data-list" autoAltRows=true>
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.WebtoolsTitle}</@th>
                <@th>${uiLabelMap.WebtoolsRelatedEntity}</@th>
                <@th>${uiLabelMap.WebtoolsRelationType}</@th>
                <@th>${uiLabelMap.WebtoolsFKName}</@th>
                <@th>${uiLabelMap.WebtoolsFieldsList}</@th>
            </@tr>
           </@thead>
            <#list (relations![]) as relation>
                <@tr>
                    <@td>${relation.title}</@td>
                    <@td class="button-col"><a href="<@pageUrl>FindGeneric?entityName=${relation.relEntityName}&amp;find=true&amp;VIEW_SIZE=${getPropertyValue("webtools", "webtools.record.paginate.defaultViewSize")!50}&amp;VIEW_INDEX=0</@pageUrl>">${relation.relEntityName}</a></@td>
                    <@td>${relation.type}</@td>
                    <@td>${relation.fkName}</@td>
                    <@td>
                        <#list relation.relFields as field>
                            ${field.fieldName} -> ${field.relFieldName}<br />
                        </#list>
                    </@td>
                </@tr>
            </#list>
        </@table>
    <#else>
        <@commonMsg type="error">${uiLabelMap.WebtoolsEntityCreatePermissionError} ${entityName} ${plainTableName}.</@commonMsg>
    </#if>
  <#else>
    <#--<#if entityName?has_content>
      <@commonMsg type="error">${getLabel('WebtoolsEntityNotFoundSpecified', '', {"entityName":entityName})}.</@commonMsg>
    </#if>-->
  </#if>
</@section>
