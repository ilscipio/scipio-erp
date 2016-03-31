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
<#include "component://widget/templates/htmlFormMacroLibrary.ftl"> 

<@row>
<@cell columns=6>
       <@heading relLevel=+1>${uiLabelMap.CommonGroup}</@heading>
       <form action="<@ofbizUrl>entitymaint</@ofbizUrl>">
        <@field type="select" name="filterByGroupName" label=uiLabelMap.WebtoolsGroupName>
             <option value="">${uiLabelMap.CommonAll}</option>
             <#list entityGroups as group>
                <option value="${group}" <#if filterByGroupName??><#if group == filterByGroupName>selected="selected"</#if></#if>>${group}</option>
             </#list>
        </@field>
        <@field type="input" name= "filterByEntityName" value=(parameters.filterByEntityName!) label=uiLabelMap.WebtoolsEntityName/>
        <@field type="submit" text=uiLabelMap.CommonApply class="+${styles.link_run_sys!} ${styles.action_find!}"/>
       </form>
</@cell>
</@row> 
<@row>
   <@cell>
        <@heading relLevel=+1>${uiLabelMap.WebtoolsEntitiesAlpha}</@heading>
    <#assign firstChar = "x">
    <#assign anchorId = "">
    <#assign anchorAttribs = {}>
    <#assign alt_row = false>
    <#assign right_col = false>
        <@nav type="magellan">
      <#list entitiesList as entity>
        <#if entity.entityName?substring(0, 1) != firstChar>
          <#assign firstChar = entity.entityName?substring(0, 1)>
              <@mli arrival="Entity_${firstChar}"><a href="#Entity_${firstChar}">${firstChar}</a></@mli>
        </#if>
      </#list>
         </@nav>
        <#assign firstChar = "*">
        <@table type="data-complex" autoAltRows=false> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
          <@thead>
          <@tr class="header-row">
            <@th>${uiLabelMap.WebtoolsEntityName}</@th>
            <@th>&nbsp;</@th>
            <@th>${uiLabelMap.WebtoolsEntityName}</@th>
            <@th>&nbsp;</@th>
          </@tr>
          </@thead>
          <#list entitiesList as entity>
            <#-- TODO: rework this to avoid splitting up @tr -->
            <#if entity.entityName?substring(0, 1) != firstChar>
              <#if right_col>
                <@td>&nbsp;</@td><@td>&nbsp;</@td><@tr close=true open=false />
                <#assign right_col = false>
                <#assign alt_row = !alt_row>
              </#if>
              <#if firstChar != "*">
                <@tr alt=alt_row><@td colspan="4">&nbsp;</@td></@tr>
                <#assign alt_row = !alt_row>
              </#if>
              <#assign firstChar = entity.entityName?substring(0, 1)>
              <#assign anchorId = "Entity_${firstChar}">
              <#assign anchorAttribs = {"data-magellan-destination": "Entity_${firstChar}"}>
            </#if>
            <#if !right_col>
              <@tr alt=alt_row open=true close=false />
            </#if>

            <@td id=anchorId attribs=anchorAttribs>${entity.entityName}<#if entity.viewEntity == 'Y'>&nbsp;(${uiLabelMap.WebtoolsEntityView})</#if></@td>
            
            <#assign anchorId = "">
            <#assign anchorAttribs = {}>
            <@td class="button-col">
              <#if entity.viewEntity == 'Y'>
                <#if entity.entityPermissionView == 'Y'>
                  <a href="<@ofbizUrl>ViewRelations?entityName=${entity.entityName}</@ofbizUrl>">${uiLabelMap.WebtoolsReln}</a>
                  <a href="<@ofbizUrl>FindGeneric?entityName=${entity.entityName}</@ofbizUrl>">${uiLabelMap.WebtoolsFind}</a>
                  <a href="<@ofbizUrl>FindGeneric?entityName=${entity.entityName}&amp;find=true&amp;VIEW_SIZE=${getPropertyValue("webtools", "webtools.record.paginate.defaultViewSize")!50}&amp;VIEW_INDEX=0</@ofbizUrl>">${uiLabelMap.WebtoolsAll}</a>
                </#if>
              <#else>
                <#if entity.entityPermissionCreate == 'Y'>
                  <a href="<@ofbizUrl>ViewGeneric?entityName=${entity.entityName}</@ofbizUrl>" title="${uiLabelMap.CommonCreateNew}">${uiLabelMap.WebtoolsCreate}</a>
                </#if>
                <#if entity.entityPermissionView == 'Y'>
                  <a href="<@ofbizUrl>ViewRelations?entityName=${entity.entityName}</@ofbizUrl>" title="${uiLabelMap.WebtoolsViewRelations}">${uiLabelMap.WebtoolsReln}</a>
                  <a href="<@ofbizUrl>FindGeneric?entityName=${entity.entityName}</@ofbizUrl>" title="${uiLabelMap.WebtoolsFindRecord}">${uiLabelMap.WebtoolsFind}</a>
                  <a href="<@ofbizUrl>FindGeneric?entityName=${entity.entityName}&amp;find=true&amp;VIEW_SIZE=${getPropertyValue("webtools", "webtools.record.paginate.defaultViewSize")!50}&amp;VIEW_INDEX=0</@ofbizUrl>" title="${uiLabelMap.WebtoolsFindAllRecords}">${uiLabelMap.WebtoolsAll}</a>
                </#if>
              </#if>
            </@td>
            <#if right_col>
              <@tr close=true open=false />
              <#assign alt_row = !alt_row>
            </#if>
            <#assign right_col = !right_col>
          </#list>
          <#if right_col>
            <@td>&nbsp;</@td><@td>&nbsp;</@td><@tr close=true open=false />
          </#if>
        </@table>
    </@cell>
</@row>
