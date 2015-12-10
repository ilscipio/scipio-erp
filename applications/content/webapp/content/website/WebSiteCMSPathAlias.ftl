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

<#-- cms menu bar -->
<div id="cmsmenu" style="margin-bottom: 8px;">
    <#if (content?has_content)>
        <a href="javascript:void(0);" onclick="javascript:callDocument(true, '${content.contentId}', '', 'ELECTRONIC_TEXT');" class="tabButton">Quick Sub-Content</a>
        <a href="javascript:void(0);" onclick="javascript:callPathAlias('${content.contentId}');" class="selected">Path Alias</a>
        <a href="javascript:void(0);" onclick="javascript:callMetaInfo('${content.contentId}');" class="tabButton">Meta Tags</a>
    </#if>
</div>

<#if (content?has_content)>
    <div style="margin-bottom: 8px;">
        New <b>PathAlias</b> attached from WebSite: <b>${webSite.webSiteId}</b> to Content: <b>${content.contentId}</b></b>
    </div>
</#if>


    <@table type="data-list" border="1" cellpadding="2" cellspacing="0" class="+calendarTable"> <#-- orig: class="calendarTable" -->
      <@thead>
      <@tr class="header-row">
        <@th>Web Site ID</@th>
        <@th>Path Alias</@th>
        <@th>Alias To</@th>
        <@th>Content ID</@th>
        <@th>Map Key</@th>
        <@th>&nbsp;</@th>
      </@tr>
      </@thead>
      <#if (aliases?has_content)>
        <#list aliases as alias>
            <@tr>
              <@td>${alias.webSiteId}</@td>
              <@td>${alias.pathAlias}</@td>
              <@td>${alias.aliasTo?default("N/A")}</@td>
              <@td>${alias.contentId?default("N/A")}</@td>
              <@td>${alias.mapKey?default("N/A")}</@td>
              <@td><a href="javascript:void(0);" onclick="javascript:pathRemove('${webSiteId}', '${alias.pathAlias}', '${contentId}');" class="${styles.link_action!}">${uiLabelMap.CommonRemove}</a></@td>
            </@tr>
        </#list>
      <#else>
        <@tr type="meta">
          <@td colspan="5"><@resultMsg>No aliases currently defined.</@resultMsg></@td>
        </@tr>
      </#if>
    </@table>



    <form name="cmspathform" method="post" action="<@ofbizUrl>/createWebSitePathAliasJson</@ofbizUrl>">
            <input type="hidden" name="webSiteId" value="${webSiteId}"/>
            <@field type="display" label="Web Site">
                ${webSite.siteName?default(webSite.webSiteId)}
            </@field>
            <input type="hidden" name="contentId" value="${contentId}"/>
            <@field type="display" label="Content">
                ${content.contentName?default(content.contentId)}
            </@field>
            <@field type="generic" label="Path Alias">
                <input type="text" name="pathAlias" value="" />
            </@field>
            <@field type="generic" label="Map Key">
                <input type="text" name="mapKey" value="" />
            </@field>
            <@field type="submitarea">
                <input id="submit" type="button" onclick="javascript:pathSave('${contentId}');" class="${styles.link_action!}" value="Create"/>
            </@field>
    </form>
