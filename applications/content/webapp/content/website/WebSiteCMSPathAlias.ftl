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
<@menu type="button" id="cmsmenu">
    <#if (content?has_content)>
        <@menuitem type="link" href="javascript:void(0);" onClick="javascript:callDocument(true, '${content.contentId}', '', 'ELECTRONIC_TEXT');" class="+${styles.link_nav!}" text="Quick Sub-Content"/>
        <@menuitem type="link" href="javascript:void(0);" onClick="javascript:callPathAlias('${content.contentId}');" class="+${styles.link_nav!} ${styles.selected!}" text="Path Alias"/>
        <@menuitem type="link" href="javascript:void(0);" onClick="javascript:callMetaInfo('${content.contentId}');" class="+${styles.link_nav!}" text="Meta Tags"/>
    </#if>
</@menu>

<#if (content?has_content)>
    <div style="margin-bottom: 8px;">
        New <b>PathAlias</b> attached from WebSite: <b>${webSite.webSiteId}</b> to Content: <b>${content.contentId}</b></b>
    </div>
</#if>


    <@table type="data-list" class="+${styles.table_spacing_tiny_hint!} calendarTable">
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
              <@td>${alias.aliasTo!(uiLabelMap.CommonNA)}</@td>
              <@td>${alias.contentId!(uiLabelMap.CommonNA)}</@td>
              <@td>${alias.mapKey!(uiLabelMap.CommonNA)}</@td>
              <@td><a href="javascript:void(0);" onclick="javascript:pathRemove('${webSiteId}', '${alias.pathAlias}', '${contentId}');" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a></@td>
            </@tr>
        </#list>
      <#else>
        <@tr type="meta">
          <@td colspan="5"><@commonMsg type="result-norecord">No aliases currently defined.</@commonMsg></@td>
        </@tr>
      </#if>
    </@table>



    <form name="cmspathform" method="post" action="<@ofbizUrl>createWebSitePathAliasJson</@ofbizUrl>">
            <input type="hidden" name="webSiteId" value="${webSiteId}"/>
            <@field type="display" label="Web Site">
                ${webSite.siteName!webSite.webSiteId}
            </@field>
            <input type="hidden" name="contentId" value="${contentId}"/>
            <@field type="display" label="Content">
                ${content.contentName!content.contentId}
            </@field>
            <@field type="input" label="Path Alias" name="pathAlias" value="" />
            <@field type="input" label="Map Key" name="mapKey" value="" />
            <@field type="submit" submitType="input-button" id="submit" onClick="javascript:pathSave('${contentId}');" class="+${styles.link_run_sys!} ${styles.action_add!}" text="Create"/>
    </form>
