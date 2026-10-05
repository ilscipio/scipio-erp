<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makePageUrl("SolrServices") text=uiLabelMap.SolrSolrServices class="+${styles.action_nav!}" />
  </@menu>
</#macro>
<@section menuContent=menuContent>

  <@section title=getLabel("WorkEffortGeneral", "WorkEffortUiLabels")>     
      <@table type="data-list" autoAltRows=true scrollable=true> 
        <@thead>
        <@tr class="header-row">
          <@th id="Service_all">${uiLabelMap.WebtoolsServiceName}</@th>
          <#--<@th>${uiLabelMap.WebtoolsEngineName}</@th>-->
          <#--<@th>${uiLabelMap.WebtoolsDefaultEntityName}</@th>-->
          <@th>${uiLabelMap.WebtoolsInvoke}</@th>
          <#--<@th>${uiLabelMap.WebtoolsLocation}</@th>-->
          <#--@th>${uiLabelMap.WebtoolsDefinitionLocation}</@th-->
        </@tr>
        </@thead>

          <@tr>
            <@td><a href="<@pageUrl>scheduleServiceSync?SERVICE_NAME=reloadVisualThemeResources&POOL_NAME=pool&_RUN_SYNC_=Y&visualThemeId=</@pageUrl>">Visual Theme Resources - Reload All, Now</a></@td>
            <#--<@td>${service.engineName}</@td>-->
            <#--<@td>${service.defaultEntityName}</@td>-->
            <@td>reloadVisualThemeResources</@td>
            <#--<@td>${service.location}</@td>-->
            <#--@td><a href="<@pageUrl>${url!}?constraint=definitionLocation@${service.definitionLocation}</@pageUrl>">${service.definitionLocation}</a></@td-->
          </@tr>

          <@tr>
            <@td><a href="<@pageUrl>setSyncServiceParameters?SERVICE_NAME=reloadVisualThemeResources&POOL_NAME=pool&_RUN_SYNC_=Y</@pageUrl>">Visual Theme Resources - Reload, Configurable</a></@td>
            <#--<@td>${service.engineName}</@td>-->
            <#--<@td>${service.defaultEntityName}</@td>-->
            <@td>reloadVisualThemeResources</@td>
            <#--<@td>${service.location}</@td>-->
            <#--@td><a href="<@pageUrl>${url!}?constraint=definitionLocation@${service.definitionLocation}</@pageUrl>">${service.definitionLocation}</a></@td-->
          </@tr>

      </@table>
      
  </@section>
 
</@section>

