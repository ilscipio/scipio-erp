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
<#include "component://cms/webapp/cms/common/common.ftl">

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makePageUrl("editScript") class="+${styles.action_nav!} ${styles.action_add!}" text=uiLabelMap.CmsNewScript/>
        <@menuitem type="link" href=makePageUrl(("Y" == showStandaloneOnly)?string("scripts", "scripts?showStandaloneOnly=Y")) class="+${styles.action_run_sys!} ${styles.action_show!}" 
            text=("Y" == showStandaloneOnly)?string(uiLabelMap.CommonShowAll, uiLabelMap.CmsShowStandaloneOnly)/>
    </@menu>  
</#macro>
<@section title=uiLabelMap.CmsScripts menuContent=menuContent>
    <#if scriptList?has_content>
        <@paginate mode="content" url=makePageUrl(("Y" == showStandaloneOnly)?string("scripts?showStandaloneOnly=Y", "scripts")) viewSize=(viewSize!50) viewIndex=(viewIndex!0) listSize=(listSize!0)>
            <@table type="data-list" autoAltRows=true>
                <@thead>
                    <@tr class="header-row">
                        <@th width="100px">${uiLabelMap.CmsScript}</@th>
                        <@th width="100px">${uiLabelMap.CmsWebSite}</@th>
                        <@th width="100px">${uiLabelMap.CommonLanguageTitle}</@th>
                        <@th width="50px">${uiLabelMap.CmsStandalone}</@th>
                        <@th>${uiLabelMap.CommonName}</@th>
                        <@th>${uiLabelMap.CommonDescription}</@th>
                    </@tr>
                </@thead>
                <#list scriptList as scriptEntity>
                    <#assign scriptTmpl = cmsScriptUtil.getScriptTemplateFromEntity(scriptEntity)>
                    <@tr>
                        <@td>${scriptTmpl.id!}</@td>
                        <@td>${scriptTmpl.webSiteId!}</@td>
                        <@td>${scriptTmpl.resolvedScriptLang!"invalid"}</@td>
                        <@td>${scriptTmpl.standalone?string("Y", "N")}</@td>
                        <@td><a href="<@pageUrl>editScript?scriptTemplateId=${scriptTmpl.id!}</@pageUrl>">${scriptTmpl.templateName!}</a></@td>
                        <@td>${makeShortCmsDesc(scriptTmpl.getDescription(locale)!)}</@td>
                    </@tr>
                </#list>
            </@table>
        </@paginate>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>
</@section>
