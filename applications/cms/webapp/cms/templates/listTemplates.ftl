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
        <@menuitem type="link" href=makePageUrl("editTemplate") class="+${styles.action_nav!} ${styles.action_add!}" text=uiLabelMap.CmsNewTemplate/>
    </@menu>  
</#macro>
<@section title=uiLabelMap.CmsTemplates menuContent=menuContent>
    <#if templateList?has_content>
        <@paginate mode="content" url=makePageUrl("templates") viewSize=(viewSize!50) viewIndex=(viewIndex!0) listSize=(listSize!0)>
            <@table type="data-list" autoAltRows=true>
                <@thead>
                    <@tr class="header-row">
                        <@th width="100px">${uiLabelMap.CmsTemplate}</@th>
                        <@th width="200px">${uiLabelMap.CmsWebSite}</@th>
                        <@th>${uiLabelMap.CmsTemplateName}</@th>
                        <@th>${uiLabelMap.CommonDescription}</@th>
                    </@tr>
                </@thead>
                <#list templateList as template>
                    <#assign templateModel = Static["com.ilscipio.scipio.cms.template.CmsPageTemplate"].getWorker().makeFromValue(template)>
                    <@tr>
                        <@td>${templateModel.id}</@td>
                        <@td>${templateModel.webSiteId!}</@td>
                        <@td><a href="<@pageUrl>editTemplate?pageTemplateId=${templateModel.id}</@pageUrl>">${templateModel.name!}</a></@td>
                        <@td>${makeShortCmsDesc(templateModel.getDescription(locale)!)}</@td>
                    </@tr>
                </#list>
            </@table>
        </@paginate>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>
</@section>