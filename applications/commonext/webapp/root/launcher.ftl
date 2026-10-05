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
<#-- SCIPIO: 4.0.0: The application launcher at the server root: every application the reader may open. -->
<#assign ofbizServerName = application.getAttribute("_serverId")!"default-server">
<#-- The key signs the reader in to the application they open, so one login at "/" is enough. -->
<#assign externalKeyParam = "">
<#if (requestAttributes.externalLoginKey)?has_content><#assign externalKeyParam = "?externalLoginKey=" + raw(requestAttributes.externalLoginKey)></#if>
<#assign loginWorker = Static["org.ofbiz.webapp.control.LoginWorker"]>
<#assign appGroups = [
    {"title": uiLabelMap.CommonPrimaryApps, "apps": loginWorker.getAppBarWebInfos(security, userLogin, ofbizServerName, "main")},
    {"title": uiLabelMap.CommonSecondaryApps, "apps": loginWorker.getAppBarWebInfos(security, userLogin, ofbizServerName, "secondary")}
]>

<div class="au-launcher">
    <div class="au-launcher-head">
        <h1 class="au-launcher-title">${uiLabelMap.CommonApplications}</h1>
        <#-- The theme turns this into the jump dialog (Ctrl K); without it, it is a plain button. -->
        <button type="button" class="au-jump au-launcher-search" data-au-jump-open="true" aria-haspopup="dialog" aria-controls="au-jump">
            <i class="fa fa-search" aria-hidden="true"></i>
            <span class="au-jump-label">${uiLabelMap.CommonJumpTo}</span>
            <kbd class="au-kbd">Ctrl K</kbd>
        </button>
    </div>
    <#list appGroups as group>
        <#if group.apps?has_content>
            <section class="au-launcher-group">
                <h2 class="au-launcher-heading">${group.title}</h2>
                <ul class="au-launcher-list">
                    <#list group.apps as display>
                        <#assign servletPath = Static["org.ofbiz.webapp.WebAppUtil"].getControlLinkPathSafeSlash(display)!"">
                        <#assign appUrl = servletPath?has_content?then(raw(servletPath) + "main", display.getContextRoot())>
                        <#assign appTitle = (uiLabelMap[display.title])!display.title>
                        <#assign appDesc = (uiLabelMap[display.description])!display.description!"">
                        <li>
                            <a href="${appUrl}${externalKeyParam}">
                                <span class="au-launcher-icon"><i class="${styles.icon!} ${(styles.app_icon[display.name])!'fa-folder-o'}" aria-hidden="true"></i></span>
                                <span>
                                    <span class="au-launcher-name">${appTitle}</span>
                                    <#if appDesc?has_content && appDesc != appTitle><span class="au-launcher-desc">${appDesc}</span></#if>
                                </span>
                            </a>
                        </li>
                    </#list>
                </ul>
            </section>
        </#if>
    </#list>
</div>
