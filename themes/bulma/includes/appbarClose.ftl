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
<#--<#include "component://base-theme/includes/appbarClose.ftl">-->
<#macro sideBarMenu>
    <#--<#if (mainSideBarMenuCfg.location)?has_content && (mainSideBarMenuCfg.name)?has_content>
                <@render type="menu" name=mainSideBarMenuCfg.name resource=mainSideBarMenuCfg.location subMenus="all" itemCondMode="disable-with-submenu"/>
            </#if-->
    <#-- NOTE: forced to use global vars because ctxVars suffer from backward-nesting issues with type="section" -->
    <#-- TODO: REVIEW: Here you can configure one of the following
    <@render type="section" name="left-column" globalCtxVars={"menuCfgSubMenuFilter":"current"}/>
    <@render type="section" name="left-column" globalCtxVars={"menuCfgSubMenuFilter":"all", "menuCfgItemCondMode":"disable"}/>
    <@render type="section" name="left-column" globalCtxVars={"menuCfgSubMenuFilter":"all", "menuCfgItemCondMode":"disable-with-submenu"}/>
    -->
    <@render type="section" name="left-column" globalCtxVars={"menuCfgSubMenuFilter":"all", "menuCfgItemCondMode":"disable-with-submenu"}/>
</#macro>

<#-- Sets sidebar based on cookie value -->
<#assign cookies = request.getCookies()!/>
<#assign hasSideBarActive = false />
<#if cookies?has_content>
    <#assign cookieName = "scpSidebar">
    <#list cookies?filter(x -> cookieName == x.name) as x>
        <#if x.value?has_content>
            <#assign hasSideBarActive = (x.value)?boolean />
        </#if>
    </#list>
</#if>

<div class="scp-container<#if hasSideBarActive> is-active</#if>" id="scpwrap">
    <#if userLogin??>
        <div class="scp-sidebar is-hidden-touch pb-0 is-narrow-mobile is-narrow hero is-fullheight is-flex is-flex-direction-column is-justify-content-space-between">
            <div>
                <div class="sidebar-toggle<#if hasSideBarActive> is-active</#if>" aria-label="expand sidebar" aria-expanded="false" data-target="scpwrap"><i class="fa fa-angle-double-left"></i></div>
                <@virtualSection name="Global-Column-Left">
                    <#if htmlwrap?has_content><${htmlwrap} class="menu-list"<#if id?has_content> id="menu_logo"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>></#if>
                    <@sideBarMenu/>
                </@virtualSection>
            </div>
            <footer class="footer">
                <div class="content has-text-centered">
                    <a href="https://www.ilscipio.com" target="_blank">&copy; ilscipio</a>
                </div>
            </footer>
        </div>
    </#if>
    <div class="scp-content">
