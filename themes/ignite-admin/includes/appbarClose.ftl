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
        <#-- Macro for rendering the sidebar. Relies on a tiny screenwidget that we are using for rendering menus-->
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
        <div class="app-body">
            <#if userLogin??>
                <div class="sidebar">
                  <@virtualSection name="Global-Column-Left">
                    <@sideBarMenu/>
                  </@virtualSection>
                <#-- Activate for additional resizing option
                <button class="sidebar-minimizer brand-minimizer" type="button"></button>-->
                </div>
            </#if>
            <main class="main">