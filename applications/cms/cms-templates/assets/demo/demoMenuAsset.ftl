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
<#-- Demo menu. -->
<@menu type=(type!"button")><#--  items=(items![]) args=(menuArgs!{}) -->
    <#assign linkUrl><@pageUrl name="BasicTemplateExample" webSiteId="cmsSite"/></#assign>
    <@menuitem type="link" href=linkUrl text="Basic Template Example" class="+${styles.action_nav}"/>
    
    <#assign linkUrl><@pageUrl name="AdvancedTemplateGlobalExample" webSiteId="cmsBackendSite"/></#assign>
    <@menuitem type="link" href=linkUrl text="Advanced Template Global Example" class="+${styles.action_nav}"/>
    
    <#assign linkUrl><@pageUrl name="AdvancedTemplateShopExample" webSiteId="cmsSite"/></#assign>
    <@menuitem type="link" href=linkUrl text="Advanced Template Shop Example" class="+${styles.action_nav}"/>
    
    <#assign linkUrl><@pageUrl name="DemoPage" webSiteId="cmsSite"/></#assign>
    <@menuitem type="link" href=linkUrl text="DemoPage" class="+${styles.action_nav}"/>
</@menu>
