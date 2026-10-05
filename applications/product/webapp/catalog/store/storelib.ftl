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
<#-- SCIPIO: Common Catalog store utilities and definitions library. May be imported by other applications' templates.
    NOTE: For this application's own templates, please include storecommon.ftl instead (which includes this). -->

<#macro webSiteWarnings webSiteList>
  <#if webSiteList?has_content && (webSiteList?size > 1)>
    <#local hasDefault = false>
    <#list webSiteList as webSite>
      <#if "Y" == (webSite.isStoreDefault!)>
        <#if hasDefault>
          <@alert type="error">${uiLabelMap.ProductMultipleDefaultWebSitesForStore}</@alert>
          <#break>
        <#else>
          <#local hasDefault = true>
        </#if>
      </#if>
    </#list>
    <#if !hasDefault>
      <@alert type="warning">${uiLabelMap.ProductNoDefaultWebSiteForStore}</@alert>
    </#if>
  </#if>
</#macro>
