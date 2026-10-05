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
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#assign catalogCol = Static["org.ofbiz.product.catalog.CatalogWorker"].getCatalogIdsAvailable(request)!>
<#assign currentCatalogId = Static["org.ofbiz.product.catalog.CatalogWorker"].getCurrentCatalogId(request)!>
<#assign currentCatalogName = Static["org.ofbiz.product.catalog.CatalogWorker"].getCatalogName(request, currentCatalogId)!>

<#-- Only show if there is more than 1 (one) catalog, no sense selecting when there is only one option... -->
<#if (catalogCol?size > 1)>
  <@section title=uiLabelMap.ProductChooseCatalog id="choosecatalog">
    <form name="choosecatalogform" method="post" action="<@pageUrl>main</@pageUrl>">
      <select name="CURRENT_CATALOG_ID" onchange="submit()">
        <option value="${currentCatalogId}">${currentCatalogName}</option>
        <option value="${currentCatalogId}"></option>
        <#list catalogCol as catalogId>
          <#assign thisCatalogName = Static["org.ofbiz.product.catalog.CatalogWorker"].getCatalogName(request, catalogId)>
          <option value="${catalogId}">${thisCatalogName}</option>
        </#list>
      </select>
    </form>
  </@section>
</#if>
