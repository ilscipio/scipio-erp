<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#assign catalogCol = Static["org.ofbiz.product.catalog.CatalogWorker"].getCatalogIdsAvailable(request)!>
<#assign currentCatalogId = Static["org.ofbiz.product.catalog.CatalogWorker"].getCurrentCatalogId(request)!>
<#assign currentCatalogName = Static["org.ofbiz.product.catalog.CatalogWorker"].getCatalogName(request, currentCatalogId)!>

<#-- Only show if there is more than 1 (one) catalog, no sense selecting when there is only one option... -->
<#if (catalogCol?size > 1)>
  <@section title=uiLabelMap.ProductChooseCatalog id="choosecatalog">
    <form name="choosecatalogform" method="post" action="<@ofbizUrl>main</@ofbizUrl>">
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
