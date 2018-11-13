<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#-- Only show if there is more than 1 (one) catalog, no sense selecting when there is only one option... -->
<#if (catalogCol?size > 1)>
    <@section title=uiLabelMap.ProductChooseCatalog>
        <form name="choosecatalogform" method="post" action="<@ofbizUrl>choosecatalog</@ofbizUrl>">
          <select name="CURRENT_CATALOG_ID">
            <option value="${currentCatalogId}">${currentCatalogName}</option>
            <option value="${currentCatalogId}"></option>
            <#list catalogCol as catalogId>
              <#assign thisCatalogName = Static["org.ofbiz.product.catalog.CatalogWorker"].getCatalogName(request, catalogId)>
              <option value="${catalogId}">${thisCatalogName}</option>
            </#list>
          </select>
          <div><a href="javascript:document.choosecatalogform.submit()" class="${styles.link_run_session!} ${styles.action_select!}">${uiLabelMap.CommonChange}</a></div>
        </form>
    </@section>
</#if>
