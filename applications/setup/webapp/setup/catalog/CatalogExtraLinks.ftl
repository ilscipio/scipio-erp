
<#if defaultWebSiteId?has_content>
<@alert type="info">
  ${uiLabelMap.SetupCatalogExtraStepsInfo}
  <ol>
    <li><a class="catei-link" href="<@ofbizInterWebappUrl uri=('/catalog/control/WebSiteSeo?webSiteId='+rawString(defaultWebSiteId)) extLoginKey=true escapeAs='html'/>"<#rt/>
        <#lt/> target="_blank">${uiLabelMap.PageTitleWebSiteSEO} + ${uiLabelMap.ContentGenerateSitemaps}</a><span class="catei-linkdesc"> - ${uiLabelMap.SetupCatalogSEOFuncDesc}</span></li>
  </ol>
</@alert>
</#if>