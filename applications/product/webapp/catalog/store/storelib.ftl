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
