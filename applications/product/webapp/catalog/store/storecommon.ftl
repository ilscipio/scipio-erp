<#include "component://product/webapp/catalog/common/common.ftl">

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