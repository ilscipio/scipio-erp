<#compress>

<#--
* 
* Cato main include script.
*
* Ensures include order, #compress and (TODO) per-context include loading.
*
* Dev note: maybe should be done from java/renderer prep for performance reasons
* (FreeMarkerWorker maybe) but currently this is only surefire way to ensure include 
* script at least always runs.
*
-->

<#function getRenderContextType>
  <#local res = "">
  <#-- check cache -->
  <#if request??>
    <#local res = request.getAttribute("catoRenderContextType")!"">
  <#else>
    <#local res = .globals["catoRenderContextType"]!"">
  </#if>
  <#if res?has_content>
    <#return res>
  <#else>
    <#-- FIXME?: this detection is primitive... not sure covers all possible cases...
         note: "request" and "application" only generally available since cato renderer patches; 
         not available in many stock web OOTB situations. -->
    <#if request??>
        <#-- TODO: ideally will have per-webapp <WebSite> visual theme loading support, and
             don't want to hardcode this.
        <#if (application.getAttribute("webSiteId")!"") == "WebStore" ||
            (application.getAttribute("localDispatcherName")!"") == "ecommerce">
            <#local res = "web-frontend">
        <#else>
            <#local res = "web-backend">
        </#if>
         -->
        <#local res = "web">
    <#else>
        <#if baseUrl??>
            <#local res = "email">
        <#else>
            <#local res = "general">
        </#if>
    </#if>

    <#-- save in cache -->
    <#if request??>
      <#local dummy = request.setAttribute("catoRenderContextType", res)!>
    </#if>
    <#global catoRenderContextType = res>
    <#return res>
  </#if>
</#function>

<#global catoRenderContextType = getRenderContextType()>

<#-- catoRenderContextType: ${catoRenderContextType} -->
<#-- request present? ${(request??)?c} -->

<#-- TODO: should have support for per-context-type and per-site loading -->
<#switch catoRenderContextType>
<#case "web">
<#case "email">
<#case "general">
<#default>
    <@"<#include 'component://common/webcommon/includes/catoHtmlVariablesDefault.ftl'>"?interpret />
    <@"<#include 'component://common/webcommon/includes/catoUtilities.ftl'>"?interpret />
<#break>
</#switch>

<#-- compatibility mode: define styles hash entries as individual style_ vars
<#list styles?keys as name>
  <@'<#global "style_${name}" = "${styles[name]}">'?interpret />
</#list>-->

</#compress>