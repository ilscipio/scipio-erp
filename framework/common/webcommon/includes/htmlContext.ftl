<#compress>

<#--
* 
* Context detection and setup.
* Shared by the common includes themselves.
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

</#compress>