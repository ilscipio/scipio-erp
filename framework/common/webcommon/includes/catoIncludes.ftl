<#compress>
<#--
* 
* Cato main include script.
* Ensures include order, #compress, keeps other templates clean and (TODO) per-context include loading.
* Automatically included at all times.
*
* DEV NOTE: this essentially takes the place of a renderer prep hook.
* maybe should be done from java/renderer prep for performance reasons
* (FreeMarkerWorker maybe) but currently this is only surefire way to ensure include 
* script at least always runs (in reality there could be a higher level hook than FreeMarkerWorker
* but doesn't seem to exist in ofbiz).
* DEV NOTE: variables like "request" are unavailable here in general in OOTB ofbiz,
* but minor cato renderer patches ("initial context mod", survey renderer) try to make some available 
* so they can be used. however, only use the major one likes "delegator" and "request"; 
* rest of context may not be current. note "request" not available in emails.
*
-->

<#assign catoOrigMainNsNamesSet = Static["org.ofbiz.base.util.UtilMisc"].toSet(.main?keys)>

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
    <#-- FIXME?: this detection is primitive... not sure covers all possible cases... -->
    <#if request??>
        <#-- ideally will have per-webapp <WebSite> visual theme loading support, and
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

<#include 'component://common/webcommon/includes/catoUtilities.ftl'>

<#-- TODO: should have support for per-context-type and per-site loading -->
<#switch catoRenderContextType>
<#case "web">
<#case "email">
<#case "general">
<#default>
    <@"<#include 'component://common/webcommon/includes/catoHtmlVariablesDefault.ftl'>"?interpret />
    <@"<#include 'component://common/webcommon/includes/catoHtmlTemplateDefault.ftl'>"?interpret />
<#break>
</#switch>

<#-- FIXME? For now we must copy/dump all cato macro and function defs from main namespace into the global namespace manually.
     Easier with a loop for now but this assumes all are meant to be public (not true)...
     If not done they are not accessible from #import-ed libraries and other places 
     (see html widget macro libs which have problems from this, as well with circular dependencies). 
     NOTE: @ofbizUrl is in global namespace for example.
     May want to revisit use of namespaces later because they are meant to address problems like this... -->
<#list .main?keys as name>
  <#if .main[name]?is_directive && !catoOrigMainNsNamesSet.contains(name)>
    <@"<#global '${name}'=.main['${name}']>"?interpret />
  </#if>
</#list>

<#-- compatibility mode: define styles hash entries as individual style_ vars
<#list styles?keys as name>
  <@'<#global "style_${name}" = "${styles[name]}">'?interpret />
</#list>-->

</#compress>