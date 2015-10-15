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
  <#local res = getRequestVar("catoRenderContextType")!"">
  <#if res?has_content>
    <#return res>
  <#else>
    <#-- FIXME?: this detection is primitive... not sure covers all possible cases... -->
    <#if request??>
        <#local res = "web">
    <#else>
        <#if baseUrl??>
            <#local res = "email">
        <#else>
            <#local res = "general">
        </#if>
    </#if>

    <#-- save -->
    <#local dummy = setRequestVar("catoRenderContextType", res)!>
    <#return res>
  </#if>
</#function>

<#global catoRenderContextType = getRenderContextType()>
<#-- catoRenderContextType: ${catoRenderContextType} -->
<#-- request present? ${(request??)?c} -->

<#-- cache these template interprets so they only need to happen once in a request -->
<#assign catoVariablesIncludeDirective = getRequestVar("catoVariablesIncludeDirective")!"">

<#if catoVariablesIncludeDirective?is_directive && false>
    <#assign catoTemplateIncludeDirective = getRequestVar("catoTemplateIncludeDirective")!"">
<#else>
    <#-- cache these var lookups so it only needs to happen once in a request -->
    <#assign catoVariablesLibraryPath = getRequestVar("catoVariablesLibraryPath")!"">
    
    <#if catoVariablesLibraryPath?has_content>
        <#assign catoTemplateLibraryPath = getRequestVar("catoTemplateLibraryPath")!"">
    <#else>
        <#-- note: rendererVisualThemeResources is set by cato-patched renderer -->
        <#switch catoRenderContextType>
        <#case "web">
            <#assign catoVariablesLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_VAR_WEB"][0])!(rendererVisualThemeResources["VT_STL_VAR_LOC"][0])!'component://common/webcommon/includes/catoHtmlVariablesDefault.ftl')>
            <#assign catoTemplateLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_TMPLT_WEB"][0])!(rendererVisualThemeResources["VT_STL_TMPLT_LOC"][0])!'component://common/webcommon/includes/catoHtmlTemplateDefault.ftl')>
            <#break>
        <#case "email">
            <#assign catoVariablesLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_VAR_MAIL"][0])!(rendererVisualThemeResources["VT_STL_VAR_LOC"][0])!'component://common/webcommon/includes/catoHtmlVariablesDefault.ftl')>
            <#assign catoTemplateLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_TMPLT_MAIL"][0])!(rendererVisualThemeResources["VT_STL_TMPLT_LOC"][0])!'component://common/webcommon/includes/catoHtmlTemplateDefault.ftl')>
            <#break>
        <#--<#case "general">-->
        <#default>
            <#assign catoVariablesLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_VAR_LOC"][0])!'component://common/webcommon/includes/catoHtmlVariablesDefault.ftl')>
            <#assign catoTemplateLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_TMPLT_LOC"][0])!'component://common/webcommon/includes/catoHtmlTemplateDefault.ftl')>
            <#break>
        </#switch>
    
        <#assign dummy = setRequestVar("catoVariablesLibraryPath", catoVariablesLibraryPath)>
        <#assign dummy = setRequestVar("catoTemplateLibraryPath", catoTemplateLibraryPath)>
    </#if>

    <#assign catoVariablesIncludeDirective = ('<#include "' + catoVariablesLibraryPath + '">')?interpret>
    <#assign catoTemplateIncludeDirective = ('<#include "' + catoTemplateLibraryPath + '">')?interpret>

    <#assign dummy = setRequestVar("catoVariablesIncludeDirective", catoVariablesIncludeDirective)>
    <#assign dummy = setRequestVar("catoTemplateIncludeDirective", catoTemplateIncludeDirective)>
</#if>

<#include 'component://common/webcommon/includes/catoUtilities.ftl'>
<@catoVariablesIncludeDirective />
<@catoTemplateIncludeDirective />



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