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

<#-- these can be included early, for use in this file, since meant to be everything-agnostic -->
<#include 'component://common/webcommon/includes/catoUtilities.ftl'>

<#-- cache these template interprets so they only need to happen once in a request -->
<#assign catoVariablesIncludeDirective = getRequestVar("catoVariablesIncludeDirective")!false>

<#if catoVariablesIncludeDirective?is_directive>
    <#assign catoTemplateIncludeDirective = getRequestVar("catoTemplateIncludeDirective")>
<#else>
    <#-- cache these var lookups so it only needs to happen once in a request -->
    <#assign catoVariablesLibraryPath = getRequestVar("catoVariablesLibraryPath")!false>
    
    <#if catoVariablesLibraryPath?is_string>
        <#assign catoTemplateLibraryPath = getRequestVar("catoTemplateLibraryPath")!"">
    <#else>
        <#-- note: rendererVisualThemeResources is set by cato-patched renderer -->
        <#assign renderPlatformType = getRenderPlatformType()!"default">
        <#assign renderContextType = getRenderContextType()!"general">

        <#switch renderContextType>
        <#case "web">
            <#-- note: visual theme resources are locations are currently assumed to apply as html and default platform only;
                have no overrides for xml, fo, etc.; not supported for now
                FIXME?: despite this, currently the VT resources will always load for all platforms... -->
            <#assign catoVariablesLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_VAR_WEB"][0])!(rendererVisualThemeResources["VT_STL_VAR_LOC"][0])!getDefaultCatoLibLocation("variables", renderPlatformType, renderContextType)!"")?string>
            <#assign catoTemplateLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_TMPLT_WEB"][0])!(rendererVisualThemeResources["VT_STL_TMPLT_LOC"][0])!getDefaultCatoLibLocation("template", renderPlatformType, renderContextType)!"")?string>
            <#break>
        <#case "email">
            <#assign catoVariablesLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_VAR_MAIL"][0])!(rendererVisualThemeResources["VT_STL_VAR_LOC"][0])!getDefaultCatoLibLocation("variables", renderPlatformType, renderContextType)!"")?string>
            <#assign catoTemplateLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_TMPLT_MAIL"][0])!(rendererVisualThemeResources["VT_STL_TMPLT_LOC"][0])!getDefaultCatoLibLocation("template", renderPlatformType, renderContextType)!"")?string>
            <#break>
        <#--<#case "general">-->
        <#default>
            <#assign catoVariablesLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_VAR_LOC"][0])!getDefaultCatoLibLocation("variables", renderPlatformType, renderContextType)!"")?string>
            <#assign catoTemplateLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_TMPLT_LOC"][0])!getDefaultCatoLibLocation("template", renderPlatformType, renderContextType)!"")?string>
            <#break>
        </#switch>
    
        <#assign dummy = setRequestVar("catoVariablesLibraryPath", catoVariablesLibraryPath)>
        <#assign dummy = setRequestVar("catoTemplateLibraryPath", catoTemplateLibraryPath)>
    </#if>

    <#if catoVariablesLibraryPath?has_content>
        <#assign catoVariablesIncludeDirective = ('<#include "' + catoVariablesLibraryPath + '">')?interpret>
    <#else>
        <#assign catoVariablesIncludeDirective = ('<#assign dummy = "">')?interpret>
    </#if>
    <#if catoTemplateLibraryPath?has_content>
        <#assign catoTemplateIncludeDirective = ('<#include "' + catoTemplateLibraryPath + '">')?interpret>
    <#else>
        <#assign catoTemplateIncludeDirective = ('<#assign dummy = "">')?interpret>
    </#if>

    <#assign dummy = setRequestVar("catoVariablesIncludeDirective", catoVariablesIncludeDirective)>
    <#assign dummy = setRequestVar("catoTemplateIncludeDirective", catoTemplateIncludeDirective)>
</#if>

<#-- Do the platform-dependent lib includes. -->
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