<#compress>
<#--
* 
* Scipio main include script.
*
* Ensures include order, #compress, keeps other templates clean and (TODO) per-context include loading.
* Automatically included at all times.
*
* DEV NOTES: 
* * Should try to avoid #compress in ALL OTHER macros and templates in general because sometimes behaves oddly in Ofbiz.
*   The one in this file should (hopefully) be OK because this include produces no markup.
* * This essentially takes the place of a renderer prep hook.
*   Maybe should be done from java/renderer prep for performance reasons
*   (FreeMarkerWorker maybe) but currently this is only surefire way to ensure include 
*   script at least always runs (in reality there could be a higher level hook than FreeMarkerWorker
*   but doesn't seem to exist in ofbiz).
* * Variables like "request" are unavailable here in general in OOTB ofbiz,
*   but minor scipio renderer patches ("initial context mod", survey renderer) try to make some available 
*   so they can be used. however, only use the major one likes "delegator" and "request"; 
*   rest of context may not be current. note "request" not available in emails.
*
-->

<#assign scipioOrigMainNsNamesSet = toSet(.main?keys)>

<#-- these can be included early, for use in this file, since meant to be everything-agnostic -->
<#include 'component://common/webcommon/includes/scipio/lib/utilities.ftl'>

<#-- cache these template interprets so they only need to happen once in a request 
    NOTE: scipioVariablesIncludeDirective is currently not really reused, is an artifact from prior code that had less caching -->
<#assign scipioVariablesIncludeDirective = getRequestVar("scipioVariablesIncludeDirective")!false>

<#if scipioVariablesIncludeDirective?is_directive>
    <#assign scipioTemplateIncludeDirective = getRequestVar("scipioTemplateIncludeDirective")>
    <#-- get the cached variables and styles -->
    <#assign dummy = globalsPutAll(getRequestVar("scipioTmplGlobalVars")!{})>
<#else>
    <#-- cache these var lookups so it only needs to happen once in a request -->
    <#assign scipioVariablesLibraryPath = getRequestVar("scipioVariablesLibraryPath")!false>
    
    <#if scipioVariablesLibraryPath?is_string>
        <#assign scipioTemplateLibraryPath = getRequestVar("scipioTemplateLibraryPath")!"">
    <#else>
        <#-- note: rendererVisualThemeResources is set by scipio-patched renderer -->
        <#assign renderPlatformType = getRenderPlatformType()!"default">
        <#assign renderContextType = getRenderContextType()!"general">

        <#switch renderContextType>
        <#case "web">
            <#assign scipioVariablesLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_VAR_WEB", "VT_STL_VAR_LOC")!getDefaultScipioLibLocation("variables", renderPlatformType, renderContextType)!"">
            <#assign scipioTemplateLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_TMPLT_WEB", "VT_STL_TMPLT_LOC")!getDefaultScipioLibLocation("template", renderPlatformType, renderContextType)!"">
            <#break>
        <#case "email">
            <#assign scipioVariablesLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_VAR_MAIL", "VT_STL_VAR_LOC")!getDefaultScipioLibLocation("variables", renderPlatformType, renderContextType)!"">
            <#assign scipioTemplateLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_TMPLT_MAIL", "VT_STL_TMPLT_LOC")!getDefaultScipioLibLocation("template", renderPlatformType, renderContextType)!"">
            <#break>
        <#--<#case "general">-->
        <#default>
            <#assign scipioVariablesLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_VAR_LOC")!getDefaultScipioLibLocation("variables", renderPlatformType, renderContextType)!"">
            <#assign scipioTemplateLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_TMPLT_LOC")!getDefaultScipioLibLocation("template", renderPlatformType, renderContextType)!"">
            <#break>
        </#switch>
    
        <#assign dummy = setRequestVar("scipioVariablesLibraryPath", scipioVariablesLibraryPath)>
        <#assign dummy = setRequestVar("scipioTemplateLibraryPath", scipioTemplateLibraryPath)>
    </#if>


    <#-- 2016-05-04: We now support and prefer groovy-based var data -->
    <#if scipioVariablesLibraryPath?ends_with(".groovy")>
        <#assign scipioVariablesIncludeDirective = ('<#assign dummy = "">')?interpret>
    <#elseif scipioVariablesLibraryPath?ends_with(".ftl")>
        <#-- DEPRECATED -->
        <#assign scipioVariablesIncludeDirective = ('<#include "' + scipioVariablesLibraryPath + '">')?interpret>
    <#else>
        <#assign scipioVariablesIncludeDirective = ('<#assign dummy = "">')?interpret>
    </#if>
    <#if scipioTemplateLibraryPath?has_content>
        <#assign scipioTemplateIncludeDirective = ('<#include "' + scipioTemplateLibraryPath + '">')?interpret>
    <#else>
        <#assign scipioTemplateIncludeDirective = ('<#assign dummy = "">')?interpret>
    </#if>

    <#assign dummy = setRequestVar("scipioVariablesLibraryPath", scipioVariablesLibraryPath)>
    <#assign dummy = setRequestVar("scipioTemplateLibraryPath", scipioTemplateLibraryPath)>
    
    <#assign dummy = setRequestVar("scipioVariablesIncludeDirective", scipioVariablesIncludeDirective)>
    <#assign dummy = setRequestVar("scipioTemplateIncludeDirective", scipioTemplateIncludeDirective)>
    
    <#-- Include and cache the global variables -->
    <#if scipioVariablesLibraryPath?ends_with(".groovy")>
        <#-- NOTE: Here we can do adapters OR copies. adapters will make faster screenwidget access,
            while copying will make faster FTL access. We'll simply do both for now, in advance. 
            NOTE: we use the simplest Freemarker ObjectWrapper here (basic), but templates are best to use the current-derived ones. -->
        <#assign scipioTmplGlobalVarsAdapted = rewrapMap(Static["org.ofbiz.base.util.GroovyUtil"].runScriptAtLocationNewEmptyContext(scipioVariablesLibraryPath, ""), "basic-adapter", "always-deep")>
        <#assign scipioTmplGlobalVars = rewrapMap(scipioTmplGlobalVarsAdapted, "basic-copy", "always-deep")>
        <#assign dummy = varsPutAll(scipioTmplGlobalVars)>
    <#elseif scipioVariablesLibraryPath?ends_with(".ftl")>
        <#-- DEPRECATED -->
        <#assign scipioMainNsPreGlobalVarsNames = toSet(.main?keys)>
        <#-- Main theme variables include -->
        <@scipioVariablesIncludeDirective />
        <#assign scipioMainNsPostGlobalVarsNames = toSet(.main?keys)>
        <#assign dummy = scipioMainNsPostGlobalVarsNames.removeAll(scipioMainNsPreGlobalVarsNames)!>
        <#assign dummy = scipioMainNsPostGlobalVarsNames.remove("scipioMainNsPreGlobalVarsNames")!>
        <#assign scipioTmplGlobalVars = copyMap(.main, "i", scipioMainNsPostGlobalVarsNames)>
        <#assign scipioTmplGlobalVarsAdapted = scipioTmplGlobalVars>
    </#if>

    <#-- make the styles var persist for anything that might need it (usually not FTL, for now always reincluded above) 
        NOTE: is guaranteed to stay FTL-wrapped; any outside code reading back must use FtlTransformUtil.unwrapXxx 
        NOTE: would have to do this even if there was no caching, because non-FTL currently needs to read back -->
    <#assign dummy = setRequestVar("scipioTmplGlobalVars", scipioTmplGlobalVars, "w")>
    <#assign dummy = setRequestVar("scipioTmplGlobalVarsAdapted", scipioTmplGlobalVarsAdapted, "w")>
        
    <#-- dump the styles into the global vars (we have no namespace) -->
    <#assign dummy = globalsPutAll(scipioTmplGlobalVars)>
</#if>

<#-- Do the platform-dependent lib always-includes. -->

<#-- Main theme macros include (TODO?: is there any way to cache this further? (like global vars)) -->
<@scipioTemplateIncludeDirective />

<#-- FIXME?L For now we must copy/dump all scipio macro and function defs from main namespace into the global namespace manually.
     Easier with a loop for now but this assumes all are meant to be public (not true)...
     If not done they are not accessible from #import-ed libraries and other places.
     This is also done so that the behavior is the same as the java-based ofbiz transforms such as @ofbizUrl,
     which are in global namespace.
<#list .main?keys as name>
  <#if .main[name]?is_directive && !scipioOrigMainNsNamesSet.contains(name)>
    <@"<#global '${name}'=.main['${name}']>"?interpret />
  </#if>
</#list> -->
<#assign dummy = globalsPutAll(.main, "e", scipioOrigMainNsNamesSet)>

<#-- compatibility mode: define styles hash entries as individual style_ vars
<#list mapKeys(styles) as name>
  <@'<#global "style_${name}" = "${styles[name]}">'?interpret />
</#list>-->

</#compress>