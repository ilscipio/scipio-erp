<#compress>
<#--
* 
* Cato main include script.
* Ensures include order, #compress, keeps other templates clean and (TODO) per-context include loading.
* Automatically included at all times.
*
* DEV NOTE: should try to avoid #compress in general because sometimes behaves oddly in Ofbiz.
*     the one in this file should (hopefully) be ok because this include produces no markup.
* DEV NOTE: this essentially takes the place of a renderer prep hook.
*     maybe should be done from java/renderer prep for performance reasons
*     (FreeMarkerWorker maybe) but currently this is only surefire way to ensure include 
*     script at least always runs (in reality there could be a higher level hook than FreeMarkerWorker
*     but doesn't seem to exist in ofbiz).
* DEV NOTE: variables like "request" are unavailable here in general in OOTB ofbiz,
*     but minor cato renderer patches ("initial context mod", survey renderer) try to make some available 
*     so they can be used. however, only use the major one likes "delegator" and "request"; 
*     rest of context may not be current. note "request" not available in emails.
*
-->

<#assign catoOrigMainNsNamesSet = Static["org.ofbiz.base.util.UtilMisc"].toSet(.main?keys)>

<#-- these can be included early, for use in this file, since meant to be everything-agnostic -->
<#include 'component://common/webcommon/includes/cato/lib/utilities.ftl'>

<#-- cache these template interprets so they only need to happen once in a request -->
<#assign catoVariablesIncludeDirective = getRequestVar("catoVariablesIncludeDirective")!false>

<#if catoVariablesIncludeDirective?is_directive>
    <#assign catoTemplateIncludeDirective = getRequestVar("catoTemplateIncludeDirective")>
    <#-- get the cached variables and styles -->
    <#assign dummy = globalsPutAll(getRequestVar("catoTmplGlobalVars")!{})>
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
            <#assign catoVariablesLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_VAR_WEB", "VT_STL_VAR_LOC")!getDefaultCatoLibLocation("variables", renderPlatformType, renderContextType)!"">
            <#assign catoTemplateLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_TMPLT_WEB", "VT_STL_TMPLT_LOC")!getDefaultCatoLibLocation("template", renderPlatformType, renderContextType)!"">
            <#break>
        <#case "email">
            <#assign catoVariablesLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_VAR_MAIL", "VT_STL_VAR_LOC")!getDefaultCatoLibLocation("variables", renderPlatformType, renderContextType)!"">
            <#assign catoTemplateLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_TMPLT_MAIL", "VT_STL_TMPLT_LOC")!getDefaultCatoLibLocation("template", renderPlatformType, renderContextType)!"">
            <#break>
        <#--<#case "general">-->
        <#default>
            <#assign catoVariablesLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_VAR_LOC")!getDefaultCatoLibLocation("variables", renderPlatformType, renderContextType)!"">
            <#assign catoTemplateLibraryPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, "VT_STL_TMPLT_LOC")!getDefaultCatoLibLocation("template", renderPlatformType, renderContextType)!"">
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
    
    <#-- Include and cache the global variables -->
    <#assign catoMainNsPreGlobalVarsNames = Static["org.ofbiz.base.util.UtilMisc"].toSet(.main?keys)>
    <#-- Main theme variables include -->
    <@catoVariablesIncludeDirective />
    <#assign catoMainNsPostGlobalVarsNames = Static["org.ofbiz.base.util.UtilMisc"].toSet(.main?keys)>
    <#assign dummy = catoMainNsPostGlobalVarsNames.removeAll(catoMainNsPreGlobalVarsNames)!>
    <#assign dummy = catoMainNsPostGlobalVarsNames.remove("catoMainNsPreGlobalVarsNames")!>
    <#assign catoTmplGlobalVars = copyMap(.main, "i", catoMainNsPostGlobalVarsNames)>

    <#-- make the styles var persist for anything that might need it (usually not FTL, for now always reincluded above) 
        NOTE: is guaranteed to stay FTL-wrapped; any outside code reading back must use FtlTransformUtil.unwrapXxx 
        NOTE: would have to do this even if there was no caching, because non-FTL currently needs to read back -->
    <#assign dummy = setRequestVar("catoTmplGlobalVars", catoTmplGlobalVars, "w")>
    
    <#-- dump the styles into the global vars (we have no namespace) -->
    <#assign dummy = globalsPutAll(catoTmplGlobalVars)>
</#if>

<#-- Do the platform-dependent lib always-includes. -->

<#-- Main theme macros include (TODO? is there any way to cache this further? (like global vars)) -->
<@catoTemplateIncludeDirective />

<#-- FIXME? For now we must copy/dump all cato macro and function defs from main namespace into the global namespace manually.
     Easier with a loop for now but this assumes all are meant to be public (not true)...
     If not done they are not accessible from #import-ed libraries and other places.
     This is also done so that the behavior is the same as the java-based ofbiz transforms such as @ofbizUrl,
     which are in global namespace.
<#list .main?keys as name>
  <#if .main[name]?is_directive && !catoOrigMainNsNamesSet.contains(name)>
    <@"<#global '${name}'=.main['${name}']>"?interpret />
  </#if>
</#list> -->
<#assign dummy = globalsPutAll(.main, "e", catoOrigMainNsNamesSet)>

<#-- compatibility mode: define styles hash entries as individual style_ vars
<#list mapKeys(styles) as name>
  <@'<#global "style_${name}" = "${styles[name]}">'?interpret />
</#list>-->

</#compress>