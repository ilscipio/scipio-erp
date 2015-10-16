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
<#assign catoVariablesIncludeDirective = getRequestVar("catoVariablesIncludeDirective")!"">

<#if catoVariablesIncludeDirective?is_directive && false>
    <#assign catoTemplateIncludeDirective = getRequestVar("catoTemplateIncludeDirective")!"">
<#else>
    <#-- cache these var lookups so it only needs to happen once in a request -->
    <#assign catoVariablesLibraryPath = getRequestVar("catoVariablesLibraryPath")!"">
    
    <#if catoVariablesLibraryPath?has_content>
        <#assign catoTemplateLibraryPath = getRequestVar("catoTemplateLibraryPath")!"">
    <#else>
        <#-- WARN: currently we ALWAYS load HTML versions of our styles and template files, regardless of renderer or if using
                a non-HTML renderer (note: this is okay for catoUtilities.ftl since it's supposed to be platform-agnostic) -->
        <#assign defaultCatoVariablesLibraryPath = 'component://common/webcommon/includes/catoHtmlVariablesDefault.ftl'>
        <#assign defaultCatoTemplateLibraryPath = 'component://common/webcommon/includes/catoHtmlTemplateDefault.ftl'>
        
        <#-- note: rendererVisualThemeResources is set by cato-patched renderer -->
        <#switch getRenderContextType()>
        <#case "web">
            <#assign catoVariablesLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_VAR_WEB"][0])!(rendererVisualThemeResources["VT_STL_VAR_LOC"][0])!defaultCatoVariablesLibraryPath)>
            <#assign catoTemplateLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_TMPLT_WEB"][0])!(rendererVisualThemeResources["VT_STL_TMPLT_LOC"][0])!defaultCatoTemplateLibraryPath)>
            <#break>
        <#case "email">
            <#assign catoVariablesLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_VAR_MAIL"][0])!(rendererVisualThemeResources["VT_STL_VAR_LOC"][0])!defaultCatoVariablesLibraryPath)>
            <#assign catoTemplateLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_TMPLT_MAIL"][0])!(rendererVisualThemeResources["VT_STL_TMPLT_LOC"][0])!defaultCatoTemplateLibraryPath)>
            <#break>
        <#--<#case "general">-->
        <#default>
            <#assign catoVariablesLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_VAR_LOC"][0])!defaultCatoVariablesLibraryPath)>
            <#assign catoTemplateLibraryPath = StringUtil.wrapString((rendererVisualThemeResources["VT_STL_TMPLT_LOC"][0])!defaultCatoTemplateLibraryPath)>
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