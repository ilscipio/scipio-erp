<#--
* 
* Master HTML template include, standard Cato markup.
*
* A set of HTML templating macros, part of standard Cato Freemarker API.
* Automatically included at all times, unless overridden by properties or themes.
* Intended to be swappable.
* Includes all other default Cato macros.
*
* NOTE: currently targeted toward Foundation CSS.
*
* NOTE: In general, macros expect to be called using named arguments (not supported for functions),
*     except where otherwise noted.
*
* IMPL NOTE: Macros should avoid using "request" directly (use setRequestVar/getRequestVar/other).
*
* DEV NOTE: some macros use attribs and inlineAttribs args to specify extra HTML attribs.
*     even though it would be convenient, we can't allow a "attribString" arg because no way
*     for macro to get attribs out of it if it needs them, cause problems.
*     FIXME: not all macros currently properly check attribMap for duplicate attribs
*         of args and inlineAttribs (priority should be: args - inlineAttribs - attribMap).
*
*
* MACRO INTERFACES
* 
* Cato standard macros have versatile interfaces. In general, all of them expect to invoked
* using named parameters (always <@row class="my-class">, never <@row "my-class">).
*
* Template-facing macros: these macros such as @field, @row, @heading, etc. are meant to be
*   used in templates and can also be overridden by themes (though not preferred method).
*   Most of these use a versatile args pattern that looks like: <@name args={} inlineArgs...>
*   From templates, these macros accept regular inlined parameters as well as a map of parameters
*   using the args map parameter. Some also accept additional arbitrary inlined parameters to be used 
*   as HTML attributes (in both the args map and the inline args).
*   Intuitively, inline args have priority over args passed in the args map.
*   This pattern is especially needed for themes to override the templating-facing macros cleanly.
*   Some non-template-facing macros also use this pattern, and some functions party use it as well (but
*   functions do not support named parameters at this time, so not fully).
*   See mergeArgMaps function in utilities library for more details.
*
* Markup macros (theme overrides): these macros such as @row_markup, @heading_markup, etc. containing
*   the "_markup" name are overridable by themes to provide alternative HTML and sometimes javascript markup.
*   They do not have a versatile interface like the template-facing macros and are intentionally kept
*   simple.
*   Nevertheless, they have some requirements: these macros should always end their parameter list with
*   a varargs catch-all parameter "extraArgs..." so that future changes do not backwards break compability
*   with themes.
*   Most also have an advanced "origArgs={}" parameter that will contain the combined arguments map of the parameters
*   that were passed to the calling macro (WARN: this may not necessarily be a template-facing macro; it may
*   be intermediate). In most cases it should not be used; it is provided only in case the other regular parameters
*   do not contain the information needed. It should be considered a last resort.
*   TODO: there needs to be a way to pass arbitrary custom args across the line of macros calls. either add
*       a "customArgs={}" to basically all macros or try to do it through origArgs. don't really want to
*       add customArgs everywhere but not sure can be done clearly through origArgs because conflicts with
*       the inline attribs pattern (remaining args used as html attribs) and more map mergings.
*
-->

<#-- 
*************************************
* EXTERNAL IMPORTS AND INCLUDES *
*************************************
* NOTE: Assumes utilities.ftl and htmlVariables.ftl included.
-->

<#-- (currently none) -->

<#-- As of dependencies rework, cato libs should have no dependencies on the stock macro libraries.
    The stock macros are implemented using cato libraries.
    There should be no circular dependencies.
<#if !(screenlib?? && screenlib?is_hash)>
  <#import "component://widget/templates/htmlScreenMacroLibrary.ftl" as screenlib>
</#if>
<#if !(formlib?? && formlib?is_hash)>
  <#import "component://widget/templates/htmlFormMacroLibrary.ftl" as formlib>
</#if>
<#if !(menulib?? && menulib?is_hash)>
  <#import "component://widget/templates/htmlMenuMacroLibrary.ftl" as menulib>
</#if>
<#if !(treelib?? && treelib?is_hash)>
  <#import "component://widget/templates/htmlTreeMacroLibrary.ftl" as treelib>
</#if>-->


<#-- 
*************************************
* API TEMPLATE MACROS AND INCLUDES *
*************************************
* Code this file is responsible for defining. Intended for use in production templates.
*
* DEV NOTE: Categories are loosely based on Foundation organization.
* DEV NOTE: There may be circular dependencies between these includes. 
     May not be avoidable without complicating further. 
-->

<#include "htmlScript.ftl">
<#include "htmlStructure.ftl">
<#include "htmlInfo.ftl">
<#include "htmlNav.ftl">
<#include "htmlContent.ftl">
<#include "htmlForm.ftl">

<#-- After everything included, create a copy of the namespace so that macros can access 
     their global variables without possibility of override (sometimes needed)
     NOTE: this is only an issue because we use the global/main namespace for everything 
     NOTE: overriding themes will also make use of this -->
<#assign catoStdTmplLib = copyObject(.namespace)>

