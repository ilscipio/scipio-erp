<#--
* 
* Master HTML template include, standard Cato markup.
*
* A set of HTML templating macros, part of standard Cato Freemarker API.
* Automatically included at all times, unless overridden by properties or themes.
* Intended to be swappable.
* Includes all other default Cato macros.
*
* NOTES:
* * Currently targeted toward Foundation CSS.
* * Macros expect to be called using named arguments, except where otherwise noted.
* * Functions in Freemarker only support positional arguments, but some Cato functions support
*   an "args" argument as a map, which emulates named arguments. This is also done
*   as part of the advanced args interface (documented below).
*
* IMPLEMENTATION NOTES: 
* * Macros should almost never use "request" object directly - use setRequestVar/getRequestVar/other.
* * Documentation formatting is sensitive to whitespace, presence and number of asterisks (*),
*   and line endings. Must be spaces-only and LF-only line endings.
*   * In parser, bullets will create HTML lists, text encloding in two bullets (* *) will create
*     a heading, notes with uppercase label and "?:" or ":" prefix will be highlighted (support multi-
*     line by indenting the next line), and indents alone will create preformatted text (usually code).
*   * Small code bits may be surrounded by three brackets (like Trac tickets) to delineate code, {{{like this}}}.
*     Indents may be enough in other cases.
*
* DEV NOTES: 
* * Some macros use attribs and inlineAttribs args to specify extra HTML attribs.
*   Even though it would be convenient, we can't allow a "attribString" arg because no way
*   for macro to get attribs out of it if it needs them.
*   FIXME: Not all macros currently properly check attribMap for duplicate attribs
*       of args and inlineAttribs (priority should be: args - inlineAttribs - attribMap).
*
*
* * MACRO INTERFACES *
* 
* Cato standard macros have versatile interfaces. 
*
* General remarks:
* * All macros expect to be invoked using named parameters only (always {{{<@row class="my-class">}}}, never {{{<@row "my-class">}}})
* * Many macros have advanced, low-level open/close controls for esoteric structure control where the macro 
*   markup open and close tags and logic needs to be split or skipped. Generally their defaults are true; must specify
*     <@macroname open=true close=false /> 
*   for open-only and 
*     <@macroname close=true open=false /> 
*   for close-only.
*   Use of these in templates is discouraged but occasionally forcefully needed.
* * These patterns are often workarounds for limitations in the FTL language.
* * The advanced args pattern described below is primarily designed to be simple to use from templates, almost
*   indistinguishable from regular macro calls, but more powerful.
*   It also provides more power for theme overrides; however, theme overrides must be careful in their use of it;
*   it's better for themes to use simple markup overrides instead, which do not use the advanced pattern.
*
* Template-facing macros (and advanced args pattern): 
* * These macros such as @field, @row, @heading, etc. are meant to be
*   used in templates and can also be overridden directly by themes (though not preferred method).
*   Most of these use a versatile args pattern that looks like: 
*     <#macro macroname args={} inlineArgs...>
* * When called from templates, these macros accept regular inlined parameters as well as a map of parameters
*   using the args map parameter. Intuitively, inline args have priority over args passed in the args map and in most cases simply override them.
*   This pattern is especially needed for themes to override the templating-facing macros cleanly and to provide a way
*   for template code to pass map content as arguments (not supported by Freemarker). It can be exploited 
*   in a large number of ways (pass maps from groovy, callback macros, simplifying repeat calls, etc.) otherwise not possible.
*   Some non-template-facing macros also use this pattern, and some functions partly use it as well (but
*   Freemarker functions do not support named parameters at this time, so only partial pattern).
*   Some macros with this pattern also accept additional arbitrary inlined parameters to be used as extra HTML attributes (that can be passed
*   both in args map or inlineArgs, but usually it's used to emulate HTML with macros, thus inlineArgs); 
*   these are not declared in the default argument lists; see individual macro documentation;
*   they are equivalent in usage to regular FTL interface macros that take extra attrbs as varargs
*   e.g. 
*     <#macro myhtmlmacro (...) inlineAttribs...> 
*   IMPL NOTE: Extra attributions are handled by a system that records args names in a "allArgNames" member in 
*       the args map through the mergeArgMaps function. See mergeArgMaps function in utilities library for more details.
* * Interface:
*     <#assign name_defaultArgs = { (...), "passArgs":{} }>
*     <#macro macroname args={} inlineArgs...>
*   * args: Macro args map. Map of parameters to pass to the macro. It can be a bean-wrapped map (from groovy/widgets) or simple FTL hash.
*     IMPL NOTE: The implementation should pass this to mergeArgMaps or equivalent function (see examples).
*   * inlineArgs: Macro inline args. Map of parameters passed inline to the macro via usual macro call syntax.
*     these have priority over args and generally will replace entries in the args map, with rare exceptions where noted.
*     IMPL NOTE: The implementation should pass this to mergeArgMaps or equivalent function (see examples).
*   * passArgs: Pass-through args. Map of args that should be passed along the major calls made by this macro or in other words
*     passed through the whole call stack. It allows arguments to pass through from the templating-facing macro 
*     to the internal implementations such as markup macros, similar to a context. This is needed especially to allow theme overrides 
*     to communicate with their markup macros without the use of globals, but it may be used for any other purpose.
*     Be careful about using sufficiently unique names.
*     IMPL NOTE: Overriding macros should avoid overriding this map; 
*         always add new members to it instead. e.g. 
*       <@somemacro passArgs=(passArgs + {"myParam":"myValue")>
*
* Markup macros (theme overrides): 
* * These macros such as @row_markup, @heading_markup, etc. containing
*   the "_markup" name are overridable by themes to provide alternative HTML and sometimes javascript markup.
*   This is the simplest and preferred way to provide alternate markup.
*   They do not have a versatile interface like the template-facing macros and are intentionally kept
*   simple.
*   * Nevertheless, they have some requirements: these macros should always end their parameter list with
*     a varargs catch-all parameter "catchArgs..." so that future changes do not backwards break compability
*     with themes.
* * Interface:
*     <#macro macroname_markup (...) origArgs={} passArgs={} catchArgs...>
*   * origArgs: Original caller's args. map of complex parameters usually roughly as they were received by the calling macro. Rarely-used and should be
*     avoided in favor of the other simpler macro arguments passed by the caller, which are usually very similar. Is 
*     needed in rare cases where the simpler macro arguments are too simplistic or don't provide all the information needed.
*     NOTE: In general orig args do not come from a template-facing macro but from an intermediate macro
*         (such as @fieldset_core or @field_input_widget). This is the intention, as the former would break 
*         abstraction too much. In many cases however, the calling macro may happen to be 
*         a template-facing macro. Do not rely on this while using origArgs.
*   * passArgs: Pass-through args. Map of args that are passed through from the template-facing macro to the parent/caller macro to 
*     this macro or in other words passed through the whole call stack, similar to a context. This is needed especially to allow theme overrides 
*     to communicate with their markup macros without the use of globals, but it may be used for any other purpose.
*     Be careful about using sufficiently unique names.
*   * catchArgs: Catch-all args. Simply catches all the parameters the macro doesn't need to handle
*     NOTE: The previous parameters may be omitted and caught with catchArgs if unused.
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
* DEV NOTES: 
* * Categories are loosely based on Foundation organization.
* * There may be circular dependencies between these includes. 
*   May not be avoidable without complicating further. 
-->

<#include "htmlScript.ftl">
<#include "htmlStructure.ftl">
<#include "htmlInfo.ftl">
<#include "htmlNav.ftl">
<#include "htmlContent.ftl">
<#include "htmlForm.ftl">

<#-- After everything included, create a copy of the namespace so that macros can access 
     their global variables without possibility of override (sometimes needed)
     NOTE: overriding themes will also make use of this. this variable name must be unique. -->
<#assign catoStdTmplLib = copyObject(.namespace)>

