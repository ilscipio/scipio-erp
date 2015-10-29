<#--
* 
* Master HTML template include, default Cato markup.
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
*       of args and inlineAttribs (priority should be: args - inlineAttribs - attribMap).
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

