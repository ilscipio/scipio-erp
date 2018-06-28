<#--
* 
* A set of HTML templating macros, part of standard Scipio Freemarker API.
* Automatically included at all times.
* Intended to be swappable.
*
-->

<#-- Imports the default macros and allows overrides-->
<#include "component://widget/templates/htmlMenuMacroLibrary.ftl"> 
<#assign defaultmenulib = copyObject(.namespace)>

<#-- 
*************************************
* SCIPIO: NEW MENU MACROS (ONE-SHOT) *
*************************************
-->

<#-- SCIPIO: Override not currently needed; htmlwraps are set in style hash menu_ entries instead
    DEV NOTE: prefer not using because this is hard to maintain...
<#macro renderMenuFull>
</#macro>-->

<#-- SCIPIO: Override not currently needed; htmlwraps are set in style hash menu_ entries instead
    DEV NOTE: prefer not using because this is hard to maintain...
<#macro renderMenuItemFull>
</#macro>-->

