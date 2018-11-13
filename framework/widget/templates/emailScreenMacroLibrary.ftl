<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "htmlScreenMacroLibrary.ftl"> <#-- Defaults back to htmlScreenMacroLibrary -->

<#-- 
SCIPIO: NOTE: since macro renderer initial context mod, macros here now have access to a few widget context objects part of the initial
context, such as request, response, locale, and to some extent (since 2016-01-06), uiLabelMap.
WARN: no code run here or indirectly from here should assume full current context present. only use well-known generic vars.

NOTE: 2016-10-05: Widget early HTML encoding is now DISABLED for all HTML macros.
    As a result all macros here must take care to html-escape as well as js-escape values.
    Use escapeVal/escapeFullUrl for this.
-->
<#macro renderScreenBegin extraArgs...>
<#-- SCIPIO: NOTE: HTML head open is now in scipio template macros. 
     In OOTB ofbiz no context is passed here (locale, etc.) so did not belong here and cleaner if in scipio macros. -->
<!DOCTYPE html>
</#macro>

<#macro renderScreenEnd extraArgs...></#macro>
<#macro renderSectionBegin boundaryComment extraArgs...></#macro>
<#macro renderSectionEnd boundaryComment extraArgs...></#macro>

<#macro renderLink parameterList targetWindow target uniqueItemName linkType actionUrl id style name height width linkUrl text imgStr extraArgs...>
    <a href="javascript:void(0);" id="${escapeVal(uniqueItemName, 'html')}_link" 
    <#if style?has_content>class="${escapeVal(style, 'html')}"</#if>>
    <#if text?has_content>${escapeVal(text, 'htmlmarkup')}</#if></a>
</#macro>