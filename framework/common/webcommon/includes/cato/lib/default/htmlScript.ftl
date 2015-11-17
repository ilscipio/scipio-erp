<#--
* 
* HTML document markup and script HTML template include, default Cato markup.
*
* Included by htmlTemplate.ftl.
*
* NOTE: May have implicit dependencies on other parts of Cato API.
*
-->

<#--
*************
* HTML Head Open
************
Opens an HTML document and header section.
IMPL NOTE: Beware of whitespace.

  * Usage Example *  
    <@htmlHeadOpen />            
                    
  * Parameters *
    includeDocType      = boolean, default false (included by screen renderer, @renderScreenBegin)
-->
<#macro htmlHeadOpen includeDocType=false>
<#if includeDocType><!DOCTYPE html></#if>
<#if locale??>
    <#local docLangAttr = locale.toString()?replace("_", "-")>
</#if>
<#local langDir = "ltr">
<#if docLangAttr?? && "ar.iw"?contains(docLangAttr?substring(0, 2))>
    <#local langDir = "rtl">
</#if>
<#if !docLangAttr?has_content>
  <#local docLangAttr = "en">
</#if>
<!--[if IE 9]><html class="lt-ie10"<#if docLangAttr?has_content> lang="${docLangAttr}"</#if><#if langDir?has_content> dir="${langDir}"</#if>><![endif]-->
<html class="no-js"<#if docLangAttr?has_content> lang="${docLangAttr}"</#if><#if langDir?has_content> dir="${langDir}"</#if>>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
</#macro>

<#-- 
*************
* Scripts
************
Optional scripts include modifier. Modifies the @script calls within it.
Not associated with any HTML element.

  * Usage Example *  
    <@scripts output=true> <#- guarantees the @script calls will output content to page at this location ->
      <@script>
        jQuery(document).ready(function() {
            alert("Page loaded.");
        });
      </@script>
      <@script>
        jQuery(document).ready(function() {
            alert("Test.");
        });
      </@script>      
    </@scripts>         
                    
  * Parameters *
    scriptType/output/cdata/wrapIf    = defaults for child @script calls (see @script)
-->
<#macro scripts inlineArgs...>
  <#if !inlineArgs?is_hash>
    <#local inlineArgs = {}>
  </#if>
  <#local dummy = setRequestVar("catoScriptsInfo", inlineArgs)>
  <#nested>
  <#local dummy = setRequestVar("catoScriptsInfo", {})>
</#macro>

<#-- 
*************
* Script
************
Inline script wrapper. By default, makes a javascript block.
DEV NOTE: In future, could be used to collect scripts for inclusion at end of page.

NOTE: Unlike others this macro explicitly currently cannot support openOnly/closeOnly structure (only wrapIf).

  * Usage Example *  
    <@script>
        jQuery(document).ready(function() {
            alert("Page loaded.");
        });
    </@script>         
                    
  * Parameters *
    type            = script type identifier (default "text/javascript")
    src             = source (if no nested content)
    language        = deprecated by HTML - ignored by macro
    output          = if true, the script must be output in the markup where the macro is used
                      and should never be delegated. in cases of scripts within common templates,
                      this should be omitted, except when used in html <head> or in a footer.
                      if not specified or "", cato decides what to do with them (output or accumulate at bottom of page).
                      TODO: code to accumulate at footer.
    wrapIf          = boolean, default true, if false don't include HTML wrapper (or cdata)
    cdata           = boolean, default true, if false don't include CDATA guard
-->
<#macro script inlineArgs...>
  <#if !inlineArgs?is_hash>
    <#local inlineArgs = {}>
  </#if>
  <#local scriptsInfo = getRequestVar("catoScriptsInfo")!{}>
  <#local type = inlineArgs.type!scriptsInfo.scriptType!"text/javascript">
  <#local src = inlineArgs.src!"">
  <#local output = inlineArgs.output!scriptsInfo.output!"">
  <#if src?has_content>
    <script type="${type}" src="${src}"></script>
  <#else>
    <#local cdata = inlineArgs.cdata!scriptsInfo.cdata!true>
    <#local wrapIf = inlineArgs.wrapIf!scriptsInfo.wrapIf!true>
    <#local open = wrapIf>
    <#local close = wrapIf>
    <#if open>
      <script type="${type}">
      <#if cdata>//<![CDATA[</#if>
    </#if>
        <#nested>
    <#if close>
      <#if cdata>//]]></#if>
      </script>
    </#if>
  </#if>
</#macro>
