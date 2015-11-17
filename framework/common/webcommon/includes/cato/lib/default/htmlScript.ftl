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
    language        = language identifier (deprecated in HTML - avoid)
    src             = source (if no nested content)
    inline          = if true, the script must be inlined in the markup where the macro is used
                      and should never be delegated. in most cases this should be omitted,
                      except when used in html <head> or in a footer.
                      if not specified or "", cato decides what to do with them (inline or accumulate at bottom of page).
                      TODO: code to accumulate at footer.
-->
<#macro script type="text/javascript" language="" src="" ofbizContentSrc="" inline="" cdata=true wrapIf=true>
  <#local open = wrapIf>
  <#local close = wrapIf>
  <#if ofbizContentSrc?has_content>
    <script type="${type}"<#if language?has_content> language="${language}"</#if> src="<@ofbizContentUrl>${ofbizContentSrc}</@ofbizContentUrl>"></script>
  <#elseif src?has_content>
    <script type="${type}"<#if language?has_content> language="${language}"</#if> src="${src}"></script>
  <#else>
    <#if open>
      <script type="${type}"<#if language?has_content> language="${language}"</#if>>
      <#if cdata>//<![CDATA[</#if>
    </#if>
        <#nested>
    <#if close>
      <#if cdata>//]]></#if>
      </script>
    </#if>
  </#if>
</#macro>
