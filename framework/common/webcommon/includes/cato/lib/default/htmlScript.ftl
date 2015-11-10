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
Inline script wrapper.

  * Usage Example *  
    <@script>
        jQuery(document).ready(function() {
            alert("Page loaded.");
        });
    </@script>         
                    
  * Parameters *
    type            = script type identifier
    language        = language identifier
    src             = source
    forceInline     = if true, the script must be inlined in the markup where the macro is used
                      and should never be delegated. in most cases this should be omitted.
-->
<#macro script type="text/javascript" language="" src="" ofbizContentSrc="" forceInline=false>
  <#if ofbizContentSrc?has_content>
    <script type="${type}"<#if language?has_content> language="${language}"</#if> src="<@ofbizContentUrl>${ofbizContentSrc}</@ofbizContentUrl>"></script>
  <#elseif src?has_content>
    <script type="${type}"<#if language?has_content> language="${language}"</#if> src="${src}"></script>
  <#else>
    <script type="${type}"<#if language?has_content> language="${language}"</#if>>
    //<![CDATA[
      <#nested>
    //]]>
    </script>
  </#if>
</#macro>
