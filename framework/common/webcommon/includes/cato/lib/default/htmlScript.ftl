<#--
* 
* HTML document markup and script HTML template include, default Cato markup.
*
* Included by htmlTemplate.ftl.
*
-->

<#--
*************
* HTML Head open
************
Opens an HTML document and header section.

    Usage example:  
    <@htmlHeadOpen />            
                    
   * General Attributes *
    includeDocType      = boolean, default false (included by screen renderer, @renderScreenBegin)
-->
<#macro htmlHeadOpen includeDocType=false>
<#if includeDocType><!DOCTYPE html></#if><#t>
<#if locale??><#local docLangAttr = locale.toString()?replace("_", "-")></#if>
<#local langDir = "ltr">
<#if docLangAttr?? && "ar.iw"?contains(docLangAttr?substring(0, 2))>
    <#local langDir = "rtl">
</#if>
<!--[if IE 9]><html class="lt-ie10" lang="${docLangAttr!"en"}" <#if langDir??>dir="${langDir}"</#if>> <![endif]-->
<html class="no-js" lang="${doctLangAttr!"en"}"<#if langDir?has_content> dir="${langDir!}"</#if>>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
</#macro>

<#-- 
*************
* Script
************
Inline script wrapper.

    Usage example:  
    <@script>
        jQuery(document).ready(function() {
            alert("Page loaded.");
        });
    </@script>         
                    
   * General Attributes *
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
