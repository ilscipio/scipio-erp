<#--
* 
* HTML document markup and script HTML template include, standard Cato markup.
*
* Included by htmlTemplate.ftl.
*
* NOTE: May have implicit dependencies on other parts of Cato API.
*
* DEV NOTE: This currently contains some theme- and platform-specific utilites... could have own file, but
*     this is somewhat generic.
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
<#assign htmlHeadOpen_defaultArgs = {
  "includeDocType":false
}>
<#macro htmlHeadOpen args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.htmlHeadOpen_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#if includeDocType><!DOCTYPE html></#if><#lt>
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
    scriptType/output/cdata/htmlwrap  = defaults for child @script calls (see @script)
-->
<#assign scripts_defaultArgs = {
  "scriptType" : "text/javascript", "output" : "", "htmlwrap" : true, "cdata" : true
}>
<#macro scripts args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.scripts_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local dummy = setRequestVar("catoScriptsInfo", args)>
  <#nested>
  <#local dummy = setRequestVar("catoScriptsInfo", {})>
</#macro>

<#-- 
*************
* Script
************
Inline script wrapper. By default, makes a javascript block.
DEV NOTE: In future, could be used to collect scripts for inclusion at end of page.

NOTE: Unlike others this macro explicitly currently cannot support openOnly/closeOnly structure (only htmlwrap).

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
    htmlwrap        = boolean, default true, if false don't include HTML wrapper (or cdata)
    cdata           = boolean, default true, if false don't include CDATA guard (only used if htmlwrap true)
-->
<#assign script_defaultArgs = {
  "type" : "text/javascript", "src" : "", "output" : "", "htmlwrap" : true, "cdata" : true
}>
<#macro script args={} inlineArgs...>
  <#local scriptsInfo = getRequestVar("catoScriptsInfo")!{}>
  <#-- this uses complex defaults from parent @scripts elem, so have to do something different -->
  <#local defaultArgs = {
    "type" : scriptsInfo.scriptType!script_defaultArgs.type,
    "src" : "",
    "output" : scriptsInfo.output!script_defaultArgs.output,
    "htmlwrap" : scriptsInfo.htmlwrap!script_defaultArgs.htmlwrap,
    "cdata" : scriptsInfo.cdata!script_defaultArgs.cdata
  }>
  <#local args = mergeArgMaps(args, inlineArgs, defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#if src?has_content>
    <script type="${type}" src="${src}"></script>
  <#else>
    <#if htmlwrap>
      <script type="${type}">
      <#if cdata>//<![CDATA[</#if>
    </#if>
        <#nested>
    <#if htmlwrap>
      <#if cdata>//]]></#if>
      </script>
    </#if>
  </#if>
</#macro>

<#-- 
*************
* requireScriptOfbizUrl
************
This informs the decorator that the given ofbiz URI must be made available to javascript
code through the getOfbizUrl(url) JS function.

the screen/ftl has to communicate to the decorator which URIs it needs to use, so
this is one such mechanism (other option: layoutSettings? TODO? any way is messy).
                    
Ideally this shouldn't needed and getOfbizUrl should just work, but URLs are generated
dynamic using controller request defs and can't predict URL patterns unless rewrite
@ofbizUrl in JS.  
         
  * Parameters *
    url             = controller request uri
    output          = if true, the include must be output in the markup where the macro is used
                      and should never be delegated. in most cases this should be omitted.  
                      DEV NOTE: if not specified, "" or false for now simply accumulates the names  
                          and will be included by decorator in footer.
    htmlwrap        = whether to wrap in @script - default FALSE for this one
-->
<#macro requireScriptOfbizUrl uri htmlwrap=false output="">
  <#local requiredScriptOfbizUrls = getRequestVar("requiredScriptOfbizUrls")!false>
  <#if requiredScriptOfbizUrls?is_boolean || !requiredScriptOfbizUrls.contains(uri)>
    <#if output?is_boolean && output == true>
      <@script htmlwrap=htmlwrap output=output>
        <#if requiredScriptOfbizUrls?is_boolean>
          if (typeof variable === 'undefined') {
              var commonOfbizUrls = {};
          }
        </#if>
        commonOfbizUrls["${uri}"] = "<@ofbizUrl>${uri}</@ofbizUrl>";
      </@script>
    <#else>
      <#if requiredScriptOfbizUrls?is_boolean>
        <#local requiredScriptOfbizUrls = Static["org.ofbiz.base.util.UtilMisc"].toSet(uri)>
      <#else>
        <#local dummy = requiredScriptOfbizUrls.add(uri)!>
      </#if>
      <#local dummy = setRequestVar("requiredScriptOfbizUrls", requiredScriptOfbizUrls)>
    </#if>
  </#if>
</#macro>

<#macro includeRecordedScriptOfbizUrls htmlwrap=false>
  <#local requiredScriptOfbizUrls = getRequestVar("requiredScriptOfbizUrls")!false>
  <#if !requiredScriptOfbizUrls?is_boolean || (!requiredScriptOfbizUrls.isEmpty())>
    <@script output=true htmlwrap=htmlwrap>

      if (typeof variable === 'undefined') {
          var commonOfbizUrls = {};
      }
  
      <#list requiredScriptOfbizUrls as uri>
      commonOfbizUrls["${uri}"] = "<@ofbizUrl>${uri}</@ofbizUrl>";
      </#list>
    </@script>
  </#if>
</#macro>

<#-- 
*************
* commonElemEventAttribStr
************
Prints a string of JS events as HTML element attributes for common macro HTML elements.
Accepts attrib names as both "onxxx" and "xxx".

  * Parameters *
    events    = map of event names to actions
-->
<#macro commonElemEventAttribStr events>
  <@elemAttribStr attribs=events attribNamePrefix="on" alwaysAddPrefix=false /><#t>
</#macro>

<#-- 
*************
* commonElemAttribStr
************
Prints a string of element attributes for common macro HTML elements.
This is nearly the same as elemAttribStr but with different defaults and with more versatile attribs map.

  * Parameters *
    (see @elemAttribStr; same parameters but defaults different)
    attribs       = the attribs map here is more versatile and supports mergeArgMaps functions more easily.
                    it may contain two special members: localArgNames and allArgNames. both should be lists.
                    localArgNames will be ignored and all names in allArgNames will be appended to the exclude list.
                    the two will be excluded themselves.
                    if this map contains an "attribs" member, it will be added for exclude as well.
                    see function getAttribMapAllExcludes for implementation details.
-->
<#macro commonElemAttribStr attribs includeEmpty=false emptyValToken="" exclude=[] 
  attribNamePrefix="" alwaysAddPrefix=true attribNamePrefixStrip="" attribNameSubstitutes={} camelCaseToDashLowerNames=true>
  <#t><@elemAttribStr attribs=attribs includeEmpty=includeEmpty emptyValToken=emptyValToken exclude=getAttribMapAllExcludes(attribs, exclude)
    attribNamePrefix=attribNamePrefix alwaysAddPrefix=alwaysAddPrefix attribNamePrefixStrip=attribNamePrefixStrip 
    attribNameSubstitutes=attribNameSubstitutes camelCaseToDashLowerNames=camelCaseToDashLowerNames /><#t>
</#macro>
