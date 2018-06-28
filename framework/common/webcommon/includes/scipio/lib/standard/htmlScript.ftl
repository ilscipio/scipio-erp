<#--
* 
* HTML & Script Wrappers
* 
* Wrapping HTML, such as the boiler plate html head content or the javascript definition tags have been
* enhanced in order to strip templates from unneccessary information. Additional options are available 
* for each element.
*
* Included by htmlTemplate.ftl.
*
* NOTES: 
* * May have implicit dependencies on other parts of Scipio API.
*
* DEV NOTES: 
* * This currently contains some theme- and platform-specific utilites... could have own file, but
*   this is somewhat generic.
-->

<#--
*************
* HTML Head Open
************
Opens an HTML document and header section.

IMPL NOTE: Beware of whitespace.

  * Usage Examples *  
    <@htmlHeadOpen />            
                    
  * Parameters *
    includeDocType      = ((boolean), default: false) Whether to include doctype or not
                          Default is false because is already included by screen renderer in @renderScreenBegin.
-->
<#assign htmlHeadOpen_defaultArgs = {
  "includeDocType":false, "passArgs":{}
}>
<#macro htmlHeadOpen args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.htmlHeadOpen_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if includeDocType><!DOCTYPE html></#if><#lt>
  <#if locale??>
    <#local docLangAttr = locale.toString()?replace("_", "-")>
  </#if>
  <#if Static["java.awt.ComponentOrientation"].getOrientation(locale).isLeftToRight()>
    <#local langDir = "ltr"/>
    <#else>
    <#local langDir = "rtl"/>
  </#if>
  <#if !docLangAttr?has_content>
    <#local docLangAttr = "en">
  </#if>
  <#assign dummy = setGlobalContextField("langDir", langDir)>
  <#assign dummy = setGlobalContextField("docLangAttr", docLangAttr)>
  <@htmlHeadOpen_markup includeDocType=includeDocType docLangAttr=docLangAttr langDir=langDir origArgs=origArgs passArgs=passArgs/><#t>
</#macro>

<#-- @htmlHeadOpen main markup - theme override -->
<#macro htmlHeadOpen_markup includeDocType=false docLangAttr="" langDir="" origArgs={} passArgs={} catchArgs...>
<!--[if IE 9]><html class="lt-ie10"<#if docLangAttr?has_content> lang="${escapeVal(docLangAttr, 'html')}"</#if><#if langDir?has_content> dir="${escapeVal(langDir, 'html')}"</#if>><![endif]-->
<html class="no-js"<#if docLangAttr?has_content> lang="${escapeVal(docLangAttr, 'html')}"</#if><#if langDir?has_content> dir="${escapeVal(langDir, 'html')}"<#local dummy = setRequestVar("langDir", args)></#if>>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
</#macro>

<#-- 
*************
* Scripts
************
Optional scripts include modifier. Modifies the @script calls within it.

NOTE: This is NOT associated with any HTML element nor does it define any.

  * Usage Examples *  
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
    scriptType,
    output,
    data,
    htmlwrap                = Default arguments for child script calls; see @script
-->
<#assign scripts_defaultArgs = {
  "scriptType" : "text/javascript", "output" : "", "htmlwrap" : true, "cdata" : true, "passArgs":{}
}>
<#macro scripts args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.scripts_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local dummy = setRequestVar("scipioScriptsInfo", args)>
  <#nested>
  <#local dummy = setRequestVar("scipioScriptsInfo", {})>
</#macro>

<#-- 
*************
* Script
************
Inline script wrapper. By default, makes a javascript block.

NOTE: Unlike others, this macro explicitly currently cannot support open/close structure (only htmlwrap).

DEV NOTE: In future, could be used to collect scripts for inclusion at end of page.

  * Usage Examples *  
    <@script>
        jQuery(document).ready(function() {
            alert("Page loaded.");
        });
    </@script>         
                    
  * Parameters *
    type                    = (default: "text/javascript") Script type identifier
    src                     = Source document address (when no nested content)
    language                = DEPRECATED by HTML - ignored by macro
    output                  = ((boolean)) If true, the script must be output in the markup where the macro is used and should never be delegated
                              In cases of scripts within common templates,
                              this should be omitted, except when used in html <head> or in a footer.
                              If not specified or "", scipio decides what to do with them (output or accumulate at bottom of page).
                              TODO: code to accumulate at footer.
    htmlwrap                = ((boolean), default: true) If false don't include HTML wrapper (or cdata)
    cdata                   = ((boolean), default: true) If false don't include CDATA guard (only used if htmlwrap true)
-->
<#assign script_defaultArgs = {
  "type" : "text/javascript", "src" : "", "output" : "", "htmlwrap" : true, "cdata" : true, "passArgs":{}
}>
<#macro script args={} inlineArgs...>
  <#local scriptsInfo = getRequestVar("scipioScriptsInfo")!{}>
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
  <#local origArgs = args>
  <@script_markup type=type src=src output=output htmlwrap=htmlwrap cdata=cdata origArgs=origArgs passArgs=passArgs><#nested></@script_markup>
</#macro>

<#-- @script main markup - theme override -->
<#macro script_markup type="" src="" output="" htmlwrap=true cdata=true origArgs={} passArgs={} catchArgs...>
  <#if src?has_content>
    <script type="${escapeVal(type, 'html')}" src="${escapeFullUrl(src, 'html')}"></script>
  <#else>
    <#if htmlwrap>
      <script type="${escapeVal(type, 'html')}">
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
Informs the rendering process (decorator) that the given ofbiz URI must be made available to javascript
code.

Normally the getOfbizUrl(url) Javascript function will be used to do this.

The screen/ftl has to communicate to the decorator which URIs it needs to use, so
this is one such mechanism 
DEV NOTE: Other options: layoutSettings? any way is messy...
                    
Ideally this shouldn't needed and getOfbizUrl should just work, but URLs are generated
dynamic using controller request defs and can't predict URL patterns unless rewrite
@ofbizUrl in JS.  
         
  * Parameters *
    url                     = Controller request uri
    output                  = ((boolean)) If true, forces the macro to write the markup/content immediately
                              Prevents delegating this script include. In most template usage this should be omitted.  
                              DEV NOTE: If not specified, "" or false for now simply accumulates the names  
                                  and will be included by decorator in footer.
    htmlwrap                = ((boolean), default: false) Whether to wrap in @script
                              NOTE: Default is FALSE for this macro (unlike others).
    onlyIfExists            = ((boolean), default: false) Whether to omit URL if not defined in controller
                              If true, the require request will not produce an entry if the controller
                              doesn't support the given URI. If false, it will always try to make a URL,
                              which if missing from controller may give a lot or other error.
-->
<#macro requireScriptOfbizUrl uri htmlwrap=false output="" onlyIfExists=false>
  <#local requiredScriptOfbizUrls = getRequestVar("requiredScriptOfbizUrls")!false>
  <#if uri?has_content && requiredScriptOfbizUrls?is_boolean || !requiredScriptOfbizUrls.contains(uri)>
    <#if !onlyIfExists || (Static["org.ofbiz.webapp.control.RequestHandler"].controllerHasRequestUriDirect(request, uri))>
      <#if output?is_boolean && output == true>
        <@script htmlwrap=htmlwrap output=output>

          <#if requiredScriptOfbizUrls?is_boolean>
          if (typeof variable === 'undefined') {
              var commonOfbizUrls = {};
          }
          </#if>
          commonOfbizUrls["${escapeVal(uri, 'js')}"] = "${escapeVal(makeOfbizUrl(rawString(uri)), 'js')}";
          
        </@script>
      <#else>
        <#if requiredScriptOfbizUrls?is_boolean>
          <#local requiredScriptOfbizUrls = toSet([uri])>
        <#else>
          <#local dummy = requiredScriptOfbizUrls.add(uri)!>
        </#if>
        <#local dummy = setRequestVar("requiredScriptOfbizUrls", requiredScriptOfbizUrls)>
      </#if>
    </#if>
  </#if>
</#macro>

<#macro includeRecordedScriptOfbizUrls htmlwrap=false>
  <#local requiredScriptOfbizUrls = getRequestVar("requiredScriptOfbizUrls")!false>
  <#if (!requiredScriptOfbizUrls?is_boolean) && (!requiredScriptOfbizUrls.isEmpty())>
    <@script output=true htmlwrap=htmlwrap>

      if (typeof variable === 'undefined') {
          var commonOfbizUrls = {};
      }
  
      <#list requiredScriptOfbizUrls as uri>
      commonOfbizUrls["${escapeVal(uri, 'js')}"] = "${escapeVal(makeOfbizUrl(rawString(uri)), 'js')}";
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

NOTE: 2016-09-28: The attributes are now automatically HTML-escaped, but NOT javascript-escaped;
    caller must identify and escape the JS parts as needed (not trivial).

  * Parameters *
    events                  = ((map)) Map of event names to actions
-->
<#macro commonElemEventAttribStr events escapeLang="html">
  <@elemAttribStr attribs=events attribNamePrefix="on" alwaysAddPrefix=false escapeLang=escapeLang /><#t>
</#macro>

<#-- 
*************
* commonElemAttribStr
************
Prints a string of element attributes for common macro HTML elements.
This is nearly the same as elemAttribStr but with different defaults and with more versatile attribs map.

NOTE: 2016-08-30: The special token values {{{_EMPTY_VALUE_}}} and {{{_NO_VALUE_}}} are now considered deprecated;
    use #attribSpecialVal instead.
    
NOTE: 2016-09-30: This now automatically HTML-escapes attribute values by default; see {{{escapeLang}}} parameter.

  * Parameters *
    attribs                 = ((map)) Attribs map
                              The attribs map here is more versatile and supports mergeArgMaps functions more easily.
                              See function getAttribMapAllExcludes for implementation details.
    noExclude               = ((list)) Prevents excludes of attribs with specified names
    (other)                 = See @elemAttribStr; mostly same parameters but with different defaults.
-->
<#macro commonElemAttribStr attribs includeEmpty=false emptyValToken="_EMPTY_VALUE_" noValToken="_NO_VALUE_" exclude=[] noExclude=[]
  attribNamePrefix="" alwaysAddPrefix=true attribNamePrefixStrip="" attribNameSubstitutes={} camelCaseToDashLowerNames=true escapeLang="html">
  <#t><@elemAttribStr attribs=attribs includeEmpty=includeEmpty emptyValToken=emptyValToken noValToken=noValToken exclude=getAttribMapAllExcludes(attribs, exclude, noExclude)
    attribNamePrefix=attribNamePrefix alwaysAddPrefix=alwaysAddPrefix attribNamePrefixStrip=attribNamePrefixStrip 
    attribNameSubstitutes=attribNameSubstitutes camelCaseToDashLowerNames=camelCaseToDashLowerNames escapeLang=escapeLang/><#t>
</#macro>
