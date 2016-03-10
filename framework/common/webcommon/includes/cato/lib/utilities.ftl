<#--
* 
* Generic Cato FTL Utilities.
*
* A set of standalone utility functions and macros, largely devoid of markup and unrelated to templating macros and with minimal dependencies, 
* part of standard Cato Freemarker API.
* Generally CSS-framework-agnostic. 
* Intended as platform-agnostic (html, fo, etc.) though some individually are only applicable for specific platforms.
* Automatically included at all times.
*
* NOTES: 
* * Macros expect to be called using named arguments, except where otherwise noted.
* * Functions in Freemarker only support positional arguments, but some Cato functions support
*   an "args" argument as a map, which emulates named arguments.
* * Default markup-producing macros are found in htmlTemplate.ftl.
*   Utilities found in utilities.ftl should not contain their logic in general.
*  
* IMPLEMENTATION NOTES: 
* * Macros should almost never use "request" object directly - use setRequestVar/getRequestVar/other.
* * It's important that these macros remain generic (and that the include for these utilities remains
*   completely static) so that any macro or function here can easily be interchanged with a transform (Java class).
*
* TODO:
* * Turn more of these into transforms.
-->

<#assign catoUtilitiesDefined = true> <#-- this one must use #assign, not #global -->

<#-- 
*************************************
* TEMPLATING API UTILITIES *
*************************************
* Intended for use anywhere in production templates and templating macros.
-->


<#-- 
*************
* ofbizUrl
************
Builds an Ofbiz navigation URL.

THIS IS THE MAIN STOCK OFBIZ URL MACRO. It may be modified with enhanced
capabilities for Cato.

With Cato, boolean arguments can be given as booleans, string representation of booleans
or empty string (signifies use defaults).

DEV NOTES:
* TODO: RequestHandler.makeLink has been modified noticeably in Ofbiz 14 and requires more review to integrate
  the code from Cato magnolia project
  * The inter-webapp is still complicated by need for webSiteId which stock apps don't have (and can't give)
* webSiteId arg below is from stock but does not fully work and will not work with stock webapps (don't have webSiteIds and can't give them any)

  * Parameters *
    type            = [intra-webapp|inter-webapp|] (default: intra-app)
                      intra-webapp: a relative intra-webapp link (either a controller URI or arbitrary servlet path)
                      inter-webapp: an inter-webapp link (either a controller URI or an absolute path to any webapp navigation resource)
                        The target webapp MUST exist on the current server as a recognized webapp (with web.xml).
                        It can be identified using either webSiteId or using an absolute full path to the webapp and as the uri.
                      (New in Cato)
    interWebapp     = boolean (default: false) Alias for type="inter-webapp".
                      If true, same as type="inter-webapp".
                      If false, same as type="" (intra-webapp implied).
                      (New in Cato)
    uri             = string (required) The request URI. May be specified as parameter or as #nested macro content.
                      For intra-webapp links and with all macro defaults, this should be a controller URI, or if controller false, a relative servlet path (relative
                      to webapp root, excluding webapp context root).
                      For inter-webapp links, if no webSiteId is specified, this must be an absolute path from
                      server root, containing webapp context root and servlet path; if webSiteId specified, 
                      this should specified relative like intra-webapp (unless absPath forced to true).
                      (New in Cato)
    absPath         = boolean (default: -depends on type-) (fallback default: false)       
                      If explicit true, the passed uri should be an absolute path from server root (including context root and servlet path)
                      If explicit false (stock Ofbiz default), the passed uri should be relative to control servlet or webapp context.
                      If not specified, will attempt for figure out based on the uri passed and other flags.
                      Current behavior when unspecified:
                      * For all intra-webapp links, absPath is assumed false.
                      * For inter-webapp links:
                        * If webSiteId is not specified, absPath is assumed true.
                        * If webSiteId is specified, absPath is assumed false.
                      NOTE: Behavior when unspecified is NOT currently influenced by present of starting slash ("/"),
                          to try to preserve compability with legacy Ofbiz behavior that accepted one for all link types.
                          It is also ambiguous in the case of intra-webapp non-controller links.
                      (New in Cato)
    webSiteId       = string (default: -current website or none-) Target web site ID 
                      This usually should only be specified for inter-webapp links.
                      Will determine the specific target webapp to use.
                      NOTE: Some Ofbiz (stock) webapps do not have their own webSiteId, and this is considered normal.
                      (Stock arg, some fixes in Cato)
    controller      = boolean (default: -depends on type-) (fallback default: true)
                      If true (stock Ofbiz case), the link is treated as pointing to an Ofbiz controller request URI, and will
                      use information from the controller to generate the link.
                      If false, the link is treated as pointing to any arbitrary servlet or resource.
                      Current behavior when unspecified:
                      * If absPath is true, the uri will be checked to see whether it points to controller
                        * This helps implementation of inter-webapp links.
                      * Otherwise, generally defaults to true.
                      (New in Cato)
    extLoginKey     = boolean (default: false) or string boolean repr. 
                      If true, this will add the external login key as parameter.
                      NOTE: This is currently FALSE by default in all cases including inter-webapp links
                          while details are sorted out.
                      (New in Cato)
    fullPath        = boolean (default: false) or string boolean repr. 
                      If true, forces a full URL with protocol (HTTP or HTTPS).
                      WARNING: MODIFIED IN CATO: In Cato, specifying fullPath true for a controller request
                          marked as secure will always generate a secure URL, not a plain URL. Some control
                          is sacrificed to allow this flag to be used safely and more easily.
                      (Stock arg, enhanced in Cato: supports both boolean and string containing boolean)
    secure          = boolean (default: false) or string boolean repr. 
                      If true, forces a full URL with secure protocol (HTTPS).
                      (Stock arg, enhanced in Cato: supports both boolean and string containing boolean)
    encode          = boolean (default: true) or string boolean repr. 
                      If true, pass through HttpServletResponse.encodeURL; otherwise, don't.
                      (Stock arg, enhanced in Cato: supports both boolean and string containing boolean)
-->
<#-- IMPLEMENTED AS TRANSFORM
<#macro ofbizUrl uri="" absPath="" interWebapp="" webSiteId="" controller="" fullPath="" secure="" encode="">
</#macro>
-->

<#-- 
*************
* makeOfbizUrl
************
Builds an Ofbiz navigation URL. Function version of the @ofbizUrl macro.

This is useful to prevent bloating templates with <#assign...><@ofbizUrl.../></#assign> captures
which is very frequent due to use of macros.

  * Parameters *
    args                 = Map of @ofbizUrl arguments OR a string containing a uri (single parameter)
                           DEV NOTE: This is the only sane way to implement this because FTL supports only positional args
                               for functions, which would be unreadable here (makeOfbizUrl("main", false, false, true, true...))
                               However majority of cases use only a URI so we can shortcut in that case.
                               Freemarker doesn't support overloading so we basically implement it ourselves.
                               Note that if we needed extra positional parameters for common cases, should keep the args map check on
                               the first param only, otherwise it creates too many checks needed; this is
                               consistent with macros anyway (you use either positional OR named params, you can't combine,
                               so you use only args map or only positionals).
   
  * Related *                           
    @ofbizUrl
-->
<#function makeOfbizUrl args>
  <#if isObjectType("map", args)> <#-- ?is_hash doesn't work right with context var strings and hashes -->
    <#local res><@ofbizUrl uri=StringUtil.wrapString(args.uri!"") webSiteId=args.webSiteId!"" absPath=args.absPath!"" interWebapp=args.interWebapp!"" controller=args.controller!"" 
        extLoginKey=args.extLoginKey!"" fullPath=args.fullPath!"" secure=args.secure!"" encode=args.encode!"" /></#local>
  <#else>
    <#local res><@ofbizUrl uri=StringUtil.wrapString(args) /></#local>
  </#if>
  <#return res>
</#function>

<#-- 
*************
* ofbizWebappUrl
************
Builds an Ofbiz navigation intra-webapp, non-controller URL.

The URI takes the basic form /control/requesturi, 
but this is normally used to access another servlet, such as /products/GZ-1000.

This calls @ofbizUrl with absPath=false, interWebapp=false, controller=false by default.

  * Parameters *
    (other)              = See @ofbizUrl

  * Related *                           
    @ofbizUrl
-->
<#macro ofbizWebappUrl uri="" fullPath="" secure="" encode="" absPath=false controller=false extLoginKey=false>
  <@ofbizUrl uri=uri absPath=absPath interWebapp=false controller=controller 
    extLoginKey=extLoginKey fullPath=fullPath secure=secure encode=encode><#nested></@ofbizUrl><#t>
</#macro>

<#-- 
*************
* makeOfbizWebappUrl
************
Builds an Ofbiz navigation intra-webapp, non-controller URL. Function version of @ofbizWebappUrl.

The URI takes the basic form /control/requesturi, 
but this is normally used to access another servlet, such as /products/GZ-1000.

This calls @ofbizUrl with absPath=false, interWebapp=false, controller=false by default.
h absPath=false, interWebapp=false, controller=false.

  * Parameters *
    (other)              = See #makeOfbizUrl, @ofbizWebappUrl

  * Related * 
    @ofbizWebappUrl                          
    @ofbizUrl
-->
<#function makeOfbizWebappUrl args>
  <#if isObjectType("map", args)>
    <#local res><@ofbizUrl uri=StringUtil.wrapString(args.uri!"") absPath=args.absPath!false interWebapp=false controller=args.controller!false 
        extLoginKey=args.extLoginKey!false fullPath=args.fullPath!"" secure=args.secure!"" encode=args.encode!"" /></#local>
  <#else>
    <#local res><@ofbizUrl uri=StringUtil.wrapString(args) absPath=false interWebapp=false controller=false 
        extLoginKey=false fullPath=fullPath secure=secure encode=encode /></#local>
  </#if>
  <#return res>
</#function>

<#-- 
*************
* ofbizInterWebappUrl
************
Builds an Ofbiz navigation inter-webapp URL.

The URI takes the basic and usual form /webappmountpoint/control/requesturi 
OR requesturi if webSiteId is specified and is a controller request.

This calls @ofbizUrl with interWebapp=true and optional webSiteId; absPath is left to interpretation
by the implementation or can be overridden; controller is left to interpretation or can be specified.

  * Parameters *
    (other)              = See @ofbizUrl

  * Related * 
    @ofbizUrl
-->
<#macro ofbizInterWebappUrl uri="" webSiteId="" absPath="" controller="" extLoginKey="" fullPath="" secure="" encode="">
  <@ofbizUrl uri=uri interWebapp=true absPath=absPath webSiteId=webSiteId controller=controller
    extLoginKey=extLoginKey fullPath=fullPath secure=secure encode=encode><#nested></@ofbizUrl><#t>
</#macro>

<#-- 
*************
* makeOfbizInterWebappUrl
************
Builds an Ofbiz navigation inter-webapp URL. Function version of @ofbizInterWebappUrl.

The URI takes the basic and usual form /webappmountpoint/control/requesturi 
OR requesturi if webSiteId is specified and is a controller request.

This calls @ofbizUrl with interWebapp=true and optional webSiteId; absPath is left to interpretation
by the implementation or can be overridden; controller is left to interpretation or can be specified.

NOTE: If args is specified as map, "webSiteId" must be passed in args, not as argument.
    (This is intentional, to be consistent with macro invocations, emulated for functions)

  * Parameters *
    (other)              = See #makeOfbizUrl, @ofbizInterWebappUrl

  * Related * 
    @ofbizInterWebappUrl
    @ofbizUrl
-->
<#function makeOfbizInterWebappUrl args webSiteId="">
  <#if isObjectType("map", args)>
    <#local res><@ofbizUrl uri=StringUtil.wrapString(args.uri!"") absPath=args.absPath!"" interWebapp=true webSiteId=args.webSiteId!  
        controller=args.controller!"" extLoginKey=args.extLoginKey!"" fullPath=args.fullPath!"" secure=args.secure!"" encode=args.encode!"" /></#local>
  <#else>
    <#local res><@ofbizUrl uri=StringUtil.wrapString(args) absPath="" interWebapp=true webSiteId=webSiteId
        controller="" extLoginKey="" fullPath="" secure="" encode="" /></#local>
  </#if>
  <#return res>
</#function>

<#-- 
*************
* ofbizContentUrl
************
Builds an Ofbiz content/resource URL.

THIS IS THE STOCK OFBIZ CONTENT URL MACRO. It may be modified with enhanced
capabilities for Cato.

TODO: Make this accept uri

  * Parameters *
    variant             = variant
                          (Stock Ofbiz parameter)
-->
<#-- IMPLEMENTED AS TRANSFORM
<#macro ofbizContentUrl ...>
</#macro>
-->

<#-- 
*************
* makeOfbizContentUrl
************
Builds an Ofbiz content/resource URL. Function version of the @ofbizContentUrl macro.

  * Parameters *
    (other)              = See @ofbizContentUrl

  * Related * 
    @ofbizContentUrl
-->
<#function makeOfbizContentUrl uri variant="">
  <#local res><@ofbizContentUrl variant=variant>${StringUtil.wrapString(uri)}</@ofbizContentUrl></#local>
  <#return res>
</#function>

<#-- 
*************
* interpretRequestUri
************
Interprets the given request URI/URL resource and transforms into a valid URL if and as needed.
Can help to cut down on macro URL arguments, but may be used anywhere.

FIXME: This is out of date with @ofbizUrl enhancements

If the uri is already a web URL, it is returned as-is.
The following URI forms are currently interpreted and transformed:
 ofbizUrl:// - Any URI that begins with this will be interpreted as an ofbiz controller URL and ran through @ofbizUrl/makeOfbizUrl.
               Form (note: order of arguments is strict; args will be stripped): 
                 ofbizUrl://myRequest;fullPath=false;secure=false;encode=true?param1=val1
                 
  * Parameters *
    url             = uri to interpret for known formats and, if matching, to produce URL
-->
<#function interpretRequestUri uri>
  <#local uri = StringUtil.wrapString(uri)?string>
  <#if uri?starts_with("ofbizUrl://")>
    <#local uriDesc = Static["org.ofbiz.webapp.control.RequestDescriptor"].fromUriStringRepr(request!, response!, uri)>
    <#if uriDesc.getType() == "ofbizUrl">
      <#-- note: although there is uriDesc.getWebUrlString(), should pass through FTL macro version instead, hence all this manual work... -->
      <#local res><@ofbizUrl fullPath=uriDesc.isFullPath()?c secure=uriDesc.isSecure()?c encode=uriDesc.isEncode()?c>${uriDesc.getBaseUriString()}</@ofbizUrl></#local>
      <#--<#local res = "uri: " + uriDesc.getBaseUriString() + "; fullPath: " + uriDesc.isFullPath()?c + "; secure: " + uriDesc.isSecure()?c + "; encode: " + uriDesc.isEncode()?c>-->
      <#return res>
    <#else>
      <#return uri>
    </#if>
  <#else>
    <#return uri>
  </#if>
</#function>

<#-- 
*************
* addExtLoginKey
************
Adds the external login key to given url

  * Parameters *
    url             = URL to augment
    escape          = boolean (default: true) If true, use escaped param delimiter.
-->
<#function addExtLoginKey url escape=true>
  <#return StringUtil.wrapString(Static["org.ofbiz.webapp.control.RequestUtil"].checkAddExternalLoginKey(StringUtil.wrapString(url)?string, request, escape))?string>
</#function>

<#-- 
*************
* getLabel
************
Returns empty string if no label is found

  * Parameters *
    name            = (required) Label name
    resource        = (optional) Resource name
                      If label not found in uiLabelMap (preferred), falls back to lookup in this 
                      resource. Usually uiLabelMap is preferred for templates, but sometimes not worth importing
                      a whole file for one label. 
-->
<#function getLabel name resource="">
  <#if name?has_content>
    <#local var=uiLabelMap[name]!"" />
    <#if var!=name>
      <#return var>
    <#elseif resource?has_content>
      <#return getPropertyMsg(resource, name)>
    <#else>
      <#return "">
    </#if>
  <#else>
    <#return ""> 
  </#if>
</#function>

<#-- 
*************
* getPropertyValue
************
Gets property or void/null if missing or has no content.

NOTE: Always use default value ("!") or other test operator!

  * Parameters *
    resource        = (required) Resource name
    name            = (required) Property name
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function getPropertyValue resource name>
  <#local value = StringUtil.wrapString(Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue(resource, name))?string>
  <#if value?has_content>
    <#return value>
  </#if> <#- else return nothing (void/null) ->
</#function>
-->

<#-- 
*************
* getPropertyMsg
************
Gets property or empty string if missing (same behavior as UtilProperties).

Uses locales. Meant for resource bundles / UI labels.
Will use context locale if none specified.
If msgArgs not specified, property is given access to context for substitute values (occasionally 
this is used in Ofbiz screens).
If msgArgs is a sequence, they are passed instead of context to the property.

TODO: implement as transform.

  * Parameters *
    resource        = (required) Resource name
    name            = (required) Property name
    msgArgs         = map or sequence (optional) (default: -use context variables-) Substitute values for message template
    specLocale      = locale (optional) (default: -locale from context-) Explicit locale
-->
<#function getPropertyMsg resource name msgArgs=false specLocale=true>
  <#if specLocale?is_boolean>
    <#if specLocale>
      <#local specLocale = locale!"">
    <#else>
      <#local specLocale = "">
    </#if>
  </#if>
  <#if msgArgs?is_sequence>
    <#return StringUtil.wrapString(Static["org.ofbiz.base.util.UtilProperties"].getMessage(resource, name, msgArgs, specLocale))?string>
  <#elseif msgArgs?is_hash>
    <#return StringUtil.wrapString(Static["org.ofbiz.base.util.UtilProperties"].getMessage(resource, name, msgArgs, specLocale))?string>
  <#else>
    <#-- WARN: context variable _could_ be missing! -->
    <#return StringUtil.wrapString(Static["org.ofbiz.base.util.UtilProperties"].getMessage(resource, name, context!{}, specLocale))?string>
  </#if>
</#function>

<#-- 
*************
* getPropertyMsgFromLocExpr
************
Gets property or empty string if missing (same behavior as UtilProperties).

TODO: implement as transform.

  * Parameters *
    resourceExpr    = (required) Resource name and property name separated with "#", or name alone
                      If name alone, assumes CommonUiLabels for resource.
    msgArgs         = map or sequence (optional) (default: -use context variables-) Substitute values for message template
    specLocale      = locale (optional) (default: -locale from context-) Explicit locale
    
  * Related *
    #getPropertyMsg
-->
<#function getPropertyMsgFromLocExpr resourceExpr msgArgs=false specLocale=true>
  <#local parts = resourceExpr?split("#")>
  <#if (parts?size >= 2)>
    <#local resource = parts[0]>
    <#local name = parts[1]>
  <#else>
    <#local resource = "CommonUiLabels">
    <#local name = parts[0]>
  </#if>
  <#return getPropertyMsg(resource, name, msgArgs, specLocale)> 
</#function>

<#-- 
*************
* getTextLabelFromExpr
************
Convenience label identifier parsing function that accepts a string in multiple formats
that may designate a label or property as label.

If textExpr starts with "#LABEL:", the following name is taken from uiLabelMap.
If textExpr starts with "#PROP:", the following location/name is passed through to getPropertyMsgFromLocExpr
If no such prefix in textExpr, returns the text as-is.

  * Parameters *
    textExpr        = (required) Label text expression 
    msgArgs         = map or sequence (optional) (default: -use context variables-) Substitute values for message template
    specLocale      = locale (optional) (default: -locale from context-) Explicit locale
-->
<#function getTextLabelFromExpr textExpr msgArgs=false specLocale=true>
  <#if textExpr?starts_with("#LABEL:")>
    <#return getLabel(textExpr[7..])!"">
  <#elseif textExpr?starts_with("#PROP:")>
    <#return getPropertyMsgFromLocExpr(textExpr[6..], msgArgs, specLocale)!"">
  <#else>
    <#return textExpr>
  </#if>
</#function>

<#-- 
*************
* addParamDelimToUrl
************
Adds a param delimiter to end of url if needed.

NOTE: 2016-01-21: New special case: if paramDelim is "/" or contains "/", treat differently (because trumps "?")
                    
  * Parameters *
    url             = URL to which to append delimiter
    paramDelim      = (default: "&amp;") Param delimiter
    paramStarter    = (default: "?") Query string delimiter. Usually "?"
                      Only significant if paramDelim does not contain "/"
-->
<#function addParamDelimToUrl url paramDelim="&amp;" paramStarter="?">
  <#if paramDelim?contains("/")>
    <#if url?ends_with(paramDelim)>
      <#return url>
    <#elseif url?ends_with(paramStarter) || url?ends_with("/")>
      <#return url[0..<(url?length-1)] + paramDelim>
    <#else>
      <#return url + paramDelim>
    </#if>
  <#else>
    <#if url?contains(paramStarter)>
      <#if url?ends_with(paramStarter) || url?ends_with(paramDelim)>
        <#return url>
      <#else>
        <#return url + paramDelim>
      </#if>
    <#else>
      <#return url + paramStarter>
    </#if>
  </#if>
</#function> 

<#-- 
*************
* addParamsToStr
************
Adds parameters from a hash to a URL param string (no full URL logic).
                    
  * Parameters *
    paramStr        = Param string
    paramMap        = Hash of keys to values to add
    paramDelim      = (default: "&amp;") Param delimiter
    includeEmpty    = boolean (default: true) If true, include empty values; if false, omit empty values
-->
<#function addParamsToStr paramStr paramMap paramDelim="&amp;" includeEmpty=true>
  <#local res = paramStr>
  <#list mapKeys(paramMap) as key>
    <#if res?has_content && (!res?ends_with(paramDelim))>
      <#local res = res + paramDelim>
    </#if>
    <#if includeEmpty || paramMap[key]?has_content>
      <#local res = res + key + "=" + paramMap[key]!?string>
    </#if>
  </#list>
  <#return res>
</#function> 

<#-- 
*************
* splitStrParams
************
Extracts parameters from a string in the following format and returns as a hash:
  name1=val1DELIMname2=val2DELIMname3=val3
where DELIM is specified delimiter (& &amp; , ; etc.).
                    
  * Parameters *
    paramStr        = Param string
    paramDelim      = (default: "&amp;") Param delimiter
-->
<#function splitStrParams paramStr paramDelim="&amp;">
  <#return Static["com.ilscipio.cato.ce.webapp.ftl.template.TemplateFtlUtil"].splitStrParams(paramStr, paramDelim)>
<#-- old FTL impl.
  <#local res = {}>
  <#local pairs = paramStr?split(paramDelim)>
  <#list pairs as pair>
    <#local parts = pair?split("=")>
    <#if (parts?size >= 2)>
      <#local res = res + {parts[0] : parts[1]}>
    </#if>
  </#list>
  <#return res>
-->
</#function> 

<#-- 
*************
* trimParamStrDelims
************
Strips leading and trailing param delims from a URL param string.
                    
  * Parameters *
    paramStr        = Param string
    paramDelim      = (default: "&amp;") Param delimiter
-->
<#function trimParamStrDelims paramStr paramDelim="&amp;">
  <#local res = paramStr>
  <#if res?starts_with(paramDelim)>
    <#local res = res?substring(paramDelim?length)>
  <#elseif res?starts_with("?") || res?starts_with("&")>
    <#local res = res?substring(1)>
  </#if>
  <#if res?ends_with(paramDelim)>
    <#local res = res?substring(0, (res?length - paramDelim?length))>
  <#elseif res?ends_with("?") || res?ends_with("&")>
    <#local res = res?substring(0, (res?length - 1))>
  </#if>
  <#return res>
</#function> 

<#-- 
*************
* splitStyleNames
************
Splits a style classes string into sequence, in same order as input.
                    
  * Parameters *
    styleString     = Style string
    
  * Return Value *
    a sequence of style names, in same order as input.
-->
<#function splitStyleNames styleString>
  <#return getPlainClassArgNames(styleString)?split(r'\s+', 'r')>
</#function> 

<#-- 
*************
* splitStyleNamesToSet
************
Splits a style classes string into a Set of unique elements, not preserving order.
                    
  * Parameters *
    styleString     = style string containing classes
    
  * Return Value *
    a java Set of style names (can be seen as sequence)
-->
<#function splitStyleNamesToSet styleString>
  <#return Static['org.ofbiz.base.util.UtilMisc'].collectionToSet(getPlainClassArgNames(styleString)?split(r'\s+', 'r'))>
</#function> 

<#-- 
*************
* joinStyleNames
************
Joins style names in a proper style string of class names.

  * Usage Example *   
    <#assign myVar = joinStyleNames("class1", "", " class3")>
       
  * Parameters *
    styleNames     = Style names, as arbitrary number of positional parameters
    
  * Return Value *
    a string of combined style names
-->
<#function joinStyleNames styleNames...>
  <#return styleNames?join(" ")?trim>
</#function> 

<#-- 
*************
* joinStyleNamesList
************
Joins style names in a proper style string of class names.

  * Usage Example *   
    <#assign myVar = joinStyleNames(["class1", "", " class3"])>
       
  * Parameters *
    styleNames     = Style names, as sequence
    
  * Return Value *
    a string of combined style names
-->
<#function joinStyleNamesList styleNames>
  <#-- this is imperfect but fast and works fine in most cases, except cases where empty strings in middle, like
    ["asdf", "", "asdf"], adds extra spaces there, but rare -->
  <#return styleNames?join(" ")?trim>
</#function> 

<#-- 
*************
* getStyleNamesByPrefix
************
Returns all style names with given prefix, as sequence.

NOTE: now recognizes special syntax cato class args.
         
  * Parameters *
    styleString         = Style string.
    classNamePrefix     = Prefix to search for.
    
  * Return Value *
    true if class/style string contains given style, false otherwise
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function getStyleNamesByPrefix styleString className>
</#function>
-->

<#-- 
*************
* containsStyleName
************
Returns true if class/style string contains given style.

NOTE: now recognizes special syntax cato class args.
                    
  * Parameters *
    styleString     = Style string
    className       = Name of class to find
    
  * Return Value *
    true if class/style string contains given style, false otherwise
-->
<#function containsStyleName styleString className>
  <#-- don't need regexp -->
  <#return getPlainClassArgNames(styleString)?split(" ")?seq_contains(className)> 
</#function> 

<#-- 
*************
* containsStyleNamePrefix
************
Returns true if class/style string contains a style with given prefix.
   
  * Parameters *
    styleString         = Style string
    classNamePrefix     = Prefix to search for
    
  * Return Value *
    true if class/style string contains given style prefix, false otherwise
-->
<#function containsStyleNamePrefix styleString classNamePrefix>
  <#-- don't need regexp -->
  <#local plainStyle = getPlainClassArgNames(styleString)>
  <#return plainStyle?starts_with(classNamePrefix) || plainStyle?contains(" " + classNamePrefix)> 
</#function> 

<#-- 
*************
* removeStyleNames
************   
Removes style classes from a style string. 
strips lead/trailing space.

NOTE: now recognizes special syntax cato class args.
           
  * Parameters *
    styleString     = Style string
    namesToRemove   = string or sequence. Sequence of names or space-separated string of names to remove,
                      or single name (as string) to remove.
  * Return Value *
    the style string with names removed, reformatted, in same order as input
-->
<#function removeStyleNames styleString namesToRemove>
  <#local prefix = getClassArgPrefix(styleString)>
  <#local styleString = getPlainClassArgNames(styleString)>
  <#if namesToRemove?is_string> <#-- NOTE: this is only ok as long as we don't accept hashes here, else use isObjectType -->
    <#local namesToRemove = splitStyleNamesToSet(namesToRemove)>
  <#else>
    <#local namesToRemove = Static['org.ofbiz.base.util.UtilMisc'].collectionToSet(namesToRemove)>
  </#if>
  <#local res = "">
  <#-- don't need regexp, multiple spaces don't affect result -->
  <#local styleArr = styleString?split(" ")>
  <#list styleArr as style>
    <#if style?has_content && !namesToRemove.contains(style)>
        <#local res = res + " " + style>
    </#if>
  </#list>
  <#return prefix + res?trim>
</#function> 

<#-- 
*************
* stripParamStrFromUrl
************
Strips param string (starting with "?" or ";") from url.
                    
  * Parameters *
    url             = URL to strip
-->
<#function stripParamStrFromUrl url>
  <#local index = url?index_of("?")>
  <#if (index >= 0)>
    <#local url = url[0..<index]>
    <#local index = url?index_of(";")>
    <#if (index >= 0)>
      <#local url = url[0..<index]>
    </#if>
  </#if>
  <#return url>
</#function> 

<#-- 
*************
* addParamsToUrl
************
Adds parameters from a hash to a URL. appends delimiters as needed.
                    
  * Parameters *
    url             = URL to augment
    paramMap        = Hash of keys to values to add
    paramDelim      = (default: "&amp;") Param delimiter
    includeEmpty    = Include empty values, or if false omit empty values
-->
<#function addParamsToUrl url paramMap paramDelim="&amp;" includeEmpty=true>
  <#return addParamsToStr(addParamDelimToUrl(url, paramDelim), paramMap, paramDelim, includeEmpty)>
</#function> 

<#-- 
*************
* escapeUrlParamDelims
************
Escapes the URL's parameter delimiters if they are not already escaped.

  * Parameters *
    url             = URL to escape
    paramDelim      = (default: "&amp;") Param delimiter for substitution
-->
<#function escapeUrlParamDelims url paramDelim="&amp;">
  <#if url?contains(paramDelim)>
    <#return url>
  <#else>
    <#return url?replace('&', paramDelim)>
  </#if>
</#function>

<#-- 
*************
* camelCaseToDashLowerName
************
Converts camelCase to camel-case.
-->
<#function camelCaseToDashLowerName name>
  <#return StringUtil.wrapString(Static["com.ilscipio.cato.ce.webapp.ftl.lang.LangFtlUtil"].camelCaseToDashLowerName(name))?string>
</#function>


<#-- 
*************
* isObjectType
************
Checks the given FTL object against a set of logical types.

This is used to ensure type checks are valid across both native FTL types and java/groovy types received in context.

WARN: The FTL built-in ?is_string and ?is_hash are insufficient for BeanModel-based widget context vars.
    In many cases you must use this function.

  * Parameters *
    type        = (string|map|simplemap|complexmap) (required)
                  string: Anything meant to be a string WITHOUT being a more complex type.
                  map: Simple hash, or context map that exposes methods as keys (BeanModel with underlying Map) 
                       (simplemap or complexmap)
                  simplemap: Simple hash only (?keys to get elems).
                  complexmap: Context map that exposes methods as keys (BeanModel) only (.keySet() to get elems).
    object      = The object to test
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function isObjectType type object>
</#function>
-->

<#-- 
*************
* copyObject
************
Performs a shallow copy of an object (map or list).

Usually not needed in FTL; provided for advanced usage.
The resulting underlying type may differ from the original, but tries to keep the type similar when 
possible.

  * Parameters *
    object      = The object to copy
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function copyObject object>
</#function>
-->

<#-- 
*************
* copyMap
************
Performs a shallow copy of a map. 

Usually not needed in FTL; for advanced usage. The resulting underlying type may differ from the 
original; in principle tries to preserve, but in most cases will create a simple FTL hash.

NOTES: 
* copyObject will also work fine on maps, but this provides more map-specific options.
* This will only copy maps that don't have ?keys support if mode is include ("i") and inExKeys specified.

  * Parameters *
    map         = The source map
    mode        = ("e"|"i"|) (default: -none-) Optional mode flags
                  "e": exclude listed keys
                  "i": include only listed keys
    inExKeys    = (optional) List or wrapped set of keys to include or exclude    
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function copyMap map mode inExKeys>
</#function>
-->

<#-- 
*************
* toSimpleMap
************
Takes a bean-wrapped map and switches it to a simple map adapter instead, without performing
any copies.

If the object is not a complex map but already another type of map, returns it as-is. Other types throw errors.

  * Parameters *
    object       = The source map
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function toSimpleMap object>
</#function>
-->

<#-- 
*************
* mapKeys
************
Gets the logical map keys from any map-like object, whether an FTL hash (?keys) or 
a bean-wrapped context var (.ketSet()).

Unlike ?keys, behaves as expected on both maps from screen context and FTL.

  * Parameters *
    object       = The source map
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function mapKeys object>
</#function> 
-->

<#-- 
*************
* concatMaps
************
Concatenates two maps similar to FTL "+" hash operator, but works with ofbiz context maps as well.

By default, result is now always a simple map, so type is predictable.

Using "+" on screen context bean maps causes problems such as "hiding" of the bean map type underneath
an FTL wrapper, which wrecks all subsequent type checks. And others.

TODO?: This is currently inefficient; but must guarantee immutability.
    Note currently forced to implement in FTL (not java transform) if want to exploit "+" operator.
    
IMPL NOTE: it's part of this function's interface that any of the arguments for which ?has_content is false,
    will not be used. this covers the cases where non-map types are passed as well ("", [], etc.).
    sometimes an empty non-map type will be passed, should be considered valid.
    shouldn't need to check for non-empty non-map types however, those are all clear coding errors.
    e.g., this is used at beginning of macros that take inlineArgs... or inlineAttribs...
    they count on this method to handle empty sequence case.
    they also depend on this for the toSimple=true conversion.

  * Parameters *
    first       = The first map
    second      = The second map
    toSimple    = boolean (default: true) If true, ensure result is a simple hash.
    
  * Related *
    #mergeArgMaps
-->
<#function concatMaps first second toSimple=true>
  <#if first?has_content>
    <#if second?has_content>
      <#if toSimple>
        <#-- convert everything to simple map -->
        <#return toSimpleMap(first) + toSimpleMap(second)>
      <#else>
        <#if isObjectType("simplemap", first) && isObjectType("simplemap", second)>
          <#return first + second>
        <#else>
          <#return Static["com.ilscipio.cato.ce.webapp.ftl.lang.LangFtlUtil"].concatMaps(first, second)>
        </#if>
      </#if>
    <#else>
      <#if toSimple>
        <#return toSimpleMap(first)>
      <#else>
        <#return first>
      </#if>
    </#if>
  <#elseif second?has_content>
    <#if toSimple>
      <#return toSimpleMap(second)>
    <#else>
      <#return second>
    </#if>
  <#else>
    <#return {}>
  </#if>
</#function> 

<#-- 
*************
* evalToSimpleMap
************
Similar to toSimpleMap but if the object is a string, tries to ?eval it to a map;
otherwise converts the map to a simple hash.

Enclosing braces are optional.

TODO: implement as transform

  * Parameters *
    object       = string or map. The object to convert to a simple map.
-->
<#function evalToSimpleMap object>
  <#if isObjectType("string", object)>
    <#local str = object?trim>
    <#if !str?starts_with("{")>
      <#local str = "{" + str + "}">
    </#if>
    <#return str?eval>
  <#else>
    <#return toSimpleMap(object)>
  </#if>
</#function>

<#-- 
*************
* toSet
************
Returns a bean-wrapped java Set for a sequence or collection. 

If already a bean-wrapped Set, returns as-is; does not create a copy (this is analogous to toSimpleMap
and also org.ofbiz.base.util.UtilMisc.toSet).
If called without parameters, creates new empty set.

  * Parameters *
    object       = collection (optional) If omitted, creates an empty Set. 
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function toSet object=[]>
</#function>
-->

<#-- 
*************
* compileProgressSuccessAction
************
Widget-related progress success action compile (see widget-form.xsd form element extra "attribs" attrib).

  * Parameters *
   progressSuccessAction        = Progress success action
-->
<#function compileProgressSuccessAction progressSuccessAction>
  <#return StringUtil.wrapString(Static["com.ilscipio.cato.ce.webapp.ftl.template.TemplateFtlUtil"].compileProgressSuccessAction(progressSuccessAction))?string>
</#function>

<#-- 
*************
* getCurrentSectionLevel
************
Gets current global section level. 

  * Parameters *
    useDefault      = boolean (default: true) If true, if no heading defined, returns default; else return void
    
  * Related *
    @section
    #setCurrentSectionLevel
-->
<#function getCurrentSectionLevel useDefault=true>
  <#local sLevel = getRequestVar("catoCurrentSectionLevel")!"">
  <#if sLevel?has_content>
    <#return sLevel>
  <#elseif useDefault>
    <#return getDefaultSectionLevel()>
  </#if>
</#function> 

<#-- 
*************
* getDefaultSectionLevel
************
Gets default section level.

This should almost never be used except in odd macro implementations. 

  * Parameters *
    (none)
   
  * Related *
    @section
-->
<#function getDefaultSectionLevel>
  <#return 1>
</#function> 

<#-- 
*************
* setCurrentSectionLevel
************
Sets current global section level manually. 

For advanced markup; bypasses @section.
    
  * Parameters *
    sLevel          = ((number)) The section level
   
  * Related *
    @section
    #getCurrentSectionLevel
-->
<#function setCurrentSectionLevel sLevel>
  <#-- set as request attrib so survives template environments and screens.render -->
  <#local dummy = setRequestVar("catoCurrentSectionLevel", sLevel)>
  <#global catoCurrentSectionLevel = sLevel>
  <#return "">
</#function>

<#-- 
*************
* getCurrentHeadingLevel
************
Gets current global heading (title) level. 

  * Parameters *
    useDefault      = ((boolean)) (default: true) If true, if no heading defined, return default; else return void

  * Related *
    @heading
    #getDefaultHeadingLevel
-->
<#function getCurrentHeadingLevel useDefault=true>
  <#local hLevel = getRequestVar("catoCurrentHeadingLevel")!"">
  <#if hLevel?has_content>
    <#return hLevel>
  <#elseif useDefault>
    <#return getDefaultHeadingLevel()>
  </#if>
</#function> 

<#-- 
*************
* getDefaultHeadingLevel
************
Gets default heading (title) level. 

  * Parameters *
    (none)
    
  * Related *
    @heading
-->
<#function getDefaultHeadingLevel>
  <#return 2>
</#function> 

<#-- 
*************
* setCurrentHeadingLevel
************
Set current global heading level manually. 

For advanced markup; bypasses @section (but a parent @section will restore heading upon closing).

  * Parameters *
    hLevel          = ((number)) Heading level
    
  * Related *
    @heading
    #getCurrentHeadingLevel
-->
<#function setCurrentHeadingLevel hLevel>
  <#-- set as request attrib so survives template environments and screens.render -->
  <#local dummy = setRequestVar("catoCurrentHeadingLevel", hLevel)>
  <#global catoCurrentHeadingLevel = hLevel>
  <#return "">
</#function>

<#-- 
*************
* objectAsScript
************
Output a Freemarker variable as a value in Javascript, JSON or similar script language.

DEV NOTE: This is complicated in Ofbiz because Maps and objects from
    widget/java/groovy context don't behave same as FTL types.
    Currently can't use ?keys on Maps and generally for most types gotten from
    groovy context, the ?is_ don't work like you'd expect (a lot of vars
    implement ?string and ?is_hash_ex that aren't really either of these, so leave for last)

TODO: doesn't handle dates (ambiguous?)
                    
  * Parameters *
    object          = (required) the FTL or context object
    lang            = (js|json) (required)
    wrap            = ((boolean)) (default: true) If true, wrap in {}, [], or "" as needed; otherwise omit enclosing characters.
    hasMore         = ((boolean)) (default: false) If true, always include trailing separator in hashes and arrays
    escape          = ((boolean)) (default: true) Escape characters in strings
    maxDepth        = ((number)) (default: -1) Maximum depth, or -1 for no limit
-->
<#macro objectAsScript object lang wrap=true hasMore=false escape=true maxDepth=-1 currDepth=1>
  <#if isObjectType("string", object)>
    <#-- WARN: context strings also implement ?is_hash when bean models; ?is_string not good enough -->
    <#if wrap>"${escapeScriptString(lang, object?string, escape)}"<#else>${escapeScriptString(lang, object?string, escape)}</#if><#t>
  <#elseif object?is_number> 
    ${object}<#t>
  <#elseif object?is_boolean>
    ${object?c}<#t>
  <#elseif object?is_date_like>
    <#-- TODO? -->
    <#if wrap>"${escapeScriptString(lang, object?string, escape)}"<#else>${escapeScriptString(lang, object?string, escape)}</#if><#t>
  <#elseif object?is_enumerable> 
    <#-- check this before string checks and hash because some of these from groovy 
        also implement ?string and ?is_hash_ex at same time (?).
        but usually for those if ?is_enumerable it means it was a list-like type. -->
    <#if (maxDepth < 0) || (currDepth <= maxDepth)>
      <#if wrap>[</#if><#lt>
      <#list object as item> 
          <#if item??><@objectAsScript lang=lang object=item wrap=true escape=escape maxDepth=maxDepth currDepth=(currDepth+1)/><#else>null</#if><#if item_has_next || hasMore>,</#if>
      </#list> 
      <#if wrap>]</#if><#rt>
    <#else>[]</#if>
  <#elseif object?is_hash_ex && isObjectType("map", object)>
    <#if (maxDepth < 0) || (currDepth <= maxDepth)>
      <#if wrap>{</#if><#lt>
      <#list mapKeys(object) as key> 
          "${escapeScriptString(lang, key, escape)}" : <#if object[key]??><@objectAsScript lang=lang object=object[key] wrap=true escape=escape maxDepth=maxDepth currDepth=(currDepth+1) /><#else>null</#if><#if key_has_next || hasMore>,</#if>
      </#list>
      <#if wrap>}</#if><#rt>
    <#else>{}</#if>
  <#elseif object?is_string> 
    <#-- WARN: this may catch a lot of different context object types, but ones we care about are above -->
    <#if wrap>"${escapeScriptString(lang, object?string, escape)}"<#else>${escapeScriptString(lang, object?string, escape)}</#if><#t>
  <#-- some of the following are invalid/inconvertible types, but catch them because otherwise debugging impossible -->
  <#elseif objectAsScriptTreatInvalidNonFatal && object?is_hash && !object?is_string> 
    "__NONEXHASH__"<#t>
  <#elseif objectAsScriptTreatInvalidNonFatal && object?is_method>
    "__METHOD__"<#t>
  <#elseif objectAsScriptTreatInvalidNonFatal && object?is_directive>
    "__DIRECTIVE__"<#t>
  <#else>
    <#-- fallback, best-effort. -->
    <#if wrap>"${escapeScriptString(lang, object?string, escape)}"<#else>${escapeScriptString(lang, object?string, escape)}</#if><#t>
  </#if> 
</#macro>

<#-- escapes a string to be placed within "" literals -->
<#function escapeScriptString lang val escape=true>
  <#if escape>
    <#switch lang?lower_case>
      <#case "json">
        <#return val?json_string>
        <#break>
      <#case "js">
      <#case "javascript">
        <#-- FIXME?: investigate this... -->
        <#return val?js_string?replace("\\'", "\'")>
        <#break>
      <#case "raw">
      <#default>
        <#return val>
        <#break>
    </#switch>
  <#else>
    <#return val>
  </#if>
</#function>

<#global objectAsScriptTreatInvalidNonFatal = true>

<#-- compresses all blankspace (space, newline) in a string into single spaces
    needed for html in javascript strings -->
<#function compressStringBlankspace str>
  <#return str?replace(r"[\n\s\r]+", " ", "r")>
</#function>


<#-- 
*************************************
* API-WRITING UTILITIES *
*************************************
* Intended to assist implementation of template macros.
-->

<#-- 
*************
* mergeArgMaps
************
Merges cato macro inlineArgs/args/defaultArgs/overrideArgs argument maps for macros implementing
the advanced argument interface.

The advanced interface follows the
  <#macro name args={} inlineArgs...>
pattern, whereby inlineArgs map is merged over the args map, over defaults.

It cannot be used for the 'attribs={} inlineAttribs...' pattern (see mergeAttribMaps). 
Instead, it can help in implementing that pattern within the 'args={} inlineArgs...' pattern.

This is specific to cato macro patterns and may contain extra handling besides concatenating maps.

For one, this currently records the macro's static named args as passed in the defaultArgs and overrideArgs maps
in a list of names with the key "localArgNames" in the resulting map.
It will also append the names to a list read back from the incoming "args" map with 
key name "allArgNames", append to it, and return it again as "allArgNames". 
allArgNames allows arg names to be automatically accumulated
between delegating/overriding macro calls that make use of mergeArgMaps without
having to recreate new intermediate maps. allArgNames keep track of all the explicit
args used by all the macros in a call chain, which can then be used to identify
the remaining "extra" arguments (analogous to varArgs in FTL's <#macro varArgs...>).
WARN: in some cases allArgNames accumulation and reuse of maps could be undesirable.
    (currently workaround this by using localArgNames only and/or creating new intermediate maps).
NOTE: currently the result may often contain duplicate names (is not a set). 
    This is normal and should not be changed; code that uses these lists may make a Set if needed.
TODO: current macros must be translated to exploit this mechanism.

The result is the caller can access any arg names in the resulting map which had a default
value using simple syntax guaranteed not to cause missing values. e.g
  <#macro name args={} inlineArgs...>
    <#local args = mergeArgMaps(inlineArgs, args, {"myParam1":true})>
    <#if args.myParam1>
      ...
    </#if>
  </#macro>
OR
  <#macro name args={} inlineArgs...>
    <#local args = mergeArgMaps(inlineArgs, args, {"myParam1":true})>
    <#local dummy = localsPutAll(args)>  
    <#if myParam1>
      ...
    </#if>
  </#macro>
  
This can also be used for functions, but for functions, the inlineArgs param should always
be passed an empty hash (functions don't support named parameters).

TODO: rewrite this macro using FTL transform; also see @mergeArgMapsToLocals below
TODO?: may want helper booleans to control in/out allArgNames?

  * Parameters *
    args          = The args map (normal priority). 
                    May be any type of map.
    inlineArgs    = The macro's inline args (high priority). 
                    Expects either FTL hash or empty sequence (but not non-empty sequence).
    defaultArgs   = Arg defaults (lowest priority). 
                    Expects an FTL hash only. The key names in this map are also used to
                    create a list of the known arguments this macro accepts.
                    As such, even if the macro does not need this map, it should pass it along
                    with the names of all supported arguments.
                    The names end up in the resulting map as localArgNames.
    overrideArgs  = Extra macro args that override all others (highest priority). 
                    Expects an FTL hash ONLY (not context map).
                    The names used here also count toward the localArgNames.
                    NOTE: If a key is present in both defaultArgs and overrideArgs, it will result in
                        two names added to localArgNames/allArgNames. The code that finally uses
                        those lists should make a Set if it needs to.
                    NOTE: This map is present for cases where the others are insufficient.
                        but otherwise should avoid using this map when possible.
-->
<#function mergeArgMaps args={} inlineArgs={} defaultArgs={} overrideArgs={}>
  <#if !inlineArgs?has_content> <#-- necessary to prevent empty sequence -->
    <#local inlineArgs = {}>
  </#if>
  <#local localArgNames = (defaultArgs?keys) + (overrideArgs?keys)>
  <#local allArgNames = (args.allArgNames![]) + localArgNames>
  <#return defaultArgs + toSimpleMap(args) + inlineArgs + overrideArgs + 
    { "localArgNames":localArgNames, "allArgNames":allArgNames }>
</#function>

<#-- 
*************
* mergeArgMapsBasic
************
a version of mergeArgMaps that only merges maps, but doesn't perform any special implied
operations on them.

  * Parameters *
    (other)         = See #mergeArgMaps
-->
<#function mergeArgMapsBasic args={} inlineArgs={} defaultArgs={} overrideArgs={}>
  <#if !inlineArgs?has_content> <#-- necessary to prevent empty sequence -->
    <#local inlineArgs = {}>
  </#if>
  <#return defaultArgs + toSimpleMap(args) + inlineArgs + overrideArgs>
</#function>

<#-- 
*************
* mergeArgMapsToLocals
************
Combines mergeArgMaps and localsPutAll into one call.

DO NOT USE AT CURRENT TIME. Use mergeArgMaps and localsPutAll instead.

TODO: implement as transform (not currently possible)

This is a helper function which should be functionally identical to doing:
  <#macro name args={} inlineArgs...>
    <#local args = mergeArgMaps(inlineArgs, args, defaultArgs, overrideArgs)>
    <#local dummy = localsPutAll(args)>
    ...
  </#macro>
  where defaultArgs and overrideArgs are valid maps.
-->
<#-- NOT IMPLEMENTED
<#function mergeArgMapsToLocals args={} inlineArgs={} defaultArgs={} overrideArgs={}>
</#function>
-->

<#-- 
*************
* mergeArgMapsToLocalsBasic
************
Same as mergeArgMapsToLocals but uses mergeArgMapsBasic logic instead of mergeArgMaps logic.
-->
<#-- NOT IMPLEMENTED
<#function mergeArgMapsToLocalsBasic args={} inlineArgs={} defaultArgs={} overrideArgs={}>
</#function>
-->

<#-- 
*************
* getCatoMacroDefaultArgs
************
Returns all the known args and default args for a cato macro that uses the advanced
args pattern, where the macro resides in the given namespace (or namespace-like map).

Basically, the macros should always have default values (in the event one was missing, this method
should automatically give one such as empty string).
If namespace is omitted, currently (2015-12-14), this will use ".vars" as the namespace.

DEV NOTE: there could be some use for the ?namespace built-in somewhere around this function
   or more generally the default args, but so far elusive. only works on macro/function so
   maybe best not to rely on it.
-->
<#function getCatoMacroDefaultArgs name namespace="">
  <#if !namespace?has_content>
    <#local namespace = .vars>
  </#if>
  <#return namespace[name + "_defaultArgs"]!{}>
</#function>

<#-- 
*************
* mergeAttribMaps
************
Merges cato macro attribs/inlineAttribs/defaultAttribs/overrideAttribs maps for macros implementing
a special attribs/inlineAttribs pattern for HTML element attributes.

This is found in the
  <#macro name arg1="" arg2="" ... argN="" attribs={} inlineAttribs...>
pattern, whereby inlineAttribs map is merged over the attribs map, and these maps only contain extra
element attributes.

This pattern is simpler but much less flexible than the 'args={} inlineArgs...' pattern.
They are different and NOT interchangeable.

Parameters are analogous to #mergeArgMaps but implied logic differs.
-->
<#function mergeAttribMaps attribs={} inlineAttribs={} defaultAttribs={} overrideAttribs={}>
  <#if !inlineAttribs?has_content> <#-- necessary to prevent empty sequence -->
    <#local inlineAttribs = {}>
  </#if>
  <#return defaultAttribs + toSimpleMap(attribs) + inlineAttribs + overrideAttribs>
</#function>

<#-- 
*************
* makeAttribMapFromArgs
************
Takes an args map returned by mergeArgMaps (NOT mergeArgMapsBasic) and checks it for an "attribs"
sub-map and blends them together logically to make an attribs map.

The resulting map will contain the allArgNames and localArgNames members from the args map for easier passing.
NOTE: this currently does not change the exclude lists (see @mergeArgMaps), but could in the future.

NOTE: The resulting map does not contain only attribs. It may contain a large number of unrelated
members plus "attribs", "allArgNames", "localArgNames", "excludeNames" and "noExcludeNames" members. 

Parameters are analogous to #mergeArgMaps but implied logic differs.

  * Related *
    #getAttribMapAllExcludes
-->
<#function makeAttribMapFromArgMap args={}>
  <#if args.attribs?has_content && args.attribs?is_hash> <#-- WARN: poor check -->
    <#local args = args.attribs + args>
  </#if>
  <#return args>
</#function>

<#-- 
*************
* getAttribMapAllExcludes
************
Returns the attrib map excludes based on allArgNames list, plus known needed excludes, plus an optional list,
plus "noExclude" alternatives of all the aforementioned that prevent excludes.
The result is returned as a bean-wrapped Set.

TODO: implement as transform.

  * Related *
    #makeAttribMapFromArgs
-->
<#function getAttribMapAllExcludes attribs={} exclude=[] noExclude=[]>
  <#local exclude = toSet(exclude)>
  <#local noExclude = toSet(noExclude)>
  
  <#if attribs.excludeNames?has_content>
    <#local dummy = exclude.addAll(attribs.excludeNames)!>
  </#if>
  <#if attribs.allArgNames?has_content>
    <#local dummy = exclude.addAll(attribs.allArgNames)!>
  </#if>

  <#local dummy = exclude.addAll(["attribs", "allArgNames", "localArgNames", "excludeNames", "noExcludeNames"])>
  
  <#if attribs.noExcludeNames?has_content>
    <#local dummy = noExclude.addAll(attribs.noExcludeNames)!>
  </#if>

  <#if noExclude?has_content>
    <#local dummy = exclude.removeAll(noExclude)!>
  </#if>
  <#return exclude>
</#function>


<#-- 
*************
* compileClassArg
************
Compiles a class argument as common to most standard macros,
producing a simple list of class names from a class arg, adding optional default.

NOTE: Default value may also be set prior to using addClassArgDefault on the class arg instead
    of passing as parameter.

CLASS ARGUMENT FUNCTIONS

This function and the others below help parse class argument passed to macros.
Not for use in templates.

Macro class arguments can have essential (required) and non-essential (default)
defaults and values added by the macro before producing the final class attribute. 
The essentials can't be omitted. 
The non-essentials in some cases you want to replace with value and other times 
you want to keep them but only add extra classes. Having multiple args for each elem 
class arg gets heavy.

So to address all the cases, these functions cause the class arg on all supporting macros 
to accept following values:

- string with "+" prefix or empty string "": 
    means allow macro to add non-essential default classes. 
    the class names after the "+" will be appended to any classes added
    by the macro, and will never replace macro defaults.
    this means the same as boolean true but with extra classes provided.
    in other words, appends extra classes.
- string with "=" prefix or non-empty string: 
    means prevent macro from adding non-essential default classes.
    class names given will replace macro defaults, i.e. non-essential classes. 
    macro may still add its own required classes.
    this is the same as boolean false but with replacement classes provided.
    in other words, replaces default (non-essential) classes.
  
In one or two cases the non-essential defaults are "conditionally essential" 
so class="=" makes little sense, but we dont really need to handle that.
NOTE: Not all macros support all the cases but the syntax is supported everywhere 
now anyway so doesn't matter to templates.

compileClassArg should usually be used as late as possible in macro:
  <#local classes = compileClassArg(class)>
A defaultVal can be set which is the same as doing:
  <#local class = addClassArgDefault(class, "default-class")>
  <#local classes = compileClassArg(class)>
  
IMPORTANT:
Currently in some cases the logic above breaks too much compatibility. So it is not universally used
for screen/form/menu widgets although often supported.
For these, in some places an "xxxExplicit" version of functions are used instead which will only
replace if explicitly prefixed with "=", but not if no prefix. Otherwise, existing widget styles break
the grid everywhere.
-->
<#function compileClassArg class defaultVal="">
  <#if defaultVal?has_content>
    <#local class = addClassArgDefault(class, defaultVal)>
  </#if>

  <#if class?starts_with("+") || class?starts_with("=")>
    <#return class?substring(1)?trim>
  <#else>
    <#return class?trim>
  </#if>
</#function>

<#-- 
*************
* compileClassArgExplicit
************
Version of compileClassArg that will only use defaultVal if it explicitly starts with "="; needed
for compatibility in some cases 
-->
<#function compileClassArgExplicit class defaultVal="">
  <#if defaultVal?has_content>
    <#local class = addClassArgDefaultExplicit(class, defaultVal)>
  </#if>

  <#if class?starts_with("+") || class?starts_with("=")>
    <#return class?substring(1)?trim>
  <#else>
    <#return class?trim>
  </#if>
</#function>

<#-- 
*************
* compiledClassAttribStr
************
Produces a class string attribute at same time as compiling class arg, with optional default,
with leading space.
-->
<#macro compiledClassAttribStr class defaultVal="">
  <#local classes = compileClassArg(class, defaultVal)>
  <#if classes?has_content> class="${classes}"</#if><#t>
</#macro>

<#-- 
*************
* compiledClassAttribStrExplicit
************
Explicit version of #compiledClassAttribStr.
-->
<#macro compiledClassAttribStrExplicit class defaultVal="">
  <#local classes = compileClassArgExplicit(class, defaultVal)>
  <#if classes?has_content> class="${classes}"</#if><#t>
</#macro>

<#-- 
*************
* toClassArgAppending
************
Converts simple class name to an appending class
-->
<#function toClassArgAppending newClass>
  <#if newClass?has_content>
    <#return "+" + newClass>
  <#else>
    <#return "+">
  </#if>
</#function>

<#-- 
*************
* toClassArgReplacing
************
Converts simple class name to a replacing class
-->
<#function toClassArgReplacing newClass>
  <#if newClass?has_content>
    <#return newClass>
  <#else>
    <#return "=">
  </#if>
</#function>

<#-- 
*************
* addClassArg
************
Adds a required class, that must appear in class string.

NOTE: Does not influence the default value logic perceived by addClassArgDefault (user intention preserved). 
    It does this by prefixing result with "+" string where necessary.

ADD CLASS ARG FUNCTIONS

These functions take a template-level/logical cato macro "class" arg and add to it the given class.
Should be called by the implementing macros only. 

Depending on the previous value in class, non-essential (default) values may simply be discarded, but
essentials (required) will always be added in some way, transforming the class value.

The newClass parameter is a simple string literal and is not interpreted.

These are non-destructive except for addClassArgReplacing which always causes the string to become
a replacing string ("=").
-->
<#function addClassArg class newClass>
  <#if !newClass?has_content>
    <#return class>
  </#if>
  <#if (!class?has_content)>
    <#return "+" + newClass> <#-- if string was empty, make sure start with "+" so we don't crush next defaults -->
  <#else>
    <#return class + " " + newClass> <#-- don't worry about spaces here; trimmed later -->
  </#if>
</#function>

<#-- 
*************
* addClassArgReplacing
************
Special case of addClassArg where the required class will become a replacing string ("=" prefix),
though will not squash previous values. 

NOTE: This destroys information about what macro user requested and affects the default value logic
    perceived by addClassArgDefault. 
-->
<#function addClassArgReplacing class newClass>
  <#if !newClass?has_content>
    <#return class>
  </#if>
  <#if (!class?has_content)>
    <#return "=" + newClass> 
  <#elseif class?starts_with("+")>
    <#return "=" + class?substring(1) + " " + newClass>
  <#else>
    <#return class + " " + newClass>
  </#if>
</#function>

<#-- 
*************
* addClassArgDefault
************
Adds a class only if the class arg does not contain default-replacing classes. 
It adds default if class is true, empty or starts with "+".
-->
<#function addClassArgDefault class newClass>
  <#if !newClass?has_content>
    <#return class>
  </#if>
  <#if (!class?has_content)>
    <#return "+" + newClass>
  <#elseif class?starts_with("+")>
    <#return class + " " + newClass>
  <#else>
    <#return class> <#-- don't add -->
  </#if>
</#function>

<#-- 
*************
* addClassArgDefaultExplicit
************
Version of addClassArgDefault that will only replace default if it explicitly starts with "="; needed
for compatibility in some cases
-->
<#function addClassArgDefaultExplicit class newClass>
  <#if !newClass?has_content>
    <#return class>
  </#if>
  <#if (!class?has_content)>
    <#return "+" + newClass>
  <#elseif class?starts_with("+") || !class?starts_with("=")>
    <#return class + " " + newClass>
  <#else>
    <#return class> <#-- don't add -->
  </#if>
</#function>

<#-- 
*************
* combineClassArgs
************
Logically combines two template-level cato macro "class" arguments. 
The second almost always overrides the first, except when second is appending a class
with "+", in which case depends on the first.

Essentially it extends the intended use of the class arg to mean "+" also allows appending
of previous values (in addition of being an appending value itself).

NOTE: Even if the second arg is merely "+" (which usually means "use defaults" for cato macros),
    this method will return "+" and dismiss the first argument. "+" is not treated as
    pass-through here. This does not seem consistent,
    but is chosen explicitly so that caller/macro decides what the "pass-through" value
    is supposed to be. Here, we assume "+" was passed explicitly to override the first.
    We only treat "+" with an arg.
    In other words, can see this as another kludge for lack of null values in freemarker.
    TODO: review this
-->
<#function combineClassArgs first second>
  <#if second?starts_with("+") && (second?length > 1)>
    <#if !first?has_content>
      <#return second>
    <#else>
      <#-- first has content; whether was replacing or appending, can simply append second here -->
      <#return first + " " + second?substring(1)>
    </#if>
  <#else>
    <#return second>
  </#if>
</#function>

<#-- 
*************
* getPlainClassArgNames
************
Returns class names without their Cato-supported prefix.
-->
<#function getPlainClassArgNames class>
  <#if class?starts_with("+") || class?starts_with("=")>
    <#return class?substring(1)>
  <#else>
    <#return class>
  </#if>    
</#function>

<#-- 
*************
* getClassArgPrefix
************
Returns the Cato-supported class prefix from a class arg.
-->
<#function getClassArgPrefix class>
  <#if class?starts_with("+") || class?starts_with("=")>
    <#return class[0]>
  <#else>
    <#return "">
  </#if>    
</#function>

<#-- 
*************
* translateStyleStrClassesArg
************
Translates a class arg from a string-only representation to a FTL value which can be passed as
macro args processed by compileClassArg.

Usually those macro args take true or false by default.

Needed for string repr of booleans and because empty string "" may have different meanings
depending on context. also translates booleans.

  * Related *
    #compileClassArg
-->
<#function translateStyleStrClassesArg val>
  <#if val?has_content>
    <#return val> <#-- this is simple since booleans were removed -->
  </#if>    
</#function>

<#-- 
*************
* translateStyleStrBoolArg
************
Translates a bool arg from a string-only representation to a FTL value which can be passed as
macro args expected to be booleans.

Usually those macro args take "" by default, to mean default.

  * Related *
    #compileClassArg
-->
<#function translateStyleStrBoolArg val>
  <#if val?has_content>
    <#return val>
  <#elseif val == "true">
    <#return true>
  <#elseif val == "false">
    <#return false>
  </#if>    
</#function>

<#-- 
*************
* translateStyleStrNumberArg
************
Translates a number arg from a string-only representation to a FTL value which can be passed as
macro args expected to be numbers.

Usually those macro args take "" by default, to mean default.

  * Related *
    #compileClassArg
-->
<#function translateStyleStrNumberArg val>
  <#if val?has_content>
    <#return val?number>
  </#if>    
</#function>

<#-- 
*************
* pushRequestStack
************
Pushes a value onto a global stack variable in request scope (request attributes, or if no request, globals).

  * Parameters *
    name        = global request stack var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
    val         = value
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function pushRequestStack name val>
</#function>
-->

<#-- 
*************
* popRequestStack
************
Pops a global stack variable in request scope (request attributes, or if no request, globals).

  * Parameters *
    name        = global request stack var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function popRequestStack name>
</#function>
-->

<#-- 
*************
* setLastRequestStack
************
Same as doing popRequestStack + pushRequestStack, but will never fail if stack is empty - will simply
do a pushRequestStack, and much more efficient.

  * Parameters *
    name        = global request stack var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
    val         = value
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function setLastRequestStack name val>
</#function>
-->

<#-- 
*************
* readRequestStack
************
Reads the last value added to the named global stack variable in request scope
(request attributes, or if no request, globals), without popping.

  * Parameters *
    name        = global request stack var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function readRequestStack name>
</#function>
-->

<#-- 
*************
* getRequestStackAsList
************
Gets a copy of the named request stack as a list (read-only).

  * Parameters *
    name        = global request stack var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
    listType    = (copy|orig) (default: copy)
                  Caller may specify "orig" to avoid a list copy.
                  WARN: "orig" means the caller must ditch the list as soon as possible, before any
                      more modifications to the stack; otherwise results will be unpredictable.
                      It should only be used for optimization.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function getRequestStackAsList name listType>
</#function>
-->

<#-- 
*************
* setRequestVar
************
Sets a global var in request scope (request attributes, or if no request, globals).
Values set by this method must be read using getRequestVar.

  * Parameters *
    name        = global request var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
    val         = value
    mode        = optional, string; mode characters
                  "u": always unwrap the TemplateModel before storing (where possible)
                  "w": always keep TemplateModel as-is (wrapped) when storing
                  "u" and "w" are usually unnecessary and should be avoided in most template and macro code.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function setRequestVar name val mode="">
</#function>
-->

<#-- 
*************
* getRequestVar
************
Gets a global var from request scope (request attributes, or if no request, globals).
Should only be used to read values set by setRequestVar.
Not meant to be used on regular request attributes.

  * Parameters *
    name        = global request var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function getRequestVar name>
</#function>
-->

<#-- 
*************
* varsPutAll
************
Puts all key-value pairs from given map into FTL current namespace variables (#assign).

  * Parameters *
    map         = the source map
    mode        = optional mode flags
                  "e": exclude listed keys
                  "i": include only listed keys
    inExKeys    = optional list or wrapped set of keys to include or exclude      
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function varsPutAll map mode="" inExKeys=[]>
</#function>
-->

<#-- 
*************
* globalsPutAll
************
Puts all key-value pairs from given map into FTL globals (#global).

  * Parameters *
  @see varsPutAll     
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function globalsPutAll map mode="" inExKeys=[]>
</#function>
-->

<#-- 
*************
* localsPutAll
************
Puts all key-value pairs from given map into FTL globals (#local).

  * Parameters *
    @see varsPutAll        
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function localsPutAll map mode="" inExKeys=[]>
</#function>
-->


<#-- 
*************
* elemAttribStr
************
Prints a string of element attributes. (HTML, FO, XML)

NOTE: This is a very generic function; for common implementation, see @commonElemAttribStr.

TODO: implement as transform.

  * Parameters *
    attribs                     = hash of attribute-value pairs. 
                                  it currently accepts string format as a fallback/legacy support, but this is highly discouraged
                                  and other args won't work with it.
    alt                         = boolean, if true alternate row (odd), if false regular (even)
    selected                    = boolean, if true row is marked selected
    exclude                     = list of attrib names to skip
    attribNamePrefix            = add this prefix to attribute names
    alwaysAddPrefix             = if false, only add prefix if not already has prefix (default true)
    attribNamePrefixStrip       = remove this prefix from all attrib names
    attribNameSubstitutes       = map of attrib names to substitute attrib names. note if this is set, the exclude names
                                  should be the names of the subtitutes, not the input attrib names.
                                  note this is applied after prefix ops are applied.
    camelCaseToDashLowerNames   = boolean, if true converts attrib names from camelCase to camel-case at the very end.
    emptyValToken               = when this (string) value encountered, will include an empty attrib
    noValToken                  = when this (string) value encountered, will include an attrib with no value
-->
<#macro elemAttribStr attribs includeEmpty=false emptyValToken="" noValToken="" exclude=[] 
  attribNamePrefix="" alwaysAddPrefix=true attribNamePrefixStrip="" attribNameSubstitutes={} camelCaseToDashLowerNames=false>
  <#if attribs?is_hash>
    <#t>${StringUtil.wrapString(Static["com.ilscipio.cato.ce.webapp.ftl.template.TemplateFtlUtil"].makeElemAttribStr(attribs, includeEmpty, 
      emptyValToken, noValToken, exclude, attribNamePrefix, alwaysAddPrefix, attribNamePrefixStrip, attribNameSubstitutes, camelCaseToDashLowerNames))}<#t>
  <#elseif attribs?is_string>
    <#t> ${attribs?string}
  </#if>
</#macro>

<#-- 
*************
* formattedDate
************
Renders a formatted date.

NOTE: formattedDate by default renders the "date" type but it also doubles as handler for the other types
    (which also have convenience wrappers below).
    
  * Parameters *
    date         = the date
    dateType     = (date-time|timestamp|date|time) (default: date)
                  "timestamp" and "date-time" are synonymous.  
    defaultVal   = if no output is produced (empty), this value (string) will be shown instead.
   
  * Related *
    @formattedDateTime
    @formattedTime
    #formatDate
    #formatDateTime
    #formatTime
-->
<#macro formattedDate date dateTimeFormat="" specLocale=true specTimeZone=true defaultVal="" dateType="date">
  ${formatDate(date, dateTimeFormat, specLocale, specTimeZone, dateType)!defaultVal}<#t>
</#macro>

<#-- 
*************
* formattedDateTime
************
Renders a formatted date-time value (convenience wrapper).

  * Related *
    @formattedDate
-->
<#macro formattedDateTime date dateTimeFormat="" specLocale=true specTimeZone=true defaultVal="">
  ${formatDate(date, dateTimeFormat, specLocale, specTimeZone, "timestamp")!defaultVal}<#t>
</#macro>

<#-- 
*************
* formattedTime
************
Renders a formatted time value (convenience wrapper).

  * Related *
    @formattedDate
-->
<#macro formattedTime date dateTimeFormat="" specLocale=true specTimeZone=true defaultVal="">
  ${formatDate(date, dateTimeFormat, specLocale, specTimeZone, "time")!defaultVal}<#t>
</#macro>

<#-- 
*************
* formatDate
************
Formats a date.

These functions return VOID (no value) if no or empty output, so default value operator can be used.

  * Related *
    @formattedDate
-->
<#function formatDate date dateTimeFormat="" specLocale=true specTimeZone=true dateType="date">
  <#if specLocale?is_boolean>
    <#if specLocale>
      <#local specLocale = locale!>
    <#else>
      <#local specLocale = ""> <#-- FIXME: won't work, must emulate null but freemarker sucks here... -->
    </#if>
  </#if>
  <#if specTimeZone?is_boolean>
    <#if specTimeZone>
      <#local specTimeZone = timeZone!>
    <#else>
      <#local specTimeZone = ""> <#-- FIXME: won't work, must emulate null but freemarker sucks here... -->
    </#if>
  </#if>
  <#if dateType == "date">
    <#local res = Static["org.ofbiz.base.util.UtilFormatOut"].formatDate(date, dateTimeFormat, locale, timeZone)!"">
  <#elseif dateType == "time">
    <#local res = Static["org.ofbiz.base.util.UtilFormatOut"].formatTime(date, dateTimeFormat, locale, timeZone)!"">
  <#else>
    <#local res = Static["org.ofbiz.base.util.UtilFormatOut"].formatDateTime(date, dateTimeFormat, locale, timeZone)!"">
  </#if>
  <#if res?has_content>
    <#return res>
    <#-- otherwise, return void (for default value operator) -->
  </#if>
</#function>

<#-- 
*************
* formatDateTime
************
Formats a date-time value.

These functions return VOID (no value) if no or empty output, so default value operator can be used.

  * Related *
    @formattedDate
-->
<#function formatDateTime date dateTimeFormat="" specLocale=true specTimeZone=true>
  <#local res = formatDate(date, dateTimeFormat, specLocale, specTimeZone, "timestamp")!"">
  <#if res?has_content>
    <#return res>
    <#-- otherwise, return void (for default value operator) -->
  </#if>
</#function>

<#-- 
*************
* formatTime
************
Formats a time value.

These functions return VOID (no value) if no or empty output, so default value operator can be used.

  * Related *
    @formattedDate
-->
<#function formatTime date dateTimeFormat="" specLocale=true specTimeZone=true>
  <#local res = formatDate(date, dateTimeFormat, specLocale, specTimeZone, "time")!"">
  <#if res?has_content>
    <#return res>
    <#-- otherwise, return void (for default value operator) -->
  </#if>
</#function>

<#-- 
*************
* getHeadingElemSpecFromStyleStr
************
Parses a complex style string meant to describe a markup element and styles, notably heading,
and converts to a hash of values.

The style strying is in the following format:
    containerElemType:containerClass;elemType:class;param1=val1,param2=val2
"h" and "heading" elem types support + notation for relative, and for these types
a level and relLevel are extracted.

Note that the class portions may be prefixed with "+" as well for append-not-replace logic.

  * Usage examples *
    h3
    h3:headingclass
    h+3:headingclass
    div:divclass;h+3:headingclass
    div:divclass;h+3:headingclass;consumeLevel=true
-->
<#function getHeadingElemSpecFromStyleStr styleStr containerStyleStr allowedHeadingElemTypes allowedElemTypes allowedContainerElemTypes cacheId="">
  <#return Static["com.ilscipio.cato.ce.webapp.ftl.template.TemplateFtlUtil"].getHeadingElemSpecFromStyleStr(styleStr, containerStyleStr,
    allowedHeadingElemTypes, allowedElemTypes, allowedContainerElemTypes, cacheId)>

<#-- old FTL impl
  <#local headingLevel = "">
  <#local relHeadingLevel = "">

  <#local isHeadingElem = false>
  <#local titleStyle = styleStr>
  <#local titleContainerStyle = containerStyleStr>

  <#local titleArgs = {}>
  <#local titleArgsStr = titleStyle>

  <#local titleStyleParts = titleStyle?split(";")>
  <#if (titleStyleParts?size > 3)>
    <#local titleArgsStr = titleStyleParts[2]>
  <#else>
    <#local titleArgsStr = titleStyleParts?last>
  </#if>

  <#if titleArgsStr?has_content && titleArgsStr?contains("=")> <#- heuristic detect params part ->
    <#local titleArgs = splitStrParams(titleArgsStr, ",")>
    <#if (titleStyleParts?size >= 3)>
      <#local titleContainerStyle = titleStyleParts[0]>
      <#local titleStyle = titleStyleParts[1]> 
    <#elseif (titleStyleParts?size == 2)>
      <#local titleStyle = titleStyleParts[0]>   
    <#elseif (titleStyleParts?size <= 1)>
      <#local titleStyle = "">
    </#if>
  <#elseif (titleStyleParts?size > 1)>
    <#local titleContainerStyle = titleStyleParts[0]>
    <#local titleStyle = titleStyleParts[1]>  
  </#if>
  
  <#local titleContainerElemType = "">
  <#local titleContainerClass = "">
  <#if titleContainerStyle?has_content>
    <#local titleContainerStyleParts = titleContainerStyle?split(":")>

     <#if (titleContainerStyleParts?size <= 1)>
      <#- here titleContainerStyle is either an elem or class, can't tell yet ->
      <#local titleContainerElemType = titleContainerStyle?lower_case>
      <#local titleContainerClass = titleContainerStyle>
    <#else>
      <#local titleContainerElemType = titleContainerStyleParts?first?lower_case>
      <#local titleContainerClass = titleContainerStyle?substring(titleContainerElemType?length + 1)>
    </#if>

    <#- if not sequence, leave to caller to figure out if titleContainerStyle elem or class ->
    <#if allowedContainerElemTypes?is_sequence>
      <#if allowedContainerElemTypes?seq_contains(titleContainerElemType)>
        <#if (titleContainerStyleParts?size <= 1)>
          <#local titleContainerClass = "">
        </#if>
      <#else>
        <#local titleContainerElemType = "">
        <#local titleContainerClass = titleContainerStyle>
      </#if>
    </#if>
  </#if>

  <#local titleElemType = "">
  <#local titleClass = "">
  <#if titleStyle?has_content>
    <#local titleStyleParts = titleStyle?split(":")>
    <#if (titleStyleParts?size <= 1)>
      <#- here titleStyle is either an elem or class, can't tell yet ->
      <#local titleElemType = titleStyle?lower_case>
      <#local titleClass = titleStyle>
    <#else>
      <#local titleElemType = titleStyleParts?first?lower_case>
      <#local titleClass = titleStyle?substring(titleElemType?length + 1)>
    </#if>

    <#local elemTypeFound = false>
  
    <#if allowedHeadingElemTypes?is_sequence && allowedHeadingElemTypes?has_content>
      <#local res = titleElemType?matches(r'(' + allowedHeadingElemTypes?join("|") + r')(\+)?(\d*)')>
      <#if res>
        <#if res?groups[2]?has_content>
          <#if res?groups[3]?has_content>
            <#local relHeadingLevel = res?groups[3]?number>
          </#if>
        <#else>
          <#if res?groups[3]?has_content>
            <#- overrides headingLevel (so style from screen affects heading calc) ->
            <#local headingLevel = res?groups[3]?number>
          </#if>
        </#if>
        <#if (titleStyleParts?size <= 1)>
          <#local titleClass = "">
        </#if>
        <#local titleElemType = res?groups[1]>
        <#local elemTypeFound = true>
        <#local isHeadingElem = true>
      </#if>
    </#if>
    
    <#- if not sequence, let caller figure it out if titleStyle elem or class ->
    <#if !elemTypeFound && allowedElemTypes?is_sequence>
      <#if allowedElemTypes?seq_contains(titleElemType)>
        <#if (titleStyleParts?size <= 1)>
          <#local titleClass = "">
        </#if>
        <#local elemTypeFound = true>
      <#else>
        <#local titleElemType = "">
        <#- if invalid type found, use the full string as class, in case ":" char is important somehow ->
        <#local titleClass = titleStyle>
      </#if>
    </#if>
  </#if>

  <#return titleArgs + {
      "containerStyleStr":titleContainerStyle, 
      "containerElemType":titleContainerElemType, 
      "containerClass":titleContainerClass, 
      
      "styleStr":titleStyle,
      "elemType":titleElemType,
      "class":titleClass,
      
      "isHeadingElem":isHeadingElem,
      "level":headingLevel,
      "relLevel":relHeadingLevel,
      
      "argsStr":titleArgsStr
  }>
-->
</#function>

<#-- 
*************
* extractPrefixedStyleNamesWithInt
************
Helper method that can extract style names by prefix with int value suffix from style strings.

  * Parameters *
    style       = style/classes string
    prefixMap   = map of CSS class prefixes to result names
    
  * Return Value *
    a hash/map of result names to integer values.
-->
<#function extractPrefixedStyleNamesWithInt style prefixMap>
  <#return Static["com.ilscipio.cato.ce.webapp.ftl.template.TemplateFtlUtil"].extractPrefixedStyleNamesWithInt(style, prefixMap)>
</#function>

<#-- 
*************
* saveCurrentContainerSizes
************
This records current container (grid) sizes into a global stack, so that it's generally possible for inner
containers to be aware of all the sizes applied to it and the general width.
Every push should be followed by a pop.
NOTE: this is generally framework-agnostic and size-key agnostic.

  * Parameters *
    sizes      = a map of size names to integer values. typically, this will be (e.g.):
                 {"large":12, "medium":12, "small":12}
                 but can be anything (the methods in this file do not care).
-->
<#function saveCurrentContainerSizes sizes>
  <#local dummy = pushRequestStack("catoCSFactorsCacheStack", false)>
  <#return pushRequestStack("catoContainerSizesStack", sizes)>
</#function>

<#-- 
*************
* saveCurrentContainerSizesFromStyleStr
************
Same as saveCurrentContainerSizes but tries to extract the container sizes from a style/classes string.

This is needed to know grid sizes because most of the facilities only support setting
class name strings (except for @cell which supports columns/small/medium/large args).

IMPL NOTE: For this method to work, the framework-/theme-specific code must override the abstract
    function parseContainerSizesFromStyleStr.

  * Parameters *
    style      = style/classes string containing grid size classes
-->
<#function saveCurrentContainerSizesFromStyleStr style>
  <#return saveCurrentContainerSizes(parseContainerSizesFromStyleStr(style))>
</#function>

<#-- 
*************
* unsetCurrentContainerSizes
************
Required call at container close following a saveCurrentContainerSizesXxx call at container open.
-->
<#function unsetCurrentContainerSizes>
  <#local dummy = popRequestStack("catoCSFactorsCacheStack")!false>
  <#return popRequestStack("catoContainerSizesStack")!{}>
</#function>

<#-- 
*************
* parseContainerSizesFromStyleStr
************   
ABSTRACT. This function should be overridden by a framework-specific implementation that parses the 
container sizes from a class names string.
           
  * Parameters *
    style     = style/classes string containing grid size classes
    
  * Return Value *
    a map/hash of size names to integer values.
-->
<#function parseContainerSizesFromStyleStr style>
  <#return {}>
</#function>

<#-- 
*************
* getCurrentContainerSizes
************
Gets the last set of size values set by saveCurrentContainerSizes.
-->
<#function getLastContainerSizes>
  <#return readRequestStack("catoContainerSizesStack")!{}>
</#function>

<#-- 
*************
* getAllContainerSizes
************
Gets a list of maps describing all (parent) container sizes nesting the current container
and the current container size (if was added).
-->
<#function getAllContainerSizes>
  <#return getRequestStackAsList("catoContainerSizesStack")![]>
</#function>

<#-- 
*************
* getAbsContainerSizeFactors
************
Returns a map of container size factors.

The return map format is:
  {"large":n1, "medium":n2, "small":n3}
where n1, n2, n3 are floating-point (NOT integer) values calculated from the combination of all
grid sizes of all the current parent containers.
NOTE: This is only an example; this method is framework-agnostic.

These numbers can be used to approximate the current absolute column width in grid sizes,
in the absence of interfering CSS. It will only be useful if the parent containers save their
sizes in the stack using saveCurrentContainerSizesFromStyleStr or saveCurrentContainerSizes, and pop appropriately.
Requires framework-specific logic.

IMPL NOTE: For this method to work, the framework-/theme-specific code must override the abstract
    function evalAbsContainerSizeFactors.
    ALTERNATIVELY, the framework-/theme-specific code may override getAbsContainerSizeFactors
    if more specific parameters are needed.

IMPL NOTE: The calculated factors are saved in their own stack and recalculated only when the main stack changes.
    The cached factors are also passed to evalAbsContainerSizeFactors so it may exploit them, 
    but evalAbsContainerSizeFactors will never be called if the last container's factors were already
    calculated (simple optimization). If need to bypass caching for some reason, theme 
    would have to override getAbsContainerSizeFactors (not recommended).
    
  * Parameters *
    maxSizes     = ((number)|(map)) (default: 0) Hash/map of per-key max container sizes OR a single number giving max size
                   for all keys (usually same in most frameworks) OR zero to mean use defaults
-->
<#function getAbsContainerSizeFactors maxSizes=0>
  <#local factors = readRequestStack("catoCSFactorsCacheStack")!false>
  <#if !factors?is_boolean>
    <#return factors>
  <#else>
    <#-- orig means won't make copy of lists so faster, but lists must be ditched a.s.a.p. 
         don't do this in getAllContainerSizes -->
    <#local sizesList = getRequestStackAsList("catoContainerSizesStack", "orig")![]>
    <#local cachedFactorsList = getRequestStackAsList("catoCSFactorsCacheStack", "orig")![]>
    <#local factors = evalAbsContainerSizeFactors(sizesList, maxSizes, cachedFactorsList)>
    <#local dummy = setLastRequestStack("catoCSFactorsCacheStack", factors)>
    <#return factors>
  </#if>
</#function>

<#-- 
*************
* evalAbsContainerSizeFactors
************   
ABSTRACT. This function should be overridden by a framework-specific implementation that evals container
size factors.
           
  * Parameters *
    sizesList           = list of hashes describing all (known) parent and current container sizes
                          note that entries may be empty and should be skipped.
    maxSizes            = same as getAbsContainerSizeFactors maxSizes
    cachedFactorsList   = list of cached factor results, same size and order as sizesList; each entry
                          corresponds to the container at same index as the sizes in sizesList.
                          if an entry is boolean, then a factor was not (yet) calculated for that container.
    
  * Return Value *
    a map/hash of size names to float values.
-->
<#function evalAbsContainerSizeFactors sizesList maxSizes=0 cachedFactorsList=[]>
  <#return {}>
</#function>

<#-- 
*************************************
* CONTEXT AND SYSTEM UTILITIES *
*************************************
* Context and system utilities.
-->

<#-- 
*************
* getRenderContextType
************
Tries to figure out if this is web render, email, or other general.
-->
<#function getRenderContextType>
  <#-- FIXME?: this detection is primitive... not sure covers all possible cases... -->
  <#local res = "general">
  <#if request??>
    <#local res = "web">
  <#else>
    <#if baseUrl??>
      <#local res = "email">
    </#if>
  </#if>
  <#return res>
</#function>

<#-- 
*************
* getRenderPlatformType
************
html, xml, etc.; best-effort.
-->
<#function getRenderPlatformType>
  <#if screens??>
    <#return StringUtil.wrapString(screens.getScreenStringRenderer().getRendererName()!"")?string>
  </#if>
</#function>

<#-- 
*************
* getDefaultCatoLibLocation
************
Returns a cato library location.

Supported names: "variables", "template"
NOTE: this is currently render context-unaware
Returns void if nothing.

  * Parameters *
    libName             = (variables|template) (required)
    renderPlatformType  = Caller should use getRenderPlatformType()
    renderContextType   = Caller should use getRenderContextType()
-->
<#function getDefaultCatoLibLocation libName renderPlatformType="default" renderContextType="general">
  <#local res = getPropertyValue("catoWebapp", "cato.templating.lib." + renderContextType + "." + renderPlatformType + "."  + libName  + ".location")!"">
  <#if res?has_content>
    <#return res>
  </#if>
  <#if renderContextType != "general">
    <#local res = getPropertyValue("catoWebapp", "cato.templating.lib.general." + renderPlatformType  + "." + libName + ".location")!"">
    <#if res?has_content>
      <#return res>
    </#if>
  </#if>
  <#if renderPlatformType != "default">
    <#local res = getPropertyValue("catoWebapp", "cato.templating.lib." + renderContextType + ".default." + libName + ".location")!"">
    <#if res?has_content>
      <#return res>
    </#if>
  </#if>
  <#local res = getPropertyValue("catoWebapp", "cato.templating.lib.general.default." + libName + ".location")!"">
  <#if res?has_content>
    <#return res>
  </#if>
</#function>

<#-- 
*************
* getMacroLibraryLocationStaticFromResources
************
Gets a lib location from a theme resources variable which contains an expression, which can be either a straight component:// location
meant as "general" context and for "html" and "default" platforms, or a EL-defined map

The foramt is:
  ${['[platform]':'component://...', ...]}
  e.g. ${[''html':'component://...', 'xml':'component://...', 'default':'component://...']}
  
Intended for use with VT_STL_VAR_LOC and VT_STL_TMPLT_LOC and variants.
Checks the resourceNames in the given order.
"default" is a special map key; usually best avoided.
-->
<#function getMacroLibraryLocationStaticFromResources renderPlatformType resources resourceNames...>
  <#local res = Static["org.ofbiz.widget.renderer.VisualThemeWorker"].getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, resourceNames)!"">
  <#if res?has_content>
    <#return StringUtil.wrapString(res)?string>
  </#if>
</#function>

<#-- 
*************************************
* DEV UTILITIES *
*************************************
* For development and debugging purposes.
-->

<#-- 
*************
* printVars
************
Iterates over all variable attributes & functions and prints in table.

Useful for determining current vars in context.

NOTE: since is in utilities.ftl, keep generic and check platform.

  * Usage Example *  
    <@printVars />           
                    
  * Parameters *
    var           = Custom var to be printed (default:context)
    platform      = ((boolean)|html|...) (default: true)
                    true: lookup in current render
    maxDepth      = default 5, to prevent endless recursion
-->
<#macro printVars var=context platform=true maxDepth=5>
  <#if platform?is_boolean><#if platform><#local platform = getRenderPlatformType()!""><#else><#local platform = ""></#if></#if>
  <#if platform == "html">
    <table>
    <#list mapKeys(var) as key>
      <tr>
        <td style="width:200px; vertical-align:top">${key}</td>
        <td>
          <@printVar value=var[key]!"" platform=platform maxDepth=maxDepth currDepth=2/>
        </td>
      </tr>
    </#list>
    </table>
  </#if>
</#macro>

<#macro printVar value="" platform="" maxDepth=-1 currDepth=1>
  <#if platform == "html">
    <#local var = value>
    <#attempt>
      <#-- WARN: ?is_ tests may not work as expected on widget context variables (BeanModel)
          see @objectAsScript -->
      <#if isObjectType("string", var)>
        ${var?string}
      <#elseif var?is_boolean>
        ${var?c}
      <#elseif var?is_date>
        ${var?time}
      <#elseif var?is_number>
        ${var?string}
      <#elseif var?is_enumerable>
        <#if (maxDepth < 0) || (currDepth <= maxDepth)>
          <ol>
          <#list var?sort as i>
              <li><@printVar value=i platform=platform maxDepth=maxDepth currDepth=(currDepth+1)/></li>
          </#list>
          </ol>
        </#if>
      <#elseif isObjectType("map", var)>
        <#if (maxDepth < 0) || (currDepth <= maxDepth)>
          <#-- takes too much space 
          <table>
          <#list mapKeys(var)?sort as key>
            <tr><td>${key}</td><td><@printVar value=var[key]!"" platform=platform maxDepth=maxDepth currDepth=(currDepth+1)/></td></tr>
          </#list>
          </table>-->
          <@objectAsScript lang="json" escape=false object=var maxDepth=maxDepth currDepth=currDepth />
        </#if>
      <#elseif var?is_string>
        ${var?string}
      </#if>
    <#recover>
      <span style="color:red"><strong>${(.error)!"(generic)"}</strong></span>
    </#attempt>
  </#if>
</#macro>

