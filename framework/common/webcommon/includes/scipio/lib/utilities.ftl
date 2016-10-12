<#--
* 
* Utility Functions
*
* A set of standalone utility functions and macros, largely devoid of markup and 
* independent from templating macros and styles and with minimal dependencies, 
* part of standard Scipio Freemarker API.
* Generally CSS-framework-agnostic. 
* Intended as platform-agnostic (html, fo, etc.) though some individually are only applicable for specific platforms.
* Automatically included at all times, for all themes, independently of theme markup override files.
*
* NOTES: 
* * Macros expect to be called using named arguments, except where otherwise noted.
* * Functions in Freemarker only support positional arguments, but some Scipio functions support
*   an "args" argument as a map, which emulates named arguments.
* * Default markup-producing macros are found in >>>standard/htmlTemplate.ftl<<<.
*   In general, utilities found in utilities.ftl should not contain their logic.
* * Except where otherwise required (placeholder/abstract), it is generally not intended for the 
*   declarations in this file to be overridden. Although possible to do from theme markup overrides, it is unsupported.
*  
* IMPLEMENTATION NOTES:
* * Macros should almost never use "request" object directly - use setRequestVar/getRequestVar/other.
* * It's important that these macros remain generic (and that the include for these utilities remains
*   completely static) so that any macro or function here can easily be interchanged with a transform (Java class).
*
* TODO:
* * Turn more of these into transforms.
* * WARN: A number of macros and functions may currently not perform sufficient string escaping
*   to receive values from context! Many have to be reviewed otherwise will not always work properly.
*   This is especially a problem for maps, because toSimpleMap will NOT prevent escaping of the keys inside!
*   Need to re-test all using values set from groovy.
-->

<#assign scipioUtilitiesDefined = true> <#-- this one must use #assign, not #global -->

<#-- scipioNullObject: Special value which may be passed by templates to some functions which
    recognize this object to mean they should substitute it with the value null. -->
<#assign scipioNullObject = Static["com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil"].getNullModelAlways()>

<#-- scipioDummyNullValue: This is a special var used in some functions and macros as an FTL hack to pass
    the value null to function calls. WARN: It must NEVER be assigned a value!
<#assign scipioDummyNullValue = (null)>-->

<#-- 
*************************************
* TEMPLATING API UTILITIES *
*************************************
* Intended for use anywhere in production templates and templating macros.
-->

<#-- 
*************
* render
************
Renders an Ofbiz screen or other resource.

Screens are rendered using Ofbiz's {{{screens.render}}} utility function.

NOTE: 2016-07-29: The default for the {{{restoreValues}}} parameter has been changed to {{{true}}}.
    By default, all variables passed using {{{ctxVars}}}, {{{globalCtxVars}}} and {{{reqAttribs}}} regain
    their previous values when the macro call returns. This helps guard against bugs in templates
    that use multiple @render calls as well as nested screens.

TODO: Reimplement as transform.

  * Parameters *
    resource                = ((string)) The resource identifier, with format depending on type
                              * {{{screen}}}: path and name, or path alone
                                Examples:
                                  "component://common/widget/CommonScreens.xml#listLocales"
                                  "component://common/widget/CommonScreens.xml"
    name                    = ((string)) A resource name part, if not already included in the resource
                              If there is no path for the type or path is optional, then name alone should be specified.
    type                    = (screen|menu|form|tree|section, default: screen) The type of resource to render
                              * {{{screen}}}: an Ofbiz screen (widget) by {{{component://}}} location
                                NOTE: this does not go through {{{include-screen}}} element - use {{{include-screen}}} to force that if needed for some reason
                              * {{{screen-widget}}}: an Ofbiz screen (widget) by {{{component://}}} location - 
                                same as {{{screen}}} but using alternate inclusion method using xml {{{include-screen}}}
                              * {{{menu}}} or {{{include-menu}}} (currently same): an Ofbiz menu (widget) by {{{component://}}} location
                              * {{{form}}} or {{{include-form}}} (currently same): an Ofbiz form (widget) by {{{component://}}} location
                              * {{{tree}}} or {{{include-tree}}} (currently same): an Ofbiz tree (widget) by {{{component://}}} location
                              * {{{section}}}: an Ofbiz screen (widget) decorator section, with {{{name}}} arg
                              NOTE: screen, menu, form and tree (xxx) can be given a {{{include-}}} prefix. The {{{include-}}} version
                                  guarantees that the include will be processed using the XML {{{include-xxx}}} element. 
                                  The non-{{{include-}}} versions may be implemented using other means
                                  and may be more efficient, but sometimes it may be needed to force the include mechanism.
    ctxVars                 = ((map), default: -empty-) A map of screen context vars to be set before the invocation
                              NOTE: Currently, this uses #setContextField. To set null, the key values may be set to a special null-representing
                                  object found in the global {{{scipioNullObject}}} variable.
    globalCtxVars           = ((map), default: -empty-) A map of screen global context vars to be set before the invocation
                              NOTE: Currently, this uses #setGlobalContextField. To set null, the key values may be set to a special null-representing
                                  object found in the global {{{scipioNullObject}}} variable.
    reqAttribs              = ((map), default: -empty-) A map of request attributes to be set before the invocation
                              NOTE: Currently, this uses #setRequestAttribute. To set null, the key values may be set to a special null-representing
                                  object found in the global {{{scipioNullObject}}} variable.
    clearValues             = ((boolean), default: false) If true, the passed request attributes and context vars are removed (or set to null) after invocation
    restoreValues           = ((boolean), default: true) If true, the original values are saved and restored after invocation
                              NOTE: 2016-07-29: The default for this parameter has been changed to {{{true}}}.
    asString                = ((boolean), default: false) If true, the render will render to a string like a regular FTL macro; otherwise goes straight to Ofbiz's writer
                              In stock Ofbiz, which is also current Scipio default behavior (for compabilitity and speed), render calls go directly to writer, 
                              which is faster but cannot be captured using freemarker {{{#assign}}} directive. If you need to capture
                              output of @render, pass true here.
                              NOTE: not supported for {{{type="section"}}} as this time.
                              TODO: implement for section
    maxDepth                = ((int), default: -1) Max menu levels to render [{{{menu}}} type only]
                              See widget-menu.xsd {{{include-menu}}} element for details.
    subMenus                = (none|active|all, default: all) Sub-menu render filter [{{{menu}}} type only]
                              See widget-menu.xsd {{{include-menu}}} element for details.
-->
<#macro render resource="" name="" type="screen" ctxVars=false globalCtxVars=false reqAttribs=false clearValues="" restoreValues="" 
    asString=false maxDepth="" subMenus="">
  <@varSection ctxVars=ctxVars globalCtxVars=globalCtxVars reqAttribs=reqAttribs clearValues=clearValues restoreValues=restoreValues>
    <#-- assuming type=="screen" for now -->
    <#if type == "screen">
      <#if name?has_content>
        ${StringUtil.wrapString(screens.render(resource, name, asString))}<#t>
      <#else>
        ${StringUtil.wrapString(screens.render(resource, asString))}<#t>
      </#if>
    <#elseif type == "section">
        ${StringUtil.wrapString(sections.render(name))}<#t>
    <#else>
      <#-- strip -widget from type, because for the rest it's all the same -->
      <#local type = type?replace("include-", "")>
      <#if !name?has_content>
        <#local parts = resource?split("#")>
        <#local resource = parts[0]>
        <#local name = (parts[1])!>
      </#if>
      <#-- DEV NOTE: WARN: name clashes -->
      <#if type == "menu">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource, "maxDepth":maxDepth, "subMenus":subMenus
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioMenuWidgetWrapper", asString))}<#t>
      <#elseif type == "form">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioFormWidgetWrapper", asString))}<#t>
      <#elseif type == "tree">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioTreeWidgetWrapper", asString))}<#t>
      <#elseif type == "screen">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioScreenWidgetWrapper", asString))}<#t>
      </#if>
    </#if>
  </@varSection>
</#macro>

<#-- 
*************
* ofbizUrl
************
Builds an Ofbiz navigation URL.

STOCK OFBIZ UTILITY. It may be modified with enhanced capabilities for Scipio.

WARN: This utility and all similar link-generating utilities do NOT escape the URLs for
    HTML or javascript! You may need to use #escapeFullUrl in addition if there is
    any chance of the link containing parameters from user input or unsafe stored database data. 
    The Ofbiz automatic context screen escaping is frequently bypassed using #rawString
    for URLs, resulting in some dangerous URLs, particularly in javascript but also HTML.

WARN: {{{fullPath}}} and {{{secure}}} parameters have different behavior than stock Ofbiz!

With Scipio, boolean arguments can be given as booleans, string representation of booleans
or empty string (signifies use defaults).

'''fullPath behavior change:''' In Scipio, when fullPath is specified for a controller request, if the 
request is defined as secure, a secure URL will be created. This method will now never allow an 
insecure URL to built for a controller request marked secure. In stock Ofbiz, this behavior was 
different: fullPath could generate insecure URLs to secure requests. In addition, fullPath will 
by default no longer downgrade HTTPS connections. To allow downgrades, you must explicitly specify 
request it by passing secure false, and this may still produce a secure link if the target
is marked secure. Currently, this applies to all links including inter-webapp links.

'''secure behavior change:''' In Scipio, if current browsing is secure, we NEVER downgrade to HTTPS unless 
explicitly requested by passing secure false, and secure false may still produce a secure link if
needed. Currently (2016-04-06), for security reasons, this 
downgrading request request only applies to the case where the target link is marked as non-secure, such
that in general, setting secure false does not may the link will be insecure in all cases.
In addition, in Scipio, secure flag no longer forces a fullPath link. Specify fullPath true in addition to 
secure to force a fullPath link. Links may still generate full-path secure links when needed even 
if not requested, however.

DEV NOTES:
* webSiteId arg below is from stock but does not fully work and will not work with stock webapps (don't have webSiteIds and can't give them any)

  * Parameters *
    type                    = (intra-webapp|inter-webapp|, default: intra-webapp)
                              * intra-webapp: a relative intra-webapp link (either a controller URI or arbitrary servlet path)
                              * inter-webapp: an inter-webapp link (either a controller URI or an absolute path to any webapp navigation resource)
                                The target webapp MUST exist on the current server as a recognized webapp (with web.xml).
                                It can be identified using either webSiteId or using an absolute full path to the webapp and as the uri.
                              (New in Scipio)
    interWebapp             = ((boolean), default: false) Alias for type="inter-webapp"
                              If true, same as type="inter-webapp"; if false, same as type="" (intra-webapp implied).
                              (New in Scipio)
    uri                     = ((string), required) The request URI. May be specified as parameter or as macro nested content
                              For intra-webapp links and with all macro defaults, this should be a controller URI, or if controller false, a relative servlet path (relative
                              to webapp root, excluding webapp context root).
                              For inter-webapp links, if no webSiteId is specified, this must be an absolute path from
                              server root, containing webapp context root and servlet path; if webSiteId specified, 
                              this should specified relative like intra-webapp (unless absPath forced to true).
                              (New in Scipio)
    absPath                 = ((boolean), default: -depends on type-, fallback default: false)       
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
                              (New in Scipio)
    webSiteId               = ((string), default: -current website or none-) Target web site ID 
                              This usually should only be specified for inter-webapp links.
                              Will determine the specific target webapp to use.
                              NOTE: Some Ofbiz (stock) webapps do not have their own webSiteId, and this is considered normal.
                              (Stock arg, some fixes in Scipio)
    controller              = ((boolean), default: -depends on type-, fallback default: true)
                              If true (stock Ofbiz case), the link is treated as pointing to an Ofbiz controller request URI, and will
                              use information from the controller to generate the link.
                              If false, the link is treated as pointing to any arbitrary servlet or resource.
                              Current behavior when unspecified:
                              * If absPath is true, the uri will be checked to see whether it points to controller
                                * This helps implementation of inter-webapp links.
                              * Otherwise, generally defaults to true.
                              (New in Scipio)
    extLoginKey             = ((boolean), default: false) or string boolean repr
                              If true, this will add the external login key as parameter.
                              NOTE: This is currently FALSE by default in all cases including inter-webapp links
                                  while details are sorted out.
                              (New in Scipio)
    fullPath                = ((boolean), default: false) or string boolean repr
                              If true, forces a full URL with protocol (HTTP or HTTPS).
                              WARN: MODIFIED IN SCIPIO: In Scipio, specifying fullPath true for a controller request
                                  marked as secure will always generate a secure URL, not a plain URL. Some control
                                  is sacrificed to allow this flag to be used safely and more easily.
                              (Stock arg, enhanced in Scipio: supports both boolean and string containing boolean)
    secure                  = ((boolean), default: false) or string boolean repr
                              If true, ensures the resulting URL will be a secure link.
                              WARN: MODIFIED IN SCIPIO: This does not guarantee a full URL will be built, only when needed.
                                  Pass fullPath {{{true}}} to always force a full path. In addition, this parameter now
                                  recognizes the value {{{false}}} to force downgrades to HTTP when the target controller request
                                  is marked as non-secure (or if there there is no target request or unknown - since 2016-07-14).
                              (Stock arg, enhanced in Scipio: supports both boolean and string containing boolean)
    encode                  = ((boolean), default: true) or string boolean repr
                              If true, pass through HttpServletResponse.encodeURL; otherwise, don't.
                              (Stock arg, enhanced in Scipio: supports both boolean and string containing boolean)
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

This is useful to prevent bloating templates with {{{<#assign...><@ofbizUrl.../></#assign>}}} captures
which is very frequent due to use of macros.

  * Parameters *
    args                    = Map of @ofbizUrl arguments OR a string containing a uri (single parameter)
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
    <#local res><@ofbizUrl uri=rawString(args.uri!"") webSiteId=args.webSiteId!"" absPath=args.absPath!"" interWebapp=args.interWebapp!"" controller=args.controller!"" 
        extLoginKey=args.extLoginKey!"" fullPath=args.fullPath!"" secure=args.secure!"" encode=args.encode!"" /></#local>
  <#else>
    <#local res><@ofbizUrl uri=rawString(args) /></#local>
  </#if>
  <#return res>
</#function>

<#-- 
*************
* ofbizWebappUrl
************
Builds an Ofbiz navigation intra-webapp, non-controller URL.

The URI takes the basic form /control/requesturi, 
but this is normally used to access another servlet, such as /products/PH-1000.

This calls @ofbizUrl with absPath=false, interWebapp=false, controller=false by default.

  * Parameters *
    (other)                 = See @ofbizUrl

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
but this is normally used to access another servlet, such as /products/PH-1000.

This calls @ofbizUrl with absPath=false, interWebapp=false, controller=false by default.

  * Parameters *
    (other)                 = See #makeOfbizUrl, @ofbizWebappUrl

  * Related * 
    @ofbizWebappUrl                          
    @ofbizUrl
-->
<#function makeOfbizWebappUrl args>
  <#if isObjectType("map", args)>
    <#local res><@ofbizUrl uri=rawString(args.uri!"") absPath=args.absPath!false interWebapp=false controller=args.controller!false 
        extLoginKey=args.extLoginKey!false fullPath=args.fullPath!"" secure=args.secure!"" encode=args.encode!"" /></#local>
  <#else>
    <#local res><@ofbizUrl uri=rawString(args) absPath=false interWebapp=false controller=false 
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
    (other)                 = See @ofbizUrl

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
    (other)                 = See #makeOfbizUrl, @ofbizInterWebappUrl

  * Related * 
    @ofbizInterWebappUrl
    @ofbizUrl
-->
<#function makeOfbizInterWebappUrl args webSiteId="">
  <#if isObjectType("map", args)>
    <#local res><@ofbizUrl uri=rawString(args.uri!"") absPath=args.absPath!"" interWebapp=true webSiteId=args.webSiteId!  
        controller=args.controller!"" extLoginKey=args.extLoginKey!"" fullPath=args.fullPath!"" secure=args.secure!"" encode=args.encode!"" /></#local>
  <#else>
    <#local res><@ofbizUrl uri=rawString(args) absPath="" interWebapp=true webSiteId=webSiteId
        controller="" extLoginKey="" fullPath="" secure="" encode="" /></#local>
  </#if>
  <#return res>
</#function>

<#-- 
*************
* ofbizContentUrl
************
Builds an Ofbiz content/resource URL.

STOCK OFBIZ UTILITY. It may be modified with enhanced capabilities for Scipio.

TODO: Make this accept uri

  * Parameters *
    variant                 = ((string)) variant
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
    (other)                 = See @ofbizContentUrl

  * Related * 
    @ofbizContentUrl
-->
<#function makeOfbizContentUrl uri variant="">
  <#local res><@ofbizContentUrl variant=variant>${rawString(uri)}</@ofbizContentUrl></#local>
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
* ofbizUrl:// - Any URI that begins with this will be interpreted as an ofbiz controller URL and ran through @ofbizUrl/makeOfbizUrl.
  Form: 
    ofbizUrl://myRequest;fullPath=false;secure=false;encode=true?param1=val1
  NOTE: Order of arguments is strict; args will be stripped.
                 
  * Parameters *
    uri                     = uri to interpret for known formats and, if matching, to produce URL
-->
<#function interpretRequestUri uri>
  <#local uri = rawString(uri)>
  <#if uri?starts_with("ofbizUrl://")>
    <#local uriDesc = Static["org.ofbiz.webapp.control.RequestDescriptor"].fromUriStringRepr(request!, response!, uri)>
    <#if uriDesc.getType() == "ofbizUrl">
      <#-- NOTE: although there is uriDesc.getWebUrlString(), should pass through FTL macro version instead, hence all this manual work... -->
      <#local res><@ofbizUrl fullPath=(uriDesc.getFullPath()!"") secure=(uriDesc.getSecure()!"") encode=(uriDesc.getEncode()!"")>${rawString(uriDesc.getBaseUriString())}</@ofbizUrl></#local>
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
    url                     = URL to augment
    escape                  = ((boolean), default: true) If true, use escaped param delimiter
-->
<#function addExtLoginKey url escape=true>
  <#return rawString(Static["org.ofbiz.webapp.control.RequestLinkUtil"].checkAddExternalLoginKey(rawString(url), request, escape))>
</#function>

<#-- 
*************
* setRequestAttribute
************
Sets a request attribute. 

STOCK OFBIZ UTILITY. It may be modified with enhanced capabilities for Scipio.

This function is enhanced to support more value types and the special value scipioNullObject
to indicate the value null.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function setRequestAttribute name value>
</#function>
-->

<#-- 
*************
* setContextField
************
Sets a field value in context

STOCK OFBIZ UTILITY. It may be modified with enhanced capabilities for Scipio.

This function is enhanced to support more value types and the special value scipioNullObject
to indicate the value null.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function setContextField name value>
</#function>
-->

<#-- 
*************
* setGlobalContextField
************
Sets a field value in global context.

New in Scipio.

This function is enhanced to support more value types and the special value scipioNullObject
to indicate the value null.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function setGlobalContextField name value>
</#function>
-->

<#-- 
*************
* setVars
************
Sets context variables and request attributes.

This function is enhanced to support more value types and the special value scipioNullObject
to indicate the value null.

  * Parameters *
    varMaps                 = ((map)) Map of maps of vars to set
                              * {{{ctxVars}}}: A map of screen context vars to be set before the invocation
                                NOTE: Currently, this uses #setContextField. To set null, the key values may be set to a special null-representing
                                    object found in the global {{{scipioNullObject}}} variable.
                              * {{{globalCtxVars}}}: A map of screen global context vars to be set before the invocation
                                NOTE: Currently, this uses #setGlobalContextField. To set null, the key values may be set to a special null-representing
                                    object found in the global {{{scipioNullObject}}} variable.
                              * {{{reqAttribs}}}: A map of request attributes to be set before the invocation
                                NOTE: Currently, this uses #setRequestAttribute. To set null, the key values may be set to a special null-representing
                                    object found in the global {{{scipioNullObject}}} variable.
-->
<#function setVars varMaps={}>
  <#if !(varMaps.ctxVars!false)?is_boolean>
    <#list mapKeys(varMaps.ctxVars) as name>
      <#local dummy = setContextField(name, varMaps.ctxVars[name])>
    </#list>
  </#if>
  <#if !(varMaps.globalCtxVars!false)?is_boolean>
    <#list mapKeys(varMaps.globalCtxVars) as name>
      <#local dummy = setGlobalContextField(name, varMaps.globalCtxVars[name])>
    </#list>
  </#if>
  <#if !(varMaps.reqAttribs!false)?is_boolean>
    <#list mapKeys(varMaps.reqAttribs) as name>
      <#local dummy = setRequestAttribute(name, varMaps.reqAttribs[name])>
    </#list>
  </#if>
  <#return "">
</#function>

<#-- 
*************
* clearVars
************
Sets context variables and request attributes.

This function is enhanced to support more value types and the special value scipioNullObject
to indicate the value null.

  * Parameters *
    varLists                = ((map)) A map of lists, or map of maps (keys used), of var names to clear
                              * {{{ctxVars}}}: A list (or map - keys used) of screen context vars names to clear
                              * {{{globalCtxVars}}}: A list (or map - keys used) of screen global context vars names to clear
                              * {{{reqAttribs}}}: A list (or map - keys used) of request attributes names to clear
-->
<#function clearVars varLists={}>
  <#list mapsKeysOrListOrBool(varLists.ctxVars!) as name>
    <#local dummy = setContextField(name, scipioNullObject)>
  </#list>
  <#list mapsKeysOrListOrBool(varLists.globalCtxVars!) as name>
    <#local dummy = setGlobalContextField(name, scipioNullObject)>
  </#list>
  <#list mapsKeysOrListOrBool(varLists.reqAttribs!) as name>
    <#local dummy = setRequestAttribute(name, scipioNullObject)>
  </#list>
  <#return "">
</#function>

<#function mapsKeysOrListOrBool object>
  <#if object?is_sequence>
    <#return object>
  <#elseif object?is_boolean>
    <#return []>
  <#else>
    <#return mapKeys(object)>
  </#if>
</#function>

<#-- 
*************
* extractVars
************
Gets all the named attributes and context vars.

This function is enhanced to support more value types and the special value scipioNullObject
to indicate the value null.

TODO: This is currently extremely inefficient; should implement as transform.

  * Parameters *
    varLists                = ((map)) A map of lists, or map of maps (keys used), of var names to extract values
                              * {{{ctxVars}}}: A list (or map - keys used) of screen context vars names to extract
                              * {{{globalCtxVars}}}: A list (or map - keys used) of screen global context vars names to extract
                              * {{{reqAttribs}}}: A list (or map - keys used) of request attributes names to extract
    saveNulls               = ((boolean), default: false) If true, null/missing values will get map entries with {{{ScipioNullObject}}}; otherwise, omitted from results
    
  * Return Value *
    A map of maps with same keys as parameters. 
-->
<#function extractVars varLists={} saveNulls=false>
  <#local ctxVarsMap = {}>
  <#local globalCtxVarsMap = {}>
  <#local reqAttribsMap = {}>
  <#list mapsKeysOrListOrBool(varLists.ctxVars!) as name>
    <#if context[name]??>
      <#local ctxVarsMap = ctxVarsMap + {name: context[name]}>
    <#elseif saveNulls>
      <#local ctxVarsMap = ctxVarsMap + {name: scipioNullObject}>
    </#if>
  </#list>
  <#list mapsKeysOrListOrBool(varLists.globalCtxVars!) as name>
    <#if globalContext[name]??>
      <#local globalCtxVarsMap = globalCtxVarsMap + {name: globalContext[name]}>
    <#elseif saveNulls>
      <#local globalCtxVarsMap = globalCtxVarsMap + {name: scipioNullObject}>
    </#if>
  </#list>
  <#list mapsKeysOrListOrBool(varLists.reqAttribs!) as name>
    <#if request.getAttribute(name)??>
      <#local reqAttribsMap = reqAttribsMap + {name: request.getAttribute(name)}>
    <#elseif saveNulls>
      <#local reqAttribsMap = reqAttribsMap + {name: scipioNullObject}>
    </#if>
  </#list>
  <#return {"ctxVars":ctxVarsMap, "globalCtxVars": globalCtxVarsMap, "reqAttribs":reqAttribsMap}>
</#function>

<#-- 
*************
* varSection
************
Sets context variables and request attributes around a nested block. Can also be used alone.

This function is enhanced to support more value types and the special value scipioNullObject
to indicate the value null.

  * Parameters *
    ctxVars                 = ((map), default: -empty-) A map of screen context vars to be set before the invocation
                              NOTE: Currently, this uses #setContextField. To set null, the key values may be set to a special null-representing
                                  object found in the global {{{scipioNullObject}}} variable.
    globalCtxVars           = ((map), default: -empty-) A map of screen global context vars to be set before the invocation
                              NOTE: Currently, this uses #setGlobalContextField. To set null, the key values may be set to a special null-representing
                                  object found in the global {{{scipioNullObject}}} variable.
    reqAttribs              = ((map), default: -empty-) A map of request attributes to be set before the invocation
                              NOTE: Currently, this uses #setRequestAttribute. To set null, the key values may be set to a special null-representing
                                  object found in the global {{{scipioNullObject}}} variable.
    clearValues             = ((boolean), default: false) If true, the passed request attributes and context vars are removed (or set to null) after invocation
    restoreValues           = ((boolean), default: true) If true, the original values are saved and restored after invocation
                              NOTE: 2016-07-29: The default for this parameter has been changed to {{{true}}}.
-->
<#macro varSection ctxVars=false globalCtxVars=false reqAttribs=false clearValues="" restoreValues="">
  <#if !clearValues?is_boolean>
    <#-- DEV NOTE: if clearValues was true as default, you'd have to check for explicit
        restoreValues (!restoreValues?is_boolean) before setting the true default here -->
    <#local clearValues = false>
  </#if>
  <#if !restoreValues?is_boolean>
    <#local restoreValues = true>
  </#if>
  <#local varMaps = {"ctxVars":ctxVars, "globalCtxVars":globalCtxVars, "reqAttribs":reqAttribs}>
  <#if restoreValues && !clearValues>
    <#local origValues = extractVars(varMaps, true)>
  </#if>
  <#local dummy = setVars(varMaps)>
  <#nested>
  <#if clearValues>
    <#local dummy = clearVars(varMaps)>
  <#elseif restoreValues>
    <#local dummy = setVars(origValues)>
  </#if>
</#macro>

<#-- 
*************
* getLabel
************
Returns empty string if no label is found

  * Parameters *
    name                    = (required) Label name
    resource                = (optional) Resource name
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

NOTE: The result from this method is '''not''' HTML-encoded, as such values are normally code and not text messages.

  * Parameters *
    resource                = (required) Resource name
    name                    = (required) Property name
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function getPropertyValue resource name>
  <#local value = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue(resource, name)>
  <#if value?has_content>
    <#return rawString(value)>
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

NOTE: The resulting message is subject to automatic HTML encoding (by Ofbiz). 
    Use #rawString on the result to prevent escaping.
    
DEV NOTE: If this is ever implemented as transform, be careful to make sure the result is HTML-escaped
    using the same method Ofbiz does! The escaping is now part of this method's interface.

TODO: implement as transform.

  * Parameters *
    resource                = (required) Resource name
    name                    = (required) Property name
    msgArgs                 = ((map)|(list), default: -use context-) Substitute values for message template
    specLocale              = ((locale), default: -locale from context-) Explicit locale
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
    <#return Static["org.ofbiz.base.util.UtilProperties"].getMessage(resource, name, msgArgs, specLocale)>
  <#elseif msgArgs?is_hash>
    <#return Static["org.ofbiz.base.util.UtilProperties"].getMessage(resource, name, msgArgs, specLocale)>
  <#else>
    <#-- WARN: context variable _could_ be missing! -->
    <#return Static["org.ofbiz.base.util.UtilProperties"].getMessage(resource, name, context!{}, specLocale)>
  </#if>
</#function>

<#-- 
*************
* getPropertyMsgFromLocExpr
************
Gets property or empty string if missing (same behavior as UtilProperties).

TODO: implement as transform.

  * Parameters *
    resourceExpr            = (required) Resource name and property name separated with "#", or name alone
                              If name alone, assumes CommonUiLabels for resource.
    msgArgs                 = ((map)|(list), default: -use context-) Substitute values for message template
    specLocale              = ((locale), default: -locale from context-) Explicit locale
    
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
    textExpr                = (required) Label text expression 
    msgArgs                 = ((map)|(list), default: -use context-) Substitute values for message template
    specLocale              = ((locale), default: -locale from context-) Explicit locale
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
    url                     = (required) URL to which to append delimiter
    paramDelim              = (default: "&amp;") Param delimiter
    paramStarter            = (default: "?") Query string delimiter, usually "?"
                              Only significant if paramDelim does not contain "/"
-->
<#function addParamDelimToUrl url paramDelim="&amp;" paramStarter="?">
  <#local url = rawString(url)>
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
    paramStr                = (required) Param string
    paramMap                = ((map), required) Map of keys to values to add
    paramDelim              = (default: "&amp;") Param delimiter
    includeEmpty            = ((boolean), default: true) If true, include empty values; if false, omit empty values
-->
<#function addParamsToStr paramStr paramMap paramDelim="&amp;" includeEmpty=true>
  <#local res = paramStr>
  <#list mapKeys(paramMap) as key>
    <#if res?has_content && (!res?ends_with(paramDelim))>
      <#local res = res + paramDelim>
    </#if>
    <#if includeEmpty || paramMap[key]?has_content>
      <#local res = res + key + "=" + rawString(paramMap[key]!"")>
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
    paramStr                = (required) Param string
    paramDelim              = (default: "&amp;") Param delimiter
-->
<#function splitStrParams paramStr paramDelim="&amp;">
  <#return rawString(Static["com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil"].splitStrParams(paramStr, paramDelim))>
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
    paramStr                = (required) Param string
    paramDelim              = (default: "&amp;") Param delimiter
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
    styleString             = (required) Style string
    
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
    styleString             = (required) Style string containing classes
    
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

  * Usage Examples *   
    <#assign myVar = joinStyleNames("class1", "", " class3")>
       
  * Parameters *
    styleNames              = (required) Style names, as arbitrary number of positional parameters
    
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

  * Usage Examples *   
    <#assign myVar = joinStyleNames(["class1", "", " class3"])>
       
  * Parameters *
    styleNames              = (required) Style names, as sequence
    
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

NOTE: now recognizes special syntax scipio class args.
         
  * Parameters *
    styleString             = (required) Style string
    classNamePrefix         = (required) Prefix to search for
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

NOTE: now recognizes special syntax scipio class args.
                    
  * Parameters *
    styleString             = (required) Style string
    className               = (required) Name of class to find
    
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
    styleString             = (required) Style string
    classNamePrefix         = (required) Prefix to search for
    
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

NOTE: now recognizes special syntax scipio class args.
           
  * Parameters *
    styleString             = (required) Style string
    namesToRemove           = ((list)|(string), required) Name or mames to remove
                              Sequence of names OR space-separated string of names OR single name

  * Return Value *
    the style string with names removed, reformatted, in same order as input
-->
<#function removeStyleNames styleString namesToRemove>
  <#local prefix = getClassArgPrefix(styleString)>
  <#local styleString = getPlainClassArgNames(styleString)>
  <#if namesToRemove?is_string> <#-- NOTE: this is only ok as long as we don't accept hashes here, else use isObjectType -->
    <#local namesToRemove = splitStyleNamesToSet(namesToRemove)>
  <#else>
    <#local namesToRemove = Static["org.ofbiz.base.util.UtilMisc"].collectionToSet(namesToRemove)>
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
    url                     = (required) URL to strip
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
    url                     = (required) URL to augment
    paramMap                = ((map), required) Map of keys to values to add
    paramDelim              = (default: "&amp;") Param delimiter
    includeEmpty            = ((boolean), default: true) Include empty values, or if false omit empty values
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
    url                     = (required) URL to escape
    paramDelim              = (default: "&amp;") Param delimiter for substitution
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
* urlContainsPathPart
************
Checks if the given URL contains the given path part, using proper delimiter checking.

WARN: The url and pathPart must not be escaped; use #rawString.

  * Parameters *
    url                     = (required) URL to check
    pathPart                = (required) Path part
                              e.g., {{{/PH-1000}}}
-->
<#function urlContainsPathPart url pathPart>
  <#if !url?has_content || !pathPart?has_content>
    <#return false>
  <#elseif pathPart == "/">
    <#return true>
  </#if>
  <#if pathPart?starts_with("/")>
    <#local pathPart = pathPart[1..]>
  </#if>
  <#if pathPart?ends_with("/")>
    <#local pathPart = pathPart[0..<(pathPart?length - 1)]>
  </#if>
  <#return url?matches("^(.*/)?(" + pathPart + ")([?/#&;].*)?$")>
</#function>

<#-- 
*************
* urlStartsWithPathPart
************
Checks if the given URL path starts with the given path, using proper delimiter checking.

WARN: The url and pathPart must not be escaped; use #rawString.

TODO: Implement (careful about absolute vs relative)

  * Parameters *
    url                     = (required) URL to check
    pathPart                = (required) Path part
-->
<#-- NOT IMPLEMENTED
<#function urlStartsWithPath url pathPart>
</#function>
-->

<#-- 
*************
* urlEndsWithPathPart
************
Checks if the given URL path ends with the given path, using proper delimiter checking.

WARN: The url and pathPart must not be escaped; use #rawString.

TODO: Implement (careful about absolute vs relative)

  * Parameters *
    url                     = (required) URL to check
    pathPart                = (required) Path part
-->
<#-- NOT IMPLEMENTED
<#function urlEndsWithPathPart url pathPart>
</#function>
-->

<#-- 
*************
* camelCaseToDashLowerName
************
Converts camelCase to camel-case.
-->
<#function camelCaseToDashLowerName name>
  <#return rawString(Static["com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil"].camelCaseToDashLowerName(name))>
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
    type                    = (string|map|simplemap|complexmap, required)
                              * string: Anything meant to be a string WITHOUT being a more complex type.
                              * map: Simple hash, or context map that exposes methods as keys (BeanModel with underlying Map) 
                                (simplemap or complexmap)
                              * simplemap: Simple hash only (?keys to get elems).
                              * complexmap: Context map that exposes methods as keys (BeanModel) only (.keySet() to get elems).
    object                  = ((object), required) The object to test
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

WARN: Behavior w.r.t. auto-escaping is currently inconsistent and poorly-defined.

FIXME: Auto-escaping issues

  * Parameters *
    object                  = ((object)) The object to copy
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

WARN: Behavior w.r.t. auto-escaping is currently inconsistent and poorly-defined.

FIXME: Auto-escaping issues

NOTES: 
* copyObject will also work fine on maps, but this provides more map-specific options.
* This will only copy maps that don't have ?keys support if mode is include ("i") and inExKeys specified.

  * Parameters *
    map                     = ((map), required)  The source map
    mode                    = ("e"|"i"|, default: -none-) Optional mode flags
                              * "e": exclude listed keys
                              * "i": include only listed keys
    inExKeys                = (optional) List or wrapped set of keys to include or exclude    
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function copyMap map mode inExKeys="">
</#function>
-->

<#-- 
*************
* toRawString
************
Returns the given string, free of Ofbiz auto HTML encoding, as a simple Freemarker string.

This is the same as the Ofbiz-provided function, {{{StringUtil.wrapString}}}, but further simplifies
the resulting type into a simple Freemarker string.

  * Parameters *
    str                     = ((string), required) The string to return raw.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function toRawString str>
  <#- ?string turns it into a basic FTL string ->
  <#return StringUtil.wrapString(str)?string> 
</#function>
-->

<#-- 
*************
* rawString
************
Returns the given string, free of Ofbiz auto HTML encoding, as a simple Freemarker string.
Alias for #toRawString (common operation).

This is the same as the Ofbiz-provided function, {{{StringUtil.wrapString}}}, but further simplifies
the resulting type into a simple Freemarker string.

NOTE: 2016-09-29: This will now also tolerate non-strings, which will be coerced to strings using ?string operator.

  * Parameters *
    str                     = ((string), required) The string to return raw.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function rawString str>
  <#- ?string turns it into a basic FTL string ->
  <#return StringUtil.wrapString(str)?string> 
</#function>
-->

<#-- 
*************
* toStringIfNot
************
Returns the given value as a string but only if not already a string

This intentionally skips calling ?string on existing strings to prevent auto-escaping,
but still result will be a string.

WARN: this works using FTL's primitive ?is_string test, which may return TRUE for complex 
    objects that aren't really strings.

  * Parameters *
    value                   = (required) The value to return as string
-->
<#function toStringIfNot value>
  <#if value?is_string>
    <#return value>
  <#else>
    <#return value?string>
  </#if>
</#function>

<#-- 
*************
* htmlContentString
************
Returns the given string, free of Ofbiz auto HTML encoding, as a simple Freemarker string, and 
depending on current implementation and system policy may process the string for allowed HTML.

Typically for database-stored content such as product descriptions which use limited HTML.

WARN: This is NOT fully implemented and currently does the same as #rawString.

  * Parameters *
    str                     = ((string), required) The string
    
  * Related *
    #rawString
-->
<#function htmlContentString str>
  <#return rawString(str)> 
</#function>

<#-- 
*************
* rewrapObject
************
Takes a object and, if applicable, rewraps it with a different Freemarker wrapper.
Used to convert complex BeansWrapper objects to simple maps and auto-HTML-escaping wrappers to non-escaping
wrappers, either shallow or deep.

This tries to avoid rewrapping unless forced by the mode or the input map.

WARN: Currently this only works for maps!

TODO: Lists, strings, etc.

  * Parameters *
    object                  = ((object), required) The source object
    mode                    = (simple|simple-raw-deep|simple-force|simple-raw-deep-force|simple-copy|simple-raw-deep-copy|simple-force-copy|simple-raw-deep-force-copy, default: simple) Rewrapping mode and target wrapper type
                              The keywords mean the following:
                              * {{{simple}}}: convert "complex" BeansWrapper maps to simple adapter maps (that have no extra unwanted keys),
                                but only if they are not already simple and the other options do not force it.
                              * {{{raw}}}: the target wrapper should be free of HTML auto-escaping
                                WARN: Currently, this forces a performance-intensive unwrap operation in many cases,
                                    because Freemarker does not allow checking which wrapper an object is currently using.
                              * {{{deep}}}: the wrapper select should apply to any children the object may have
                                WARN: Currently, this forces a performance-intensive deep unwrap operation in many cases,
                                    because Freemarker does not allow checking which wrapper an object is currently using.
                              * {{{force}}}: advanced option: this will force re-wrapping even if the target already appears adequate.
                              * {{{copy}}}: requests a copy operation where possible. The copy operation will be slow, but
                                it may make the resulting type faster.
                              NOTE: Only the listed combinations are supported.
                              WARN: {{{simple-raw-deep}}} uses a heuristic to avoid rewrapping simple FTL hashes. It is NOT
                                  accurate. If problems arise, use {{{simple-raw-deep-force}}} instead.
                              TODO?: {{{simple-deep}}} may be desirable but currently not supported.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function rewrapObject object mode="simple">
</#function>
-->

<#-- 
*************
* rewrapMap
************
Takes a map and, if applicable, rewraps it with a different wrapper.
Used to convert complex BeansWrapper objets to simple maps and auto-HTML-escaping wrappers to non-escaping
wrappers, either shallow or deep.

Alias for #rewrapObject.

  * Related *
    #rewrapMap
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function rewrapMap object mode="simple">
</#function>
-->

<#-- 
*************
* toSimpleMap
************
Takes a bean-wrapped map and switches it to a simple map adapter instead, without performing
any copies. Alias for #rewrapMap(object, "simple").

If the object is not a complex map but already another type of map, returns it as-is. Other types throw errors.

NOTE: This only changes the complexity of the map; it does NOT prevent auto-escaping. In fact, if
    called on certain types of unescaped complex maps, this function may cause auto-escaping to return, which
    is why its behavior is to leave maps alone unless they are complex bean maps.
    Calling ?keys on this map may give escaped keys; use #mapKeys or #rewrapMap with args (object, "simple-raw").

  * Parameters *
    object                  = ((map), required) The source map
    
  * Related *
    #rewrapMap
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

WARN: The resulting keys are unescaped, not passed through auto-escaping, so that 
    the map values can be keyed back without having to call #rawString.

  * Parameters *
    object                  = ((map), required) The source map
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
    
IMPL NOTE: It's part of this function's interface that any of the arguments for which ?has_content is false,
    will not be used. This covers the cases where non-map types are passed as well ("", [], etc.).
    Sometimes an empty non-map type will be passed, should be considered valid.
    Shouldn't need to check for non-empty non-map types however, as those are all clear coding errors.
    e.g., This is used at beginning of macros that take inlineArgs... or inlineAttribs...
    They count on this method to handle empty sequence case.
    They also depend on this for the toSimple=true conversion.

  * Parameters *
    first                   = ((map), required) The first map
    second                  = ((map), required) The second map
    toSimple                = ((boolean), default: true) If true, ensure result is a simple hash
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
          <#return Static["com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil"].concatMaps(first, second)>
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
    object                  = ((string)|(map), required) The object to convert to a simple map
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
    object                  = ((collection)) (optional) The collection
                              If omitted, creates an empty Set.
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
   progressSuccessAction    = (required) Progress success action
-->
<#function compileProgressSuccessAction progressSuccessAction>
  <#return rawString(Static["com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil"].compileProgressSuccessAction(progressSuccessAction))>
</#function>

<#-- 
*************
* getCurrentSectionLevel
************
Gets current global section level. 

  * Parameters *
    useDefault              = ((boolean), default: true) If true, if no heading defined, returns default; else return void
    
  * Related *
    @section
    #setCurrentSectionLevel
-->
<#function getCurrentSectionLevel useDefault=true>
  <#local sLevel = getRequestVar("scipioCurrentSectionLevel")!"">
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
    sLevel                  = ((int), required) The section level
   
  * Related *
    @section
    #getCurrentSectionLevel
-->
<#function setCurrentSectionLevel sLevel>
  <#-- set as request attrib so survives template environments and screens.render -->
  <#local dummy = setRequestVar("scipioCurrentSectionLevel", sLevel)>
  <#global scipioCurrentSectionLevel = sLevel>
  <#return "">
</#function>

<#-- 
*************
* getCurrentHeadingLevel
************
Gets current global heading (title) level. 

  * Parameters *
    useDefault              = ((boolean), default: true) If true, if no heading defined, return default; else return void

  * Related *
    @heading
    #getDefaultHeadingLevel
-->
<#function getCurrentHeadingLevel useDefault=true>
  <#local hLevel = getRequestVar("scipioCurrentHeadingLevel")!"">
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
    hLevel                  = ((int), required) Heading level
    
  * Related *
    @heading
    #getCurrentHeadingLevel
-->
<#function setCurrentHeadingLevel hLevel>
  <#-- set as request attrib so survives template environments and screens.render -->
  <#local dummy = setRequestVar("scipioCurrentHeadingLevel", hLevel)>
  <#global scipioCurrentHeadingLevel = hLevel>
  <#return "">
</#function>


<#-- 
*************
* getRequestNextElemIndex
************
Returns and increases a global request count for a certain name.

The value stored is the last one returned by the function. By default, starts at 1.

  * Parameters *
    name                    = ((string), required) The global request var name
    start                   = ((int), default: 1) The initial value
-->
<#function getRequestNextElemIndex name start=1>
  <#-- set as request attrib so survives template environments and screens.render -->
  <#local index = getRequestVar(name)!(start - 1)>
  <#local index = index + 1>
  <#local dummy = setRequestVar(name, index)>
  <#return index>
</#function>

<#-- 
*************
* objectAsScript
************
Output a Freemarker variable as a value in Javascript, JSON or similar script language.

Automatically tries to detect types and wrap in appropriate syntax.
To manually prevent the interpretation of a value inside a structure such as a map,
either use the {{{rawVal}}} parameter or wrap the nested value itself using
#wrapRawScript.

DEV NOTE: This is complicated in Ofbiz because Maps and objects from
    widget/java/groovy context don't behave same as FTL types.
    Currently can't use ?keys on Maps and generally for most types gotten from
    groovy context, the ?is_ don't work like you'd expect (a lot of vars
    implement ?string and ?is_hash_ex that aren't really either of these, so leave for last)

TODO: doesn't handle dates (ambiguous?)
                    
  * Parameters *
    object                  = ((object), required) The FTL or context object
    lang                    = (js|json, required)
    wrap                    = ((boolean), default: true) If true, wrap in {}, [], or "" as needed; otherwise omit enclosing characters
    hasMore                 = ((boolean), default: false) If true, always include trailing separator in hashes and arrays
    escape                  = ((boolean), default: true) Escape characters in strings
    maxDepth                = ((int), default: -1) Maximum depth, or -1 for no limit
    rawVal                  = ((boolean)|(map)|(list), default: false) If true, treat the object as a pure JSON/other script string.
                              This can be a map or list of boolean to parallel the object, for recursion.
                              NOTE: is cumbersome for lists; mostly useful for maps.
                              NOTE: this is kept separate from the object for security reasons.
                              
  * Related *
    #wrapRawScript
-->
<#macro objectAsScript object lang wrap=true hasMore=false escape=true maxDepth=-1 currDepth=1 rawVal=false>
  <#if (rawVal?is_boolean && rawVal == true)><#-- NOTE: there's a duplicate of this further down for performance reasons -->
    <#if isObjectType("string", object)>
      ${rawString(object)}<#t>
    <#else>
      ${object?string}<#t>
    </#if>
  <#elseif isObjectType("string", object)>
    <#-- WARN: context strings also implement ?is_hash when bean models; ?is_string not good enough -->
    <#if wrap>"${escapeScriptString(lang, object, escape)}"<#else>${escapeScriptString(lang, object, escape)}</#if><#t>
  <#elseif object?is_number> 
    ${object}<#t>
  <#elseif object?is_boolean>
    ${object?c}<#t>
  <#elseif object?is_date_like>
    <#-- TODO? -->
    <#if wrap>"${escapeScriptString(lang, object, escape)}"<#else>${escapeScriptString(lang, object, escape)}</#if><#t>
  <#elseif object?is_enumerable> 
    <#-- check this before string checks and hash because some of these from groovy 
        also implement ?string and ?is_hash_ex at same time (?).
        but usually for those if ?is_enumerable it means it was a list-like type. -->
    <#if (maxDepth < 0) || (currDepth <= maxDepth)>
      <#if wrap>[</#if><#lt>
      <#list object as item> 
          <#if item??><#rt/>
          <#t/><#if !rawVal?is_boolean><#local rawValNext = rawVal[item_index]!false><#else><#local rawValNext = false></#if>
          <#lt/><@objectAsScript lang=lang object=item wrap=true escape=escape maxDepth=maxDepth currDepth=(currDepth+1) rawVal=rawValNext/><#else>null</#if><#if item_has_next || hasMore>,</#if>
      </#list> 
      <#if wrap>]</#if><#rt>
    <#else>[]</#if>
  <#elseif object?is_hash_ex && isObjectType("map", object)>
    <#if (maxDepth < 0) || (currDepth <= maxDepth)>
      <#if wrap>{</#if><#lt>
      <#list mapKeys(object) as key>
          <#-- NOTE: must use rawString on the keys because FTL will coerce them to strings (forcing auto-escaping from Ofbiz context) before using them as hash keys! -->
          "${escapeScriptString(lang, key, escape)}" : <#if object[rawString(key)]??><#rt/>
            <#t/><#if !rawVal?is_boolean><#local rawValNext = rawVal[rawString(key)]!false><#else><#local rawValNext = false></#if>
            <#t/><@objectAsScript lang=lang object=object[rawString(key)] wrap=true escape=escape maxDepth=maxDepth currDepth=(currDepth+1) rawVal=rawValNext/>
            <#lt/><#else>null</#if><#if key_has_next || hasMore>,</#if>
      </#list>
      <#if wrap>}</#if><#rt>
    <#else>{}</#if>
  <#elseif isRawScript(object)><#-- NOTE: this is done at the end for performance reasons only (it really belongs beside rawVal test) -->
      ${object?string}<#t>
  <#elseif object?is_string> 
    <#-- WARN: this may catch a lot of different context object types, but ones we care about are above -->
    <#if wrap>"${escapeScriptString(lang, object, escape)}"<#else>${escapeScriptString(lang, object, escape)}</#if><#t>
  <#-- some of the following are invalid/inconvertible types, but catch them because otherwise debugging impossible -->
  <#elseif objectAsScriptTreatInvalidNonFatal && object?is_hash && !object?is_string> 
    "__NONEXHASH__"<#t>
  <#elseif objectAsScriptTreatInvalidNonFatal && object?is_method>
    "__METHOD__"<#t>
  <#elseif objectAsScriptTreatInvalidNonFatal && object?is_directive>
    "__DIRECTIVE__"<#t>
  <#else>
    <#-- fallback, best-effort. -->
    <#if wrap>"${escapeScriptString(lang, object, escape)}"<#else>${escapeScriptString(lang, object, escape)}</#if><#t>
  </#if> 
</#macro>

<#-- escapes a string to be placed within "" literals -->
<#function escapeScriptString lang val escape=true>
  <#-- 2016-03-21: prevent auto-escaping the screen widget context string variables -->
  <#if isObjectType("string", val)>
    <#local val = rawString(val)>
  <#else>
    <#local val = val?string>
  </#if>
  <#if escape>
    <#switch lang>
      <#case "json">
        <#return val?json_string>
        <#break>
      <#case "js">
        <#-- NOTE: We un-escape the single quotes because we know our containing quotes are double-quotes.
            We can only assume this here. -->
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
*************
* wrapAsRaw
************
Wraps a string in a special string wrapper that when passed to markup- or script-handling macros gets included as
a raw string bypassing html, js or other language escaping. 
This include @objectAsScript and macros that escape values using #escapePart or #escapeFull.

WARN: This will only be safe if an explicit language(s) is/are passed!

NOTE: This has no functional relationship to Ofbiz's StringWrapper ({{{StringUtil.wrapString}}} or #rawString);
    its scope is unrelated to Ofbiz's screen auto-escaping.

  * Parameters *
    object                  = ((string)|(map)) the string to wrap OR map of languages to strings
                              If this is a string, {{{lang}}} parameter should always be specified.
                              If this is a map, it is a map of languages to strings, for example:
                                {"html":"<em>my title</em>", "raw":"my title"}
                              The supported map languages are the same as the single {{{lang}}} parameter, except 
                              that "script" has no meaning in this case and should not be used, and in most cases,
                              at least "raw" should be specified.
                              The map version allows templates to specify alternate markup for different languages
    lang                    = (html|js|json|script|...|, default: -empty/unspecific-) the specific language toward which this should be considered "raw"
                              This accepts dash-separated string of names.
                              Special values:
                              * {{{script}}}: for use with @objectAsScript: prevents both escaping and enclosing string literals
                              NOTE: If {{{object}}} is a string (not map), this argument is usually '''required''' for safety and correctness.
                              WARN: Leaving empty with string object will prevent macro escaping for any language! In virtually all cases you should specify
                                  a specific language. The unspecific mode is for rare workarounds only.
-->
<#function wrapAsRaw object lang="">
  <#if isObjectType("map", object)>
    <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].wrap(object)>
  <#else>
    <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].wrap(object?string, lang)>
  </#if>
</#function>

<#-- 
*************
* isWrappedAsRaw
************
Checks if the object was wrapped with #wrapAsRaw.

NOTE: This has no functional relationship to Ofbiz's StringWrapper ({{{StringUtil.wrapString}}} or #rawString)
    and will not detect any such wrappers.

  * Parameters *
    object                  = the object to check
    lang                    = (html|js|json|script|...|, default: -empty/unspecific-) the specific language toward which this should be considered "raw"
    
  * Related *
    #wrapAsRaw
-->
<#function isWrappedAsRaw object lang="">
  <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].isRawScript(object, lang)>
</#function>

<#-- 
*************
* getRawWrappedForLang
************
Gets the raw wrapped value for the given language, as string.

If not applicable, returns void (use default operator, {{{!}}}).

  * Parameters *
    object                  = the object wrapped with #wrapAsRaw
    lang                    = (html|js|json|script|...|, default: -empty/unspecific-) the specific language to get
    
  * Related *
    #wrapAsRaw
-->
<#function getRawWrappedForLang object lang="">
  <#local res = Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].getValueForLang(object, lang)!false>
  <#if !res?is_boolean>
    <#return rawString(res)>
  </#if>
</#function>

<#-- 
*************
* wrapRawScript
************
Alias for #wrapAsRaw(object, "script"), and easier to remember in relation to @objectAsScript.
                   
  * Parameters *
    object                  = the string to wrap

  * Related *
    @objectAsScript
-->
<#function wrapRawScript object>
  <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].wrap(object?string, "script")>
</#function>

<#-- 
*************
* isRawScript
************
Checks if the value was wrapped using #wrapAsRaw(object, "script").
                                      
  * Parameters *
    object                  = the value to check
    
  * Related *
    #wrapRawScript
    @objectAsScript
-->
<#function isRawScript object>
  <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].isRawScript(object, "script")>
</#function>


<#-- 
*************
* escapePart
************
Encodes/escapes a SINGLE VALUE string for a given language, crushing delimiters.
Will automatically call #rawString on the passed string (bypassing screen auto-escaping) 
and encode in the requested language.

Essentially this is a wrapper around #rawString and encoders.
It abstracts the encoder selection. 
NOTE: At current time (2016-10-05), this uses Freemarker built-ins (subject to change).
    Although freemarker built-ins can still be used, use of this function is preferred to centralize
    the escaping logic and automatically prevent some double-escaping.

This ONLY works to escape individual values of the given language. For example, "url"
can only encode individual parameters in a URL, not the full URL; it will encoding delimiters.
To encode a full URL, you must use #escapeFullUrl. 

For Javascript and JSON ("js", "json"), this can ONLY encode strings between {{{""}}} or {{{''}}}.
There is no #escapeFull implementation for these because safely escaping full javascript code is impossible (see OWASP).

For HTML: You should use this on HTML attributes. For full HTML texts, in theory you should use #escapeFull, but currently 
#escapePart works generally and is better defined.
NOTE: In practice #escapePart will work on full HTML bodies/texts anyway, and at current time (2016-10-05)
    many macros are using this indiscriminately.

WARN: CSS/STYLE escaping may not be fully implemented at this time!

For Javascript strings placed within HTML attributes (onchange, etc.), typically "js-html" is needed; however 
when invoking macros, most macros will already escape the "html" part and the templates only need
to escape for "js".

NOTE: 2016-09-29: This will now also tolerate non-strings, which will be coerced to strings using ?string operator.

NOTE: 2016-10-05: Supports #wrapAsRaw values: Values passed that were wrapped using #wrapAsRaw will be treated intelligently and when possible, previously-done
    escaping (as specified by the #wrapAsRaw caller) will be reused when the wrapped value's language matches or is a prefix of the language
    passed to this function.

  * Parameters *
    str                     = The string or string-like value to escape
    lang                    = (js|jsdq|json|html|url|xml|style|js-html|html-js|style-html|html-style|raw) The target language
                              These are similar to the Freemarker built-in counterparts, but may
                              not produce the exact same results.
                              {{{jsdq}}}: special case of js where it is assumed the value
                                will be contained in double quotes, such that single quotes
                                don't need to be escaped.
                              WARN: {{{style}}} is not properly implemented!
                              FIXME: escaping for {{{style}}}
    strict                  = ((boolean), default: false) Whether should always escape unconditionally/strictly, or allow heuristics
                              If true, escaping is always applied unconditionally.
                              If false, the function ''may'' attempt heuristics to prevent double-escaping issues (not always desirable),
                              mainly to mitigate screen auto-escaping and early escaping.
                    
  * Related*
    #escapeFull
-->
<#function escapePart str lang strict=false>
  <#if isWrappedAsRaw(str)>
    <#local resolved = Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].resolveScriptForLang(str, lang)>
    <#local str = rawString(resolved.value)><#-- NOTE: this rawString call actually only escapes the ofbiz auto-escaping from the resolveScriptForLang call... obscure -->
    <#local lang = resolved.lang>
  <#else>
    <#local str = rawString(str)><#-- performs coercion to string if needed -->
  </#if>
  <#switch lang?lower_case>
    <#case "json">
      <#return str?json_string>
      <#break>
    <#case "js">
      <#return str?js_string>
      <#break>
    <#case "jsdq">
      <#return str?js_string?replace("\\'", "\'")>
      <#break>
    <#case "html">
      <#return str?html>
      <#break>
    <#case "js-html">
      <#return str?js_string?html>
      <#break>
    <#case "html-js">
      <#return str?html?js_string>
      <#break>
    <#case "xml">
      <#return str?xml>
      <#break>
    <#case "url">
      <#return str?url>
      <#break>
    <#case "style">
      <#-- TODO: FIXME: IMPLEMENT -->
      <#return str>
      <#break>
    <#case "style-html">
      <#-- TODO: FIXME: IMPLEMENT -->
      <#return str?html>
      <#break>
    <#case "html-style">
      <#-- TODO: FIXME: IMPLEMENT -->
      <#return str?html>
      <#break>
    <#case "raw">
    <#default>
      <#return str>
      <#break>
  </#switch>
</#function>

<#-- 
*************
* escapeFull
************
Encodes/escapes a COMPLETE text string for a given language, recognizing and sparing the language's delimiters.
Will automatically call #rawString on the passed string (bypassing screen auto-escaping) 
and encode in the requested language.

WARN: Not currently fully/properly implemented! In most cases should currently use #escapePart and #escapeFullUrl.

This can be used on full HTML snippets, etc.

This is a convenience wrapper around #rawString and the Ofbiz and Freemarker encoders.
It abstracts the encoder selection. 

Currently, it uses Ofbiz's encoder (subject to change).

WARN: implementation subject to change. Currently produces different (but all safe) results compared to #escapePart for HTML.

NOTE: 2016-09-29: This will now also tolerate non-strings, which will be coerced to strings using ?string operator.

NOTE: 2016-10-05: Supports #wrapAsRaw values: Values passed that were wrapped using #wrapAsRaw will be treated intelligently and when possible, previously-done
    escaping (as specified by the #wrapAsRaw caller) will be reused when the wrapped value's language matches or is a prefix of the language
    passed to this function.
    
  * Parameters *
    str                     = The string to escape
    lang                    = (html|xml|raw) The target language
                              NOTE: This does not currently support js/json because it does not
                                  usually make sense to escape anything but single value strings.
                                  It also does not encode URLs; see #escapeFullUrl.
    strict                  = ((boolean), default: false) Whether should always escape unconditionally/strictly, or allow heuristics
                              If true, escaping is always applied unconditionally.
                              If false, the function ''may'' attempt heuristics to prevent double-escaping issues (not always desirable),
                              mainly to mitigate screen auto-escaping and early escaping.
                    
  * Related*
    #escapePart
-->
<#function escapeFull str lang strict=false>
  <#if isWrappedAsRaw(str)>
    <#local resolved = Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].resolveScriptForLang(str, lang)>
    <#local str = rawString(resolved.value)>
    <#local lang = resolved.lang>
  <#else>
    <#local str = rawString(str)><#-- performs coercion to string if needed -->
  </#if>
  <#-- NOTE: Currently we support the same types as Ofbiz, so no need for a switch -->
  <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].getEncoder(lang).encode(str))>
</#function>

<#-- 
*************
* escapeFullUrl
************
Encodes/escapes a COMPLETE URL for a given language.
Will automatically call #rawString on the passed string (bypassing screen auto-escaping) 
and encode in the requested language.

This is meant to be used to encode full URLs so they can be safely included in HTML attributes,
javascript strings, etc.

This is a convenience wrapper around #rawString and the Ofbiz and Freemarker encoders.
It abstracts the encoder selection. 

Currently, it uses Freemarker built-ins (subject to change); the Ofbiz encoders do not appear
suited to handling URLs.

NOTE: In addition to the above, for compability reaons, this function will currently accept param delimiters 
    in either the escaped {{{&amp;}}} form or unescaped {{{&}}} form.
    Ideally, we should not receive escaped delimiters here, but Ofbiz frequently escapes early.

DEV NOTE: Unfortunately this method adds some overhead, but it's the only safe way to process URLs.

NOTE: 2016-10-05: Supports #wrapAsRaw values: Values passed that were wrapped using #wrapAsRaw will be treated intelligently and when possible, previously-done
    escaping (as specified by the #wrapAsRaw caller) will be reused when the wrapped value's language matches or is a prefix of the language
    passed to this function.

  * Usage Examples *
  
    <a href="${escapeFullUrl('http://www.ilscipio.com/test?param1=val1&param2=val2;jsessionid=fake', 'html')}">Link</a>

    <@script>
      var link = "${escapeFullUrl('http://www.ilscipio.com/test?param1=val1&param2=val2;jsessionid=fake', 'js')}";
    </@script>

  * Parameters *
    str                     = The string to escape
    lang                    = (html|js|jsdq|json|js-html|jsdq-html|xml|style|raw) The target language
                              {{{jsdq}}}: special case of js where it is assumed the value
                                will be contained in double quotes, such that single quotes
                                don't need to be escaped.
                              WARN: {{{style}}} is not properly implemented!
                              FIXME: escaping for {{{style}}}
    strict                  = ((boolean), default: false) Whether should always escape unconditionally/strictly, or allow heuristics
                              If true, escaping is always applied unconditionally.
                              If false, the function ''may'' attempt heuristics to prevent double-escaping issues (not always desirable),
                              mainly to mitigate screen auto-escaping and early escaping.
-->
<#function escapeFullUrl str lang strict=false>
  <#if isWrappedAsRaw(str)>
    <#local resolved = Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].resolveScriptForLang(str, lang)>
    <#local str = rawString(resolved.value)>
    <#local lang = resolved.lang>
  <#else>
    <#local str = rawString(str)><#-- performs coercion to string if needed -->
  </#if>
  <#if !strict>
    <#-- Ofbiz compatibility mode: Replace &amp; back to &. Freemarker's ?html function will re-encode them after. -->
    <#local str = str?replace("&amp;", "&")>
  </#if>
  <#switch lang?lower_case>
    <#case "json">
      <#return str?json_string>
      <#break>
    <#case "js">
      <#return str?js_string>
      <#break>
    <#case "jsdq">
      <#return str?js_string?replace("\\'", "\'")>
      <#break>
    <#case "html">
      <#return str?html>
      <#break>
    <#case "js-html">
      <#return str?js_string?html>
      <#break>
    <#case "html-js">
      <#return str?html?js_string>
      <#break>
    <#case "jsdq-html">
      <#return str?js_string?replace("\\'", "\'")?html>
      <#break>
    <#case "html-jsdq">
      <#return str?html?js_string?replace("\\'", "\'")>
      <#break>
    <#case "xml">
      <#return str?xml>
      <#break>
    <#case "style">
      <#-- TODO: FIXME: IMPLEMENT -->
      <#return str>
      <#break>
    <#case "raw">
    <#default>
      <#return str>
      <#break>
  </#switch>
</#function>


<#-- 
*************************************
* API-WRITING UTILITIES *
*************************************
* Intended to assist implementation of template macros.
-->

<#-- 
*************
* contentArgRender
************
Renders an argument supposed to contain a string or macro representing template content.
If it is a macro, it must accept a single {{{args}}} parameter (map), which may
be empty or populating depending on the specific point of use.

  * Parameters *
    content                 = ((string)|(macro)) The content to render
    doTrim                  = ((boolean), default: false) If string, trim it
    args                    = ((map)) Map of arguments to pass to macro
-->
<#macro contentArgRender content doTrim=false args={}>
  <#if content?is_directive>
    <@content args=args /><#t>
  <#elseif doTrim>
    ${content?trim}<#t>
  <#else>
    ${content}<#t>
  </#if>
</#macro>

<#-- 
*************
* mergeArgMaps
************
Merges scipio macro inlineArgs/args/defaultArgs/overrideArgs argument maps for macros implementing
the advanced argument interface.

The advanced interface follows the
  <#macro name args={} inlineArgs...>
pattern, whereby inlineArgs map is merged over the args map, over defaults.

It cannot be used for the 'attribs={} inlineAttribs...' pattern (see mergeAttribMaps). 
Instead, it can help in implementing that pattern within the 'args={} inlineArgs...' pattern.

This is specific to scipio macro patterns and may contain extra handling besides concatenating maps.

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
    args                    = ((map)) The args map (normal priority)
                              May be any type of map.
    inlineArgs              = ((inline-args)) The macro's inline args (high priority)
                              Expects either FTL hash or empty sequence (but not non-empty sequence).
    defaultArgs             = ((map)) Arg defaults (lowest priority)
                              Expects an FTL hash only. The key names in this map are also used to
                              create a list of the known arguments this macro accepts.
                              As such, even if the macro does not need this map, it should pass it along
                              with the names of all supported arguments.
                              The names end up in the resulting map as localArgNames.
    overrideArgs            = ((map)) Extra macro args that override all others (highest priority)
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
* mergeArgMapsEx
************
Variant of #mergeArgMaps that returns more than one map.

  * Parameters *
    (other)                 = See #mergeArgMaps
    
  * Return Value *
    a map of arg maps: {{{allArgs}}} (full combined), {{{explArgs}}} (only explicitly passed args)
-->
<#function mergeArgMapsEx args={} inlineArgs={} defaultArgs={} overrideArgs={}>
  <#if !inlineArgs?has_content> <#-- necessary to prevent empty sequence -->
    <#local inlineArgs = {}>
  </#if>
  <#local localArgNames = (defaultArgs?keys) + (overrideArgs?keys)>
  <#local allArgNames = (args.allArgNames![]) + localArgNames>
  <#local explArgs = toSimpleMap(args) + inlineArgs>
  <#local allArgs = defaultArgs + explArgs + overrideArgs + 
    { "localArgNames":localArgNames, "allArgNames":allArgNames }>
  <#return {"allArgs":allArgs, "explArgs":explArgs}>
</#function>

<#-- 
*************
* mergeArgMapsBasic
************
A version of mergeArgMaps that only merges maps, but doesn't perform any special implied
operations on them.

  * Parameters *
    (other)                 = See #mergeArgMaps
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
NOT IMPLEMENTED
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
NOT IMPLEMENTED
Same as mergeArgMapsToLocals but uses mergeArgMapsBasic logic instead of mergeArgMaps logic.
-->
<#-- NOT IMPLEMENTED
<#function mergeArgMapsToLocalsBasic args={} inlineArgs={} defaultArgs={} overrideArgs={}>
</#function>
-->

<#-- 
*************
* getScipioMacroDefaultArgs
************
Returns all the known args and default args for a scipio macro that uses the advanced
args pattern, where the macro resides in the given namespace (or namespace-like map).

Basically, the macros should always have default values (in the event one was missing, this method
should automatically give one such as empty string).
If namespace is omitted, currently (2015-12-14), this will use ".vars" as the namespace.

DEV NOTE: there could be some use for the ?namespace built-in somewhere around this function
   or more generally the default args, but so far elusive. only works on macro/function so
   maybe best not to rely on it.
-->
<#function getScipioMacroDefaultArgs name namespace="">
  <#if !namespace?has_content>
    <#local namespace = .vars>
  </#if>
  <#return namespace[name + "_defaultArgs"]!{}>
</#function>

<#-- 
*************
* mergeAttribMaps
************
Merges scipio macro attribs/inlineAttribs/defaultAttribs/overrideAttribs maps for macros implementing
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

It uses args.allArgNames to determine the extra "inline" arguments.

NOTE: 2016-08-02: This function has been modified so that the result fully represents a usable
    attribs map as-is. It no longer contains superfluous members (allArgNames, localArgNames, etc.).
    
  * Related *
    #getAttribMapAllExcludes
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function makeAttribMapFromArgMap args={} excludes=[]>
  <#local res = copyMap(args, "e", ["allArgNames", "localArgNames"] + (args.allArgNames![]) + excludes)>
  <#if args.attribs?has_content && args.attribs?is_hash>
    <#local attribs = toSimpleMap(args.attribs)>
    <#if excludes?has_content>
      <#local res = copyMap(attribs, "e", excludes) + res>
    <#else>
      <#local res = attribs + res>
    </#if>
  </#if>
  <#return res>
</#function>
-->

<#-- 
*************
* getAttribMapAllExcludes
************
Returns the attrib map excludes from the given attribs map, which is composed of its
"scipioExcludeNames" and "scipioNoExcludeNames" members, plus optional additional
includes passed as arguments.
The result is returned as a bean-wrapped Set.

TODO: implement as transform.

  * Related *
    #makeAttribMapFromArgs
-->
<#function getAttribMapAllExcludes attribs={} exclude=[] noExclude=[]>
  <#local exclude = toSet(exclude)>
  <#local noExclude = toSet(noExclude)>
  
  <#if attribs.scipioExcludeNames?has_content>
    <#local dummy = exclude.addAll(attribs.scipioExcludeNames)!>
  </#if>

  <#local dummy = exclude.addAll(["scipioExcludeNames", "scipioNoExcludeNames"])>
  
  <#if attribs.scipioNoExcludeNames?has_content>
    <#local dummy = noExclude.addAll(attribs.scipioNoExcludeNames)!>
  </#if>

  <#if noExclude?has_content>
    <#local dummy = exclude.removeAll(noExclude)!>
  </#if>
  <#return exclude>
</#function>

<#-- 
*************
* getFilteredAttribMap
************
Returns the attrib map with all the basic excludes applied.

TODO: implement as transform.

  * Related *
    #makeAttribMapFromArgs
    @elemAttribStr
-->
<#function getFilteredAttribMap attribs={} exclude=[] noExclude=[]>
  <#local allExcludes = getAttribMapAllExcludes(attribs, exclude, noExclude)>
  <#return toSimpleMap(copyMap(attribs, "e", allExcludes))>
</#function>

<#-- 
*************
* compileClassArg
************
Compiles a class argument as common to most standard macros,
producing a simple list of class names from a class arg, adding optional default.

This function and the others related help parse class argument passed to macros.
Generally not for use in templates (though may still be useful in some).

* Macro Class Arguments *

Macro class arguments can have essential (required) and non-essential (default)
defaults and values added by the macro before producing the final class attribute. 
The essentials can't be omitted. 
The non-essentials in some cases you want to replace with value and other times 
you want to keep them but only add extra classes. Having multiple args for each elem 
class arg gets heavy.

So to address all the cases, these functions cause the class arg on all supporting macros 
to accept following values:

* string with {{{+}}} prefix or empty string {{{""}}}: Means allow macro to add non-essential default classes. 
  The class names after the {{{+}}} will be appended to any classes added
  by the macro, and will never replace macro defaults.
  This means the same as boolean true but with extra classes provided.
  In other words, appends extra classes.
* string with {{{=}}} prefix or non-empty string: Means prevent macro from adding non-essential default classes.
  Class names given will replace macro defaults, i.e. non-essential classes. 
  Macro may still add its own required classes.
  This is the same as boolean false but with replacement classes provided.
  In other words, replaces default (non-essential) classes.
  
In a few cases, the non-essential defaults are "conditionally essential" 
so {{{class="="}}} makes little sense, but usually doesn't warrant special handling.

NOTES: 
* Not all macros act on all prefixes, though the syntax is always accepted.
* Currently, in some cases of traditional Ofbiz templating, the logic above breaks too much compatibility. So it is not universally used
  for screen/form/menu widgets although often supported.
  For these, in some places an {{{xxxExplicit}}} version of functions are used instead which will only
  replace if explicitly prefixed with {{{=}}}, but not if no prefix. Otherwise, existing widget styles break
  the grid everywhere.
  
  * Usage Examples *
  
In macro implementations, compileClassArg should usually be used as late as possible:
  <#local classes = compileClassArg(class)>
or
  <#local classes = compileClassArg(class, "default-class")>
  
Specifying the default value is the same as doing:
  <#local class = addClassArgDefault(class, "default-class")>
  <#local classes = compileClassArg(class)>
  
  * Related *
    #addClassArg
    #addClassArgDefault
    @compiledClassAttribStr
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
  <#if classes?has_content> class="${escapePart(classes, 'html')}"</#if><#t>
</#macro>

<#-- 
*************
* compiledClassAttribStrExplicit
************
Explicit version of #compiledClassAttribStr.
-->
<#macro compiledClassAttribStrExplicit class defaultVal="">
  <#local classes = compileClassArgExplicit(class, defaultVal)>
  <#if classes?has_content> class="${escapePart(classes, 'html')}"</#if><#t>
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

These functions take a template-level/logical scipio macro "class" arg and add to it the given class.
Should be called by the implementing macros only. 

Depending on the previous value in class, non-essential (default) values may simply be discarded, but
essentials (required) will always be added in some way, transforming the class value.

The newClass parameter is a simple string literal and is not interpreted.

These are non-destructive except for addClassArgReplacing which always causes the string to become
a replacing string ("=").

  * Related *
    #compileClassArg
    #addClassArgDefault
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
Special case of addClassArg where the required class will become a replacing string ("=" prefix, as in "=my-class"),
though will not squash previous values. 

NOTE: This destroys information about what macro user requested and affects the default value logic
    perceived by addClassArgDefault. 
    
  * Related *
    #compileClassArg
    #addClassArg
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

  * Related *
    #compileClassArg
    #addClassArg
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

  * Related *
    #compileClassArg
    #addClassArg
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
Logically combines two template-level scipio macro "class" arguments. 
The second almost always overrides the first, except when second is appending a class
with "+", in which case depends on the first.

Essentially it extends the intended use of the class arg to mean "+" also allows appending
of previous values (in addition of being an appending value itself).

NOTE: Even if the second arg is merely "+" (which usually means "use defaults" for scipio macros),
    this method will return "+" and dismiss the first argument. "+" is not treated as
    pass-through here. This does not seem consistent,
    but is chosen explicitly so that caller/macro decides what the "pass-through" value
    is supposed to be. Here, we assume "+" was passed explicitly to override the first.
    We only treat "+" with an arg.
    In other words, can see this as another kludge for lack of null values in freemarker.
    TODO: review this
    
  * Related *
    #compileClassArg
    #addClassArg
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
Returns class names without their Scipio-supported prefix.
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
Returns the Scipio-supported class prefix from a class arg.
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
* addStringToBoolStringVal
************
Adds a string with optional delimiter to a string that supports boolean values.
-->
<#function addStringToBoolStringVal orig toAdd delim="">
  <#if orig?is_boolean>
    <#if orig>
      <#return toAdd>
    <#else>
      <#return orig>
    </#if>
  <#else>
    <#if orig?has_content>
      <#return orig + delim + toAdd>
    <#else>
      <#return toAdd>
    </#if>
  </#if>
</#function>


<#-- 
*************
* pushRequestStack
************
Pushes a value onto a global stack variable in request scope (request attributes, or if no request, globals).

  * Parameters *
    name                    = (required) Global request stack var name
                              Must be unique across all known types of contexts (request attribs, screen context, FTL globals)
    val                     = ((object)) Value
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

NOTE: AUTO-ESCAPING: Unlike {{{request.getAttribute}}}, values retrieved are not auto-escaped,
    unless they were inserted already escaped. In general, what you put in is what you get out.
    
  * Parameters *
    name                    = (required) Global request stack var name
                              Must be unique across all known types of contexts (request attribs, screen context, FTL globals)
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
    name                    = (required) Global request stack var name
                              Must be unique across all known types of contexts (request attribs, screen context, FTL globals)
    val                     = (required) Value
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

NOTE: AUTO-ESCAPING: Unlike {{{request.getAttribute}}}, values retrieved are not auto-escaped,
    unless they were inserted already escaped. In general, what you put in is what you get out.

  * Parameters *
    name                    = (required) Global request stack var name
                              Must be unique across all known types of contexts (request attribs, screen context, FTL globals)
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

NOTE: AUTO-ESCAPING: Unlike {{{request.getAttribute}}}, values retrieved are not auto-escaped,
    unless they were inserted already escaped. In general, what you put in is what you get out.
    
  * Parameters *
    name                    = (required) Global request stack var name
                              Must be unique across all known types of contexts (request attribs, screen context, FTL globals)
    listType                = (copy|orig, default: copy)
                              * orig: Avoids a list copy.
                                WARN: "orig" means the caller must ditch the list as soon as possible, before any
                                    more modifications to the stack; otherwise results will be unpredictable.
                                    It should only be used for optimization.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function getRequestStackAsList name listType="copy">
</#function>
-->

<#-- 
*************
* getRequestStackSize
************
Gets the current size of the named stack.
If the stack doesn't exist, returns void, so can be used to check if a stack exists using {{{??}}} operator.
    
  * Parameters *
    name                    = (required) Global request stack var name
                              Must be unique across all known types of contexts (request attribs, screen context, FTL globals)
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function getRequestStackSize name>
</#function>
-->

<#-- 
*************
* setRequestVar
************
Sets a global var in request scope (request attributes, or if no request, globals).

Values set by this method must be read using getRequestVar.

  * Parameters *
    name                    = (required) Global request var name
                              Must be unique across all known types of contexts (request attribs, screen context, FTL globals)
    val                     = ((object), required) Value
    mode                    = (u|w|, default: -empty-) Modes
                              * u: always unwrap the TemplateModel before storing (where possible)
                              * w: always keep TemplateModel as-is (wrapped) when storing
                              NOTE: "u" and "w" modes are usually unnecessary and should be avoided in most template and macro code.
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

NOTE: AUTO-ESCAPING: Unlike {{{request.getAttribute}}}, values retrieved are not auto-escaped,
    unless they were inserted already escaped. In general, what you put in is what you get out.

  * Parameters *
    name                    = (required) Global request var name
                              Must be unique across all known types of contexts (request attribs, screen context, FTL globals)
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
    map                     = the source map
    mode                    = (e|i|, default: -empty-) Optional mode flags
                              * e: Exclude listed keys
                              * i: Include only listed keys
    inExKeys                = ((list)|(set), default: -empty-) Optional list or wrapped set of keys to include or exclude      
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
    (other)                 = See #varsPutAll     
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
    (other)                 = See #varsPutAll        
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

NOTE: If a context map is passed containing strings, they will not be auto-HTML-escaped by the renderer.

NOTE: 2016-09-30: This now automatically HTML-escapes attribute values by default; see {{{escapeLang}}} parameter.

TODO: implement as transform.

  * Parameters *
    attribs                     = ((map)) Map of attribute-value pairs
                                  It currently accepts string format as a fallback/legacy support, but this is highly discouraged
                                  and other args won't work with it.
    alt                         = ((boolean)) If true alternate row (odd), if false regular (even)
    selected                    = ((boolean)) If true row is marked selected
    exclude                     = ((list)) List of attrib names to skip
    attribNamePrefix            = ((string)) Add this prefix to attribute names
    alwaysAddPrefix             = ((boolean)) If false, only add prefix if not already has prefix (default true)
    attribNamePrefixStrip       = ((string)) Remove this prefix from all attrib names
    attribNameSubstitutes       = ((map)) Map of attrib names to substitute attrib names
                                  NOTES: 
                                  * If this is set, the exclude names should be the names of the subtitutes, not the input attrib names.
                                  * This is applied after prefix ops are applied.
    camelCaseToDashLowerNames   = ((boolean)) If true converts attrib names from camelCase to camel-case at the very end
    emptyValToken               = ((string)) When this value encountered, will include an empty attrib
    noValToken                  = ((string)) When this value encountered, will include an attrib with no value
    escapeLang                  = (html|...|none, default: 'html') Language to escape each attribute (passed to #escapePart)
                                  Callers may bypass escaping by wrapping their values using #wrapAsRaw.
-->
<#macro elemAttribStr attribs includeEmpty=false emptyValToken="" noValToken="" exclude=[] 
  attribNamePrefix="" alwaysAddPrefix=true attribNamePrefixStrip="" attribNameSubstitutes={} camelCaseToDashLowerNames=false escapeLang="html">
  <#if isObjectType("map", attribs)>
    <#t>${rawString(Static["com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil"].makeElemAttribStr(attribs, includeEmpty, 
      emptyValToken, noValToken, exclude, attribNamePrefix, alwaysAddPrefix, attribNamePrefixStrip, attribNameSubstitutes, camelCaseToDashLowerNames, escapeLang))}<#t>
  <#elseif attribs?is_string>
    <#t> ${attribs?string}
  </#if>
</#macro>

<#-- 
*************
* attribSpecialVal
************
Returns a special value that can be passed to some attribs maps arguments on
macros (works via @elemAttribStr).

  * Parameters *
    type                        = (none|empty) The special value type

  * Related *
    @elemAttribStr
    #isAttribSpecialVal
-->
<#function attribSpecialVal type>
  <#-- old (deprecated, should not even be here)
  <#if type == "none">
    <#return "_NO_VALUE_">
  <#elseif type == "empty">
    <#return "_EMPTY_VALUE_">
  </#if>
  -->
  <#if !scipioAttribSpecialValMap??>
    <#global scipioAttribSpecialValMap = toSimpleMap(Static["com.ilscipio.scipio.ce.webapp.ftl.template.AttribSpecialValue"].getTypeNameMap())>
  </#if>
  <#return scipioAttribSpecialValMap[type]>
</#function>

<#-- 
*************
* isAttribSpecialVal
************
Checks if a value is one returned by #attribSpecialVal.

  * Parameters *
    object                      = the object to test                     
    type                        = (none|empty|, default: -empty-) The special value type to test for, or empty for any special value

  * Related *
    #attribSpecialVal
-->
<#function isAttribSpecialVal val type="">
  <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.AttribSpecialValue"].isSpecialValue(val, type)>
</#function>

<#-- 
*************
* formattedDate
************
Renders a formatted date.

NOTE: formattedDate by default renders the "date" type but it also doubles as handler for the other types
    (which also have convenience wrappers below).
    
  * Parameters *
    date                    = ((date), required) The date
    dateType                = (date-time|timestamp|date|time, default: date)
                              "timestamp" and "date-time" are synonymous.  
    defaultVal              = If no output is produced (empty), this value (string) will be shown instead
    specLocale              = ((locale), default: -locale from context-) Override locale
    specTimeZone            = ((timezone), default: -timeZone from context-) Override time zones
    
  * Related *
    @formattedDateTime, @formattedTime, #formatDate, #formatDateTime, #formatTime
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

NOTE: The result is auto-HTML-escaped (where applicable); use #rawString to prevent.

  * Related *
    @formattedDate
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function formatDate date dateTimeFormat="" specLocale=true specTimeZone=true dateType="date">
  <#- old implementation; similar but transform handles nulls/booleans better ->
  <#if specLocale?is_boolean>
    <#if specLocale>
      <#local specLocale = locale!>
    <#else>
      <#local specLocale = ""> <#- FIXME: won't work, must emulate null but freemarker sucks here... ->
    </#if>
  </#if>
  <#if specTimeZone?is_boolean>
    <#if specTimeZone>
      <#local specTimeZone = timeZone!>
    <#else>
      <#local specTimeZone = ""> <#- FIXME: won't work, must emulate null but freemarker sucks here... ->
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
    <#- otherwise, return void (for default value operator) ->
  </#if>
</#function>
-->

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

  * Usage Examples *
    h3
    h3:headingclass
    h+3:headingclass
    div:divclass;h+3:headingclass
    div:divclass;h+3:headingclass;consumeLevel=true
-->
<#function getHeadingElemSpecFromStyleStr styleStr containerStyleStr allowedHeadingElemTypes allowedElemTypes allowedContainerElemTypes cacheId="">
  <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil"].getHeadingElemSpecFromStyleStr(styleStr, containerStyleStr,
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
    style                   = (required) CSS Style/classes string
    prefixMap               = ((map), required) Map of CSS class prefixes to result names
    
  * Return Value *
    a map of result names to integer values.
-->
<#function extractPrefixedStyleNamesWithInt style prefixMap>
  <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil"].extractPrefixedStyleNamesWithInt(style, prefixMap)>
</#function>

<#-- 
*************
* saveCurrentContainerSizes
************
This records current container (grid) sizes into a global stack

This makes it possible for inner containers to be aware of all the sizes applied to it and the general width.

Every push should be followed by a pop.

NOTE: This function is generally framework-agnostic and size-key agnostic.

  * Parameters *
    sizes                   = ((map), required) Map of size names to integer values
                              Typically, this will be (e.g.):
                                {"large":12, "medium":12, "small":12}
                              but can be anything (the methods in this file do not care).
-->
<#function saveCurrentContainerSizes sizes>
  <#local dummy = pushRequestStack("scipioCSFactorsCacheStack", false)>
  <#return pushRequestStack("scipioContainerSizesStack", sizes)>
</#function>

<#-- 
*************
* saveCurrentContainerSizesFromStyleStr
************
Same as saveCurrentContainerSizes but tries to extract the container sizes from a style string.

This is needed to know grid sizes because most of the facilities only support setting
class name strings (except for @cell which supports columns/small/medium/large args).

IMPL NOTE: For this method to work, the framework-/theme-specific code must override the abstract
    function parseContainerSizesFromStyleStr.

  * Parameters *
    style                   = (required) Style string containing grid size classes
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
  <#local dummy = popRequestStack("scipioCSFactorsCacheStack")!false>
  <#return popRequestStack("scipioContainerSizesStack")!{}>
</#function>

<#-- 
*************
* parseContainerSizesFromStyleStr
************   
ABSTRACT. This function should be overridden by a framework-specific implementation that parses the 
container sizes from a class names string.
           
  * Parameters *
    style                   = (required) Style string containing grid size classes
    
  * Return Value *
    a map of size names to integer values.
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
  <#return readRequestStack("scipioContainerSizesStack")!{}>
</#function>

<#-- 
*************
* getAllContainerSizes
************
Gets a list of maps describing all (parent) container sizes nesting the current container
and the current container size (if was added).
-->
<#function getAllContainerSizes>
  <#return getRequestStackAsList("scipioContainerSizesStack")![]>
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

NOTE: The above is only an example; this method is framework-agnostic.

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
    maxSizes                = ((int)|(map), default: 0) Max container sizes (grid size)
                              This is can a map of per-key max container sizes OR a single number giving max size
                              for all keys (usually same in most frameworks) OR zero to mean use defaults.
                   
  * Return Value *
    ((map)) a map of container size names to floating-point values
-->
<#function getAbsContainerSizeFactors maxSizes=0>
  <#local factors = readRequestStack("scipioCSFactorsCacheStack")!false>
  <#if !factors?is_boolean>
    <#return factors>
  <#else>
    <#-- orig means won't make copy of lists so faster, but lists must be ditched a.s.a.p. 
         don't do this in getAllContainerSizes -->
    <#local sizesList = getRequestStackAsList("scipioContainerSizesStack", "orig")![]>
    <#local cachedFactorsList = getRequestStackAsList("scipioCSFactorsCacheStack", "orig")![]>
    <#local factors = evalAbsContainerSizeFactors(sizesList, maxSizes, cachedFactorsList)>
    <#local dummy = setLastRequestStack("scipioCSFactorsCacheStack", factors)>
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
    sizesList               = ((list), required) List of hashes describing all (known) parent and current container sizes
                              NOTE: Entries may be empty and should be skipped.
    maxSizes                = (default: 0) See #getAbsContainerSizeFactors maxSizes parameter
    cachedFactorsList       = ((list), default: -empty-) List of cached factor results, of same size and order as sizesList
                              Each entry corresponds to the container at same index as the sizes in sizesList.
                              If an entry is boolean, then a factor was not (yet) calculated for that container.
    
  * Return Value *
    a map of size names to float values.
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
    <#return rawString(screens.getScreenStringRenderer().getRendererName()!"")>
  </#if>
</#function>

<#-- 
*************
* getDefaultScipioLibLocation
************
Returns a scipio library location.

Supported names: "variables", "template"
NOTE: this is currently render context-unaware
Returns void if nothing.

  * Parameters *
    libName                 = (variables|template, required)
    renderPlatformType      = ((string), default: default) Platform type
                              Caller should use #getRenderPlatformType().
    renderContextType       = ((string), default: general) Render context type
                              Caller should use #getRenderContextType().
-->
<#function getDefaultScipioLibLocation libName renderPlatformType="default" renderContextType="general">
  <#local res = getPropertyValue("scipioWebapp", "scipio.templating.lib." + renderContextType + "." + renderPlatformType + "."  + libName  + ".location")!"">
  <#if res?has_content>
    <#return res>
  </#if>
  <#if renderContextType != "general">
    <#local res = getPropertyValue("scipioWebapp", "scipio.templating.lib.general." + renderPlatformType  + "." + libName + ".location")!"">
    <#if res?has_content>
      <#return res>
    </#if>
  </#if>
  <#if renderPlatformType != "default">
    <#local res = getPropertyValue("scipioWebapp", "scipio.templating.lib." + renderContextType + ".default." + libName + ".location")!"">
    <#if res?has_content>
      <#return res>
    </#if>
  </#if>
  <#local res = getPropertyValue("scipioWebapp", "scipio.templating.lib.general.default." + libName + ".location")!"">
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

The format is:
  ${['[platform]':'component://...', ...]}
  e.g. ${[''html':'component://...', 'xml':'component://...', 'default':'component://...']}
  
Intended for use with VT_STL_VAR_LOC, VT_STL_TMPLT_LOC and variants.

Checks the resourceNames in the given order.

NOTE: "default" is a special map key; should be avoided.
-->
<#function getMacroLibraryLocationStaticFromResources renderPlatformType resources resourceNames...>
  <#local res = Static["org.ofbiz.widget.renderer.VisualThemeWorker"].getMacroLibraryLocationStaticFromResources(renderPlatformType, rendererVisualThemeResources!, resourceNames)!"">
  <#if res?has_content>
    <#return rawString(res)>
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

  * Usage Examples *  
    <@printVars />           
                    
  * Parameters *
    var                     = ((object), default: context) Var to be printed 
    platform                = ((boolean)|html|..., default: true) The target platform (HTML, etc.)
                              If true, looks up current render.
    maxDepth                = ((int), default: 5) Max depth, to prevent endless recursions
-->
<#macro printVars var=context platform=true maxDepth=5>
  <#if platform?is_boolean><#if platform><#local platform = getRenderPlatformType()!""><#else><#local platform = ""></#if></#if>
  <#if platform == "html">
    <table>
    <#list mapKeys(var) as key>
      <tr>
        <td style="width:200px; vertical-align:top">${escapePart(key, 'html')}</td>
        <td>
          <@printVar value=(var[key]!"") platform=platform maxDepth=maxDepth currDepth=2/>
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
        ${escapePart(var, 'html')}
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
            <tr><td>${escapePart(key, 'html')}</td><td><@printVar value=(var[key]!"") platform=platform maxDepth=maxDepth currDepth=(currDepth+1)/></td></tr>
          </#list>
          </table>-->
          <@objectAsScript lang="json" escape=false object=var maxDepth=maxDepth currDepth=currDepth />
        </#if>
      <#elseif var?is_string>
        ${escapePart(var, 'html')}
      </#if>
    <#recover>
      <span style="color:red"><strong>${(.error)!"(generic)"}</strong></span>
    </#attempt>
  </#if>
</#macro>

