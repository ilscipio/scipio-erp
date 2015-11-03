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
* NOTE: In general, macros expect to be called using named arguments (not supported for functions),
*     except where otherwise noted.
* NOTE: Default markup-producing macros are found in htmlTemplate.ftl.
*     Utilities found in utilities.ftl should not contain their logic in general (TODO?: there could be a template helpers file to isolate logic from markup).
*  
* IMPL NOTE: Macros should avoid using "request" directly (use setRequestVar/getRequestVar/other).
*
* DEV NOTE: for performance, some of these could probably later be turned into freemarker transforms (java) or
*     delegate to java methods.
* DEV NOTE: freemarker functions (and java calls) don't support named arguments so in some cases 
*     macros are easier/better to use even in this file (but macro calls don't inline well in other macro/function calls, 
*     so give and take).
* DEV NOTE: It's important that these macros remain generic and the include for these utilities is
*     completely static so that any macro or function here can easily be interchanged with java-based transforms.
*
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
* makeOfbizUrl function
************
Function version of the @ofbizUrl macro.
Boolean arguments can be given as booleans, string representation of booleans
or empty string (signifies use defaults).
-->
<#function makeOfbizUrl uri fullPath=false secure=false encode=true>
  <#if fullPath?is_boolean><#local fullPath = fullPath?c><#elseif !fullPath?has_content><#local fullPath = "false"></#if>
  <#if secure?is_boolean><#local secure = secure?c><#elseif !secure?has_content><#local secure = "false"></#if>
  <#if encode?is_boolean><#local encode = encode?c><#elseif !encode?has_content><#local encode = "true"></#if>
  <#local res><@ofbizUrl fullPath=fullPath secure=secure encode=encode>${uri}</@ofbizUrl></#local>
  <#return res>
</#function>

<#-- 
*************
* interpretRequestUri function
************
Interprets the given request URI/URL resource and transforms into a valid URL if and as needed.
Can help to cut down on macro URL arguments, but may be used anywhere.

If the uri is already a web URL, it is returned as-is.
The following URI forms are currently interpreted and transformed:
 ofbizUrl:// - Any URI that begins with this will be interpreted as an ofbiz controller URL and ran through @ofbizUrl/makeOfbizUrl.
               Form (note: order of arguments is strict; args will be stripped): 
                 ofbizUrl://myRequest;fullPath=false;secure=false;encode=true?param1=val1
-->
<#function interpretRequestUri uri>
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
* label function
************
Returns empty string if no label is found
-->
<#function label value="">
  <#if value?has_content>
    <#local var="${uiLabelMap[value]}" />
    <#if var!=value>
      <#return var>
    <#else>
      <#return "">
    </#if>
  <#else>
    <#return ""> 
  </#if>
</#function>

<#-- 
*************
* getPropertyValue function
************
Gets property or empty string if missing (same behavior as UtilProperties).
note: the ?string kludge is to get rid of the wrapString wrapper which can break.
-->
<#function getPropertyValue resource name>
  <#return StringUtil.wrapString(Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue(resource, name))?string>
</#function>

<#-- 
*************
* getPropertyValueOrNull function
************
Gets property or void if missing (use default operator).
-->
<#function getPropertyValueOrNull resource name>
  <#local value = StringUtil.wrapString(Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue(resource, name))?string>
  <#if value?has_content>
    <#return value>
  </#if>
</#function>

<#-- 
*************
* addParamDelimToUrl function
************
Adds a param delimiter to end of url if needed.
                    
   * Parameters *
    url             = Url to which to append delimiter
    paramDelim      = Param delimiter to use (escaped, "&amp;" by default)
-->
<#function addParamDelimToUrl url paramDelim="&amp;">
  <#if url?contains("?")>
    <#if url?ends_with("?")>
      <#return url>
    <#elseif url?ends_with(paramDelim)>
      <#return url>
    <#else>
      <#return url + paramDelim>
    </#if>
  <#else>
    <#return url + "?">
  </#if>
</#function> 

<#-- 
*************
* addParamsToStr function
************
Adds parameters from a hash to a URL param string (no full URL logic).
                    
   * Parameters *
    paramStr        = Escaped param string
    paramMap        = Hash of keys to values to add (FIXME: java Maps from ofbiz widgets may not work)
    paramDelim      = Param delimiter (escaped, "&amp;" by default)
    includeEmpty    = Include empty values, or if false omit empty values
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
* splitStrParams function
************
Extracts parameters from a string in the format and returns as a hash:
    name1=val1DELIMname2=val2DELIMname3=val3
where DELIM is specified delimiter (& &amp; , ; etc.)
                    
   * Parameters *
    paramStr        = Escaped param string
    paramDelim      = Param delimiter ("&amp;" by default)
-->
<#function splitStrParams paramStr paramDelim="&amp;">
  <#return Static["com.ilscipio.cato.webapp.ftl.CommonFtlUtil"].splitStrParams(paramStr, paramDelim)>
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
* trimParamStrDelims function
************
Strips leading and trailing param delims from a URL param string.
                    
   * Parameters *
    paramStr        = param string
    paramDelim      = Param delimiter (escaped, "&amp;" by default)
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
* splitStyleNames function
************
splits a style classes string into sequence, same order.
                    
   * Parameters *
    styleString     = style string containing classes
   * Return value *
    a sequence of style names, same order.
-->
<#function splitStyleNames styleString>
  <#return styleString?split(r'\s+', 'r')>
</#function> 

<#-- 
*************
* splitStyleNamesToSet function
************
splits a style classes string into a Set of unique elems, no order.
                    
   * Parameters *
    styleString     = style string containing classes
   * Return value *
    a java Set of style names (can be seen as sequence)
-->
<#function splitStyleNamesToSet styleString>
  <#return Static['org.ofbiz.base.util.UtilMisc'].collectionToSet(styleString?split(r'\s+', 'r'))>
</#function> 

<#-- 
*************
* joinStyleNames function
************
Joins style names in a nice string
Usage example:   
    <#assign myVar = joinStyleNames("class1", "", " class3")>
       
   * Parameters *
    styleNames     = style names (strings), as positional params.
   * Return value *
    a string of combined style names
-->
<#function joinStyleNames styleNames...>
  <#return joinStyleNamesList(styleNames)>
</#function> 

<#function joinStyleNamesList styleNames>
  <#local res = "">
  <#list styleNames as name>
    <#local res = (res + " " + name)?trim>
  </#list>
  <#return res>
</#function> 

<#-- 
*************
* containsStyleName function
************
Returns true if class/style string contains given style.
                    
   * Parameters *
    styleString     = style string containing classes
    className       = name of class to find
   * Return value *
    true if class/style string contains given style, false otherwise.
-->
<#function containsStyleName styleString className>
  <#-- don't need regexp -->
  <#return styleString?split(" ")?seq_contains(className)> 
</#function> 

<#-- 
*************
* removeStyleNames function
************   
Removes style classes from a style string. 
strips lead/trailing space.
           
   * Parameters *
    styleString     = style string containing classes
    namesToRemove   = array of names or space-separated string of names to remove 
                      (can be single name)
   * Return value *
    the style string with names removed, same order but reformatted.
-->
<#function removeStyleNames styleString namesToRemove>
  <#if namesToRemove?is_string>
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
  <#return res?trim>
</#function> 

<#-- 
*************
* addParamsToUrl function
************
Adds parameters from a hash to a URL. appends delimiters as needed.
                    
   * Parameters *
    url             = Url
    paramMap        = Hash of keys to values to add (FIXME: java Maps from ofbiz widgets may not work)
    paramDelim      = Param delimiter (escaped, "&amp;" by default)
    includeEmpty    = Include empty values, or if false omit empty values
-->
<#function addParamsToUrl url paramMap paramDelim="&amp;" includeEmpty=true>
  <#return addParamsToStr(addParamDelimToUrl(url, paramDelim), paramMap, paramDelim, includeEmpty)>
</#function> 

<#-- 
*************
* escapeUrlParamDelims function
************
Escapes the URL's parameter delimiters if they are not already escaped.
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
* isObjectType function
************
Checks the given FTL object against a set of logical types.
Perform special logical type checks because ?is_string and ?is_hash are insufficient for BeanModel-based
widget context vars.
Implemented as java transform.

   * Parameters *
    type        = [string|map|simplemap|complexmap]
                  string: Anything meant to be a string WITHOUT being a more complex type.
                  map: Simple hash, or context map that exposes methods as keys (BeanModel with underlying Map) 
                       (simplemap or complexmap)
                  simplemap: Simple hash only (?keys to get elems).
                  complexmap: Context map that exposes methods as keys (BeanModel) only (.keySet() to get elems).
    object      = the object to test

<#function isObjectType type object>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* copyObject function
************
Performs a shallow copy of a map (TODO: lists). Usually not needed in FTL; for advanced usage.
The resulting underlying type may differ from the original, but by default will be similar.
Implemented as java transform.

<#function copyObject object>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* toSimpleMap function
************
Takes a bean-wrapped map and gives it a simple map adapter instead. Does not perform a copy.
If it is not a complex map but already another type of map, returns it as-is. Other types throw errors.
Implemented as java transform.

<#function toSimpleMap object>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* mapKeys function
************
Gets the logical map keys from any object whether FTL hash (?keys) or context var (.ketSet()).
Unlike ?keys, behaves as expected on both maps from screen context and FTL.
Implemented as java transform.

<#function mapKeys object>
- implemented as java transform -
</#function> 
-->

<#-- 
*************
* concatMaps function
************
Concatenates two maps similar to FTL "+" hash operator, but works with ofbiz maps as well.
By default, result is now always a simple map, so type is more predictable, and usually this is what we want.

Using "+" on screen context bean maps causes problems such as "hiding" of the bean map type underneath
an FTL wrapper, which wrecks all subsequent type checks. And others.

TODO? This is currently inefficient; but must guarantee immutability.
    Note currently forced to implement in FTL (not java transform) if want to exploit "+" operator.
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
          <#return Static["com.ilscipio.cato.webapp.ftl.CommonFtlUtil"].concatMaps(first, second)>
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
* requireScriptOfbizUrl macro
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
    forceInline     = if true, the include must be inlined in the markup where the macro is used
                      and should never be delegated. in most cases this should be omitted.  
                      DEV NOTE: if false (default) for now simply accumulates the names  
                      and will be included by decorator in footer
-->
<#macro requireScriptOfbizUrl uri htmlwrap=false forceInline=false>
  <#local requiredScriptOfbizUrls = getRequestVar("requiredScriptOfbizUrls")!false>
  <#if requiredScriptOfbizUrls?is_boolean || !requiredScriptOfbizUrls.contains(uri)>
    <#if forceInline>
      <#if htmlwrap>
<script language="JavaScript" type="text/javascript">
<!-- //
      </#if>
      <#if requiredScriptOfbizUrls?is_boolean>
    if (typeof variable === 'undefined') {
        var commonOfbizUrls = {};
    }
      </#if>

    commonOfbizUrls["${uri}"] = "<@ofbizUrl>${uri}</@ofbizUrl>";
      <#if htmlwrap>
// -->
</script>
      </#if>
    </#if>
    <#if requiredScriptOfbizUrls?is_boolean>
      <#local requiredScriptOfbizUrls = Static["org.ofbiz.base.util.UtilMisc"].toSet(uri)>
    <#else>
      <#local dummy = requiredScriptOfbizUrls.add(uri)!>
    </#if>
    <#local dummy = setRequestVar("requiredScriptOfbizUrls", requiredScriptOfbizUrls)>
  </#if>
</#macro>

<#macro includeRecordedScriptOfbizUrls htmlwrap=false>
  <#local requiredScriptOfbizUrls = getRequestVar("requiredScriptOfbizUrls")!false>
  <#if !requiredScriptOfbizUrls?is_boolean || (!requiredScriptOfbizUrls.isEmpty())>
    <#if htmlwrap>
<script language="JavaScript" type="text/javascript">
<!-- //
    </#if>
    if (typeof variable === 'undefined') {
        var commonOfbizUrls = {};
    }

    <#list requiredScriptOfbizUrls as uri>
    commonOfbizUrls["${uri}"] = "<@ofbizUrl>${uri}</@ofbizUrl>";
    </#list>
    <#if htmlwrap>
// -->
</script>
    </#if>
  </#if>
</#macro>

<#-- 
*************
* getCurrentSectionLevel function
************
Gets current @section level. 

Currently must be a function because global var is not always set and request attrib is messy. 

   * Parameters *
    useDefault      = default true; if true, if no heading defined, return default; else return void
-->
<#function getCurrentSectionLevel useDefault=true>
  <#local sLevel = getRequestVar("catoCurrentSectionLevel")!"">
  <#if sLevel?has_content>
    <#return sLevel>
  <#elseif useDefault>
    <#return getDefaultSectionLevel()>
  </#if>
</#function> 

<#function getDefaultSectionLevel>
  <#return 1>
</#function> 

<#-- 
*************
* setCurrentSectionLevel function
************
Set current @section level manually. For advanced markup, bypassing @section.
-->
<#function setCurrentSectionLevel sLevel>
  <#-- set as request attrib so survives template environments and screens.render -->
  <#local dummy = setRequestVar("catoCurrentSectionLevel", sLevel)>
  <#global catoCurrentSectionLevel = sLevel>
  <#return "">
</#function>

<#-- 
*************
* getCurrentHeadingLevel function
************
Gets current heading level. 

Currently must be a function because global var is not always set and request attrib is messy. 

   * Parameters *
    useDefault      = default true; if true, if no heading defined, return default; else return void
-->
<#function getCurrentHeadingLevel useDefault=true>
  <#local hLevel = getRequestVar("catoCurrentHeadingLevel")!"">
  <#if hLevel?has_content>
    <#return hLevel>
  <#elseif useDefault>
    <#return getDefaultHeadingLevel()>
  </#if>
</#function> 

<#function getDefaultHeadingLevel>
  <#return 2>
</#function> 

<#-- 
*************
* setCurrentHeadingLevel function
************
Set current heading level manually. For advanced markup, bypassing @section (but a parent
@section will restore heading upon closing).
-->
<#function setCurrentHeadingLevel hLevel>
  <#-- set as request attrib so survives template environments and screens.render -->
  <#local dummy = setRequestVar("catoCurrentHeadingLevel", hLevel)>
  <#global catoCurrentHeadingLevel = hLevel>
  <#return "">
</#function>


<#-- 
*************
* objectAsScript macro
************
Output a Freemarker variable as Javascript or JSON

DEV NOTE: This is complicated in Ofbiz because Maps and objects from
    widget/java/groovy context don't behave same as FTL types.
    Currently can't use ?keys on Maps and generally for most types gotten from
    groovy context, the ?is_ don't work like you'd expect (a lot of vars
    implement ?string and ?is_hash_ex that aren't really either of these, so leave for last)

TODO: doesn't handle dates (ambiguous?)
                    
   * Parameters *
    object          = the FTL or context object
    lang            = [js|json]
    wrap            = boolean, default true, if true, wrap in {}, [], "" as needed, otherwise omit
    hasMore         = boolean, default false, if true, always include trailing separator in hashes and arrays
    escape          = escape characters in strings
    maxDepth        = maximum depth or -1 for no limit
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


<#-- 
*************************************
* API-WRITING UTILITIES *
*************************************
* Intended to assist implementation of template macros.
-->

<#-- 
*************
* Class argument functions
************
Internal functions to help parse class argument passed to macros.
Don't use in templates.

There is problem with class argument. It can have essential and non-essential
defaults. The essentials can't be omitted. the non-essentials in some cases
you want to replace with value and other times you want to keep them but only
add extra classes. having multiple args for each elem class arg gets heavy.

so currently these functions do the following:
- accept boolean value for class. true means use non-essential defaults,
  false means don't use non-essential defaults
  should put class=true as macro default on macros.
  NOW ALSO accepts string repr of booleans.
- 2015-10-27: empty string now means use defaults, same as class=true.
  This is more consistent with other arguments in general 
- if class starts with "+", you only append additional classes to the defaults,
  never replace
- class now also supports being a list of strings that will be joined;
  first in list may be "+" to append-only
- 2015-11-03: now also supports prepending with "=". 
  in makeClassesArg, "=" is the same as specifying a value directly. 
  the "=" is actually a marker for the boolean value "false", so classes can be combined
  without losing the original "false" flag passed.
  
this should be intuitive

in one or two cases the non-essential defaults are "conditionally essential" 
so class=false makes little sense but we dont really need to handle that.
NOTE: not all macros support all the cases but the syntax is supported everywhere now anyway
so doesnt matter to templates.

makeClassesArg should usually be used, as late as possible in macro; defaultVal
can be complex to determine:
  <#local classes = makeClassesArg(class, "my-calculated-default")>
in some case need to split logical class and addClass like this, which can be done
earlier in macro:
  <#local addClass = parseAddClassArg(class)>
  <#local class = parseClassArg(class, "my-macro-default")>
where my-macro-default is what you'd have had as arg default in the macro def
-->

<#-- get combined classes string with additionals/boolean logic, use default as needed,
     explicit class overrides (non-essential) default -->
<#function makeClassesArg class defaultVal>
  <#if class?is_boolean>
    <#return class?string(defaultVal, "")>
  <#elseif class?is_sequence>
    <#if class?has_content>
      <#if class?first == "+">
        <#return joinStyleNames(defaultVal, joinStyleNamesList(class)?substring(1)?trim)>
      <#elseif class?first == "=">
        <#return joinStyleNamesList(class)?substring(1)?trim>
      <#else>
        <#return joinStyleNamesList(class)>
      </#if>
    <#else>
      <#return defaultVal>
    </#if>
  <#-- check string last because ?is_string doesn't always behave as expected -->
  <#elseif class?is_string>
    <#local class = class?trim> <#-- for convenience, trim the class here, though may hide minor errors -->
    <#if class?starts_with("+")>
      <#return (defaultVal + " " + class?substring(1))?trim>
    <#elseif class?starts_with("=")>
      <#return class?substring(1)> <#-- same as false but with an added class -->
    <#elseif class?has_content>
      <#if class == "true">
        <#return defaultVal>
      <#elseif class == "false">
        <#return "">
      <#else>
        <#return class>
      </#if>
    <#else>
      <#return defaultVal>
    </#if>
  <#else>
    <#-- invalid -->
    <#return class>
  </#if>
</#function>

<#-- extract additional classes from orig class string -->
<#function parseAddClassArg class>
  <#if class?is_string && class?starts_with("+")>
    <#return class?substring(1)>
  <#elseif class?is_sequence && class?has_content && class?first == "+">
    <#return joinStyleNamesList(class)?substring(1)?trim>
  <#else>
    <#return "">
  </#if>
</#function>

<#-- get class string (minus additionals) from orig class string -->
<#function parseClassArg class defaultVal>
  <#if class?is_boolean>
    <#if class>
      <#return defaultVal>
    <#else>
      <#return "">
    </#if>
  <#elseif class?is_sequence>
    <#if class?has_content>
      <#if class?first == "+">
        <#return defaultVal>
      <#elseif class?first == "=">
        <#return joinStyleNamesList(class)?substring(1)?trim>
      <#else>
        <#return joinStyleNamesList(class)>
      </#if>
    <#else>
      <#return defaultVal>
    </#if>
  <#elseif class?is_string> 
    <#if class?starts_with("+")>
      <#return defaultVal>
    <#elseif class?starts_with("=")>
      <#return class?substring(1)> <#-- same as false but with a value -->
    <#elseif class?has_content>
      <#if class == "true">
        <#return defaultVal>
      <#elseif class == "false">
        <#return "">
      <#else>
        <#return class>
      </#if>
    <#else>
      <#return defaultVal>
    </#if>
  <#else>
    <#-- invalid -->
    <#return class>
  </#if>
</#function>

<#-- incomplete
<#function isClassArgUseDefault class>
  <#return (class?is_string && class?starts_with("+")) || (class?is_boolean && class == false)>
</#function>-->


<#-- 
*************
* addClassArg function
************
This function takes template-level/logical cato macro "class" arg and adds to it the given class. 

TODO
-->



<#-- 
*************
* combineClassArgs functions
************
This function logically combines two template-level cato macro "class" arguments. 
The second almost always overrides the first, except when second is appending a class
with "+", in which case depends on the first.

Essentially it extends the intended use of the class arg to mean "+" also allows appending
of previous values (in addition of being an appending value itself).

NOTE: even if the second arg is merely "true" (which usually means "use defaults" for cato macros),
    this method will return true and dismiss the first argument. "true" is not treated as
    pass-through here. this does not seem consistent,
    but is chosen explicitly so that caller/macro decides what the "pass-through" value
    is supposed to be. here, we assume "true" was passed explicitly to override the first.
    we only treat "+".
    in other words, can see this as another kludge for lack of null values in freemarker.
-->
<#function combineClassArgs first second>
  <#if second?is_string && second?starts_with("+")>
    <#if first?is_string>
      <#if first == "false">
        <#local first = false>
      <#elseif first == "true">
        <#local first = true>
      </#if> 
    </#if>

    <#if first?is_boolean>
      <#if first>
        <#-- if first true, return second -->  
        <#return second>
      <#else>
        <#-- here, the first's attempt to remove class defaults with "false" cause the second's append to become a replacement -->
        <#return second?substring(1)>
      </#if>
    <#elseif !first?has_content>
      <#-- empty currently same as true -->
      <#return second>
    <#else>
      <#-- first has content; whether was absolute or appending, can simply append second here -->
      <#-- note: I think this handles the ?starts_with("=") case... -->
      <#return first + " " + second?substring(1)>
    </#if>
  <#else>
    <#return second>
  </#if>
</#function>

<#-- 
*************
* translateStyleStrClassesArg function
************
Translates a class arg from a string-only representation to a FTL value which can be passed as
macro args processed by makeClassesArg.

usually those macro args take true or false by default.

Needed for string repr of booleans and because empty string "" may have different meanings
depending on context. also translates booleans.

see makeClassesArg, results of getElemSpecFromStyleStr.
-->
<#function translateStyleStrClassesArg val>
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
* translateStyleStrBoolArg function
************
Translates a bool arg from a string-only representation to a FTL value which can be passed as
macro args expected to be booleans.

usually those macro args take "" by default, to mean default.

see makeClassesArg, results of getElemSpecFromStyleStr.
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
Now implemented as java transform.

   * Parameters *
    name        = global request stack var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
    val         = value
    
<#function pushRequestStack name val>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* readRequestStack function
************
Reads the last value added to the named global stack variable in request scope
(request attributes, or if no request, globals), without popping.
Now implemented as java transform.

   * Parameters *
    name        = global request stack var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)

<#function readRequestStack name>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* popRequestStack function
************
Pops a global stack variable in request scope (request attributes, or if no request, globals).
Now implemented as java transform.

   * Parameters *
    name        = global request stack var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
    
<#function popRequestStack name>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* setRequestVar function
************
Sets a global var in request scope (request attributes, or if no request, globals).
Values set by this method must be read using getRequestVar.
Now implemented as java transform.

   * Parameters *
    name        = global request var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
    val         = value
    mode        = optional, string; mode characters
                  "u": always unwrap the TemplateModel before storing (where possible)
                  "w": always keep TemplateModel as-is (wrapped) when storing
                  "u" and "w" are usually unnecessary and should be avoided in most template and macro code.
    
<#function setRequestVar name val mode="">
- implemented as java transform -
</#function>
-->

<#-- 
*************
* getRequestVar function
************
Gets a global var from request scope (request attributes, or if no request, globals).
Should only be used to read values set by setRequestVar.
Not meant to be used on regular request attributes.
Now implemented as java transform.

   * Parameters *
    name        = global request var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)

<#function getRequestVar name>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* globalPutAll function
************
Puts all key-value pairs from given map into FTL globals.
Now implemented as java transform.

   * Parameters *
    map         = the source map
    mode        = optional mode flags
                  "e": exclude listed keys
                  "i": include only listed keys
                  "d": include only directives (?is_directive)
    inExKeys    = optional list or wrapped set of keys to include or exclude           

<#function globalPutAll map mode="" inExKeys=[]>
- implemented as java transform -
</#function>
-->


<#-- 
*************
* elemAttribStr macro
************
Prints a string of element attributes. (HTML, FO, XML)
TODO: implement as transform instead

   * General Attributes *
    attribs         = hash of attribute-value pairs. 
                      it currently accepts string format as a fallback/legacy support, but this is highly discouraged
                      and other args won't work with it.
    alt             = boolean, if true alternate row (odd), if false regular (even)
    selected        = boolean, if true row is marked selected
    exclude         = list of attrib names to skip
-->
<#macro elemAttribStr attribs includeEmpty=false emptyValToken="" exclude=[]>
  <#if attribs?is_hash>
    <#t>${StringUtil.wrapString(Static["com.ilscipio.cato.webapp.ftl.CommonFtlUtil"].makeElemAttribStr(attribs, includeEmpty, emptyValToken, exclude))}
  <#elseif attribs?is_string>
    <#t> ${attribs?string}
  </#if>
</#macro>

<#-- 
*************
* getHeadingElemSpecFromStyleStr function
************
Parses a complex style string meant to describe an element notably heading 
in the following formats to a hash of values:
    containerElemType:containerClass;elemType:class;param1=val1,param2=val2
"h" and "heading" elem types support + notation for relative, and for these types
a level and relLevel are extracted.
  e.g.:  
    h3
    h3:headingclass
    h+3:headingclass
    div:divclass;h+3:headingclass
    div:divclass;h+3:headingclass;consumeLevel=true
Note that the class portions may be prefixed with "+" as well for append-not-replace logic.
-->
<#function getHeadingElemSpecFromStyleStr styleStr containerStyleStr allowedHeadingElemTypes allowedElemTypes allowedContainerElemTypes cacheId="">
    <#return Static["com.ilscipio.cato.webapp.ftl.CommonFtlUtil"].getHeadingElemSpecFromStyleStr(styleStr, containerStyleStr,
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
*************************************
* CONTEXT AND SYSTEM UTILITIES *
*************************************
* Context and system utilities.
-->

<#-- 
*************
* getRenderContextType function
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
* getRenderPlatformType function
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
* getDefaultCatoLibLocation function
************
supported names: "variables", "template"
note: this is currently render context-unaware
returns void if nothing.

   * Parameters *
    libName             = [variables|template]
    renderPlatformType  = just call getRenderPlatformType()
    renderContextType   = just call getRenderContextType()
-->
<#function getDefaultCatoLibLocation libName renderPlatformType="default" renderContextType="general">
  <#local res = getPropertyValue("catorender", "cato.templating.lib." + renderContextType + "." + renderPlatformType + "."  + libName  + ".location")!"">
  <#if res?has_content>
    <#return res>
  </#if>
  <#if renderContextType != "general">
    <#local res = getPropertyValue("catorender", "cato.templating.lib.general." + renderPlatformType  + "." + libName + ".location")!"">
    <#if res?has_content>
      <#return res>
    </#if>
  </#if>
  <#if renderPlatformType != "default">
    <#local res = getPropertyValue("catorender", "cato.templating.lib." + renderContextType + ".default." + libName + ".location")!"">
    <#if res?has_content>
      <#return res>
    </#if>
  </#if>
  <#local res = getPropertyValue("catorender", "cato.templating.lib.general.default." + libName + ".location")!"">
  <#if res?has_content>
    <#return res>
  </#if>
</#function>

<#-- 
*************
* getMacroLibraryLocationStaticFromResources function
************
Gets a lib location from a theme resources variable which contains an expression, which can be either a straight component:// location
meant as "general" context and for "html" and "default" platforms, or a EL-defined map in format such as:
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
* printVars macro
************
Iterates over all variable attributes & functions and prints in table; useful for determining current vars in context
NOTE: since is in utilities.ftl, keep generic and check platform.

Usage example:  
    <@printVars />           
                    
   * General Attributes *
    var           = Custom var to be printed (default:context)
    platform      = [html], default is do lookup
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

      <#attempt><#compress>
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
        
      </#compress>
      <#recover>
        <span style="color:red"><strong>${(.error)!"(generic)"}</strong></span>
      </#attempt>

</#if>
</#macro>

