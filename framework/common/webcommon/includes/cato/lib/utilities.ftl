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
* makeOfbizUrl
************
Function version of the @ofbizUrl macro.
Boolean arguments can be given as booleans, string representation of booleans
or empty string (signifies use defaults).
-->
<#function makeOfbizUrl uri fullPath=false secure=false encode=true>
  <#if fullPath?is_boolean><#local fullPath = fullPath?c><#elseif !fullPath?has_content><#local fullPath = "false"></#if>
  <#if secure?is_boolean><#local secure = secure?c><#elseif !secure?has_content><#local secure = "false"></#if>
  <#if encode?is_boolean><#local encode = encode?c><#elseif !encode?has_content><#local encode = "true"></#if>
  <#local res><@ofbizUrl fullPath=fullPath secure=secure encode=encode>${StringUtil.wrapString(uri)}</@ofbizUrl></#local>
  <#return res>
</#function>

<#-- 
*************
* makeOfbizContentUrl
************
Function version of the @ofbizContentUrl macro.
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

If the uri is already a web URL, it is returned as-is.
The following URI forms are currently interpreted and transformed:
 ofbizUrl:// - Any URI that begins with this will be interpreted as an ofbiz controller URL and ran through @ofbizUrl/makeOfbizUrl.
               Form (note: order of arguments is strict; args will be stripped): 
                 ofbizUrl://myRequest;fullPath=false;secure=false;encode=true?param1=val1
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
* label
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
* getPropertyValue
************
Gets property or empty string if missing (same behavior as UtilProperties).
note: the ?string kludge is to get rid of the wrapString wrapper which can break.
-->
<#function getPropertyValue resource name>
  <#return StringUtil.wrapString(Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue(resource, name))?string>
</#function>

<#-- 
*************
* getPropertyValueOrNull
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
* addParamDelimToUrl
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
* addParamsToStr
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
* splitStrParams
************
Extracts parameters from a string in the format and returns as a hash:
    name1=val1DELIMname2=val2DELIMname3=val3
where DELIM is specified delimiter (& &amp; , ; etc.)
                    
  * Parameters *
    paramStr        = Escaped param string
    paramDelim      = Param delimiter ("&amp;" by default)
-->
<#function splitStrParams paramStr paramDelim="&amp;">
  <#return Static["com.ilscipio.cato.webapp.ftl.template.TemplateFtlUtil"].splitStrParams(paramStr, paramDelim)>
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
* splitStyleNames
************
splits a style classes string into sequence, same order.
                    
  * Parameters *
    styleString     = style string containing classes
    
  * Return Value *
    a sequence of style names, same order.
-->
<#function splitStyleNames styleString>
  <#return getPlainClassArgNames(styleString)?split(r'\s+', 'r')>
</#function> 

<#-- 
*************
* splitStyleNamesToSet
************
splits a style classes string into a Set of unique elems, no order.
                    
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
Joins style names in a nice string
  * Usage Example *   
    <#assign myVar = joinStyleNames("class1", "", " class3")>
       
  * Parameters *
    styleNames     = style names (strings), as positional params.
    
  * Return Value *
    a string of combined style names
-->
<#function joinStyleNames styleNames...>
  <#return styleNames?join(" ")?trim>
</#function> 

<#function joinStyleNamesList styleNames>
  <#-- this is imperfect but fast and works fine in most cases, except cases where empty strings in middle, like
    ["asdf", "", "asdf"], adds extra spaces there, but rare -->
  <#return styleNames?join(" ")?trim>
</#function> 

<#-- 
*************
* containsStyleName
************
Returns true if class/style string contains given style.
NOTE: now recognizes special syntax cato class args.
                    
  * Parameters *
    styleString     = style string containing classes
    className       = name of class to find
    
  * Return Value *
    true if class/style string contains given style, false otherwise.
-->
<#function containsStyleName styleString className>
  <#-- don't need regexp -->
  <#return getPlainClassArgNames(styleString)?split(" ")?seq_contains(className)> 
</#function> 

<#-- 
*************
* removeStyleNames
************   
Removes style classes from a style string. 
strips lead/trailing space.
NOTE: now recognizes special syntax cato class args.
           
  * Parameters *
    styleString     = style string containing classes
    namesToRemove   = array of names or space-separated string of names to remove 
                      (can be single name)
  * Return Value *
    the style string with names removed, same order but reformatted.
-->
<#function removeStyleNames styleString namesToRemove>
  <#local prefix = getClassArgPrefix(styleString)>
  <#local styleString = getPlainClassArgNames(styleString)>
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
  <#return prefix + res?trim>
</#function> 

<#-- 
*************
* addParamsToUrl
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
* escapeUrlParamDelims
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
* camelCaseToDashLowerName
************
Converts camelCase to camel-case.
-->
<#function camelCaseToDashLowerName name>
    <#return StringUtil.wrapString(Static["com.ilscipio.cato.webapp.ftl.lang.LangFtlUtil"].camelCaseToDashLowerName(name))?string>
</#function>


<#-- 
*************
* isObjectType
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
* copyObject
************
Performs a shallow copy of an object (map or list). Usually not needed in FTL; for advanced usage.
The resulting underlying type may differ from the original, but by default will be similar.
Implemented as java transform.

<#function copyObject object>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* copyMap
************
Performs a shallow copy of a map. Usually not needed in FTL; for advanced usage.
The resulting underlying type may differ from the original; in principle tries to preserve, but in most
cases will create a simple hash.
Implemented as java transform.

NOTE: copyObject will also work fine on maps, but this has more map-specific options.

NOTE: This can only copy maps without ?keys support if mode is include ("i") and keys specified.

  * Parameters *
    map         = the source map
    mode        = optional mode flags
                  "e": exclude listed keys
                  "i": include only listed keys
    inExKeys    = optional list or wrapped set of keys to include or exclude    

<#function copyMap map mode inExKeys>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* toSimpleMap
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
* mapKeys
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
* concatMaps
************
Concatenates two maps similar to FTL "+" hash operator, but works with ofbiz maps as well.
By default, result is now always a simple map, so type is more predictable, and usually this is what we want.

Using "+" on screen context bean maps causes problems such as "hiding" of the bean map type underneath
an FTL wrapper, which wrecks all subsequent type checks. And others.

TODO? This is currently inefficient; but must guarantee immutability.
    Note currently forced to implement in FTL (not java transform) if want to exploit "+" operator.
    
IMPL NOTE: it's part of this function's interface that any of the arguments for which ?has_content is false,
    will not be used. this covers the cases where non-map types are passed as well ("", [], etc.).
    sometimes an empty non-map type will be passed, should be considered valid.
    shouldn't need to check for non-empty non-map types however, those are all clear coding errors.
    e.g., this is used at beginning of macros that take inlineArgs... or inlineAttribs...
    they count on this method to handle empty sequence case.
    they also depend on this for the toSimple=true conversion.

Also see mergeArgMaps.
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
          <#return Static["com.ilscipio.cato.webapp.ftl.lang.LangFtlUtil"].concatMaps(first, second)>
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
This is similar to toSimpleMap but if the object is a string, tries to ?eval it to a map.
Enclosing braces are optional.
TODO: this should be transform... maybe integrate into toSimpleMap, but then lose type safety,
and ?eval harder there.
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
* compileProgressSuccessAction
************
widget-related progress success action compile (see widget-form.xsd form element extra "attribs" attrib).
-->
<#function compileProgressSuccessAction progressSuccessAction>
  <#return StringUtil.wrapString(Static["com.ilscipio.cato.webapp.ftl.template.TemplateFtlUtil"].compileProgressSuccessAction(progressSuccessAction))?string>
</#function>

<#-- 
*************
* getCurrentSectionLevel
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
* setCurrentSectionLevel
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
* getCurrentHeadingLevel
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
* setCurrentHeadingLevel
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
* objectAsScript
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
* mergeArgMaps
************
Merges cato macro inlineArgs/args/defaultArgs/overrideArgs maps for macros implementing the 
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
the remaining "extra" arguments (analogous to extraArgs in FTL's <#macro extraArgs...>).
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
    args          = the args map (normal priority). may be any type of map.
    inlineArgs    = the macro's inline args (high priority). expects either FTL hash or empty sequence (but not non-empty sequence).
    defaultArgs   = arg defaults (lowest priority).expects an FTL hash only. the key names in this map are also used to
                    create a list of the known arguments this macro accepts.
                    as such, even if the macro does not need this map, it should pass it along
                    with the names of all supported arguments.
                    the names end up in the resulting map as localArgNames.
    overrideArgs  = extra macro args that override all others (highest priority). expects a FTL hash only.
                    the names used here also count toward the localArgNames.
                    NOTE: if a key is present in both defaultArgs and overrideArgs, it will result in
                        two names added to localArgNames/allArgNames. The code that finally uses
                        those lists should make a Set if it needs to.
                    NOTE: this map is present for cases where the others are insufficient.
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

<#-- a version of mergeArgMaps that only merges maps, nothing else -->
<#function mergeArgMapsBasic args={} inlineArgs={} defaultArgs={} overrideArgs={}>
  <#if !inlineArgs?has_content> <#-- necessary to prevent empty sequence -->
    <#local inlineArgs = {}>
  </#if>
  <#return defaultArgs + toSimpleMap(args) + inlineArgs + overrideArgs>
</#function>

<#function mergeArgMapsToLocals args={} inlineArgs={} defaultArgs={} overrideArgs={}>
  <#-- TODO: WRITE THIS AS A TRANSFORM - CANNOT BE IMPLEMENTED IN FTL
  this should be equivalent to doing:
    <#local args = mergeArgMaps(args, inlineArgs, defaultArgs, overrideArgs)>
    <#local dummy = localsPutAll(args)>   
  in the caller. but the localsPutAll part cannot be written in an FTL function.
  the other problem is can't exploit FTL hash concat from transforms.
  note this would still also need to return the args map, as well as dump it in locals.
  if could be done as one step, probably would help performance of these heavy functions.
   -->
</#function>

<#-- a version of mergeArgMaps that only merges maps, nothing else -->
<#function mergeArgMapsBasicToLocals args={} inlineArgs={} defaultArgs={} overrideArgs={}>
  <#-- TODO: basic version of mergeArgMapsToLocals -->
</#function>

<#-- 
*************
* mergeAttribMaps
************
Merges cato macro attribs/inlineAttribs/defaultAttribs/overrideAttribs maps for macros still implementing the 
  <#macro name arg1="" arg2="" ... argN="" attribs={} inlineAttribs...>
pattern, whereby inlineAttribs map is merged over the attribs map, and these maps only contain extra
element attributes.

This pattern is simpler but much less flexible than the 'args={} inlineArgs...' pattern.
They are not interchangeable.

  * Parameters *
    (see mergeArgMaps; parameters are analogous, even though macro implementation may differ)
-->
<#function mergeAttribMaps attribs={} inlineAttribs={} defaultAttribs={} overrideAttribs={}>
  <#if !inlineAttribs?has_content> <#-- necessary to prevent empty sequence -->
    <#local inlineAttribs = {}>
  </#if>
  <#return defaultAttribs + toSimpleMap(attribs) + inlineAttribs + overrideAttribs>
</#function>

<#-- 
*************
* Class argument functions
************
Internal functions to help parse class argument passed to macros.
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
  <#local class = addClassArgDefault("default-class")>
  <#local classes = compileClassArg(class)>
-->

<#-- Produces a simple list of class names from a class arg, adding optional default 
    NOTE: default may also be set prior using addClassArgDefault -->
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

<#-- produces a class string attribute at same time as compiling class arg, with optional default,
     with leading space
    NOTE: default may also be set prior using addClassArgDefault -->
<#macro compiledClassAttribStr class defaultVal="">
  <#local classes = compileClassArg(class, defaultVal)>
  <#if classes?has_content> class="${classes}"</#if><#t>
</#macro>

<#-- Converts simple class name to an appending class -->
<#function toClassArgAppending newClass>
  <#if newClass?has_content>
    <#return "+" + newClass>
  <#else>
    <#return "+">
  </#if>
</#function>

<#-- Converts simple class name to a replacing class -->
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
These functions take a template-level/logical cato macro "class" arg and add to it the given class.
Should be called by the implementing macros only. 

Depending on the previous value in class, non-essential (default) values may simply be discarded, but
essentials (required) will always be added in some way, transforming the class value.

The newClass parameter is a simple string literal and is not interpreted.

These are non-destructive except for addClassArgReplacing which always causes the string to become
a replacing string ("=").
-->

<#-- Adds a required class, that must appear in class string.
    Note: Does not influence the default value logic perceived by addClassArgDefault (user intention preserved). 
        It does this by prefixing result with "+" string where necessary. -->
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

<#-- Special case of addClassArg where the required class will become a replacing string ("=" prefix),
     though will not squash previous values. 
     Note: this destroys information about what macro user requested and affects the default value logic
        perceived by addClassArgDefault. -->
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

<#-- Adds a class only if the class arg does not contain default-replacing classes. 
    It adds default if class is true, empty or starts with "+". -->
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
* combineClassArgs
************
This function logically combines two template-level cato macro "class" arguments. 
The second almost always overrides the first, except when second is appending a class
with "+", in which case depends on the first.

Essentially it extends the intended use of the class arg to mean "+" also allows appending
of previous values (in addition of being an appending value itself).

NOTE: even if the second arg is merely "+" (which usually means "use defaults" for cato macros),
    this method will return "+" and dismiss the first argument. "+" is not treated as
    pass-through here. this does not seem consistent,
    but is chosen explicitly so that caller/macro decides what the "pass-through" value
    is supposed to be. here, we assume "+" was passed explicitly to override the first.
    we only treat "+" with an arg.
    in other words, can see this as another kludge for lack of null values in freemarker.
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

<#function getPlainClassArgNames class>
  <#if class?starts_with("+") || class?starts_with("=")>
    <#return class?substring(1)>
  <#else>
    <#return class>
  </#if>    
</#function>

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

usually those macro args take true or false by default.

Needed for string repr of booleans and because empty string "" may have different meanings
depending on context. also translates booleans.

see compileClassArg, results of getElemSpecFromStyleStr.
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

usually those macro args take "" by default, to mean default.

see compileClassArg, results of getElemSpecFromStyleStr.
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
* popRequestStack
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
* setLastRequestStack
************
Same as doing popRequestStack + pushRequestStack, but will never fail if stack is empty - will simply
do a pushRequestStack, and much more efficient.
Now implemented as java transform.

  * Parameters *
    name        = global request stack var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
    val         = value
    
<#function setLastRequestStack name val>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* readRequestStack
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
* getRequestStackAsList
************
Gets a copy of the named request stack as a list (read-only).
Now implemented as java transform.

  * Parameters *
    name        = global request stack var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
    listType    = [copy|orig], default copy.
                  caller may specify "orig" to avoid a list copy.
                  WARN: "orig" means the caller must ditch the list as soon as possible, before any
                      more modifications to the stack; otherwise results will be unpredictable.
                      It should only be used for optimization.

<#function getRequestStackAsList name listType>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* setRequestVar
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
* getRequestVar
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
* globalsPutAll
************
Puts all key-value pairs from given map into FTL globals (#global).
Now implemented as java transform.

  * Parameters *
    map         = the source map
    mode        = optional mode flags
                  "e": exclude listed keys
                  "i": include only listed keys
                  "d": include only directives (?is_directive)
    inExKeys    = optional list or wrapped set of keys to include or exclude           

<#function globalsPutAll map mode="" inExKeys=[]>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* varsPutAll
************
Puts all key-value pairs from given map into FTL current namespace variables (#assign).
Now implemented as java transform.

  * Parameters *
    @see globalsPutAll       

<#function varsPutAll map mode="" inExKeys=[]>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* localsPutAll
************
Puts all key-value pairs from given map into FTL globals (#local).
Now implemented as java transform.

  * Parameters *
    @see globalsPutAll        

<#function localsPutAll map mode="" inExKeys=[]>
- implemented as java transform -
</#function>
-->


<#-- 
*************
* elemAttribStr
************
Prints a string of element attributes. (HTML, FO, XML)
TODO: implement directly as transform instead.
NOTE: this is a very generic function; for common implementation, see commonElemAttribStr.

  * Parameters *
    attribs         = hash of attribute-value pairs. 
                      it currently accepts string format as a fallback/legacy support, but this is highly discouraged
                      and other args won't work with it.
    alt             = boolean, if true alternate row (odd), if false regular (even)
    selected        = boolean, if true row is marked selected
    exclude         = list of attrib names to skip
    attribNamePrefix      = add this prefix to attribute names
    alwaysAddPrefix       = if false, only add prefix if not already has prefix (default true)
    attribNamePrefixStrip = remove this prefix from all attrib names
    attribNameSubstitutes = map of attrib names to substitute attrib names. note if this is set, the exclude names
                            should be the names of the subtitutes, not the input attrib names.
                            note this is applied after prefix ops are applied.
    camelCaseToDashLowerNames = boolean, if true converts attrib names from camelCase to camel-case at the very end.
-->
<#macro elemAttribStr attribs includeEmpty=false emptyValToken="" exclude=[] 
  attribNamePrefix="" alwaysAddPrefix=true attribNamePrefixStrip="" attribNameSubstitutes={} camelCaseToDashLowerNames=false>
  <#if attribs?is_hash>
    <#t>${StringUtil.wrapString(Static["com.ilscipio.cato.webapp.ftl.template.TemplateFtlUtil"].makeElemAttribStr(attribs, includeEmpty, 
      emptyValToken, exclude, attribNamePrefix, alwaysAddPrefix, attribNamePrefixStrip, attribNameSubstitutes, camelCaseToDashLowerNames))}<#t>
  <#elseif attribs?is_string>
    <#t> ${attribs?string}
  </#if>
</#macro>

<#-- 
*************
* getHeadingElemSpecFromStyleStr
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
    <#return Static["com.ilscipio.cato.webapp.ftl.template.TemplateFtlUtil"].getHeadingElemSpecFromStyleStr(styleStr, containerStyleStr,
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
  <#return Static["com.ilscipio.cato.webapp.ftl.template.TemplateFtlUtil"].extractPrefixedStyleNamesWithInt(style, prefixMap)>
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
* parseContainerSizesFromStyleStr [PLACEHOLDER]
************   
This function should be overridden by a framework-specific implementation that parses the 
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
This returns a map of container size factors along the format:
{"large":n1, "medium":n2, "small":n3}
where n1, n2, n3 are floating-point (NOT integer) values calculated from the combination of all
grid sizes of all the current parent containers.
NOTE: this is only an example; this method is framework-agnostic.

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
    maxSizes     = hash/map of per-key max container sizes OR a single number giving max size
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
* evalAbsContainerSizeFactors [PLACEHOLDER]
************   
This function should be overridden by a framework-specific implementation that evals container
size factors 
           
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
* getMacroLibraryLocationStaticFromResources
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
* printVars
************
Iterates over all variable attributes & functions and prints in table; useful for determining current vars in context
NOTE: since is in utilities.ftl, keep generic and check platform.

  * Usage Example *  
    <@printVars />           
                    
  * Parameters *
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

