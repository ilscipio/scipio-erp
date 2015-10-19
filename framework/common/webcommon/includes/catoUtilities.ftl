<#--
* 
* A set of standalone utility functions and macros, largely devoid of markup and unrelated to templating macros and with minimal dependencies, 
* part of standard Cato Freemarker API.
* Generally CSS-framework-agnostic. 
* Intended as platform-agnostic (html, fo, etc.) though some individually are only applicable for specific platforms.
* Automatically included at all times.
*
* NOTE: In general, macros expect to be called using named arguments (not supported for functions),
*     except where otherwise noted.
* NOTE: Default markup-producing macros are found in catoHtmlTemplateDefault.ftl.
*     Utilities found in catoUtilities.ftl should not contain their logic in general (TODO?: there could be a catoHtmlTemplateHelpers.ftl to isolate logic from markup).
*  
* IMPL NOTE: Macros should avoid using "request" directly (use setRequestVar/getRequestVar/other).
*
* DEV NOTE: for performance, some of these could probably later be turned into freemarker transforms (java) or
*     delegate to java methods.
* DEV NOTE: freemarker functions (and java calls) don't support named arguments so in some cases 
*     macros are easier/better to use even in this file (but macro calls don't inline well in other macro/function calls, 
*     so give and take).
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
  <#list paramMap?keys as key>
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
-->
<#macro objectAsScript object lang wrap=true hasMore=false escape=true>
    <#if object?is_hash && object.keySet?? && object.keySet?is_method>
        <#-- Map from java/groovy; doesn't work properly with ?keys even though implements ?is_hash_ex -->
        <#if wrap>{</#if><#lt>
        <#list object.keySet() as key>
            "${escapeScriptString(lang, key, escape)}}" : <#if object[key]??><@objectAsScript lang=lang object=object[key] wrap=true escape=escape /><#else>null</#if><#if key_has_next || hasMore>,</#if>
        </#list> 
        <#if wrap>}</#if><#rt>
    <#elseif object?is_enumerable> 
        <#-- check this early because some of these from groovy 
             also implement ?string and ?is_hash_ex at same time (?).
             but usually for those if ?is_enumerable it means it was a list-like type. -->
        <#if wrap>[</#if><#lt>
        <#list object as item> 
            <#if item??><@objectAsScript lang=lang object=item wrap=true escape=escape /><#else>null</#if><#if item_has_next || hasMore>,</#if>
        </#list> 
        <#if wrap>]</#if><#rt>
    <#elseif object?is_number> 
        ${object}<#t>
    <#elseif object?is_boolean>
        ${object?c}<#t>
    <#elseif object?is_date_like>
        <#-- TODO? -->
        <#if wrap>"${escapeScriptString(lang, object?string, escape)}"<#else>${escapeScriptString(lang, object?string, escape)}</#if><#t>
    <#elseif object?is_hash_ex && !object?is_string> 
        <#-- check last because a lot of things implement ?is_hash_ex you might not expect - including strings... -->
        <#if wrap>{</#if><#lt>
        <#list object?keys as key> 
            "${escapeScriptString(lang, key, escape)}" : <#if object[key]??><@objectAsScript lang=lang object=object[key] wrap=true escape=escape /><#else>null</#if><#if key_has_next || hasMore>,</#if>
        </#list> 
        <#if wrap>}</#if><#rt>
    <#-- the following are invalid/inconvertible types, but catch them because otherwise debugging impossible -->
    <#elseif objectAsJsonTreatInvalidNonFatal && object?is_hash && !object?is_string> 
        "__NONEXHASH__"<#t>
    <#elseif objectAsJsonTreatInvalidNonFatal && object?is_method>
        "__METHOD__"<#t>
    <#elseif objectAsJsonTreatInvalidNonFatal && object?is_directive>
        "__DIRECTIVE__"<#t>
    <#elseif object?is_string>
        <#if wrap>"${escapeScriptString(lang, object?string, escape)}"<#else>${escapeScriptString(lang, object?string, escape)}</#if><#t>
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

<#global objectAsJsonTreatInvalidNonFatal = true>


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
- empty string means don't use non-essentail defaults, same as class=false
  (i.e. if you specify class="" it's like saying "I want empty class")
  could change this and use class=false only... tbd
- if class starts with "+", you only append additional classes to the defaults,
  never replace
- class now also supports being a list of strings that will be joined;
  first in list may be "+" to append-only
  
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
      <#else>
        <#return joinStyleNamesList(class)>
      </#if>
    <#else>
      <#return "">
    </#if>
  <#-- check string last because ?is_string doesn't always behave as expected -->
  <#elseif class?is_string>
    <#local class = class?trim> <#-- for convenience, trim the class here, though may hide minor errors -->
    <#if class?starts_with("+")>
      <#return (defaultVal + " " + class?substring(1))?trim>
    <#else>
      <#return class>
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
  <#elseif class?is_string>
    <#if class?starts_with("+")>
      <#return defaultVal>
    <#elseif class?has_content>
      <#return class>
    <#else>
      <#return "">
    </#if>
  <#elseif class?is_sequence>
    <#if class?has_content>
      <#if class?first == "+">
        <#return defaultVal>
      <#else>
        <#return joinStyleNamesList(class)>
      </#if>
    <#else>
      <#return "">
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
* translateStyleStrClassesArg function
************
Translates a class arg from a string-only representation to a FTL value which can be passed as
macro args processed by makeClassesArg.

usually those macro args take true or false by default.

Needed for string repr of booleans and because empty string "" in style string means "not specified"
and not "omit" as usually specified on macros. also translates booleans.

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

   * Parameters *
    name        = global request var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)
    val         = value
    
<#function setRequestVar name val>
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

   * Parameters *
    name        = global request var name; must be unique 
                  across all known types of contexts (request attribs, screen context, FTL globals)

<#function getRequestVar name>
- implemented as java transform -
</#function>
-->

<#-- 
*************
* elemAttribStr macro
************
Prints a string of element attributes. (HTML, FO, XML)

   * General Attributes *
    attribs         = hash of attribute-value pairs. 
                      it currently accepts string format as a fallback/legacy support, but this is highly discouraged
                      and other args won't work with it.
    alt             = boolean, if true alternate row (odd), if false regular (even)
    selected        = boolean, if true row is marked selected
    exclude         = list of attrib names to skip
-->
<#macro elemAttribStr attribs includeEmpty=false emptyValToken="" exclude=[]>
  <#if attribs?is_hash_ex>
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
* getCatoLibLocationFromExpr function
************
Gets a lib location from an expression, which can be either a straight component:// location
meant as "general" context and for "html" and "default" platforms, or a JSON-like map in string format following:
{'[platform]':'component://...', ...}
e.g. {'html':'component://...', 'xml':'component://...', 'default':'component://...'}
Intended for use with VT_STL_VAR_LOC and VT_STL_TMPLT_LOC.
-->
<#function getCatoLibLocationFromExpr locationExpr renderPlatformType="default">
<#if locationExpr?has_content>
  <#if locationExpr?is_string>
    <#local locationExpr = locationExpr?trim>
    <#if locationExpr?has_content>
      <#if locationExpr?starts_with("{")>
        <#-- a json-like map -->
        <#local locationExpr = locationExpr?eval>
      <#else>
        <#-- simple location. meant for platform "html" mostly, 
             but for now simply always return even for unrelated platforms (also acts as "default"). -->
        <#return locationExpr>
      </#if>
    </#if>
  </#if>
  <#if locationExpr?is_hash>
    <#local res = locationExpr[renderPlatformType]!"">
    <#if res?has_content>
      <#return res>
    </#if>
    <#if renderPlatformType != "default">
      <#local res = locationExpr["default"]!"">
      <#if res?has_content>
        <#return res>
      </#if>
    </#if>
  </#if>
</#if>
</#function>

<#-- 
*************************************
* DEV UTILITIES *
*************************************
* For development and debugging purposes.
-->

<#-- (currently none) -->
