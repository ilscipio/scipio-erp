<#--
* 
* A set of standalone utility functions and macros, largely devoid of markup and unrelated to templating macros and with minimal dependencies, 
* part of standard Cato Freemarker API.
* Generally CSS-framework-agnostic. 
* Intended as platform-agnostic (html, fo, etc.) though some individually are only applicable for specific platforms.
* Automatically included at all times.
*
* NOTE: Default markup-producing macros are found in catoHtmlTemplateDefault.ftl.
*       Utilities found in catoUtilities.ftl should not contain their logic in general (TODO?: there could be a catoHtmlTemplateHelpers.ftl to isolate logic from markup).
* NOTE: Macros should avoid using "request" directly (use setRequestVar/getRequestVar/other).
* 
* DEV NOTE: for performance, some of these could probably later be turned into freemarker transforms (java) or
* delegate to java methods.
* DEV NOTE: freemarker functions (and java calls) don't support named arguments so in some cases 
* macros are easier/better to use even in this file (but macro calls don't inline well in other macro/function calls, 
* so give and take).
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
-->
<#macro requireScriptOfbizUrl uri htmlwrap=false>
  <#local requiredScriptOfbizUrls = getRequestVar("requiredScriptOfbizUrls", [])>
  <#if !requiredScriptOfbizUrls?seq_contains(uri)>
    <#if htmlwrap>
<script language="JavaScript" type="text/javascript">
<!-- //
    </#if>
    commonOfbizUrls["${uri}"] = "<@ofbizUrl>${uri}</@ofbizUrl>";
    <#if htmlwrap>
// -->
</script>
    </#if>
    <#-- FIXME: inefficient -->
    <#local requiredScriptOfbizUrls = requiredScriptOfbizUrls + [uri]>
    <#local dummy = setRequestVar("requiredScriptOfbizUrls", requiredScriptOfbizUrls)>
  </#if>
</#macro>

<#-- 
*************
* getCurrentSectionLevel function
************
Gets current @section level. 

Currently must be a function because global var is not always set and request attrib is messy. 

   * Parameters *
    defaultVal      = default number to return if no section level define, or boolean:
                      true: return cato default number value, false: return nothing (empty string)
                      template code should leave this to true.
-->
<#function getCurrentSectionLevel defaultVal=true>
  <#local sLevel = getRequestVar("catoCurrentSectionLevel", "")>
  <#if !sLevel?has_content>
    <#if defaultVal?is_boolean>
      <#if defaultVal>
        <#local sLevel = 1>
      </#if>
    <#else>
      <#local sLevel = defaultVal>
    </#if>
  </#if>
  <#return sLevel>
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
    defaultVal      = default number to return if no section level define, or boolean:
                      true: return cato default number value, false: return nothing (empty string)
                      template code should leave this to true.
-->
<#function getCurrentHeadingLevel defaultVal=true>
  <#local hLevel = getRequestVar("catoCurrentHeadingLevel", "")>
  <#if !hLevel?has_content>
    <#if defaultVal?is_boolean>
      <#if defaultVal>
        <#local hLevel = 2>
      </#if>
    <#else>
      <#local hLevel = defaultVal>
    </#if>
  </#if>
  <#return hLevel>
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
* objectAsJson macro
************
Outputs a Freemarker variable as JSON.

DEV NOTE: This is complicated in Ofbiz because Maps and objects from
    widget/java/groovy context don't behave same as FTL types.
    Currently can't use ?keys on Maps and generally for most types gotten from
    groovy context, the ?is_ don't work like you'd expect (a lot of vars
    implement ?string and ?is_hash_ex that aren't really either of these, so leave for last)

FIXME: escaping? (var?json)?
-->
<#macro objectAsJson object wrap=true hasMore=false> 
    <#if object?is_hash && object.keySet?? && object.keySet?is_method>
        <#-- Map from java/groovy; doesn't work properly with ?keys even though implements ?is_hash_ex -->
        <#if wrap>{</#if><#lt>
        <#list object.keySet() as key> 
            "${key}" : <@objectAsJson object=object[key] wrap=true /><#if key_has_next || hasMore>,</#if>
        </#list> 
        <#if wrap>}</#if><#rt>
    <#elseif object?is_enumerable> 
        <#-- check this early because some of these from groovy 
             also implement ?string and ?is_hash_ex at same time (?).
             but usually for those if ?is_enumerable it means it was a list-like type. -->
        <#if wrap>[</#if><#lt>
        <#list object as item> 
            <@objectAsJson object=item wrap=true /><#if item_has_next || hasMore>,</#if>
        </#list> 
        <#if wrap>]</#if><#rt>
    <#elseif object?is_number> 
        ${object}<#t>
    <#elseif object?is_boolean>
        ${object?c}<#t>
    <#elseif object?is_hash_ex && !object?is_string> 
        <#-- check last because a lot of things implement ?is_hash_ex you might not expect - including strings... -->
        <#if wrap>{</#if><#lt>
        <#list object?keys as key> 
            "${key}" : <@objectAsJson object=object[key] wrap=true /><#if key_has_next || hasMore>,</#if>
        </#list> 
        <#if wrap>}</#if><#rt>
    <#else>
        <#-- ?is_string and fallback. ?string last because implemented by other types unexpectedly. -->
        <#if wrap>"${object?string}"<#else>${object?string}</#if><#t>
    </#if> 
</#macro> 


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
  <#elseif class?is_string>
    <#if class?starts_with("+")>
      <#return (defaultVal + " " + class?substring(1))?trim>
    <#else>
      <#return class>
    </#if>
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
* createStack
************
Creates a stack. Often not necessary.
FTL kludge for special use cases and internal use.
WARNING: stack type/format is subject to change; assume unknown.
    Usage example:  
     <#global myGlobalStack = createStack()>
-->
<#function createStack>
  <#return []>
</#function>

<#-- 
*************
* pushStack
************
Pushes a value onto the given stack. The stack will be modified in-place _if possible_ and returned, but
return value should always be used instead of the original to guarantee function.
If variable is a non-stack, will be created as necessary.
FTL kludge for special use cases and internal use.
WARNING: stack type/format is subject to change; assume unknown.
    Usage example:  
     <#global myGlobalStack = pushStack(myGlobalStack!, {"attr1": "val1"})>
-->
<#function pushStack stack val>
  <#-- FIXME: currently uses an FTL list, this is highly suboptimal... -->
  <#if stack?has_content>
    <#local stack = stack + [val]>
  <#else>
    <#local stack = [val]>
  </#if>
  <#return stack>
</#function>

<#-- 
*************
* readStack function
************
Reads the last value from the given stack. The stack is not modified.
FTL kludge for special use cases and internal use.
WARNING: stack type/format is subject to change; assume unknown.
    Usage example:  
     <#local val = readStack(myGlobalStack!)>
-->
<#function readStack stack defaultVal="">
  <#if stack?has_content>
    <#return stack?last>
  <#else>
    <#return defaultVal>
  </#if>
</#function>

<#-- 
*************
* popStack function
************
Pops the given stack. The stack will be modified in-place _if possible_ and returned, but
return value should always be used instead of the original to guarantee function. 
FTL kludge for special use cases and internal use. Use readStack to get value.
WARNING: stack type/format is subject to change; assume unknown.
    Usage example:  
     <#global myGlobalStack = popStack(myGlobalStack!)>
-->
<#function popStack stack>
  <#-- FIXME: this is highly suboptimal way to use stack... -->
  <#if stack?has_content && (stack?size > 1)>
    <#local stackSize = stack?size>
    <#return stack?chunk(stackSize - 1)?first>
  <#else>
    <#return []>
  </#if>
</#function>

<#-- 
*************
* pushRequestStack
************
Pushes a value onto a global stack variable in request scope (request attributes, or if no request, FTL globals).

Dev note: currently no pushGlobalStack version of this exists for FTL globals only; 
can be achieved with pushStack/readStack/popStack instead, but in most cases pushRequestStack is better anyhow.

TODO: this is highly suboptimal; should move much of this to a java method.
-->
<#function pushRequestStack stackName val>
<#if request??>
  <#local stack = request.getAttribute(stackName)!"">
  <#if stack?has_content>
    <#local stack = stack + [val]>
  <#else>
    <#local stack = [val]>
  </#if>
  <#local dummy = request.setAttribute(stackName, stack)!>
  <#return val>
<#else>
  <#-- fallback to globals -->
  <#local stack = .globals[stackName]!"">
  <#if stack?has_content>
    <#local stack = stack + [val]>
  <#else>
    <#local stack = [val]>
  </#if>
  <@"<#global ${stackName}=stack>"?interpret />
  <#return val>
</#if>
</#function>

<#-- 
*************
* readRequestStack function
************
Reads the last value added to the named global stack variable in request scope
(request attributes, or if no request, FTL globals), without popping.
-->
<#function readRequestStack stackName defaultVal="">
<#if request??>
  <#local stack = request.getAttribute(stackName)!"">
  <#if stack?has_content>
    <#return stack?last>
  <#else>
    <#return defaultVal>
  </#if>
<#else>
  <#-- fallback to globals -->
  <#local stack = .globals[stackName]!"">
  <#if stack?has_content>
    <#return stack?last>
  <#else>
    <#return defaultVal>
  </#if>
</#if>
</#function>

<#-- 
*************
* popRequestStack function
************
Pops a global stack variable in request scope (request attributes, or if no request, FTL globals).
note: differs from popStack, which returns the stack.

TODO: this is highly suboptimal; should move much of this to a java method.
-->
<#function popRequestStack stackName defaultVal="">
<#if request??>
  <#local stack = request.getAttribute(stackName)!""> <#-- should be list -->
  <#if stack?has_content>
    <#local res = stack?last>
    <#local stackSize = stack?size>
    <#if (stackSize > 1)>
      <#local dummy = request.setAttribute(stackName, stack?chunk(stackSize - 1)?first)!>
    <#else>
      <#local dummy = request.removeAttribute(stackName)!>
    </#if>
  <#else>
    <#local res = defaultVal>
  </#if>
  <#return res>
<#else>
  <#-- fallback to globals -->
  <#local stack = .globals[stackName]!"">
  <#if stack?has_content>
    <#local res = stack?last>
    <#local stackSize = stack?size>
    <#if (stackSize > 1)>
      <#local newStack = stack?chunk(stackSize - 1)?first>
      <@"<#global ${stackName}=newStack>"?interpret />
    <#else>
      <@"<#global ${stackName}=''>"?interpret />
    </#if>
  <#else>
    <#local res = defaultVal>
  </#if>
  <#return res>
</#if>
</#function>

<#-- 
*************
* setRequestVar function
************
Sets a global var in request scope (request attributes, or if no request, FTL globals).
-->
<#function setRequestVar varName val>
<#if request??>
  <#local dummy = request.setAttribute(varName, val)!>
<#else>
  <#-- fallback to globals -->
  <@"<#global ${varName}=val>"?interpret />
</#if>
  <#return val>
</#function>

<#-- 
*************
* getRequestVar function
************
Gets a global var from request scope (request attributes, or if no request, FTL globals).
-->
<#function getRequestVar varName defaultVal="">
<#if request??>
  <#return request.getAttribute(varName)!defaultVal>
<#else>
  <#-- fallback to globals -->
  <#return .globals[varName]!defaultVal>
</#if>
</#function>

<#-- 
*************
* elemAttribStr macro
************
Prints a string of element attributes.
-->
<#macro elemAttribStr attribs includeEmpty=false emptyValToken="">
  <#if attribs?is_hash_ex>
    <#if includeEmpty>
      <#t><#list attribs?keys as name> ${name}="${attribs[name]?string}"</#list>
    <#elseif emptyValToken?has_content>
      <#t><#list attribs?keys as name><#if attribs[name]?has_content || emptyValToken?string == attribs[name]?string> ${name}="${attribs[name]?string}"</#if></#list>
    <#else>
      <#t><#list attribs?keys as name><#if attribs[name]?has_content> ${name}="${attribs[name]?string}"</#if></#list>
    </#if>
  </#if>
</#macro>


<#-- 
*************************************
* DEV UTILITIES *
*************************************
* For development and debugging purposes.
-->

<#-- (currently none) -->
