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

NOTE: 2016-11-14: Scope handling is revamped for consistency.
    A new {{{shareScope}}} parameter is available to control it.
    The default {{{shareScope}}} for ALL types is now {{{false}}}, meaning the context stack gets pushed/poped.
    The logical implemented defaults for type {{{screen}}} AND {{{section}}} have CHANGED to {{{false}}}; 
    they were previously {{{true}}} and this is probably a flaw in the original ofbiz handling of screens
    and sections in ftl files (their XML equivalents perform stack pushing), though it rarely manifests as
    an issue due to the ftl bindings being copies from context.

NOTE: 2017-09-10: If both {{{resource}}} and {{{name}}} are empty, this macro simply returns nothing, without
    producing any error. This is to make it more friendly to template code, so that surrounding #if statements
    can be safely omitted.

TODO: Reimplement as transform.

  * Parameters *
    resource                = ((string)) The resource identifier, with format depending on type
                              * {{{screen}}}: path and name, or path alone
                                Examples:
                                  "component://common/widget/CommonScreens.xml#listLocales"
                                  "component://common/widget/CommonScreens.xml"
    name                    = ((string)) A resource name part, if not already included in the resource
                              If there is no path for the type or path is optional, then name alone should be specified.
    type                    = (screen|menu|form|tree|decorator|section|ftl, default: -dependent on resource-, fallback default: screen) The type of resource to render
                              * {{{screen}}}: an Ofbiz screen (widget) by {{{component://}}} location
                                NOTE: this does not go through {{{include-screen}}} element - use {{{include-screen}}} to force that if needed for some reason
                                NOTE: this is the default in most cases, but not all.
                              * {{{screen-widget}}}: an Ofbiz screen (widget) by {{{component://}}} location - 
                                same as {{{screen}}} but using alternate inclusion method using xml {{{include-screen}}}
                              * {{{menu}}} or {{{include-menu}}}: an Ofbiz menu (widget) by {{{component://}}} location
                              * {{{form}}} or {{{include-form}}}: an Ofbiz form (widget) by {{{component://}}} location
                              * {{{tree}}} or {{{include-tree}}}: an Ofbiz tree (widget) by {{{component://}}} location
                              * {{{section}}}: an Ofbiz screen (widget) decorator section, with {{{name}}} arg
                              * {{{decorator}}} (WORK-IN-PROGRESS): an Ofbiz decorator-screen.
                                the {{{sections}}} parameter maps decorator-section names to Freemarker code to be included.
                              * {{{ftl}}}: special standalone isolated Freemarker template include mode. {{{resource}}} is 
                                expected to point to an FTL file. this differs from the Freemarker #include command in that
                                the FTL is processed in a standard ofbiz way similar to a screen, the context stack is by default pushed/pop
                                around the include (unless context does not support), and the template gets its own variable binding environment
                                so it does not interfere with other templates. You cannot use this to reuse definitions
                                from other FTL files; use #include for that. for more advanced options or 
                                to render inline strings as templates, try #interpretStd instead.
                                NOTE: if type is omitted and resource ends with the extension ".ftl", this type is implied; allows brevity.
                              NOTE: screen, menu, form and tree (xxx) can be given a {{{include-}}} prefix. The {{{include-}}} version
                                  guarantees that the include will be processed using the XML {{{include-xxx}}} element. 
                                  The non-{{{include-}}} versions may be implemented using other means
                                  and may be more efficient, but sometimes it may be needed to force the include mechanism.
    ctxVars                 = ((map), default: -empty-) A map of screen context vars to be set before the invocation
                              WARN: For {{{type="section"}}}, {{{ctxVars}}} may not work as expected; you may have to pass
                                  {{{globalCtxVars}}} instead, due to issues with scoping and nesting.
                                  {{{globalCtxVars}}} will work in most cases, but unfortunately they are overridden
                                  by the invoked's sections local vars, so they can't be used to provide overrides.
                              NOTE: Currently, this uses #setContextField. To set null, the key values may be set to a special null-representing
                                  object found in the global {{{scipioNullObject}}} variable.
    globalCtxVars           = ((map), default: -empty-) A map of screen global context vars to be set before the invocation
                              NOTE: Currently, this uses #setGlobalContextField. To set null, the key values may be set to a special null-representing
                                  object found in the global {{{scipioNullObject}}} variable.
    reqAttribs              = ((map), default: -empty-) A map of request attributes to be set before the invocation
                              NOTE: Currently, this uses #setRequestAttribute. To set null, the key values may be set to a special null-representing
                                  object found in the global {{{scipioNullObject}}} variable.
    restoreValues           = ((boolean), default: true) If true, the original values are saved and restored after invocation
                              NOTE: 2016-07-29: The default for this parameter has been changed to {{{true}}}.
    clearValues             = ((boolean), default: false) If true, the passed request attributes and context vars are removed (or set to null) after invocation
    asString                = ((boolean), default: false) If true, the render will render to a string like a regular FTL macro; otherwise goes straight to Ofbiz's writer
                              In stock Ofbiz, which is also current Scipio default behavior (for compabilitity and speed), render calls go directly to writer, 
                              which is faster but cannot be captured using freemarker {{{#assign}}} directive. If you need to capture
                              output of @render, pass true here.
                              NOTE: not supported for {{{type="section"}}} as this time.
                              TODO: implement for section
    shareScope              = ((boolean), default: false) Whether context modifications by the widget should be shared with caller (no stack push) or discarded (stack push)
                              NOTE: 2016-11-14: As of now the logical defaults for all types are {{{false}}}; prior to this,
                                  the specific types {{{screen}}} and {{{section}}} had logical defaults of true,
                                  which were probably errors in the original ofbiz. In Scipio, a consistent
                                  default of {{{false}}} is now implemented for all types.
                              2016-11-14: Added for 1.14.3.
    maxDepth                = ((int), default: -1) Max menu levels to render [{{{menu}}} type only]
                              See widget-menu.xsd {{{include-menu}}} element for details.
    subMenus                = (none|active|all, default: all) Sub-menu render filter [{{{menu}}} type only]
                              See widget-menu.xsd {{{include-menu}}} element for details.
    secMap                  = ((map)) For type="decorator", maps decorator-section names to Freemarker code to execute.
                              WORK-IN-PROGRESS
                              The entries may be TemplateInvoker instances returned from #interpretStd or #interpretStdLoc.
                              Alternatively, simple strings may be passed which will be interpreted as template locations
                              or screen widgets based on extension.
                              Only file locations are supported this way due to security risks of mixing locations and inline content,
                              and because locations allow for optimizations.            
    
  * History *
    Enhanced for 1.14.4 (type="ftl", type="decorator" (WORK-IN-PROGRESS), improved defaults handling).
    Enhanced for 1.14.3 (shareScope).
    Enhanced for 1.14.2.
-->
<#macro render resource="" name="" type="" ctxVars={} globalCtxVars={} reqAttribs={} clearValues=false restoreValues=true 
    asString=false shareScope=false maxDepth="" subMenus="" secMap={}>
  <#if resource?has_content || name?has_content><#t><#-- NEW: 2017-03-10: we'll simply render nothing if no resource or name - helps simplify template code -->
  <#if !type?has_content>
    <#-- assuming type=="screen" as default for now, unless .ftl extension (2017-03-10)-->
    <#local type = resource?ends_with(".ftl")?string("ftl", "screen")>
  </#if>
  <#-- WARN: 2017-04-26: For type="section" we MUST pass the ctxVars down to the called method,
          because it may be using different "context" object (even if derived from the original context stack).
          We do not have to do this with globalCtxVars the globalContext should be the same since the section
          context should be derived from the original context stack.
      WARN 2: Even with this, ctxVars may not work as expected for type="section" due to further nesting.
          May be forced to rely on globalCtxVars and leave it at that... (TODO: REVIEW)
      NOTE: We make assumption that the nested context is a MapStack. if it's not we'd have even more problems,
          but shouldn't be anywhere. 
  -->
  <#local innerCtxVars = ctxVars>
  <#local outerCtxVars = {}>
  <#local skipSetCtxVars = false>
  <#if shareScope>
    <#-- WARN: if shareScope=true and restoreValues=true, we can't rely on the called function to restore the values...
          so this complicates further. -->
    <#if restoreValues>
      <#local innerCtxVars = ctxVars>
      <#local outerCtxVars = ctxVars>
      <#local skipSetCtxVars = true><#-- the assignments are done via innerCtxVars, outer just needs to restore after -->
    <#elseif clearValues>
      <#local innerCtxVars = ctxVars>
      <#local outerCtxVars = ctxVars>
      <#local skipSetCtxVars = true><#-- the assignments are done via innerCtxVars, outer just needs to clear after -->
    </#if>
  </#if>
  <#if type == "screen">
    <@varSection ctxVars=outerCtxVars globalCtxVars=globalCtxVars reqAttribs=reqAttribs clearValues=clearValues restoreValues=restoreValues skipSetCtxVars=skipSetCtxVars><#t>
      ${StringUtil.wrapString(screens.renderScopedGen(resource, name, asString, shareScope, innerCtxVars))}<#t>
    </@varSection><#t>
  <#elseif type == "decorator">
    <@varSection ctxVars=outerCtxVars globalCtxVars=globalCtxVars reqAttribs=reqAttribs clearValues=clearValues restoreValues=restoreValues skipSetCtxVars=skipSetCtxVars><#t>
      <p>(@render type="decorator" is not yet implemented)</p><#t>
      <#--${StringUtil.wrapString(screens.renderDecoratorScopedGen(resource, name, asString, shareScope, sections, innerCtxVars)})}--><#t>
    </@varSection><#t>  
  <#elseif type == "section">
    <@varSection ctxVars=outerCtxVars globalCtxVars=globalCtxVars reqAttribs=reqAttribs clearValues=clearValues restoreValues=restoreValues skipSetCtxVars=skipSetCtxVars><#t>
      ${StringUtil.wrapString((sections.renderScopedGen(name, asString, shareScope, innerCtxVars))!"")}<#t>
    </@varSection><#t>  
  <#elseif type == "ftl">
    <@varSection ctxVars=outerCtxVars globalCtxVars=globalCtxVars reqAttribs=reqAttribs clearValues=clearValues restoreValues=restoreValues skipSetCtxVars=skipSetCtxVars><#t>
      <#-- DEV NOTE: using envOut to emulate screens.render behavior, so even though not always good, is more predictable. -->
      ${interpretStd({"location":resource, "envOut":!asString, "shareScope":shareScope, "ctxVars":innerCtxVars})}<#t>
    </@varSection><#t>  
  <#else>
    <@varSection ctxVars=outerCtxVars globalCtxVars=globalCtxVars reqAttribs=reqAttribs clearValues=clearValues restoreValues=restoreValues skipSetCtxVars=skipSetCtxVars><#t>
      <#-- strip include- prefix from type, because for the rest it's all the same -->
      <#local type = type?replace("include-", "")>
      <#if !name?has_content>
        <#local parts = resource?split("#")>
        <#local resource = parts[0]>
        <#local name = (parts[1])!>
      </#if>
      <#-- DEV NOTE: WARN: name clashes -->
      <#if type == "menu">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource, "shareScope":shareScope, "maxDepth":maxDepth, "subMenus":subMenus, "ctxVars":innerCtxVars
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioMenuWidgetWrapper", asString))}<#t>
      <#elseif type == "form">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource, "shareScope":shareScope, "ctxVars":innerCtxVars
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioFormWidgetWrapper", asString))}<#t>
      <#elseif type == "tree">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource, "shareScope":shareScope, "ctxVars":innerCtxVars
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioTreeWidgetWrapper", asString))}<#t>
      <#elseif type == "screen">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource, "shareScope":shareScope, "ctxVars":innerCtxVars
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioScreenWidgetWrapper", asString))}<#t>
      </#if>
    </@varSection><#t>
  </#if>
  </#if>
</#macro>

<#-- 
*************
* interpretStd
************
Interprets/compiles a string or file location as a template and returns the template
in a self-sufficient template invoker wrapper, which can later be evaluated using
a simple invocation form (by default, simple string evaluation).

This can be seen as an alternative to the {{{?interpret}}} built-in, 
with significant differences. Unlike {{{?interpret}}}, the default behavior
is to treat the invocation as a standalone template render, rather than
evaluating within the current template environment.
In this respect, #interpretStd is closer to the @render directive, but
with no widget renderer involvement.

Furthermore, while {{{?interpret}}} returns a directive as wrapper (evaluated using {{{<@value/>}}} syntax),
by default, #interpretStd returns a scalar (string)-implementing wrapper,
so that it can be evaluated using the same syntax used for regular string variables,
and thus can substitute more easily.

Unlike {{{?interpret}}}, variables from the FTL environment are NOT available
from #interpretStd invocations; only variables from the widget renderer
{{{context}}} ({{{MapStack}}}) are available. This is closer to @render and is an explicit feature
intended to prevent interfering with FTL environment of parent templates.
Furthermore, by default, the context {{{MapStack}}} is pushed (see {{{pushCtx}}} parameter).

This function accepts one parameter which is a map of parameters, described below.
If a single string is supplied instead, it is taken as the inline string template
to interpret, and all other parameters get defaults.

NOTE: It is also possible to pass the map as the second parameter instead of the first, with
    first being the template str.

  * Parameters *
    body                    = ((string)) An inline string to use as template body
                              The input itself automatically bypasses screen auto-html escaping.
                              NOTE: if instead of an args map, the function receives a single
                                  string parameter, then it is considered the value of this
                                  parameter, and all others receive defaults.
    location                = ((string)) A file location, alternative to inline {{{template}}}
                              The input itself automatically bypasses screen auto-html escaping.
    invokeMode              = (ofbiz-std, default: ofbiz-std) The general invocation and context mode
                              Possible values:
                              * {{{ofbiz-std}}}: Causes a standalone, standard ofbiz template invocation,
                                with the {{{context}}} variable as root binding,
                                similar to using FreeMarkerWorker directly. By default, the current
                                context is reused and pushed (unless specified otherwise).
                                NOTE: this is completely different from freemarker's {{{?interpret}}} built-in,
                                    which evaluates the template as if it were part of the current environment and template.
                              NOTE: By default, in all invokeModes, the function will attempt to use
                                  template compilation caches appropriate for the current renderer. 
                                  Currently there is no parameter to disable it and no reason to.
                              NOTE: By default, in all invokeModes, the function will attempt to use 
                                  the same ObjectWrapper as currently in use, meaning the same
                                  auto-html-escaping hack will apply.
                              TODO: parameters to specify cache and configuration/objectwrappers.
    invokeCtx               = ((map), default: -mode-dependent-) Context object to use for invocation
                              For {{{ofbiz-std}}}, the default is to use the well-known {{{context}}} variable
                              found in the freemarker environment at time of invocation.
                              NOTE: In most case this should not be set and leave it to fetch the context by itself;
                                  in which case, to pass vars, leave pushCtx to {{{true}}} and and pass them in {{{ctxVars}}} instead.                    
    pushCtx                 = ((boolean), default: -mode-dependent-) Whether to push/pop the context around the evaluation
                              For {{{ofbiz-std}}}, the default is {{{true}}}.
                              If the invokeCtx is not a MapStack, this has no effect.
    ctxVars                 = ((map)) Additional context vars to pass at time of invocation
                              If pushCtx is true (default), these are lost after render finish.
    unwrapCtxVars           = ((boolean), default: false) Whether to bother to ftl-unwrap the ctxVars or not                          
    model                   = (hybrid|scalar|directive|hybrid, default: hybrid) The Freemarker TemplateModel to wrap the interpreted/compiled template
                              * {{{scalar}}}: The returned value will evaluate (render) the template
                                when it is coerced to string or passed through the {{{?string}}} built-in.
                                This allows the interpreted template to substitute for a regular string variable.
                              * {{{directive}}}: The returned value behaves like the return value of
                                the {{{?interpret}}} built-in and must be evaluated using the
                                {{{<@value />}}} syntax.
                              * {{{hybrid}}}: implements both {{{scalar}}} and {{{directive}}} 
                                at the same time.
    envOut                  = ((boolean), default: false) Whether to output as string or environment writer
                              If set to true, the string-rendering methods of the model ({{{?string}}}) will output
                              to the current Freemarker environment output INSTEAD of returning as a string,
                              and will instead always return an empty string.
                              This is for advanced usage and is usually only needed so that the string-like
                              models behave more like Ofbiz's {{{screens}}} object, or for performance
                              reasons.
                              This only applies to {{{scalar}}} and {{{hybrid}}} models, and doesn't affect
                              the {{{directive}}} model or the directive mode of the {{{hybrid}}} model.
    
  * History *
    Added for 1.14.3.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function interpretStd args={}>
</#function>
-->

<#-- 
*************
* interpretStdLoc
************
Interprets/compiles a file location as a template and returns the template
in a self-sufficient template invoker wrapper, which can later be evaluated using
a simple invocation form (by default, simple string evaluation).

This is merely an alias to #interpretStd, where if a single string is passed instead of
an args map, it is interpreted to be a file location instead of the template itself.

NOTE: It is also possible to pass the map as the second parameter instead of the first, with
    first being the template location.

  * Parameters *
    (other)                 = See #interpretStd

  * Related *                           
    @ofbizUrl
    
  * History *
    Added for 1.14.3.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function interpretStdLoc args={}>
</#function>
-->

<#-- 
*************
* makeSectionsRenderer
************
Creates a sections renderer that can be used in place of the "sections" object.

The returned object implements a single {{{render(name)}}} method that can be used to
render the section by name.

WARN: 2017-03-28: currently this only implements the sections renderer "ftl" type,
    which means the sections cannot be passed back to the widget renderer.
TODO: widget renderer-compatible sections renderer ("screen" type).

  * Parameters *
    type                    = (ftl) (required) The sections renderer type
                              Possible values:
                              * {{{ftl}}}: creates an FTL-only sections renderer
                                it cannot be used from the widget renderer.
                                the sectionsMap values should usually be return values
                                from either #interpretStd, #interpretStdLoc or {{{?interpret}}}.
                                if strings are passed, they are simply outputted as-is.
    sectionsMap             = ((map)) Map of section names to implementations
                              See type for allowed values.
    
  * History *
    Added for 1.14.3.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function makeSectionsRenderer type sectionsMap>
</#function>
-->

<#-- 
*************
* ofbizUrl
************
Builds an Ofbiz navigation URL - for direct output into template document (primarily).

STOCK OFBIZ UTILITY. It is highly modified with enhanced capabilities for Scipio.

See also the function version, #makeOfbizUrl; #makeOfbizUrl should be used instead of @ofbizUrl 
when passing fully-built URLs to other macros (rather than trying to capture the output of @ofbizUrl) 
and in some other cases; meanwhile @ofbizUrl is more appropriate for writing generate URLs directly
to document output in templates (no intermediate captures). To this end, their default behaviors differ.

'''(Non-)HTML/JS escaping behavior:''' By default, neither @ofbizUrl, #makeOfbizUrl nor any of their variants
perform any HTML or Javascript escaping on their input URIs or parameters - it is not their responsibility.
HTML/JS escaping must be done either (preferably) using #escapeFullUrl, #escapeVal, or (simplest) the ''optional'' {{{escapeAs}}} parameter added 
to the URL utilities for Scipio 1.14.2, OR (often problematic) by letting screen html auto-escaping handle it.

'''Auto-escaping bypass behavior''': '''The macro URL builders behave differently than their function counterparts.'''
For legacy-compatibility reasons, as an exception to Scipio macros (see >>>standard/htmlTemplate<<<), @ofbizUrl 
does '''not''' perform an implied #rawString call on its parameters, and is thus subject to receiving context/data-model
values html-escaped to its inputs due to the renderer's automatic html escaping. By ofbiz's design, @ofbizUrl
historically received almost exclusively pre-html-escaped values as inputs in ofbiz templates and code.

In contrast, #makeOfbizUrl automatically calls #rawString on its parameters like standard Scipio html macros, such
that the caller only needs to call #rawString if he is composing strings before passing them to the function.
Furthermore, as noted, #makeOfbizUrl performs no extra language escaping by default, so its result remains unescaped
This means the result must be passed to another macro which performs escaping or to #escapeFullUrl - otherwise
it would be unsafe to output. Ultimately the goal is point-of-use escaping.

NOTE: 2016-11-04: The return value behavior for #makeOfbizUrl ''may'' be changed in the near future; for the current time, 
    it better to use #rawString explicitly on the result ''if'' you explicitly need a raw unescapted value;
    this may be set to return an auto-html-wrapped string instead. If you need to encure escaping, #escapeVal automatically
    handles such auto-escaped values in prevision of the future. In most cases such as passing the URL to other
    macros, there is no significant impact (and is only made possible) because of other improvements in 1.14.2.

''Note that the previous paragraphs describe default behaviors only''; the Scipio-modified utilities (all of them) support
extra parameters to handle escaping and switch the uri parameter handling: 
  escapeAs, rawParams, strict.
Specifically, {{{rawParams}}} if set to true will make @ofbizUrl behave like #makeOfbizUrl does by default
- and it is made safe by using {{{escapeAs}}} to apply html escaping on the final result. Conveniently,
specifying {{{escapeAs}}} automatically turns on {{{rawParams}}}, so it's the only one to remember.

In most cases it comes down to using the right tool for the job. #makeOfbizUrl is perfect for passing URLs
to Scipio macros which generally now (since 1.14.2) perform html escaping automatically on their parameters. 
So the following suffices for a simple hardcoded URL (for parameter values coming from screen context/data-model, you may
need to use #rawString):

  <@menuitem type="link" href=makeOfbizUrl('myRequest?param1=val1&param2=val2') ... />

Meanwhile, URLs outputted directly into templates or text are usually most quickly done using 
@ofbizUrl, but in newer code it is better done by specifying the {{{escapeAs}}} parameter, 
which will then escape the resulting URL in the given language ''and'' turn on the 
{{{rawParams}}} option. Such that, to illustrate, unlike stock ofbiz
there is no need to pre-escape special characters like the parameter delimiter ("&" vs "&amp;"):

  <a href="<@ofbizUrl uri='myRequest?param1=val1&param2=val2' escapeAs='html' />">some text</a>

'''Boolean parameters:''' In Scipio, boolean arguments can be given as booleans, string representation of booleans
or empty string (ternary, signifying defaults or emulating null).

WARN: {{{fullPath}}} and {{{secure}}} parameters have different behavior than stock Ofbiz!

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
                              WARN: At current time (2016-10-14), this macro version of @ofbizUrl does NOT prevent automatic
                                  screen html escaping on the URI parameter, because too many templates use @ofbizUrl
                                  directly without consideration to escaping.
                                  However, the function versions of this macro such as #makeOfbizUrl DO bypass the
                                  auto screen escaping on this parameter.
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
                              DEV NOTE: webSiteId arg is from stock but does not fully work and will not work with stock 
                                  webapps (don't have webSiteIds and can't give them any)
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
    rawParams               = ((boolean), default: -false, unless escapeAs is set-) Whether macro should call #rawString on its string parameters
                              If true, the macro will automatically call #rawString on {{{uri}}} and other string params, bypassing screen html auto-escaping.
                              If false, strings params are subject to screen html auto-escaping and {{{uri}}} behaves more like nested.
                              For legacy reasons, the default for all URL generation ''macros'' (which are derived from stock Ofbiz)
                              defined in this file is {{{false}}} ''UNLESS'' the {{{escapeAs}}} parameter is set,
                              in which case this is toggled to {{{true}}}.
                              For URL generation ''functions'' (which are new in Scipio), the default is always {{{true}}}.
    escapeAs                = (html|js|js-html|...|, default: -empty-) Language in which to escape the whole resulting URL
                              See #escapeFullUrl for possible values.
                              When this is empty (default), the macro performs no escaping whatsoever.
                              If this is set to a language, it toggles {{{rawParams}}} and {{{strict}}} to {{{true}}}, and doing
                                <@ofbizUrl uri=someUri escapeAs='html'... />
                              is basically equivalent to doing:
                                <#assign urlContent><@ofbizUrl uri=someUri rawParams=true strict=true ... /></#assign>
                                ${escapeFullUrl(urlContext, 'html')}
                              Usually, if you use this shortcut, you should use the {{{uri}}} parameter instead of nested, to bypass screen html auto-escaping;
                              but note you may need to use #rawString manually if you are adding parameters to the uri.
    strict                  = ((boolean), default: -false, unless escapeAs is set-) Whether to handle only raw strings or recognize pre-escaped strings
                              Default is false unless {{{escapeAs}}} is non-empty, in which case this switches to {{{true}}}.
                              NOTE: 2016-10-19: Currently this parameter has no effect on this macro (subject to change in a revision). 
                              NOTE: 2016-10-19: Currently this parameter is ''not'' passed to #escapeFullUrl (when {{{escapeAs}}} is set), because the
                                  pre-escaped ampersand {{{&amp;}}} is too ubiquitous in existing code.

  * History *
    Enhanced for 1.14.2.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#macro ofbizUrl uri="" absPath="" interWebapp="" webSiteId="" controller="" fullPath="" secure="" encode="" rawParams="" escapeAs="" strict="">
</#macro>
-->

<#-- 
*************
* makeOfbizUrl
************
Builds an Ofbiz navigation URL - for passing to other utilities (primarily).

Function version of the @ofbizUrl macro, supporting all the same parameters,
but with slight differences in defaults and default behavior.

This is useful to avoid bloating templates with heavy {{{<#assign...><@ofbizUrl.../></#assign>}}} captures
and instead passing results directly to other macros and functions.

'''This function's default escaping behavior is different from the default behavior of its macro counterpart''', @ofbizUrl; 
unlike @ofbizUrl this function was primarily intended to manipulate unescaped strings at input. See @ofbizUrl for details.

NOTE: 2016-11-04: The return value behavior for #makeOfbizUrl ''may'' be changed in the near future; for the current time, 
    it better to use #rawString explicitly on the result ''if'' you explicitly need a raw unescapted value;
    this may be set to return an auto-html-wrapped string instead. If you need to encure escaping, #escapeVal automatically
    handles such auto-escaped values in prevision of the future. In most cases such as passing the URL to other
    macros, there is no significant impact (and is only made possible) because of other improvements in 1.14.2.

  * Parameters *
    args                    = Map of @ofbizUrl arguments OR a string containing a uri (single parameter)
                              NOTE: The {{{rawParams}}} default is {{{true}}}, unlike the macro version.
                              DEV NOTE: This is the only sane way to implement this because FTL supports only positional args
                                  for functions, which would be unreadable here ({{{makeOfbizUrl("main", false, false, true, true...)}}})
                                  However majority of cases use only a URI so we can shortcut in that case.
                                  Freemarker doesn't support overloading so we basically implement it ourselves.
                                  Note that if we needed extra positional parameters for common cases, should keep the args map check on
                                  the first param only, otherwise it creates too many checks needed; this is
                                  consistent with macros anyway (you use either positional OR named params, you can't combine,
                                  so you use only args map or only positionals).
   
  * Related *                           
    @ofbizUrl
    
  * History *
    Enhanced for 1.14.2.
-->
<#function makeOfbizUrl args>
  <#if isObjectType("map", args)><#-- ?is_hash doesn't work right with context var strings and hashes -->
    <#local rawParams = args.rawParams!true>
    <#if !rawParams?has_content><#-- handles empty string case -->
      <#local rawParams = true>
    </#if>
    <#local strict = args.strict!true>
    <#if !strict?has_content><#-- handles empty string case -->
      <#local strict = true>
    </#if>
    <#local res><@ofbizUrl uri=(args.uri!"") webSiteId=(args.webSiteId!"") absPath=(args.absPath!"") interWebapp=(args.interWebapp!"") controller=(args.controller!"") 
        extLoginKey=(args.extLoginKey!"") fullPath=(args.fullPath!"") secure=(args.secure!"") encode=(args.encode!"") 
        rawParams=rawParams strict=strict escapeAs=(args.escapeAs!"")/></#local>
  <#else>
    <#local res><@ofbizUrl uri=args rawParams=true strict=true/></#local>
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

NOTE: This macro is subject to escaping particularities - see its cousin @ofbizUrl for details.

  * Parameters *
    (other)                 = See @ofbizUrl

  * Related *                           
    @ofbizUrl
    
  * History *
    Enhanced for 1.14.2.
-->
<#macro ofbizWebappUrl uri="" fullPath="" secure="" encode="" absPath=false controller=false extLoginKey=false rawParams="" strict="" escapeAs="">
  <@ofbizUrl uri=uri absPath=absPath interWebapp=false controller=controller 
    extLoginKey=extLoginKey fullPath=fullPath secure=secure encode=encode rawParams=rawParams strict=strict escapeAs=escapeAs><#nested></@ofbizUrl><#t>
</#macro>

<#-- 
*************
* makeOfbizWebappUrl
************
Builds an Ofbiz navigation intra-webapp, non-controller URL. Function version of @ofbizWebappUrl.

The URI takes the basic form /control/requesturi, 
but this is normally used to access another servlet, such as /products/PH-1000.

This calls @ofbizUrl with absPath=false, interWebapp=false, controller=false by default.

NOTE: This function is subject to escaping particularities - see its cousin #makeOfbizUrl for details.

  * Parameters *
    (other)                 = See #makeOfbizUrl, @ofbizWebappUrl

  * Related * 
    @ofbizWebappUrl                          
    @ofbizUrl
    
  * History *
    Enhanced for 1.14.2.
-->
<#function makeOfbizWebappUrl args>
  <#if isObjectType("map", args)>
    <#local rawParams = args.rawParams!true>
    <#if !rawParams?has_content><#-- handles empty string case -->
      <#local rawParams = true>
    </#if>
    <#local strict = args.strict!true>
    <#if !strict?has_content><#-- handles empty string case -->
      <#local strict = true>
    </#if>
    <#local res><@ofbizUrl uri=(args.uri!"") absPath=(args.absPath!false) interWebapp=false controller=(args.controller!false) 
        extLoginKey=(args.extLoginKey!false) fullPath=(args.fullPath!"") secure=(args.secure!"") encode=(args.encode!"") 
        rawParams=rawParams strict=strict escapeAs=(args.escapeAs!"")/></#local>
  <#else>
    <#local res><@ofbizUrl uri=args absPath=false interWebapp=false controller=false extLoginKey=false
        rawParams=true strict=true/></#local>
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

NOTE: This macro is subject to escaping particularities - see its cousin @ofbizUrl for details.

  * Parameters *
    (other)                 = See @ofbizUrl

  * Related * 
    @ofbizUrl
    
  * History *
    Enhanced for 1.14.2.
-->
<#macro ofbizInterWebappUrl uri="" webSiteId="" absPath="" controller="" extLoginKey="" fullPath="" secure="" encode="" rawParams="" strict="" escapeAs="">
  <@ofbizUrl uri=uri interWebapp=true absPath=absPath webSiteId=webSiteId controller=controller
    extLoginKey=extLoginKey fullPath=fullPath secure=secure encode=encode rawParams=rawParams strict=strict escapeAs=escapeAs><#nested></@ofbizUrl><#t>
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

NOTE: This function is subject to escaping particularities - see its cousin #makeOfbizUrl for details.

  * Parameters *
    (other)                 = See #makeOfbizUrl, @ofbizInterWebappUrl

  * Related * 
    @ofbizInterWebappUrl
    @ofbizUrl
    
  * History *
    Enhanced for 1.14.2.
-->
<#function makeOfbizInterWebappUrl args webSiteId="">
  <#if isObjectType("map", args)>
    <#local rawParams = args.rawParams!true>
    <#if !rawParams?has_content><#-- handles empty string case -->
      <#local rawParams = true>
    </#if>
    <#local strict = args.strict!true>
    <#if !strict?has_content><#-- handles empty string case -->
      <#local strict = true>
    </#if>
    <#local res><@ofbizUrl uri=(args.uri!"") absPath=(args.absPath!"") interWebapp=true webSiteId=(args.webSiteId!"")  
        controller=(args.controller!"") extLoginKey=(args.extLoginKey!"") fullPath=(args.fullPath!"") secure=(args.secure!"") encode=(args.encode!"") 
        rawParams=rawParams strict=strict escapeAs=(args.escapeAs!"")/></#local>
  <#else>
    <#local res><@ofbizUrl uri=args absPath="" interWebapp=true webSiteId=webSiteId
        controller="" extLoginKey="" rawParams=true strict=true/></#local>
  </#if>
  <#return res>
</#function>

<#-- 
*************
* ofbizContentUrl
************
Builds an Ofbiz content/resource URL.

STOCK OFBIZ UTILITY. It may be modified with enhanced capabilities for Scipio.

NOTE: 2016-10-18: URL decoding: The default behavior of this macro has been '''changed''' from 
    stock Ofbiz. By default this macro NO LONGER url-decodes the uri/nested. The stock
    Ofbiz behavior was presumably originally written to URL-decode whole URLs that had been stored
    URL-encoded in the database or encoded elsewhere; however, in the general-purpose use case of this macro (extends
    being database-stored URLs), applying URL-decoding by default is ''dangerous''.
    In general, if ever applicable, events and services that may receive fully-URL-encoded URLs should URL-decode them
    ''before'' storing in database - but note that URL-encoded parameters should probably not be decoded if
    they are stored with the rest of the URL as-is - only full URLs should be decoded, if received encoded
    (in such cases, parameters could effectively be double-encoded, and in that case only the first encoding layer should be removed).

NOTE: This macro is subject to escaping particularities - see its cousin @ofbizUrl for details.

NOTE: 2017-07-04: The {{{variant}}} parameter's usage in filenames has been fixed in Scipio and will be modified again soon;
    see the parameter's documentation below. 

  * Parameters *
    uri                     = (string) URI or path as parameter; alternative to nested
                              WARN: At current time (2016-10-14), this macro version of @ofbizContentUrl does NOT prevent automatic
                                  screen html escaping on the URI parameter, because too many templates use @ofbizUrl
                                  directly without consideration to escaping.
                                  However, the function versions of this macro such as #makeOfbizContentUrl DO bypass the
                                  auto screen escaping on this parameter.
                              NOTE: SPECIAL VALUES: Stock ofbiz originally recognized the string "/images/defaultImage.jpg" as
                                  a special value; when this value was specified, the {{{variant}}} parameter was ignored.
                                  However, this check does not work properly in general.
                                  In Scipio, while this behavior is left intact for compatibility with old code, 
                                  you should simply avoid relying on any such check and not consider
                                  "/images/defaultImage.jpg" as a special value, or simply not use it.
    variant                 = ((string)) Variant image, normally same image with different dimensions
                              2017-07-04: The variant name is now appended using one of the following 3 filename patterns:
                              * {{{/file.ext}}} -> {{{/file-${variant}.ext}}} [STOCK]: in most cases the variant is added this way, before extension with dash, EXCEPT when:
                              * {{{/file-original.ext}}} -> {{{/file-${variant}.ext}}} [NEW]: when the filename ends with the keyword "original" after dash, it is replaced with the variant word, and:
                              * {{{/original.ext}}} -> {{{/${variant}.ext}}} [NEW]: when the filename part is exactly the keyword "original", it is substituted with the variant word.
                              The NEW cases have been added so that the macro now supports the stock product image upload configuration (see catalog.properties),
                              rather than conflicting with it.
                              SPECIAL PREFIXES: The variant can be prefixed with one of the following characters:
                              * {{{-}}}: this forces the first STOCK case above, to support filenames that originally were named "original".
                              * {{{~}}}: TODO: NOT IMPLEMENTED: special operator: if the variant begins with the tilde character (~), a special closest-matching behavior will be enabled... 
                              (Stock Ofbiz parameter, modified in Scipio)
    autoVariant             = (min|max|true|false|, default: false) Enable automatic variant selection with the specified selection mode
                              * {{{true/min}}}: selects the smallest image that is bigger than the dimensions specified by width/height parameters
                              * {{{max}}}: selects the largest image that is smaller than the dimensions specified by width/height parameters
                              WARN: Like the {{{variant}}} parameter, in order for this to work properly for an image, the filesystem must contain
                                  the proper variants for the image that match the explicit or implied variant configuration ({{{variantCfg}}} parameter).
    width                   = ((int)) Target image width, for autoVariant 
    height                  = ((int)) Target image height, for autoVariant
    variantCfg              = Path to a variant configuration file (ImageProperties.xml)
                              If omitted, this will lookup a default configuration file based on the settings in:
                                {{{/framework/common/config/imagecommon.properties}}}
                              The common/default/fallback/reference file is:
                                {{{/framework/common/config/ImageProperties.xml}}}                              
    ctxPrefix               = ((boolean)|(string), default: false) Contextual path prefix
                              Extra path prefix prepended to the uri, which may replace the central system default prefix if
                              it produces an absolute URL (prefixed with "http:", "https:", or "//").
                              If string, it is used as given.
                              If boolean: if false, no extra prefix; if true, the context variable
                              (New in Scipio)
    urlDecode               = ((boolean), default: false) Whether to URL-decode (UTF-8) the uri/nested
                              NOTE: 2016-10-18: The new default is FALSE (changed from stock Ofbiz - or what it would have been).
                              (New in Scipio)
    rawParams               = ((boolean), default: -false, unless escapeAs is set-) Whether macro should call #rawString on its string parameters
                              See @ofbizUrl for description.
    escapeAs                = (html|js|js-html|...|, default: -empty-) Language in which to escape the whole resulting URL
                              See #escapeFullUrl for possible values.
                              See @ofbizUrl for description.
    strict                  = ((boolean), default: -false, unless escapeAs is set-) Whether to handle only raw strings or recognize pre-escaped strings
                              This macro must perform checks and concatenations on the passed uri; if pre-escaped
                              values are passed (such as HTML), this parameter must be false.
                              When false, currently (2016-10-19), only HTML and Javascript pre-escaped strings are handled.
                              If {{{escapeAs}}} is set to a specific language, the default for {{{strict}}} becomes {{{true}}}.
                              NOTE: 2016-10-19: Currently this parameter is ''not'' passed to #escapeFullUrl (when {{{escapeAs}}} is set), because the
                                  pre-escaped ampersand {{{&amp;}}} is too ubiquitous in existing code.
                              WARN: Pre-escaped string handling is heuristic-like and only tries to detect encodings
                                  done by {{{UtilCodec}}} class and Freemarker built-ins.
                                  In some edge cases, the resulting Javascript may not necessarily be secure!
                                  The method will try to warn in log.
                                  Known cases: 
                                  * If a prefix ends with raw less-than ("<") character and the uri does not begin
                                    with a forward slash or escaped equivalent ("/"), the method could produce
                                    a dangerous result! This is prevented by not passing unsafe values as the
                                    {{{ctxPrefix}}}, or simply not pre-escaping for javascript.
                              NOTE: The function version of this macro, #makeOfbizContentUrl, uses
                                  true as default for this parameter, unlike this macro.
                              (New in Scipio) 
                              
  * History *
    Enhanced for 1.14.4 (variant parameter enhancement; autoVariant parameters added).
    Enhanced for 1.14.2.
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

NOTE: This function is subject to escaping particularities - see its cousin #makeOfbizUrl for details.

  * Parameters *
    rawParams               = ((boolean), default: true) Whether macro should call #rawString on string parameters (mainly {{{uri}}})
                              NOTE: Unlike @ofbizContentUrl, the default here is true.
    strict                  = ((boolean), default: true) Whether to handle only raw strings or recognize pre-escaped strings
                              NOTE: Unlike @ofbizContentUrl, the default here is true, such that, by default, this
                                  function is meant to operate on raw unescaped strings only.
    (other)                 = See @ofbizContentUrl

  * Related * 
    @ofbizContentUrl
    
  * History *
    Enhanced for 1.14.2.
-->
<#function makeOfbizContentUrl args variant="">
  <#if isObjectType("map", args)>
    <#local rawParams = args.rawParams!true>
    <#if !rawParams?has_content><#-- handles empty string case -->
      <#local rawParams = true>
    </#if>
    <#local strict = args.strict!true>
    <#if !strict?has_content><#-- handles empty string case -->
      <#local strict = true>
    </#if>
    <#-- DEV NOTE: no rawString around ctxPrefix because already done by the macro (exceptionally) -->
    <#local res><@ofbizContentUrl uri=(args.uri!"") variant=(args.variant!"") 
        ctxPrefix=(args.ctxPrefix!false) urlDecode=(args.urlDecode!"") 
        autoVariant=(args.autoVariant!"") width=(args.width!"") height=(args.height!"") variantCfg=(args.variantCfg!"") 
        strict=strict rawParams=rawParams/></#local>
  <#else>
    <#local res><@ofbizContentUrl uri=args variant=variant rawParams=true strict=true/></#local>
  </#if>
  <#return res>
</#function>

<#-- 
*************
* makeOfbizContentCtxPrefixUrl
************
Version of #makeOfbizContentUrl that is preset to recognize the {{{contentPathPrefix}}} context prefix.
Same as (shorthand for):
  makeOfbizContentUrl({"uri":someUri, "ctxPrefix":true, ...})

NOTE: This function is subject to escaping particularities - see its cousin #makeOfbizUrl for details.

  * Related * 
    #makeOfbizContentUrl
    @ofbizContentUrl
    
  * History *
    Enhanced for 1.14.2.
-->
<#function makeOfbizContentCtxPrefixUrl args variant="">
  <#if isObjectType("map", args)>
    <#return makeOfbizContentUrl({"ctxPrefix":true} + args) />
  <#else>
    <#return makeOfbizContentUrl({"uri":args, "variant":variant, "ctxPrefix":true}) />
  </#if>
</#function>

<#-- 
*************
* ofbizContentAltUrl
************
Builds an Ofbiz content/resource Alt URL, from a URL stored in the database by contentId.

STOCK OFBIZ UTILITY. It may be modified with enhanced capabilities for Scipio.

NOTE: 2016-10-18: URL decoding: The default behavior of this macro has been '''changed''' from 
    stock Ofbiz. By default this macro NO LONGER url-decodes the URL retrieved by contentId. The stock
    Ofbiz behavior was presumably originally written to URL-decode whole URLs that had been stored
    URL-encoded in the database.
    For consistency and prevention of some less obvious security issues (not as dramatic as @ofbizContentUrl),
    the default has been changed to ''not'' decode by default.
    In general, if ever applicable, events and services that may receive fully-URL-encoded URLs should URL-decode them
    ''before'' storing in database - but note that URL-encoded parameters should probably not be decoded if
    they are stored with the rest of the URL as-is - only full URLs should be decoded, if received encoded
    (parameters could effectively be double-encoded, and in that case only the first encoding layer should be removed).

NOTE: This macro is subject to escaping particularities - see its cousin @ofbizUrl for details.

  * Parameters *
    contentId               = (string) Content ID
                              (Stock Ofbiz parameter)
    viewContent             = (string) view content
                              (Stock Ofbiz parameter)                          
    urlDecode               = ((boolean), default: false) Whether to URL-decode (UTF-8) the stored URL
                              NOTE: 2016-10-18: The new default is FALSE (changed from stock Ofbiz - or what it would have been).
                              (New in Scipio)
    rawParams               = ((boolean), default: -false, unless escapeAs is set-) Whether macro should call #rawString on its string parameters
                              See @ofbizUrl for description.
    escapeAs                = (html|js|js-html|...|, default: -empty-) Language in which to escape the whole resulting URL
                              See #escapeFullUrl for possible values.
                              See @ofbizUrl for description.
    strict                  = ((boolean), default: -false, unless escapeAs is set-) Whether to handle only raw strings or recognize pre-escaped strings
                              NOTE: 2016-10-19: Currently this parameter has no effect on this macro (subject to change in a revision). 
                              NOTE: 2016-10-19: Currently this parameter is ''not'' passed to #escapeFullUrl (when {{{escapeAs}}} is set), because the
                                  pre-escaped ampersand {{{&amp;}}} is too ubiquitous in existing code.
                              See @ofbizUrl for description.
                              
  * History *
    Enhanced for 1.14.2.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#macro ofbizContentAltUrl ...>
</#macro>
-->

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
<#-- IMPLEMENTED AS TRANSFORM
<#function setVars varMaps={}>
</#function>
-->

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
<#-- IMPLEMENTED AS TRANSFORM
<#function clearVars varLists={}>
</#function>
-->

<#-- 
*************
* extractVars
************
Gets all the named attributes and context vars.

This function is enhanced to support more value types and the special value scipioNullObject
to indicate the value null.

  * Parameters *
    varLists                = ((map)) A map of lists, or map of maps (keys used), of var names to extract values
                              * {{{ctxVars}}}: A list (or map - keys used) of screen context vars names to extract
                              * {{{globalCtxVars}}}: A list (or map - keys used) of screen global context vars names to extract
                              * {{{reqAttribs}}}: A list (or map - keys used) of request attributes names to extract
    saveNulls               = ((boolean), default: true) If true, null/missing values will get map entries with null value; otherwise, omitted from results
                              NOTE: 2017-04-28: default is now 
    
  * Return Value *
    A map of maps with same keys as parameters.
    
  * History *
    Modified for 1.14.3.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function extractVars varLists={} saveNulls=false>
</#function>
-->

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
    restoreValues           = ((boolean), default: true) If true, the original values are saved and restored after invocation
                              NOTE: 2016-07-29: The default for this parameter has been changed to {{{true}}}.
    clearValues             = ((boolean), default: false) If true, the passed request attributes and context vars are removed (or set to null) after invocation
-->
<#-- IMPLEMENTED AS TRANSFORM
<#macro varSection ctxVars={} globalCtxVars={} reqAttribs={} clearValues=false restoreValues=true>
</#macro>
-->

<#-- 
*************
* virtualSection
************
Defines a virtual section that produces no markup (used in targeted rendering),
with a name of global scope.
This is equivalent to defining a {{{<section>}}} widget element 
(whereas @section is equivalent to {{{<screenlet>}}} widget element).

This is required to be able to re-implement some widgets screens and decorators as FTL.

See {{{widget-screen.xsd}}} "contains" expression attribute definition for more information.

NOTE: this implicitly defines a @renderTarget.

  * Parameters *
    name                    = Virtual section name (global scope)
    contains                = contains-expression
                              See {{{widget-screen.xsd}}} "contains" expression attribute definition for more information.
    
  * History *
    Added for 1.14.3.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#macro virtualSection name="" contains="*">
</#macro>
-->

<#-- 
*************
* renderTarget
************
Used within a standard library macro definition to implemented targeted rendering for the directive.

See {{{widget-screen.xsd}}} "contains" expression attribute definition for more information.

NOTE: Due to possible performance concerns, only a few of the scipio standard Freemarker API currently support this:
    @container, @form, @table, @section (NOTE: @section actually matches as "screenlet" element name with % selector).
    They are mostly meant to work with the {{{scpRenderTargetExpr}}} ID selector ({{{#}}}).

FIXME: Some of the behavior is currently hardcoded inside the renderTarget implementation.

  * Parameters *
    dirName                 = Name of the containing directive
    dirArgs                 = ((map)) Map of arguments that were passed to the directive
                              The implementation may extract name and ID OR it may do nothing.
                              TODO: clarify
    id                      = id                
    name                    = name        
    
  * History *
    Added for 1.14.3.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#macro renderTarget dirName="" dirArgs={} id="" name="">
</#macro>
-->

<#-- 
*************
* getLabel
************
Returns label from global label map or resource, or empty string if no label is found,
with automatic screen html-escaping applied.
This is a higher-level, abstracted function for fetching labels.

By default this function tries to maintain the same behavior as {{{uiLabelMap}}} with respect to locale
selection and the context map used for label substitutions/arguments.

However, unlike stock {{{uiLabelMap}}}, this supports explicit message arguments using the {{{msgArgs}}} parameter.

NOTE: The default context used by {{{uiLabelMap}}} (which is used by default by {{{getLabel}}}) is 
    ''not'' the current Freemarker namespace ''nor'' the same as the {{{context}}} variable that is 
    present as a freemarker global. 
    It uses a context created earlier, and the only predictable way to set variables
    in it is through the screen global context, using #setGlobalContextField.
    Therefore, to pass arguments to label reliably, #getLabel must be used with explicit {{{msgArgs}}} parameter.
    
The locale is determined by uiLabelMap. If you must 

DEV NOTE: It is not possible to add custom locale here; already loaded into the {{{uiLabelMap}}}.

  * Parameters *
    name                    = ((string), required) Label name
    resource                = ((string)) Resource name for fallback
                              If label not found in {{{uiLabelMap}}} (preferred), falls back to lookup in this 
                              resource. Usually {{{uiLabelMap}}} is preferred for templates, but sometimes not worth importing
                              a whole file for one label.
                              NOTE: If a map is passed as second parameter instead of string, it is interpreted
                                  as {{{msgArgs}}} instead of a resource and {{{resource}}} is interpreted as empty.
    msgArgs                 = ((map)|(boolean), default: -true / use uiLabelMap's context-) Message arguments
                              If boolean: if true, uses uiLabelMap's default/arbitrary context; if false,
                              prevents any context from being used.
                              NOTE: For convenience, {{{msgArgs}}} can be specified as the 
                                  second parameter instead of third (like a java function overload), by
                                  passing a map type to the second parameter.
                                  So {{{getLabel("xxx", {})}}} is the same as {{{getLabel("xxx", "", {})}}}.

  * Related *
    #rawLabel
    #getPropertyMsg
-->
<#function getLabel name resource="" msgArgs=true>
  <#if name?has_content>
    <#if isObjectType("map", resource)><#-- msgArgs can be passed as 2nd param -->
      <#local msgArgs = resource>
      <#local resource = "">
    </#if>
    <#if msgArgs?is_boolean>
      <#if msgArgs>
        <#local var=(uiLabelMap[name])!false />
      <#else>
        <#local var=(uiLabelMap.get(name, _NULL_PLACEHOLDER))!false />
      </#if>
    <#else>
      <#local var=(uiLabelMap.get(name, msgArgs))!false />
    </#if>
    <#if (!var?is_boolean) && var != name>
      <#return var>
    <#elseif resource?has_content>
      <#-- 2016-10-13: getPropertyMsg must use the exact same arguments that uiLabelMap is using,
          meaning same context for args and same locale -->
      <#if msgArgs?is_boolean>
        <#if msgArgs>
          <#return getPropertyMsg(resource, name, (uiLabelMap.getContext())!false, (uiLabelMap.getLocale())!true)>
        <#else>
          <#return getPropertyMsg(resource, name, false, (uiLabelMap.getLocale())!true)>
        </#if>
      <#else>
        <#return getPropertyMsg(resource, name, msgArgs, (uiLabelMap.getLocale())!true)>
      </#if>
    <#else>
      <#return "">
    </#if>
  <#else>
    <#return ""> 
  </#if>
</#function>

<#-- 
*************
* rawLabel
************
Returns label from global label map or resource, or empty string if no label is found,
and prevents automatic html-escaping on the result.
This is a higher-level, abstracted function for fetching labels.

Shorthand for {{{rawString(getLabel(...))}}}.

  * Parameters *
    name                    = ((string), required) Label name
    resource                = ((string)) Resource name for fallback
                              If label not found in {{{uiLabelMap}}} (preferred), falls back to lookup in this 
                              resource. Usually {{{uiLabelMap}}} is preferred for templates, but sometimes not worth importing
                              a whole file for one label.
                              NOTE: If a map is passed as second parameter instead of string, it is interpreted
                                  as {{{msgArgs}}} instead of a resource and {{{resource}}} is interpreted as empty.
    msgArgs                 = ((map)|(boolean), default: -true / use uiLabelMap's context-) Message arguments
                              If boolean: if true, uses uiLabelMap's default/arbitrary context; if false,
                              prevents any context from being used.
                              NOTE: For convenience, {{{msgArgs}}} can be specified as the 
                                  second parameter instead of third (like a java function overload), by
                                  passing a map type to the second parameter.
                                  So {{{rawLabel("xxx", {})}}} is the same as {{{rawLabel("xxx", "", {})}}}.

  * Related *
    #getLabel
    #rawString
    
  * History *
    Added for 1.14.2.
-->
<#function rawLabel name resource="" msgArgs=true>
  <#return rawString(getLabel(name, resource, msgArgs))>
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
Gets property or empty string if missing, using behavior and rules of the {{{UtilProperties}}}
class (low-level).

NOTE: The resulting message is subject to automatic HTML encoding (by Ofbiz). 
    Use #rawString on the result to prevent escaping.
    
DEV NOTE: If this is ever implemented as transform, be careful to make sure the result is HTML-escaped
    using the same method Ofbiz does! The escaping is now part of this method's interface.

TODO: implement as transform.

  * Parameters *
    resource                = (required) Resource name
    name                    = (required) Property name
    msgArgs                 = ((map)|(list)|(boolean), default: -false / none-) Substitute values for message template
    locale                  = ((locale)|(boolean), default: -true / locale from context-) Locale
                              If boolean: if true, uses locale from context; if false, forced to use system default.
                              NOTE: There should almost always be a locale in context or explicit.
                                  Fallback on system default usually means something is missing.
-->
<#function getPropertyMsg resource name msgArgs=false locale=true>
  <#if locale?is_boolean>
    <#if locale>
      <#local locale = .globals.locale!Static["java.util.Locale"].getDefault()>
    <#else>
      <#local locale = Static["java.util.Locale"].getDefault()>
    </#if>
  </#if>
  <#if msgArgs?is_sequence || msgArgs?is_hash><#-- NOTE: these will actually call different overloads -->
    <#return Static["org.ofbiz.base.util.UtilProperties"].getMessage(resource, name, msgArgs, locale)>
  <#else>
    <#-- don't use context by default here (only uiLabelMap/getLabel should do that): context!{} -->
    <#return Static["org.ofbiz.base.util.UtilProperties"].getMessage(resource, name, locale)>
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
    msgArgs                 = ((map)|(list)|(boolean), default: -false / none-) Substitute values for message template
    locale                  = ((locale)|(boolean), default: -true / locale from context-) Explicit locale
    
  * Related *
    #getPropertyMsg
-->
<#function getPropertyMsgFromLocExpr resourceExpr msgArgs=false locale=true>
  <#local parts = resourceExpr?split("#")>
  <#if (parts?size >= 2)>
    <#local resource = parts[0]>
    <#local name = parts[1]>
  <#else>
    <#local resource = "CommonUiLabels">
    <#local name = parts[0]>
  </#if>
  <#return getPropertyMsg(resource, name, msgArgs, locale)> 
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
    locale                  = ((locale)|(boolean), default: -true / locale from context-) Explicit locale
-->
<#function getTextLabelFromExpr textExpr msgArgs=false locale=true>
  <#if textExpr?starts_with("#LABEL:")>
    <#return getLabel(textExpr[7..])!"">
  <#elseif textExpr?starts_with("#PROP:")>
    <#return getPropertyMsgFromLocExpr(textExpr[6..], msgArgs, locale)!"">
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
    urlEncode               = ((boolean), default: false) If true, URL-encode each value.
-->
<#function addParamsToStr paramStr paramMap paramDelim="&amp;" includeEmpty=true urlEncode=false>
  <#local res = paramStr>
  <#local paramMap = toSimpleMap(paramMap)>
  <#list mapKeys(paramMap) as key>
    <#if res?has_content && (!res?ends_with(paramDelim))>
      <#local res = res + paramDelim>
    </#if>
    <#if includeEmpty || paramMap[key]?has_content>
      <#if urlEncode>
        <#local res = res + key + "=" + rawString(paramMap[key]!"")?url>
      <#else>
        <#local res = res + key + "=" + rawString(paramMap[key]!"")>
      </#if>
    </#if>
  </#list>
  <#return res>
</#function>

<#-- 
*************
* addParamsToStrUrlEnc
************
Adds url-encoded parameters from a hash to a URL param string (no full URL logic).
                    
  * Parameters *
    paramStr                = (required) Param string
    paramMap                = ((map), required) Map of keys to values to add
    paramDelim              = (default: "&amp;") Param delimiter
    includeEmpty            = ((boolean), default: true) If true, include empty values; if false, omit empty values
-->
<#function addParamsToStrUrlEnc paramStr paramMap paramDelim="&amp;" includeEmpty=true>
  <#return addParamsToStr(paramStr, paramMap, paramDelim, includeEmpty, true)>
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
* rawString
************
Returns the given value, bypassing the screen renderer html auto-escaping, as a simple Freemarker string.

This is the same as the Ofbiz-provided function, {{{StringUtil.wrapString}}}, but further simplifies
the resulting type into a simple Freemarker string.

This can be seen as the reverse operation of #rewrapStringStd.

NOTE: 2016-09-29: Now tolerates non-strings, which will be coerced to strings using ?string operator.

NOTE: 2016-10-20: Now supports multiple parameters, which are each {{{rawString}}}-ed and then
    concatenated together.

  * Parameters *
    value...                    = ((string), required) Value(s) to return without/bypassing screen renderer html auto-escaping
                                  If more than one parameter is passed to #rawString, 
                                  each is applied the logical #rawString bypass, and
                                  the result is a concatenation of all the parameters.
                                  So
                                    rawString(var1, " ", var2)
                                  is equivalent to
                                    rawString(var1) + " " + rawString(var2)
                                  except the former is more efficient.
  * Related *
    #rewrapStringStd  
    
  * History *
    Enhanced for 1.14.2.                        
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function rawString value...>
  <#- ?string turns it into a basic FTL string ->
  <#return StringUtil.wrapString(value)?string> 
</#function>
-->

<#-- 
*************
* toRawString
************
Returns the given value, bypassing ofbiz screen renderer html auto-escaping, as a simple Freemarker string.
Alias for #rawString.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function toRawString value...>
</#function>
-->

<#-- 
*************
* toStringIfNot
************
Returns the given value as a string but only if not already a string.

This intentionally skips calling ?string on existing strings to prevent auto-escaping.
Result will always be a string (or something implementing {{{TemplateScalarModel}}}).

WARN: this works using FTL's primitive ?is_string test, which may return TRUE for complex 
    objects that aren't really strings.

  * Parameters *
    value                   = (required) The value to return as string
    
  * History *
    Added for 1.14.2.
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
DEPRECATED: Use #escapeVal with the {{{allow}}} option instead.

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
Re-wraps a value using the Freemarker ObjectWrapper in the current environment (by default)
or a different wrapper, such as one producing simple type adapters, simple types as copies,
and raw strings (without screen html auto-escaping).

By default (using current wrapper and default mode), this will rewrap a value to a state as if it had just
come out of the screen's data model. For strings, in a typical rendering, this can be seen as the reverse
operation of #rawString, using simply {{{rewrapObject(someRawString)}}}. 
In this default mode, the value will ''always'' be rewrapped, which is the safest mode (but suboptimal).

The function can remove the html auto-escaping behavior of entire maps coming from the data model.
Simply doing {{{rewrapObject(someScreenMap, 'raw')}}} or {{{rewrapObject(someScreenMap, 'raw-simple')}}} (to
further simplify the map for iteration).

Using {{{wrapper}}} and {{{mode}}} parameters, different object wrappers such as simple-type-producing
ones can be used, and some unnecessary rewrapping can be avoided.

NOTE: 2016-10-20: This method was completely revamped, such that the default behavior is flipped
    and behaves more predictably.
    
NOTE: 2016-10-20: The "simple*" current wrapper derivatives currently only handle maps.
    If you are required to use simple or copies for other types of containers, you may
    have to use "basic*" in the meantime (TODO), or if raw values are acceptable,
    the "raw-simple*" types do handle non-maps.
    
NOTE: 2016-10-20: Currently only supports "always" (deep) rewrapping mode; ideally need a "needed" and/or "fast"
    mode is needed to prevent needless container copies (TODO).
    So currently this method is very slow for SimpleHash and similar types, forces copies. 
    However optimization of this requires many assumptions and risks.
    In non-deep cases, for maps, you may also use #toSimpleMap instead, which is more constrained.
    
  * Parameters *
    object                  = ((object), required) The source object
    wrapper                 = (current|complex-default|complex-extended|simple|simple-copy|..., default: current) Name/type of wrapper to use
                              General values (abstracted):
                              * {{{current}}}: the current wrapper
                                In Scipio, this is usually {{{complex-extended}}}, though not guaranteed.
                              Current ObjectWrapper derivatives (abstracted):
                              * {{{raw}}}: derivative of current wrapper that performs NO auto html-escaping
                                It does not simplify the collections in any other way.
                              * {{{simple-adapter}}}: derivative of current wrapper but - if map - generates a simple map adapter (SimpleMapModel or DefaultMapAdapter)
                                WARN: 2016-10-20: Only PARTIALLY implemented, only handles maps.
                              * {{{simple-copy}}}: derivative of current wrapper - but - if map - generates a simple map copy (SimpleHash)
                                WARN: 2016-10-20: Only PARTIALLY implemented, only handles maps.
                              * {{{simple}}}: will use {{{simple-adapter}}} or {{{simple-copy}}}, whichever appears less costly
                                WARN: 2016-10-20: Only PARTIALLY implemented, only handles maps.
                                NOTE: 2016-10-20: selection optimization logic not fully implemented; currently makes adapters.
                              * {{{raw-simple-adapter}}}: same as {{{simple-adapter}}} but performs NO auto html-escaping
                                Works on any collections.
                              * {{{raw-simple-copy}}}: same as {{{simple-copy}}} but performs NO auto html-escaping
                                Works on any collections.
                              * {{{raw-simple}}}: same as {{{simple}}} but performs NO auto html-escaping
                                Works on any collections.
                                NOTE: 2016-10-20: selection optimization logic not fully implemented for this; currently makes adapters.
                              NOTE: 2016-10-20: all these "simple" types currently only operate on maps.
                                  Lists are usually not as meaningful because the BeansWrapper list type 
                                  works without any issues.
                              Specific ObjectWrappers (advanced usage - poor abstraction):
                              * {{{complex-default}}}: basic BeansWrapper from Freemarker, the default one kept in FreeMarkerWorker; performs NO auto html-escaping
                                Produces complex maps that double as beans.
                              * {{{complex-default-simplemap}}}: Version of {{{complex-default}}} that produces simple maps (SimpleMapAdapter).
                              * {{{complex-extended}}}: extended BeansWrapper used in most rendering,
                                and that (currently, 2016-10-20) implements screen html auto-escaping.
                                the function will try to determine the escaping language needed (usually html).
                                Produces complex maps that double as beans.
                              * {{{complex-extended-simplemap}}}: Version of {{{complex-extended}}} that produces simple maps (SimpleMapAdapter).
                              * {{{basic-adapter}}}: The Freemarker DefaultObjectWrapper, with adapters enabled; performs NO auto html-escaping
                              * {{{basic-copy}}}: The Freemarker DefaultObjectWrapper, with adapters disabled, forcing object copies; performs NO auto html-escaping
                                NOTE: Copies are slow to produce, but faster to access afterward, compared to adapters.
                              * {{{basic}}}: same as either {{{basic-adapter}}} or {{{basic-copy}}}, at function's discretion (currently prefers adapters)
    mode                    = (always-deep|, default: always-deep) Rewrapping mode
                              Values:
                              * {{{always-deep}}}: force re-wrapping even if target appears adequate, including any
                                and all children - this the safest and most reliable, albeit slowest.
                                WORKS FOR: any value and wrapper type (as long as the value is a TemplateModel properly recognized by
                                Freemarker's DeepUnwrap function)
                              NOTE: For optimization reasons, in the future, the default value may be changed (to something other than "always"),
                                  to something better optimized, but the result will not change. If you absolutely count on the "always" strict
                                  behavior (for some reason), you should pass it explicitly, but in most cases you
                                  should not specify it or you should pass explicit empty.
                              TODO: add support for always (non-deep), needed, needed-deep and some "fast"/dangerous variants

  * History *
    Rewritten for 1.14.2.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function rewrapObject object wrapper="" mode="">
</#function>
-->

<#-- 
*************
* rewrapString
************
Re-wraps a string using the Freemarker ObjectWrapper in the current environment (by default)
or a different ObjectWrapper, such as one producing simple adapters or simple types.
Alias for #rewrapObject but expected to receive only strings.

With default wrapper and mode, this can be seen as the reverse operation of #rawString, or re-enabling
screen renderer html auto-escaping (if enabled) for a specific value.

  * Related *
    #rewrapObject
    
  * History *
    Added for 1.14.2.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function rewrapString object mode="">
</#function>
-->

<#-- 
*************
* rewrapMap
************
Re-wraps a map using the Freemarker ObjectWrapper in the current environment (by default)
or a different ObjectWrapper, such as one producing simple adapters or simple types.
Alias for #rewrapObject but expected to receive only maps.

  * Related *
    #rewrapObject
    
  * History *
    Rewritten for 1.14.2.
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function rewrapMap object mode="">
</#function>
-->

<#-- 
*************
* toSimpleMap
************
Takes a bean-wrapped map and switches it to a simple map adapter instead, without performing
any copies. 

This is similar to {{{#rewrapObject}}} except 

If the object is not a complex map but already another type of map, returns it as-is. Other types throw errors.

NOTE: This only changes the complexity of the map; it does NOT prevent auto-escaping. In fact, if
    called on certain types of unescaped complex maps, this function may cause auto-escaping to return, which
    is why its behavior is to leave maps alone unless they are complex bean maps.
    Calling {{{?keys}}} on this map may give escaped keys; use #mapKeys to prevent this.

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

WARN (2016-12-02): Do not rely on this for keying back the values out of the original
    map; you should still call #toSimpleMap on the original map
    before getting the values out of it. This is because BeansWrapper-mapped maps
    treat some key values such as the "class" key as special.

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
    
  * History *
    Added for 1.14.2.
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
    ${object?c}<#t>
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
      <#-- NOTE: 2016-12-02: can't rely on mapKeys, because special keys like "class" will fail; must use toSimpleMap
      <#list mapKeys(object) as key>--><#t>
      <#local object = toSimpleMap(object)><#t>
      <#list object?keys as key>
        <#-- NOTE: must use rawString on the keys because FTL will coerce them to strings (forcing auto-escaping from Ofbiz context) before using them as hash keys! --><#t>
        <#local rawKey = rawString(key)><#t>
          "${escapeScriptString(lang, key, escape)}" : <#if object[rawKey]??><#rt/>
            <#t/><#if !rawVal?is_boolean><#local rawValNext = rawVal[rawKey]!false><#else><#local rawValNext = false></#if>
            <#t/><@objectAsScript lang=lang object=object[rawKey] wrap=true escape=escape maxDepth=maxDepth currDepth=(currDepth+1) rawVal=rawValNext/>
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
Wraps pre-escaped values for specific languages in a special wrapper object. 
When passed to markup- or script-handling macros which normally escape values in these languages,
the values are used as-is with no additional escape.

These wrapper objects are automatically recognized by #escapeVal and #escapeFullUrl.

gets included as
a raw string bypassing html, js or other language escaping. 
This include @objectAsScript and macros that escape values using #escapeVal or #escapeFull.

WARN: This is only safe to use if an explicit language is passed and the pre-escaping performed
    is adequate for that language.

NOTE: This has no functional relationship to Ofbiz's StringWrapper ({{{StringUtil.wrapString}}} or #rawString);
    its working scope is unrelated to Ofbiz's screen auto-escaping. It is primarily intended
    for Scipio macros and templates that use #escapeVal or equivalents.

For more information about escaping in general, see >>>standard/htmlTemplate.ftl<<<.

  * Parameters *
    value                   = ((string)|(map)) The value to wrap OR a map of languages to strings
                              If this is a string, {{{lang}}} parameter should always be specified.
                              If this is a map, it is a map of languages to strings, for example:
                                {"htmlmarkup":"<em>my title</em>", "raw":"my title"}
                                {"htmlmarkup":"<em>${getLabel('CommonYes')}</em>", "raw":rawLabel('CommonYes')}
                              The supported map languages are the same as the single {{{lang}}} parameter, except 
                              that "script" has no meaning in this case and should not be used, and in most cases,
                              at least {{{raw}}} should be specified.
                              This map version allows templates to specify alternate markup for different languages.
    lang                    = (html|js|json|script|...|, default: -empty/unspecific-) The specific language toward which this should be considered "raw"
                              This accepts dash-separated string of names.
                              Values (and special values):
                              * {{{script}}}: for use with @objectAsScript: prevents both escaping and enclosing string literals
                              * {{{htmlmarkup}}}: html destined for markup only. This may contain html markup
                                as it will never be used within html attributes.
                              * {{{html}}}: html destined for html attributes or markup. This must NOT contain
                                html markup (elements) because this may get inserted into html attributes.
                              NOTE: If {{{object}}} is a string (not map), this argument is usually '''required''' for safety and correctness.
                              WARN: Leaving empty with string object will prevent macro escaping for any language! In virtually all cases you should specify
                                  a specific language. The unspecific mode is for rare workarounds only.

  * Related *
    #escapeVal
    #escapeFullUrl 
    
  * History *
    Added for 1.14.2.                     
-->
<#function wrapAsRaw value lang="">
  <#if isObjectType("map", value)>
    <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].wrap(value)>
  <#else>
    <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].wrap(value?string, lang)>
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
    
  * History *
    Added for 1.14.2.
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
    
  * History *
    Added for 1.14.2.
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
Wraps a script value in a special wrapper that marks it as raw script code. 
When passed to @objectAsScript will cause the script string to be included as-is, instead of
being interpreted as a value to escape.

Alias for {{{#wrapAsRaw(value, "script")}}}, and easier to remember in relation to @objectAsScript.
                   
  * Parameters *
    object                  = the string to wrap

  * Related *
    @objectAsScript
    #wrapAsRaw
    
  * History *
    Added for 1.14.2.
-->
<#function wrapRawScript value>
  <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].wrap(value?string, "script")>
</#function>

<#-- 
*************
* isRawScript
************
Checks if the value was wrapped using {{{#wrapAsRaw(object, "script")}}}.

''Added for 1.14.2''.
            
  * Parameters *
    object                  = the object to check
    
  * Related *
    #wrapRawScript
    @objectAsScript
-->
<#function isRawScript object>
  <#return Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].isRawScript(object, "script")>
</#function>

<#-- 
*************
* escapeVal
************
Escapes an individual value or code "part" for a given language, ignoring and crushing delimiters.

WARN: 2016-10-10: {{{css}}} not currently implemented. '''Do not pass''' input of unsafe origin for CSS to this method at this time!

Essentially this is a wrapper around #rawString and language encoders. It abstracts the encoder selection.
It first performs a #rawString call to bypass the screen auto-escaping on the value.

In addition, this function accepts values produced by #wrapAsRaw. These can be used to bypass the escaping in part or in full.
If the value was wrapped using the same language as specified in this call, the wrapped value will be used as-is.

NOTES: 
* 2016-10-05: Currently, this is mostly implemented using Freemarker built-ins such as {{{?html}}}, {{{?js}}}, etc. but is subject to change.
  Although Freemarker built-ins can still be used directly in templates and client macros, use of this function 
  is recommended to centralize the escaping logic, automatically prevent some forms of double-escaping, create better versatility,
  and help hasten resolution of security issues.
  IMPL NOTE: All Scipio standard API implementations, however, should strictly use this function.

For more information about escaping in general, see >>>standard/htmlTemplate.ftl<<<.

'''Single languages'''

''HTML'': Two language identifiers are supported: "html" and "htmlmarkup". 
Callers of #escapeVal should usually use "html" on html attributes, and "htmlmarkup" on text placed within element body.
By default, this function escapes all markup delimiters for ''both''.
The difference is that callers upstream using #wrapAsRaw will then able to override markup specifically using:
  #wrapAsRaw(xxx, 'htmlmarkup')
or for all html ('''only''' if careful to escape for attributes safely) using:
  #wrapAsRaw(xxx, 'html')
"html" is also a special case because it also fills in for "htmlmarkup" (if not included in the #wrapAsRaw call),
but not vice-versa since markup cannot be used in attributes.
NOTE: Callers upstream using #wrapAsRaw should usually use "htmlmarkup" in most cases.

''Javascript, JSON'': Only simple single string parts are supported via "js" and "json" (and some variants).
This can ONLY encode strings between {{{""}}} or {{{''}}}.
It is impossible to safely encode javascript outside string literals (see OWASP).

'''Composed languages'''

''Javascript and HTML'': For Javascript strings placed within HTML attributes (e.g. events such as {{{onchange}}}), 
typically "js-html" is needed. 
NOTE: From template perspective, macros generally escape html by default, so templates only need to escape the javascript part.

'''Validation and allowed code'''

This function is scheduled to support ''some'' language-specific code validation/filters, to provide
a middle ground between allowing no code (full escaping - default behavior) and allowing all code (no escaping - #rawString alone).
See the {{{opts.allow}}} parameter below.

It is usually logical to perform such validation nearest to point-of-use as possible using this function.
However, note that, ''in addition'', it is usually either a very good idea or simply required to validate input 
such as code markup before storage in database (server-side, or even client-side yet in addition), long before
and elsewhere from a template containing this function call is rendered.
Thus, the safest solution is to do both; validate markup in content from actors server-side, and apply
the equivalent filter using this function in templates. This guards against errors and stale content spanning security policy 
and filter implementation changes.

NOTE: Validation and allowed code filters are not fully implemented (TODO), but will be supported by this function.

  * Parameters *
    value                   = The string or string-like value to escape
                              2016-09-29: This now automatically coerces non-strings to string, for convenience.
    lang                    = (js|jsdq|json|html|htmlmarkup|url|xml|css|js-html|html-js|htmlmarkup-js|css-html|html-css|raw) The target language for escaping
                              These are analogous to the Freemarker built-in counterparts of same names, but
                              with implementation details subject to change.
                              In composed types, the order is meaningful, such that "js-html" performs like {{{?js?html}}}
                              and "html-js" like {{{?html?js}}}.
                              Values and special values:
                              * {{{jsdq}}}: special case of {{{js}}} where it is assumed the value will be contained in double quotes, 
                                such that single quotes don't need to be escaped.
                              * {{{html}}}: safely escapes ''any'' html, but primarily attribute content
                              * {{{htmlmarkup}}: safely escapes only ''markup'' html, but not (necessarily) attributes. Must be
                                used to allow callers to insert html markup using #wrapAsRaw in the right places (not in attributes!).
                                NOTE: by default this can safely escapes any html, even for attributes; 
                                    but the caller overrides (#wrapAsRaw) can make the value unsafe for insertion in attributes, so
                                    the distinction is important.
                              WARN: 2016-10-10: {{{css}}} not currently implemented. '''Do not pass''' input of unsafe origin for CSS to this method at this time!
                              NOTE: The previous language name "style" has been deprecated and will be removed. Use {{{css}}} instead, even if not implemented.
    opts                    = ((map)) Additional options, including language-specific options
                              Members:
                              * {{{strict}}} {{{((boolean), default: false)}}} Whether to escape strictly or allow handling of pre-escaped characters
                                If true, escaping is always applied unconditionally, and any pre-escaped characters
                                are not recognized (and ''may'' be errors if due to double-escaping errors).
                                If false, the function ''may'' attempt heuristics to prevent double-escaping issues (not always desirable),
                                mainly to mitigate screen auto-escaping and early escaping.
                              * {{{allow}}} {{{((string)|any|none|...|, default: none)}}} Allowed code exceptions (validation filter)
                                By default, no code exceptions are allowed ("none"), and regular aggressive escaping is applied.
                                At the other extreme ({{{any}}}), escaping may be disabled entirely.
                                In between, each language may support filtering levels or profiles to restrict allowed code. 
                                The possible values depend on the language.
                                Recognized {{{allow}}} filters:
                                * {{{htmlmarkup}}}: {{{(any|none|any-valid|internal|external, default: none)}}}
                                  * {{{none}}}: no HTML elements or code allowed, regular escaping applied (default behavior)
                                  * {{{external}}}: allow only very basic HTML elements that are always safe to use, even from
                                    and assuming coming from completely untrusted sources (public)
                                    NOTE: 2016-10-20: NOT IMPLEMENTED. Currently does same as {{{none}}}.
                                  * {{{internal}}}: allow HTML from trusted sources (employees)
                                    NOTE: 2016-10-20: NOT IMPLEMENTED. Currently does same as {{{any}}}.
                                  * {{{any-valid}}}: any HTML allowed, as long as it is well-formed
                                    NOTE: 2016-10-20: NOT IMPLEMENTED. Currently does same as {{{any}}}.
                                  * {{{any}}}: escaping disabled/bypassed, for debugging purposes

  * Related *
    #rawString
    #wrapAsRaw
    #escapeFullUrl
    
  * History *
    Added for 1.14.2, previously known as {{{escapePart}}}.
-->
<#function escapeVal value lang opts={}>
  <#if lang?contains("style")><#-- DEPRECATED: TODO: remove (slow) -->
    <#local lang = lang?replace("style", "css")>
  </#if>
  <#local resolved = Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].resolveScriptForLang(value, lang)!false>
  <#if resolved?is_boolean>
    <#local value = rawString(value)><#-- performs coercion to string if needed -->
  <#else>
    <#local value = rawString(resolved.value)><#-- NOTE: this rawString call actually only escapes the ofbiz auto-escaping from the resolveScriptForLang call... obscure -->
    <#local lang = resolved.lang>
  </#if>
  <#switch lang?lower_case>
    <#case "json">
      <#return value?json_string>
      <#break>
    <#case "js">
      <#return value?js_string>
      <#break>
    <#case "jsdq">
      <#return value?js_string?replace("\\'", "\'")>
      <#break>
    <#case "html">
      <#return value?html>
      <#break>
    <#case "htmlmarkup">
      <#if opts.allow?has_content>
        <#-- TODO: implement external, internal, any-valid -->
        <#switch opts.allow>
          <#case "any">
            <#return value>
            <#break>
          <#case "any-valid">
            <#return value><#-- TODO: NOT IMPLEMENTED (validation library required) -->
            <#break>
          <#case "internal">
            <#return value><#-- TODO: NOT IMPLEMENTED (validation library required) -->
            <#break>
          <#case "external">
            <#return value?html><#-- TODO: NOT IMPLEMENTED (validation library required) -->
            <#break>
          <#case "none">
          <#default>
            <#return value?html>
            <#break>
        </#switch>
      <#else>
        <#return value?html>
      </#if>
      <#break>
    <#case "js-html">
      <#return value?js_string?html>
      <#break>
    <#case "html-js">
    <#case "htmlmarkup-js">
      <#return value?html?js_string>
      <#break>
    <#case "xml">
      <#return value?xml>
      <#break>
    <#case "url">
      <#return value?url("UTF-8")><#-- FIXME: lang should not be hardcoded, ofbiz config issue -->
      <#break>
    <#case "css">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value))> -->
      <#return value>
      <#break>
    <#case "css-html">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value))?html>-->
      <#return value?html>
      <#break>
    <#case "html-css">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value?html))>-->
      <#return value?html>
      <#break>
    <#case "raw">
    <#default>
      <#return value>
      <#break>
  </#switch>
</#function>

<#-- alternative super-simple implementation, TODO: review if worth its own alias
<#function escapeHtmlMarkup value allow="">
    <#switch rawString(allow)>
      <#case "any">
        <#return value>
        <#break>
      <#case "any-valid">
        <#return value>
        <#break>
      <#case "internal">
        <#return value>
        <#break>
      <#case "external">
        <#return value?html>
        <#break>
      <#case "none">
      <#default>
        <#return value?html>
        <#break>
    </#switch>
</#function>
-->

<#-- 
*************
* escapePart
************
Escapes an individual value or code "part" for a given language, ignoring and crushing delimiters.
DEPRECATED: Replaced by #escapeVal.

  * Related *
    #escapeVal
-->
<#function escapePart value lang opts={}>
  <#return escapeVal(value, lang, opts)>
</#function>

<#-- 
*************
* escapeFull
************
Encodes/escapes a value in a given language.
DEPRECATED: This was never properly defined or implemented and no longer meaningful. Use #escapeVal or #escapeFullUrl.

  * Related*
    #escapeVal
    #escapeFullUrl
-->
<#function escapeFull value lang opts={}>
  <#if lang?contains("style")><#-- DEPRECATED: TODO: remove (slow) -->
    <#local lang = lang?replace("style", "css")>
  </#if>
  <#local resolved = Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].resolveScriptForLang(value, lang)!false>
  <#if resolved?is_boolean>
    <#local value = rawString(value)><#-- performs coercion to string if needed -->
  <#else>
    <#local value = rawString(resolved.value)><#-- NOTE: this rawString call actually only escapes the ofbiz auto-escaping from the resolveScriptForLang call... obscure -->
    <#local lang = resolved.lang>
  </#if>
  <#if lang == "htmlmarkup">
    <#local lang = "html">
  </#if>
  <#-- FIXME -->
  <#-- NOTE: Currently we support almost the same types as Ofbiz, so no need for a switch -->
  <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode(lang, value))>
</#function>

<#-- 
*************
* escapeFullUrl
************
Escapes a complete URL for safe insertion in code of a given language.

WARN: 2016-10-10: {{{css}}} not currently implemented. '''Do not pass''' input of unsafe origin for CSS to this method at this time!

Essentially this is a wrapper around #rawString and language encoders. It abstracts the encoder selection.
It first performs a #rawString call to bypass the screen auto-escaping on the value.

For compability reasons, this function when not in strict mode (default is not strict) accepts param delimiters 
in either the escaped {{{&amp;}}} form or unescaped {{{&}}} form.
Ideally, such escaped delimiters would not be received, but they are very prevalent in Ofbiz due
to early escaping.

In addition, this function accepts values produced by #wrapAsRaw. These can be used to bypass the escaping in part or in full.
If the value was wrapped using the same language as specified in this call, the wrapped value will be used as-is.
Note that most times, this is not nearly as useful as it is for #escapeVal.

NOTES: 
* 2016-10-05: Currently, this is mostly implemented using Freemarker built-ins such as {{{?html}}}, {{{?js}}}, etc. but is subject to change.
  Although Freemarker built-ins can still be used directly in templates and client macros, use of this function 
  is recommended to centralize the escaping logic, automatically prevent some forms of double-escaping, create better versatility,
  and help hasten resolution of security issues.
  IMPL NOTE: All Scipio standard API implementations, however, should strictly use this function.

For more information about escaping in general, see >>>standard/htmlTemplate.ftl<<<.

  * Usage Examples *
  
    <a href="${escapeFullUrl('http://www.ilscipio.com/test?param1=val1&param2=val2;jsessionid=fake', 'html')}">Link</a>

    <@script>
      var link = "${escapeFullUrl('http://www.ilscipio.com/test?param1=val1&param2=val2;jsessionid=fake', 'js')}";
    </@script>

  * Parameters *
    value                   = The string or string-like value to escape
    lang                    = (html|js|jsdq|json|js-html|jsdq-html|xml|css|raw) The target language
                              * {{{jsdq}}}: special case of js where it is assumed the value
                                will be contained in double quotes, such that single quotes
                                don't need to be escaped.
                              WARN: 2016-10-10: {{{css}}} not currently implemented. '''Do not pass''' input of unsafe origin for CSS to this method at this time!
                              NOTE: The previous language name "style" has been deprecated and will be removed. Use {{{css}}} instead, even if not implemented.
                              WARN: Inserting URLs into CSS (using {{{url()}}}) is known to be unsafe even with escaping.
    opts                    = ((map)) Additional options, including lang-specific options
                              Members:
                              * {{{strict}}} {{{((boolean), default: false)}}} Whether to escape strictly or allow handling of pre-escaped characters
                                If true, escaping is always applied unconditionally, and any pre-escaped characters
                                are not recognized (and ''may'' be errors if due to double-escaping errors).
                                If false, the function ''may'' attempt heuristics to prevent double-escaping issues (not always desirable),
                                mainly to mitigate screen auto-escaping and early escaping.
                                NOTE: 2016-10-20: Currently, when strict false (default), the method will
                                    tolerated pre-escaped param-separator ampersands "&amp;" ONLY.
                                    In the future this behavior could be removed if all pre-escaping is eliminated in the framework.
                                    It is recommended NOT to write them in templates when passing links to Scipio macro parameters
                                    or to this function or equivalent.
-->
<#function escapeFullUrl value lang opts={}>
  <#if lang?contains("style")><#-- DEPRECATED: TODO: remove (slow) -->
    <#local lang = lang?replace("style", "css")>
  </#if>
  <#local resolved = Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].resolveScriptForLang(value, lang)!false>
  <#if resolved?is_boolean>
    <#local value = rawString(value)><#-- performs coercion to string if needed -->
  <#else>
    <#local value = rawString(resolved.value)><#-- NOTE: this rawString call actually only escapes the ofbiz auto-escaping from the resolveScriptForLang call... obscure -->
    <#local lang = resolved.lang>
  </#if>
  <#if !(opts.strict!false)>
    <#-- Ofbiz compatibility mode: Replace &amp; back to &. Freemarker's ?html (or any working encoder) will re-encode them after. -->
    <#local value = value?replace("&amp;", "&")>
  </#if>
  <#switch lang?lower_case>
    <#case "json">
      <#return value?json_string>
      <#break>
    <#case "js">
      <#return value?js_string>
      <#break>
    <#case "jsdq">
      <#return value?js_string?replace("\\'", "\'")>
      <#break>
    <#case "html">
    <#case "htmlmarkup">
      <#return value?html>
      <#break>
    <#case "js-html">
      <#return value?js_string?html>
      <#break>
    <#case "html-js">
    <#case "htmlmarkup-js">
      <#return value?html?js_string>
      <#break>
    <#case "jsdq-html">
      <#return value?js_string?replace("\\'", "\'")?html>
      <#break>
    <#case "html-jsdq">
    <#case "htmlmarkup-jsdq">
      <#return value?html?js_string?replace("\\'", "\'")>
      <#break>
    <#case "xml">
      <#return value?xml>
      <#break>
    <#case "css">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value))> -->
      <#return value>
      <#break>
    <#case "css-html">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value))?html>-->
      <#return value?html>
      <#break>
    <#case "html-css">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value?html))>-->
      <#return value?html>
      <#break>
    <#case "raw">
    <#default>
      <#return value>
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
    
  * History *
    Added for 1.14.2.
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
  <#if classes?has_content> class="${escapeVal(classes, 'html')}"</#if><#t>
</#macro>

<#-- 
*************
* compiledClassAttribStrExplicit
************
Explicit version of #compiledClassAttribStr.
-->
<#macro compiledClassAttribStrExplicit class defaultVal="">
  <#local classes = compileClassArgExplicit(class, defaultVal)>
  <#if classes?has_content> class="${escapeVal(classes, 'html')}"</#if><#t>
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
  <#if !(class?has_content)>
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
                              
  * History *
    Added for 1.14.2.
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
    escapeLang                  = (html|...|none, default: 'html') Language to escape each attribute (passed to #escapeVal)
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
    
  * History *
    Added for 1.14.2.
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
    
  * History *
    Added for 1.14.2.
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
    
WARN: The locale and timeZone (explicit or from context) should not resolve to null/empty;
    if they do a log warning is printed.
    
  * Parameters *
    date                    = ((date), required) The date
    dateType                = (date-time|timestamp|date|time, default: date)
                              "timestamp" and "date-time" are synonymous.  
    defaultVal              = If no output is produced (empty), this value (string) will be shown instead
    locale                  = ((locale), default: -locale from context-) Override locale
    timeZone                = ((timezone), default: -timeZone from context-) Override time zones
    
  * Related *
    @formattedDateTime, @formattedTime, #formatDate, #formatDateTime, #formatTime
-->
<#macro formattedDate date dateTimeFormat="" locale=true timeZone=true defaultVal="" dateType="date">
  ${formatDate(date, dateTimeFormat, locale, timeZone, dateType)!defaultVal}<#t>
</#macro>

<#-- 
*************
* formattedDateTime
************
Renders a formatted date-time value (convenience wrapper).

WARN: The locale and timeZone (explicit or from context) should not resolve to null/empty;
    if they do a log warning is printed.
    
  * Related *
    @formattedDate
-->
<#macro formattedDateTime date dateTimeFormat="" locale=true timeZone=true defaultVal="">
  ${formatDate(date, dateTimeFormat, locale, timeZone, "timestamp")!defaultVal}<#t>
</#macro>

<#-- 
*************
* formattedTime
************
Renders a formatted time value (convenience wrapper).

WARN: The locale and timeZone (explicit or from context) should not resolve to null/empty;
    if they do a log warning is printed.
    
  * Related *
    @formattedDate
-->
<#macro formattedTime date dateTimeFormat="" locale=true timeZone=true defaultVal="">
  ${formatDate(date, dateTimeFormat, locale, timeZone, "time")!defaultVal}<#t>
</#macro>

<#-- 
*************
* formatDate
************
Formats a date.

These functions return VOID (no value) if no or empty output, so default value operator can be used.

NOTE: The result is auto-HTML-escaped (where applicable); use #rawString to prevent.

WARN: The locale and timeZone (explicit or from context) should not resolve to null/empty;
    if they do a log warning is printed.

  * Related *
    @formattedDate
-->
<#-- IMPLEMENTED AS TRANSFORM
<#function formatDate date dateTimeFormat="" locale=true timeZone=true dateType="date">
</#function>
-->

<#-- 
*************
* formatDateTime
************
Formats a date-time value.

These functions return VOID (no value) if no or empty output, so default value operator can be used.

WARN: The locale and timeZone (explicit or from context) should not resolve to null/empty;
    if they do a log warning is printed.

  * Related *
    @formattedDate
-->
<#function formatDateTime date dateTimeFormat="" locale=true timeZone=true>
  <#local res = formatDate(date, dateTimeFormat, locale, timeZone, "timestamp")!"">
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

WARN: The locale and timeZone (explicit or from context) should not resolve to null/empty;
    if they do a log warning is printed.

  * Related *
    @formattedDate
-->
<#function formatTime date dateTimeFormat="" locale=true timeZone=true>
  <#local res = formatDate(date, dateTimeFormat, locale, timeZone, "time")!"">
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
  <#return rewrapMap(Static["com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil"].getHeadingElemSpecFromStyleStr(styleStr, containerStyleStr,
    allowedHeadingElemTypes, allowedElemTypes, allowedContainerElemTypes, cacheId), "raw-simple")>

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
* limitStrValToItems
************
Ensures that the given string value is contained in a field items list, 
by only returning the value if it is in fact contained or void (missing value) otherwise.

This is usually used in conjunction with @field {{{items}}} parameter for field types {{{select}}}, {{{radio}}} or {{{checkbox}}}.

If the value is matched, the value itself is returned; otherwise void (no result) is returned.
This should always be combined with one of the missing value Freemarker operators (??, !).

The value and item values automatically get the #rawString operation applied (along with coercion to string);
however, the original value is returned and not the raw one.

NOTE: for performance and other reasons it is usually best to have a groovy script do this, but this is a frequently needed operation.

  * Parameters *
    value                   = ((string), required) Value to check
    items                   = ((list), required) Field items list, a list of maps, each map containing a {{{value}}} key
                              Alternatively, this may be a map, in which case the map keys are checked instead.
    key                     = (string, default: value) The name of the key to check for each map entry in the items list                          
    
  * Return Value *
    the original value if it matched an item, or void if no item was matched (for use with missing value operator).
-->
<#function limitStrValToItems value items key="value">
  <#local val = rawString(value)>
  <#if items?is_sequence>
    <#list items as item>
      <#if rawString(item[key]!"") == val>
        <#return value>
      </#if>
    </#list>
  <#elseif isObjectType("map", items)>
    <#if items[val]??>
      <#return value>
    </#if>
  </#if>
  <#-- (return void if nothing matched) -->
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
  <#-- DEV NOTE: the conditions below almost look wrong, but they are actually correct and are here to prevent useless double-lookups already covered by the first check above -->
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
        <td style="width:200px; vertical-align:top">${escapeVal(key, 'html')}</td>
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
        ${escapeVal(var, 'html')}
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
            <tr><td>${escapeVal(key, 'html')}</td><td><@printVar value=(var[key]!"") platform=platform maxDepth=maxDepth currDepth=(currDepth+1)/></td></tr>
          </#list>
          </table>-->
          <@objectAsScript lang="json" escape=false object=var maxDepth=maxDepth currDepth=currDepth />
        </#if>
      <#elseif var?is_string>
        ${escapeVal(var, 'html')}
      </#if>
    <#recover>
      <span style="color:red"><strong>${(.error)!"(generic)"}</strong></span>
    </#attempt>
  </#if>
</#macro>

