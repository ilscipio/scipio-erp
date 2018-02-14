<#--
* 
* Forms
*
* In the same fashion as the content elements, SCIPIO form elements are meant as a mean to standardize
* the development of complexe forms. The elements follow standard html patterns, with the only difference
* being the @field attribute replacing the input tag. 
*
* All elements come with additional options, that simplifies their use quite a bit. A good example are the
* <@field definitions, where each element are wrapped in containing rows and cells, labels added when defined 
* and ids set automatically. The use of the SCIPIO elements largely simplify the creation of complexe forms and
* standardize the html output for visualization.
* 
* Included by htmlTemplate.ftl.
*
* NOTES: 
* * May have implicit dependencies on other parts of Scipio API.
*
-->

<#include "htmlFormFieldWidget.ftl">

<#-- 
*************
* Form
************
Defines a form. Analogous to <form> HTML element.

[[[<img src="http://www.scipioerp.com/files/2016/05/fields.png" alt="" style="height:400px"/>]]]

  * Usage Examples *  
    <@form name="myform">
      <@fields>
        <input type="hidden" ... />
        <@field ... />
        <@field ... />
      </@fields>
    </@form>            
                    
  * Parameters *
    type                    = (input|display, default: input) Form type
                              DEV NOTE: "display" is special for time being, probably rare or unused;
                                  maybe it should cause to omit <form> element
    id                      = Form ID                              
    class                   = ((css-class)) CSS classes on form element itself
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)  
    attribs                 = ((map)) Extra attributes for HTML <form> element 
                              Needed for names containing dashes.
                              NOTE: These are automatically HTML-escaped, but not escaped for javascript or other languages (caller responsible for these).
    validate                = ((boolean), default: -implicit-) If true, adds an explicit default validation script to the form (e.g. jQuery validate)
                              NOTE: in many cases forms receive this automatically through submit button even if this is false (through global JS);
                                  this is only needed in special cases.
                              NOTE: only works if {{{id}}} or {{{name}}} present
                              Added 2017-09-29.
    inlineAttribs...        = ((inline-args)) Extra attributes for HTML <form> element
                              NOTE: camelCase names are automatically converted to dash-separated-lowercase-names.
                              NOTE: These are automatically HTML-escaped, but not escaped for javascript or other languages (caller responsible for these).
                              
  * History *
    Added explicit validate option (1.14.4).
-->
<#assign form_defaultArgs = {
  "type":"input", "name":"", "id":"", "class":"", "open":true, "close":true, "validate":"",
  "attribs":{}, "passArgs":{}
}>
<#macro form args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.form_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local attribs = makeAttribMapFromArgMap(args)>
  <#local origArgs = args>
<@renderTarget dirName="form" id=id dirArgs=args>
  <#if open>
    <#local formInfo = {"type":type, "name":name, "id":id}>
    <#local dummy = pushRequestStack("scipioFormInfoStack", formInfo)>
  </#if>

  <#if open && !close>
    <#local dummy = pushRequestStack("scipioFormMarkupStack", {
      "type":type, "name":name, "id":id, "class":class, "attribs":attribs, "origArgs":origArgs, "passArgs":passArgs
    })>
  <#elseif close && !open>
    <#local stackValues = popRequestStack("scipioFormMarkupStack")!{}>
    <#local dummy = localsPutAll(stackValues)>
  </#if>
  <@form_markup type=type name=name id=id class=class open=open close=close attribs=attribs origArgs=origArgs passArgs=passArgs><#nested></@form_markup>
  <#if validate?is_boolean && validate == true>
      <@formValidateScript type=type name=name id=id htmlwrap=true/>
  </#if>
  <#if close>
    <#local dummy = popRequestStack("scipioFormInfoStack")>
  </#if>
</@renderTarget>
</#macro>

<#-- @form main markup - theme override -->
<#macro form_markup type="" name="" id="" class="" open=true close=true attribs={} origArgs={} passArgs={} catchArgs...>
  <#if open>
    <form<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#rt>
      <#lt><#if name?has_content> name="${escapeVal(name, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs /></#if>>
  </#if>
      <#nested>
  <#if close>
    </form>
  </#if>
</#macro>

<#-- @form validate script - (TODO: document once better established)
    NOTE: the code may be enclosed in another javascript code block by caller - beware -->
<#macro formValidateScript formExpr="" name="" id="" htmlwrap=true onload=true catchArgs...>
  <@script htmlwrap=htmlwrap>
    <#if !formExpr?has_content>
      <#if id?has_content>
        <#local formExpr>"#${escapeVal(id, 'js')}"</#local>
      <#elseif name?has_content>
        <#local formExpr>document['${escapeVal(name, 'js')}']</#local>
      </#if>
    </#if>
    <#if formExpr?has_content>
      <#-- NOTE: 2017-09-29: this onload trigger is new, may have been an error in stock ofbiz -->
      <#if onload>jQuery(document).ready(function() {</#if>
          jQuery(${formExpr}).validate({
              submitHandler: function(form) {
                  form.submit();
              }
          });
      <#if onload>});</#if>
    </#if>
  </@script>
</#macro>

<#-- 
*************
* Progress Script
************
Generates script data and markup needed to make an instance to initialize upload progress 
javascript anim for a form, with progress bar and/or text.

The server-side upload event for the form must register a Java FileUploadProgressListener in session
for getFileUploadProgressStatus controller AJAX calls.
                    
  * Parameters *
    enabled                 = ((boolean), default: true) If true, disables whole macro
                              Occasionally needed in templates as FTL workaround.
    progressOptions         = ((map)) Elem IDs and options passed to ScipioUploadProgress Javascript class
                              In addition, supports: 
                              * {{{submitHook}}}: one of: 
                                * {{{formSubmit}}}: The default
                                * {{{validate}}}: Use jquery validate
                                * {{{none}}}: Caller does manually 
                              * {{{validateObjScript}}}: If submitHook is "validate", add this script text to jquery validate({...}) object body
                                WARN: this is NOT js-escaped by the macro - caller is responsible for escaping.
                              See ScipioUploadProgress javascript class for available options.
    htmlwrap                = ((boolean), default: true) If true, wrap in @script
-->
<#assign progressScript_defaultArgs = {
  "enabled":true, "htmlwrap":true, "progressOptions":{}, "passArgs":{}
}>
<#macro progressScript args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.progressScript_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#if enabled>
    <#if progressOptions?has_content && progressOptions.formSel?has_content>
      <@script htmlwrap=htmlwrap>
        <@requireScriptOfbizUrl uri="getFileUploadProgressStatus" htmlwrap=false/>
 
          (function() {
              var uploadProgress = null;
          
              jQuery(document).ready(function() {
                <#if progressOptions.successRedirectUrl??>
                  <#-- shouldn't have &amp; in script tag... but code may escape and should support... -->
                  <#local progressOptions = concatMaps(progressOptions, {"successRedirectUrl":progressOptions.successRedirectUrl?replace("&amp;", "&")})>
                </#if>
                  uploadProgress = new ScipioUploadProgress(<@objectAsScript lang="js" object=progressOptions />);
                  uploadProgress.reset();
              });
              
            <#if (progressOptions.submitHook!) == "validate">
              jQuery("${escapeVal(progressOptions.formSel, 'js')}").validate({
                  submitHandler: function(form) {
                      var goodToGo = uploadProgress.initUpload();
                      if (goodToGo) {
                          form.submit();
                      }
                  },
                  ${progressOptions.validateObjScript!""}
              });
            <#elseif (progressOptions.submitHook!) != "none" >
              jQuery("${escapeVal(progressOptions.formSel, 'js')}").submit(function(event) {
                  var goodToGo = uploadProgress.initUpload();
                  if (!goodToGo) {
                      event.preventDefault();
                  }
              });
            </#if>
          })();
      </@script>
    </#if>
  </#if>
</#macro>

<#-- 
*************
* Progress Bar 
************
A progress bar.

[[[<img src="http://www.scipioerp.com/files/2016/05/progress.png" alt=""/>]]]

Can be animated using Javascript manually or by using progressOptions argument.  
Presence of progressOptions activates use of ScipioUploadProgress script for this progress bar by linking it 
to a form submit.

  * Usage Examples *  
    <@progress value=40/>             
    
    Javascript animation (manual):
    $('#${id}_meter').css("width", "78%");
                     
  * Parameters *
    value                   = ((int)) Percentage done
    id                      = Custom ID; can also be specified as progressOptions.progBarId instead
                              The meter will get an id of "${id}_meter".
                              If omitted, no progress bar per se will be created, but script will still be generated for progressOptions.progTextBoxId.
    type                    = (alert|success|info, default: info)
    class                   = ((css-class)) CSS classes
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    containerClass          = ((css-class)) Classes added only on container
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    showValue               = Display value inside bar
    progressArgs            = ((map)) If present, attaches progress bar to an upload form with javascript-based progress
                              Attaches results to page using elem IDs and options specified via these arguments,
                              which are passed to @progress macro (see @progress macro for supported options)
    progressOptions         = ((map)) Convenience parameter; same as passing:
                              progressArgs={"enabled":true, "progressOptions":progressOptions}
-->
<#assign progress_defaultArgs = {
  "value":0, "id":"", "type":"", "class":"", "showValue":false, "containerClass":"", "progressArgs":{}, 
  "progressOptions":{}, "passArgs":{}
}>
<#macro progress args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.progress_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  
  <#local value = value?number>
  
  <#local progressOptions = progressArgs.progressOptions!progressOptions>
  <#local explicitId = id?has_content>
  <#if !id?has_content>
    <#local id = (progressOptions.progBarId)!"">
  </#if>

  <#if !type?has_content>
    <#local type = "info">
  </#if>
  <#local stateClass = styles["progress_state_" + type]!styles["progress_state_info"]!"">

  <@progress_markup value=value id=id class=class showValue=showValue containerClass=containerClass stateClass=stateClass origArgs=origArgs passArgs=passArgs/>
    
  <#if progressOptions?has_content>
    <#local opts = progressOptions>
    <#if explicitId>
      <#local opts = concatMaps(opts, {"progBarId":id})>
    </#if>
    <#-- inlines always override args map -->
    <@progressScript progressOptions=opts htmlwrap=true args=progressArgs passArgs=passArgs />
  </#if>
</#macro>

<#-- @progress main markup - theme override -->
<#macro progress_markup value=0 id="" class="" showValue=false containerClass="" stateClass="" origArgs={} passArgs={} catchArgs...>
  <#local classes = compileClassArg(class)>
  <#local containerClasses = compileClassArg(containerClass)>
  <div class="${styles.progress_container}<#if !styles.progress_wrap?has_content && classes?has_content> ${escapeVal(classes, 'html')}</#if><#if stateClass?has_content> ${escapeVal(stateClass, 'html')}</#if><#if containerClasses?has_content> ${escapeVal(containerClasses, 'html')}</#if>"<#if id?has_content> id="${escapeVal(id, 'html')}"</#if>>
    <#if styles.progress_wrap?has_content><div class="${styles.progress_wrap!}<#if classes?has_content> ${escapeVal(classes, 'html')}</#if>"<#if id?has_content> id="${escapeVal(id, 'html')}_meter"</#if> role="progressbar" aria-valuenow="${value}" aria-valuemin="0" aria-valuemax="100" style="width: ${value}%"></#if>
      <span class="${styles.progress_bar!}"<#if !styles.progress_wrap?has_content> style="width: ${value}%"<#if id?has_content> id="${escapeVal(id, 'html')}_meter"</#if></#if>><#if showValue>${value}</#if></span>
    <#if styles.progress_wrap?has_content></div></#if>
  </div>
</#macro>

<#-- 
*************
* dynamicSelectFieldScript
************
Special select field-generating script that dynamically fetches values based on related fields.

Based on the original component://common/webcommon/includes/setMultipleSelectJs.ftl Ofbiz code interface,
which was originally implemented with asmSelect.

IMPL NOTE: This must support legacy Ofbiz parameters.

DEV NOTE: this interface is still needed to support the related-field dynamic population, 
    even though asmSelect is done. this is already incorporated into @field as dynSelectArgs option.
                    
  * Parameters *
    * General *
    enabled                 = ((boolean), default: true) If enabled, disables the whole macro
                              Sometimes needed in templates as FTL workaround.
    id                      = Select elem id
    title                   = Select title
    sortable                = ((boolean), default: false) Request for values to be sortable, IF applicable
                              NOTE: 2017-04-12: this currently does nothing in the standard markup,
                                  but it may be left specified as a preference, for future use.
    formId                  = Form ID
    formName                = Form name
    relatedFieldId          = Related field ID (optional)
    htmlwrap                = ((boolean), default: true) If true, wrap in @script
    useDefaults             = ((boolean), default: true) If false, will not include any defaults and use explicit options only
    
    * Needed only if relatedFieldId specified *
    relatedTypeName         = Related type, name
    relatedTypeFieldId      = Related type field ID
    paramKey                = Param key 
    requestName             = Request name
    responseName            = Response name
    
  * History *
    Added for 1.14.3 to replace and provide ''some'' compatibility for code that used @asmSelectScript (2017-04-12). 
-->
<#assign dynamicSelectFieldScript_defaultArgs = {
  "enabled":true, "id":"", "title":false, "sortable":false, "formId":"", "formName":"", "relatedFieldId":"", 
  "relatedTypeName":"", "relatedTypeFieldId":"", "paramKey":"", 
  "requestName":"", "responseName":"", "htmlwrap":true, "useDefaults":true, "passArgs":{}
}>
<#macro dynamicSelectFieldScript args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.dynamicSelectFieldScript_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#if enabled>
    <#-- MIGRATED FROM component://common/webcommon/includes/setMultipleSelectJs.ftl -->
    <#if id?has_content>
    <@script htmlwrap=htmlwrap>
    jQuery(document).ready(function() {
        multiple = jQuery("#${escapeVal(id, 'js')}");
    
      <#if !(title?is_boolean && title == false)>
        <#if title?is_boolean>
          <#local title = "">
        </#if>
        <#-- set the dropdown "title" if?? -->
        multiple.attr('title', '${escapeVal(title, 'js')}');
      </#if>
      
      <#-- 2017-04-12: REMOVED asmSelect - let the regular select multiple=true work instead
          NOTE: for some reason the sortable=true flag no longer working in asmSelect.
        <#if useDefaults>
          <#local defaultAsmSelectOpts = {
            "addItemTarget": 'top',
            "removeLabel": uiLabelMap.CommonRemove
            <#- -, debugMode: true- ->
          }>
          <#local asmSelectOpts = defaultAsmSelectOpts + (styles.field_select_asmselect!{}) + {"sortable": sortable}>
        <#else>
          <#local asmSelectOpts = {"sortable": sortable}>
        </#if>
        
        // use asmSelect in Widget Forms
        multiple.asmSelect(<@objectAsScript lang="js" object=asmSelectOpts />);
      -->
          
      <#if relatedFieldId?has_content> <#-- can be used without related field -->
        // track possible relatedField changes
        // on initial focus (focus-field-name must be relatedFieldId) or if the field value changes, select related multi values. 
        typeValue = jQuery('#${escapeVal(relatedTypeFieldId, 'js')}').val();
        jQuery("#${escapeVal(relatedFieldId, 'js')}").one('focus', function() {
          selectMultipleRelatedValues('${escapeVal(requestName, 'js')}', '${escapeVal(paramKey, 'js')}', '${escapeVal(relatedFieldId, 'js')}', '${escapeVal(id, 'js')}', '${escapeVal(relatedTypeName, 'js')}', typeValue, '${escapeVal(responseName, 'js')}');
        });
        jQuery("#${escapeVal(relatedFieldId, 'js')}").change(function() {
          selectMultipleRelatedValues('${escapeVal(requestName, 'js')}', '${escapeVal(paramKey, 'js')}', '${escapeVal(relatedFieldId, 'js')}', '${escapeVal(id, 'js')}', '${escapeVal(relatedTypeName, 'js')}', typeValue, '${escapeVal(responseName, 'js')}');
        });
        selectMultipleRelatedValues('${escapeVal(requestName, 'js')}', '${escapeVal(paramKey, 'js')}', '${escapeVal(relatedFieldId, 'js')}', '${escapeVal(id, 'js')}', '${escapeVal(relatedTypeName, 'js')}', typeValue, '${escapeVal(responseName, 'js')}');
      </#if>
      });  
    </@script>
    </#if>
  </#if>
</#macro>

<#-- 
*************
* asmSelectScript
************
Generates script data and markup needed to turn a multiple-select form field into dynamic jquery asmselect.
DEPRECATED: asmSelect came from older Ofbiz but was no longer maintained by the asmSelect authors; ca
    use @dynamicSelectFieldScript instead, or {{{<@field type="select" multiple=true>}}} if you don't need
    the related-field logic.
    
  * Related *
    @dynamicSelectFieldScript
    
  * History *
    Deprecated for 1.14.3.
-->
<#macro asmSelectScript args={} inlineArgs...>
  <@dynamicSelectFieldScript args=mergeArgMaps(args, inlineArgs, {})/>
</#macro>

<#-- 
*************
* Fieldset
************
A visible fieldset, including the HTML element.

  * Usage Examples *  
    <@fieldset title="">
        Inner Content
    </@fieldset>            
                    
  * Parameters *
    class                   = ((css-class)) CSS classes 
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    containerClass          = ((css-class)) CSS classes for wrapper 
                              Includes width in columns, or append only with "+".
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    id                      = Fieldset ID
    title                   = Fieldset title
    collapsed               = Show/hide the fieldset
-->
<#assign fieldset_defaultArgs = {
  "id":"", "title":"", "class":"", "containerClass":"", "collapsed":false, "open":true, "close":true, "passArgs":{}
}>
<#macro fieldset args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.fieldset_defaultArgs)>
  <#-- NOTE: this macro's args are a subset of core's args (but with potentially different defaults), so can just pass the whole thing
  <#local dummy = localsPutAll(args)>
  -->
  <@fieldset_core args=args> <#-- implied: passArgs=passArgs -->
    <#nested />
  </@fieldset_core>
</#macro>

<#-- DEV NOTE: see @section_core for details on "core" pattern 
     migrated from @renderFieldGroupOpen/Close form widget macro -->
<#assign fieldset_core_defaultArgs = {
  "class":"", "containerClass":"", "id":"", "title":"", "collapsed":false, "collapsibleAreaId":"", "expandToolTip":"", "collapseToolTip":"", "collapsible":false, 
  "open":true, "close":true, "passArgs":{}
}>
<#macro fieldset_core args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.fieldset_core_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>

  <#if id?has_content>
    <#local containerId = rawString(id) + "_container">
  <#else>
    <#local containerId = "">
  </#if>

  <#if open && !close>
    <#local dummy = pushRequestStack("scipioFieldsetCoreMarkupStack", {
      "class":class, "containerClass":containerClass, "id":id, "containerId":containerId, "title":title, 
      "collapsed":collapsed, "collapsibleAreaId":collapsibleAreaId, "expandToolTip":expandToolTip, 
      "collapseToolTip":collapseToolTip, "collapsible":collapsible, 
      "origArgs":origArgs, "passArgs":passArgs
    })>
  <#elseif close && !open>
    <#local stackValues = popRequestStack("scipioFieldsetCoreMarkupStack")!{}>
    <#local dummy = localsPutAll(stackValues)>
  </#if>
  <@fieldset_markup open=open close=close class=class containerClass=containerClass id=id containerId=containerId title=title collapsed=collapsed collapsibleAreaId=collapsibleAreaId expandToolTip=expandToolTip collapseToolTip=collapseToolTip collapsible=collapsible origArgs=origArgs passArgs=passArgs><#nested></@fieldset_markup>
</#macro>

<#-- @fieldset main markup - theme override -->
<#macro fieldset_markup open=true close=true class="" containerClass="" id="" containerId="" title="" collapsed=false collapsibleAreaId="" expandToolTip="" collapseToolTip="" collapsible=false origArgs={} passArgs={} catchArgs...>
  <#if open>
    <#local containerClass = addClassArg(containerClass, "fieldgroup")>
    <#if collapsible || collapsed>
      <#local containerClass = addClassArg(containerClass, "toggleField")>
      <#if collapsed>
        <#local containerClass = addClassArg(containerClass, styles.collapsed)>
      </#if>
    </#if>
    <#local classes = compileClassArg(class)>
    <#local containerClasses = compileClassArg(containerClass, "${styles.grid_large!}12")>
    <@row open=true close=false />
      <@cell open=true close=false class=containerClasses id=containerId />
        <fieldset<#if classes?has_content> class="${escapeVal(classes, 'html')}"</#if><#if id?has_content> id="${escapeVal(id, 'html')}"</#if>>
      <#--<#if collapsible>
        <ul>
          <li class="<#if collapsed>${styles.collapsed!}">
                      <a onclick="javascript:toggleCollapsiblePanel(this, '${escapeVal(collapsibleAreaId, 'js-html')}', '${escapeVal(expandToolTip, 'js-html')}', '${escapeVal(collapseToolTip, 'js-html')}');">
                    <#else>expanded">
                      <a onclick="javascript:toggleCollapsiblePanel(this, '${escapeVal(collapsibleAreaId, 'js-html')}', '${escapeVal(expandToolTip, 'js-html')}', '${escapeVal(collapseToolTip, 'js-html')}');">
                    </#if>
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<#if title?has_content>${escapeVal(title, 'htmlmarkup')}</#if></a>
          </li>
        </ul>
      <#else>
        <#if title?has_content>${escapeVal(title, 'htmlmarkup')}</#if>
      </#if><#rt/>
    </div>
    <div id="${escapeVal(collapsibleAreaId, 'html')}" class="fieldgroup-body"<#if collapsed && collapsible> style="display: none;"</#if>>
    -->
          <#if title?has_content><legend><#if collapsible || collapsed>[ <i class="${styles.icon!} ${styles.icon_arrow!}"></i> ] </#if>${escapeVal(title, 'htmlmarkup')}</legend></#if>
  </#if>
          <#nested>
  <#if close>
        </fieldset>
      <@cell close=true open=false />
    <@row close=true open=false />
  </#if>
</#macro>

<#-- 
*************
* Fields
************ 
Fields helper that helps modify a set of @field definitions, or group of fields.
Not associated with a visible element, as is @fieldset.
Can be omitted.
May sometimes need multiple of these per form (so @form insufficient for this purpose),
or even multiple per fieldset. 

  * Usage Examples * 
    <@fields>
      <@field attr="" />
      <@field attr="" />
    </@field>
     
    <@fields type="default-nolabelarea">
      <@field attr="" />
      <@field attr="" />
    </@field>
    
    <@fields type="generic" labelArea=false>
      <@field attr="" />
      <@field attr="" />
    </@field>
    
    <@fields autoValue={"record": myEntity!{}, "defaults":{"field1": "some default value"}}>
      <@field name="field1" />
    </@fields>
    
  * Parameters *
    type                        = (default|inherit|inherit-all|generic|..., default: inherit-all) The type of fields arrangement. 
                                  Affects layout and styling of contained fields.
                                  Scipio standard markup types:
                                  * {{{default}}}: default scipio field arrangement. This is the type assumed when no @fields element is present.
                                    Currently, it mostly influences the label area (present for all @field types except submit).
                                  * {{{inherit}}}: SPECIAL VALUE: causes the {{{type}}} value (only) to inherit from the current parent container. 
                                    If there is no parent, the default is {{{default}}}.
                                    This does not affect the other parameters' fallback behaviors.
                                  * {{{inherit-all}}}: SPECIAL VALUE: Inherits all parameters (including type) from parent @fields element(s).
                                    With this, all unspecified args are inherited from parent @fields element if one exists.
                                    No global style lookups are performed.
                                    If there are no parents, the regular defaults are used, and the default type is {{{default}}}.
                                    This is currently the default when no type is specified or empty. If you want to prevent inheritance of other parameters,
                                    use "inherit", or to prevent inheritance completely, use any other type.
                                  * {{{default-nolabelarea}}}: default scipio field arrangement for common sets of fields with no label area.
                                    It expects that @field entries won't be passed any labels except for field types where they may trickle inline into the widget's inline label area.
                                  * {{{default-compact}}}: default scipio field arrangement for fields that are in limited space.
                                    By default, this means the labels will be arranged vertically with the fields.
                                  * {{{default-manual}}}: manual field arrangement. Means field arrangement is custom and field macro and theme should not impose
                                    Any layout, but may still apply minor low-level default styling choices and non-optional layout fallbacks. caller determines arrangement/layout/label type/etc.
                                  * {{{default-manual-widgetonly}}}: manual field arrangement without containers. Same as {{{default-manual}}} but with wrappers/containers omitted by default.
                                  * {{{generic}}}: generic field arrangement of no specific pattern and no specific styling. Means field arrangement is custom and field macro and theme should not
                                    Make any assumptions except where a default is required. Caller determines arrangement/layout/label type/etc.
                                  NOTE: For default-manual, generic and similar where styles hash does not specify a label area by default, 
                                      to show a label area for a field, it is NOT sufficient to specify label="xxx".
                                      You must specify both labelArea=true and label="xxx". label arg does not influence presence of label area.
                                      This is explicitly intended, as the label arg is general-purpose in nature and is not associated only with the label area (and anything else will break logic);
                                      Generally, @field specifies label as pure data and theme decides where and how to display it.
                                      In the majority of cases, this should rarely be used anyway; use another more appropriate @fields type instead.
                                  DEV NOTE: Internally, the {{{type}}} values {{{inherit}}} and {{{inherit-all}}} are not stored.
                                      The inheritance is merged immediately and the internal default becomes {{{default}}}.
    labelType                   = (horizontal|vertical|none, default: -type-specific-) Override for type of the field labels themselves
                                  * {{{horizontal}}}: A label area added to the left (or potentially to the right) a field, horizontally. 
                                    the implementation decides how to do this.
                                    DEV NOTE: previously this was called "gridarea". But in the bootstrap code, this no longer makes sense.
                                        It would be perfectly valid for us to add an extra type here called "gridarea" that specifically requires
                                        a grid (TODO?). "horizontal" simply delegates the choice to the implementation.
                                  * {{{vertical}}}: a label area added before (or potentially after) a field, vertically. 
                                    the implementation decides how to do this.
                                  * {{{none}}}: no labels or label areas. Expects the @field macro won't be passed any.
                                  TODO: we should have types here that specifically request that either "gridarea" or "inline" are used for markup:
                                      gridarea-horizontal, gridarea-vertical, inline-horizontal, inline-vertical
                                      The current implementation is unspecific.
    labelPosition               = (left|right|top|bottom|none, default: -type-specific-) Override for layout/positioning of the labels
                                  Some values only make sense for some arrangements.
    labelArea                   = ((boolean), default: -from global styles-) Overrides whether fields are expected to have a label area or not, mainly when label omitted
                                  Logic is influenced by other arguments.
                                  NOTE: This does not determine label area type (horizontal, etc.); only labelType does that (in current code).
                                      They are decoupled. This only controls presence of it.
                                  NOTE: This is weaker than labelArea arg of @field macro, but stronger than other args of this macro.
    labelAreaExceptions         = ((string)|(list), default: -from global styles-) String of space-delimited @field type names or list of names
                                  NOTE: radio and checkbox support special names: radio-single, radio-multi, checkbox-single, checkbox-multi
    labelAreaRequireContent     = ((boolean)) If true, the label area will only be included if label or labelDetail have content
                                  This is generally independent of labelArea boolean and other settings. 
                                  NOTE: This will not affect the fallback logic of labels to inline labels (a.k.a. whether the label area "consumes" the label for itself);
                                      otherwise that would mean labels would always be forced into the label area and never inline.
    labelAreaConsumeExceptions  = ((string)|(list), default: -from global styles-) String of space-delimited @field type names or list of names 
                                  List of field types that should never have their label appear in the main label area.
                                  for these, the label will trickle down into the field's inline area, if it has any (otherwise no label).
                                  NOTE: radio and checkbox support special names: radio-single, radio-multi, checkbox-single, checkbox-multi
    formName                    = The form name the child fields should assume  
    formId                      = The form ID the child fields should assume   
    inlineItems                 = ((boolean)) Change default for @field inlineItems parameter
    checkboxType                = Default checkbox type
    radioType                   = Default radio type  
    open, close                 = ((boolean)) Advanced structure control, for esoteric cases
    ignoreParentField           = ((boolean), default: false) If true, causes all fields within to ignore their parent and behave as if no parent
    fieldArgs                   = A map of @field parameters that will be used as new defaults for each field call
                                  This is an automated mechanism. The map will be blended over the standard @field defaults before the invocation.
                                  In addition, contrary to the parameters, a map passed directly to @fields will be blended over both the @field defaults
                                  AND over any defaults set in the styles for the given @fields type: 
                                  {@field regular defaults} + {fieldargs from styles hash} + {@fields fieldArgs direct arg}
                                  NOTES:
                                  * This may overlap with some of the existing parameters above. Covers other cases not made explicit above.
                                  * If set to boolean false, will prevent all custom default field args and prevent using those set in styles hash. Probably never needed.
                                  e.g.
                                    <@fields type="default" fieldArgs={"labelArea":false}>
    autoValue                   = ((map)|(boolean), default: -empty/disabled-) Auto value configuration to be set as globals for children @field and #getAutoValue calls
                                  WARN: NOT FULLY IMPLEMENTED (2016-07-15)
                                  This enables children @field and #getAutoValue calls to use automatic value lookups using parameters, record and defaults maps, which
                                  are set in globals using this parameter. Disabled by default.
                                  Possible values:
                                  * as {{{map}}}: if set to a map, all contents are passed as arguments to the #setAutoValueCfg function ({{{setAutoValueCfg(autoValue)}}}), with the exception that if
                                    the {{{autoValue.autoValue}}} boolean is omitted, @fields assumes and sets it to true (only explicit false will disable auto value; this case is rare). 
                                    See #setAutoValueCfg for available configuration arguments.
                                  * as {{{boolean}}}: if set to a boolean, it is equivalent to calling {{{setAutoValueCfg({"autoValue":true})}}}, where only defaults are used.
                                  In all cases, the previous global auto value configuration is saved before setting these and is restored after @fields is closed.
                                  NOTE: Unlike other @fields parameters, this parameter is always inherited unless specifically overridden.
                                  NOTE: Unlike other @fields parameters, this parameter does not survive screen @render boundaries (only has page/template scope, not request scope).
-->
<#assign fields_defaultArgs = {
  "type":"", "open":true, "close":true, "labelType":"", "labelPosition":"", "labelArea":"", "labelAreaExceptions":true, "labelAreaRequireContent":"", "labelAreaConsumeExceptions":true,
  "formName":"", "formId":"", "inlineItems":"", "collapse":"", "collapsePostfix":"", "collapsedInlineLabel":"", "checkboxType":"", "radioType":"", "ignoreParentField":"", 
  "fieldArgs":true, "autoValue":0, "passArgs":{}
}>
<#macro fields args={} inlineArgs...>
  <#-- NOTE: this is non-standard args usage -->
  <#local explArgs = mergeArgMapsBasic(args, inlineArgs)>
  <#local fieldsInfo = makeFieldsInfo(explArgs)>
  <#if (fieldsInfo.open!true) == true>
    <#local dummy = pushRequestStack("scipioFieldsInfoStack", fieldsInfo)>
  </#if>
  <#local autoValue = explArgs.autoValue!0>
  <#if !autoValue?is_number>
    <#local prevAutoValueCfg = getAutoValueCfg()>
    <#if autoValue?is_boolean>
      <#local dummy = setAutoValueCfg({"autoValue":autoValue})>
    <#else>
      <#local dummy = setAutoValueCfg({"autoValue":true} + autoValue)>
    </#if>
  </#if>
    <#nested>
  <#if !autoValue?is_number>
    <#local dummy = setAutoValueCfg(prevAutoValueCfg)>
  </#if>
  <#if (fieldsInfo.close!true) == true>
    <#local dummy = popRequestStack("scipioFieldsInfoStack")>
  </#if>
</#macro>

<#function makeFieldsInfo args={}>
  <#local origType = args.type!"">
  <#if !origType?has_content>
    <#local origType = "inherit-all">
  </#if>

  <#local parentFieldsInfo = readRequestStack("scipioFieldsInfoStack")!{}>

  <#local effType = origType>
  <#local effFieldArgs = args.fieldArgs!true>
  <#local defaultArgs = scipioStdTmplLib.fields_defaultArgs>
  
  <#if !origType?has_content>
    <#local effType = "default">
  <#elseif origType == "inherit-all">
    <#if parentFieldsInfo.type??>
      <#-- inherit everything from parent -->
      <#local effType = parentFieldsInfo.type>
      <#local defaultArgs = parentFieldsInfo.origArgs>
      
      <#if args.fieldArgs??>
        <#if args.fieldArgs?is_boolean>
          <#if args.fieldArgs>
            <#local effFieldArgs = parentFieldsInfo.fieldArgs!true>
          <#else>
            <#-- here, prevent combining field args (special case); keep ours -->
          </#if>
        <#else>
          <#if parentFieldsInfo.fieldArgs?is_boolean>
            <#-- no parent field args; keep ours -->
          <#else>
            <#-- combine with parent -->
            <#local effFieldArgs = concatMaps(parentFieldsInfo.fieldArgs, args.fieldArgs)>
          </#if>
        </#if>
      <#else>
        <#local effFieldArgs = parentFieldsInfo.fieldArgs!true>
      </#if>
    <#else>
      <#local effType = "default">
    </#if>
  <#elseif origType == "inherit">
    <#if parentFieldsInfo.type??>
      <#local effType = parentFieldsInfo.type>
    <#else>
      <#local effType = "default">
    </#if>
  </#if>
  
  <#local args = mergeArgMapsBasic(args, {}, defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  
  <#local type = effType>
  <#local fieldArgs = effFieldArgs>
  
  <#local stylesType = type?replace("-","_")>
  <#local stylesPrefix = "fields_" + stylesType>
  <#-- DON'T do this, it messes with intuition - individual default fallbacks are good enough
  <#if !styles[stylesPrefix + "_labeltype"]??>
    <#local stylesType = "default">
    <#local stylesPrefix = "fields_default">
  </#if>-->

  <#if !labelArea?is_boolean>
    <#local stylesLabelArea = styles[stylesPrefix + "_labelarea"]!styles["fields_default_labelarea"]!"">
    <#if stylesLabelArea?is_boolean>
      <#local labelArea = stylesLabelArea>
    </#if>
  </#if>
  <#if !labelType?has_content>
    <#local labelType = styles[stylesPrefix + "_labeltype"]!styles["fields_default_labeltype"]!"horizontal">
  </#if>
  <#if !labelPosition?has_content>
    <#local labelPosition = styles[stylesPrefix + "_labelposition"]!styles["fields_default_labelposition"]!"left">
  </#if>
  <#if !labelArea?is_boolean>
    <#local labelArea = (labelType != "none" && labelPosition != "none")>
  </#if>

  <#if !labelAreaExceptions?is_sequence && !labelAreaExceptions?is_string>
    <#if labelAreaExceptions?is_boolean && labelAreaExceptions == false>
      <#local labelAreaExceptions = []>
    <#else>
      <#local labelAreaExceptions = styles[stylesPrefix + "_labelareaexceptions"]!styles["fields_default_labelareaexceptions"]!"">
    </#if>
  </#if>
  <#if labelAreaExceptions?is_string> <#-- WARN: ?is_string unreliable -->
    <#if labelAreaExceptions?has_content>
      <#local labelAreaExceptions = labelAreaExceptions?split(" ")>
    <#else>
      <#local labelAreaExceptions = []>
    </#if>
  </#if>

  <#if !labelAreaRequireContent?is_boolean>
    <#local labelAreaRequireContent = styles[stylesPrefix + "_labelarearequirecontent"]!styles["fields_default_labelarearequirecontent"]!"">
  </#if>

  <#if !labelAreaConsumeExceptions?is_sequence && !labelAreaConsumeExceptions?is_string>
    <#if labelAreaConsumeExceptions?is_boolean && labelAreaConsumeExceptions == false>
      <#local labelAreaConsumeExceptions = []>
    <#else>
      <#local labelAreaConsumeExceptions = styles[stylesPrefix + "_labelareaconsumeexceptions"]!styles["fields_default_labelareaconsumeexceptions"]!"">
    </#if>
  </#if>
  <#if labelAreaConsumeExceptions?is_string> <#-- WARN: ?is_string unreliable -->
    <#if labelAreaConsumeExceptions?has_content>
      <#local labelAreaConsumeExceptions = labelAreaConsumeExceptions?split(" ")>
    <#else>
      <#local labelAreaConsumeExceptions = []>
    </#if>
  </#if>

  <#if !collapse?is_boolean>
    <#local collapse = styles[stylesPrefix + "_collapse"]!styles["fields_default_collapse"]!"">
  </#if>
  <#if !collapsePostfix?is_boolean>
    <#local collapsePostfix = styles[stylesPrefix + "_collapsepostfix"]!styles["fields_default_collapsepostfix"]!"">
  </#if>
  <#if !collapsedInlineLabel?has_content>
    <#local collapsedInlineLabel = styles[stylesPrefix + "_collapsedinlinelabel"]!styles["fields_default_collapsedinlinelabel"]!"">
  </#if>
  <#if collapsedInlineLabel?is_string>
    <#if collapsedInlineLabel?has_content> <#-- WARN: ?is_string unreliable -->
      <#local collapsedInlineLabel = collapsedInlineLabel?split(" ")>
    </#if>
  </#if>
  <#if !checkboxType?has_content>
    <#local checkboxType = styles[stylesPrefix + "_checkboxtype"]!styles["fields_default_checkboxtype"]!"">
  </#if>
  <#if !radioType?has_content>
    <#local radioType = styles[stylesPrefix + "_radiotype"]!styles["fields_default_radiotype"]!"">
  </#if>

  <#if !inlineItems?has_content>
    <#local inlineItems = styles[stylesPrefix + "_inlineitems"]!styles["fields_default_inlineitems"]!"">
  </#if>

  <#local fieldArgsFromStyles = styles[stylesPrefix + "_fieldargs"]!true>
  <#local fieldArgsFromDefaultStyles = styles["fields_default_fieldargs"]!true>
  <#if fieldArgs?is_boolean>
    <#if fieldArgs == true>
      <#if fieldArgsFromStyles?is_boolean>
        <#if fieldArgsFromStyles>
          <#local fieldArgs = fieldArgsFromDefaultStyles>
        <#else>
          <#-- if false, prevents fallback on defaults -->
        </#if>
      <#else>
        <#local fieldArgs = fieldArgsFromStyles>
        <#if !fieldArgsFromDefaultStyles?is_boolean>
          <#local fieldArgs = fieldArgsFromDefaultStyles + fieldArgs>
        </#if>
      </#if>
    </#if>
  <#else>
    <#local fieldArgs = toSimpleMap(fieldArgs)>
    <#if fieldArgsFromStyles?is_boolean>
      <#if fieldArgsFromStyles>
        <#if !fieldArgsFromDefaultStyles?is_boolean>
          <#local fieldArgs = fieldArgsFromDefaultStyles + fieldArgs>
        </#if>
      <#else>
        <#-- if false, prevents fallback on defaults -->
      </#if>
    <#else>
      <#local fieldArgs = fieldArgsFromStyles + fieldArgs>
      <#if !fieldArgsFromDefaultStyles?is_boolean>
        <#local fieldArgs = fieldArgsFromDefaultStyles + fieldArgs>
      </#if>
    </#if>
  </#if>

  <#return {"type":type, "origType":origType, "origArgs":origArgs, "stylesType":stylesType, "stylesPrefix":stylesPrefix, "labelType":labelType, "labelPosition":labelPosition, 
    "labelArea":labelArea, "labelAreaExceptions":labelAreaExceptions, 
    "labelAreaRequireContent":labelAreaRequireContent, "labelAreaConsumeExceptions":labelAreaConsumeExceptions,
    "formName":formName, "formId":formId, "inlineItems":inlineItems,
    "collapse":collapse, "collapsePostfix":collapsePostfix, "collapsedInlineLabel":collapsedInlineLabel,
    "checkboxType":checkboxType, "radioType":radioType,
    "ignoreParentField":ignoreParentField,
    "fieldArgs":fieldArgs}>
</#function>

<#-- 
*************
* mapScipioFieldTypeToStyleName
************ 
Maps a scipio field type to a style name representing the type.

Should be coordinated with mapOfbizFieldTypeToStyleName to produce common field type style names.
-->
<#function mapScipioFieldTypeToStyleName fieldType>
  <#local res = (styles.field_type_stylenames_scipio[fieldType])!(styles.field_type_stylenames_scipio["default"])!"">
  <#if res?is_boolean>
    <#return res?string(fieldType, "")>
  </#if>
  <#return res>
</#function>

<#-- 
*************
* mapOfbizFieldTypeToStyleName
************ 
Maps an Ofbiz field type to a style name representing the type.

Should be coordinated with mapScipioFieldTypeToStyleName to produce common field type style names.
-->
<#function mapOfbizFieldTypeToStyleName fieldType>
  <#local res = (styles.field_type_stylenames_ofbiz[fieldType])!(styles.field_type_stylenames_ofbiz["default"])!"">
  <#if res?is_boolean>
    <#return res?string(fieldType, "")>
  </#if>
  <#return res>
</#function>

<#-- 
*************
* mapOfbizFieldTypeToScipioFieldType
************ 
Maps an Ofbiz field type to a Scipio field type.
-->
<#function mapOfbizFieldTypeToScipioFieldType fieldType>
  <#if !ofbizFieldTypeToScipioFieldTypeMap??>
    <#global ofbizFieldTypeToScipioFieldTypeMap = {
      "display": "display",
      "hyperlink": "hyperlink",
      "text": "input",
      "textarea": "textarea",
      "date-time": "datetime",
      "drop-down": "select",
      "check": "checkbox",
      "radio": "radio",
      "submit": "submit",
      "reset": "reset",
      "hidden": "hidden",
      "ignored": "ignored",
      "text-find": "textfind",
      "date-find": "datefind",
      "range-find": "rangefind",
      "lookup": "lookup",
      "file": "file",
      "password": "password",
      "image": "image",
      "display-entity": "displayentity",
      "container": "container",
      "default": "other"
    }>
  </#if>
  <#return ofbizFieldTypeToScipioFieldTypeMap[fieldType]!ofbizFieldTypeToScipioFieldTypeMap["default"]!"">
</#function>

<#-- 
*************
* Field
************ 
A form field input widget with optional label and post-input (postfix) content.

@field can be used as a low-level field control (similar to original Ofbiz
form widget macros, but with friendlier parameters) and for high-level declarations
of fields (similar to the actual <field> elements in Ofbiz form widget definitions, but friendlier
and more configurable). This versatility is the main reason for its implementation complexity.

In the high-level role, the macro takes care of label area logic and alignment
such that you will not get fields looking out of place even if they have no label,
giving all the fields in a given form a default uniform look, which can be customized globally.

@field's behavior can be customized for a set of fields using a parent @fields macro invocation
as well as using the global styles hash (preferred where possible). A set of fields may be grouped under a @fields
call with a @fields "type" selected, which will give all fields within it a predefined look
and behavior. This behavior can be set in the global styles hash (preferred) or overridden directly
in the @fields element.

If no @fields element is used, by default @field will behave the same as if it
were surrounded by a @fields element with "default" type, which gives all fields a default
look out of the box.

To use @field as a low-level control, it should be given a parent @fields with "generic" type.

This system can accodomate custom @fields types, but a default set are provided in the scipio
standard markup.

NOTE: All @field arg defaults can be overridden by the @fields fieldArgs argument.

  * Usage Examples *  
    <@field attr="" /> <#- single field using default look ->
    
    <@fields type="default"> <#- single field using default look, same as previous ->
      <@field attr="" />
    </@fields>

    <@fields type="default-nolabelarea"> <#- specific arrangement needed ->
      <@field attr="" />
    </@fields>
    
    <@fields type="default-compact"> <#- compactified arrangement ->
      <@field attr="" />
    </@fields>
    
    <@fields type="default-manual"> <#- use @field as low-level control (also possible: type="generic") ->
      <@field attr="" labelArea=true label="My Label" />
    </@fields>    
    
    <@row> <#- widget-only field: no containers, label area or postfix area, and label automatically inlines as widget label (checkbox label); do everything yourself ->
      <@cell columns=3>My Text</@cell>
      <@cell columns=9>
        <@field type="checkbox" widgetOnly=true label="My Checkbox" name="check1" /> 
      </@cell>
    </@row>
    
    <p>My <@field type="input" inline=true name="check1" value="inlined" /> field</p> <#- inlined field: no containers, label area or postfix area, display:inline, no width spanning ->
    
    <@field attr="" label="My Label" class="+${styles.field_inline!}" /> <#- special form of regular field where the widget element alone is inlined and no spanning, but still has label area and container; widget element receives an inlining class ->
    
    
  * Parameters *
    * General *
    type                    = (|generic|..., default: generic) Form element type 
                              Supported values and their parameters are listed in this documentation as
                              parameter sections (groups of parameters), as there are type-specific field parameters.
                              * {{{generic}}}: Means input defined manually with nested content. Mostly for grouping multiple sub-fields, but can be used anywhere.
                                  Specific field types should be preferred to manually defining content, where possible.
    fieldsType              = (|default|..., default: -empty-) CONVENIENCE fields type override
                              By default, this is empty and inherited from parent @fields element.
                              Specifying {{{fieldsType="xxx"}}} as in:
                                <@field type="generic" fieldsType="xxx" .../>
                              is the same as doing:
                                <@fields type="xxx">
                                  <@field type="generic .../>
                                </@fields>
    label                   = Field label
                              For top-level @field elements and and parent fields, normally the label will get consumed
                              by the label area and shown there. for child fields and some other circumstances, or whenever there is
                              no label area, the label will instead be passed down as an "inline label" to the input
                              widget implementation. in some cases, this "inline label" is
                              re-implemented using the label area - see collapsedInlineLabel parameter.
                              NOTE: Presence of label arg does not guarantee a label area will be shown; this is controlled
                                  by labelArea (and labelType) and its defaults, optionally coming from @fields container.
                                  label arg is mainly to provide data; theme and other flags decide what to do with it.
                                  For generic parent fields, label type must be specified explicitly, e.g.
                                    {{{<@fields type="generic"><@field labelType="horizontal" label="mylabel">...</@fields>}}}
                              NOTE: label area behavior may also be influenced by containing macros such as @fields
    labelContent            = ((string)|(macro)) Alternative to {{{label}}} arg which may be a macro and allows manually overriding the basic label markup
                              WARN: Currently (2016-04-12), unlike the {{{label}}} arg, {{{labelContent}}} will not follow any label inlining logic and
                                  is only used by the label area markup. 
                              FIXME?: May want to have labelContent follow label more closely.
                              NOTE: Not escaped by macro.
    labelDetail             = ((string)|(macro)) Extra content markup inserted with label (normally after label, but theme may decide)
                              2016-04-12: This may also be a macro used to generate the label, which must accept a single {{{args}}} map parameter.
                              NOTE: If need to guarantee post-markup label content, may also use {{{postLabelContent}}} (lower-level control).
                              NOTE: Not escaped by macro.
    labelContentArgs        = ((map)) Optional map of args to be passed to {{{labelContent}}} and {{{labelDetail}}} in cases where they are macros
                              NOTE: In addition to these values, all the parameters of the theme-implementing @field_markup_labelarea macros
                                  are also passed.
    labelType               = Explicit label type (see @fields)
    labelPosition           = Explicit label layout (see @fields)
    labelArea               = ((boolean), default: -from global styles-) If true, forces a label area; if false, prevents a label area
                              NOTE: This does not determine label area type (horizontal, etc.); only labelType does that (in current code).
                                  They are decoupled. This only controls presence of it.
    labelAreaRequireContent = ((boolean), default: false) If true, the label area will only be included if label or labelDetail have content
                              By default, this is empty string (use @fields type default), and if no styles defaults,
    labelAreaConsume        = ((boolean), default: true) If set to false, will prevent the label area from consuming (displaying) the label
                              The label will trickle down into an inline area if one exists for the field type.
    inlineLabelArea         = ((boolean), default: -from global styles-, fallback default: false) Manual override for inline label logic
                              In general can be left to macro.
    inlineLabel             = ((string)) Manual override for inline label logic
                              In general can be left to macro.
                              NOTE: Often if you specify this it means you might want to set inlineLabelArea=true as well.
    tooltip                 = Small field description - to be displayed to the customer
                              May be set to boolean false to manually prevent tooltip defaults.                       
    description             = Field description
                              NOTE: currently this is treated as an alternative arg for tooltip
                              TODO?: DEV NOTE: this should probably be separate from tooltip in the end...
    name                    = Field name
    value                   = Field value
    gridArgs                = ((map)) Grid size and configuration arguments roughly equivalent to #getDefaultFieldGridStyles arguments
    widgetPostfixCombined   = ((boolean), default: -markup decision, usually true-) CONVENIENCE alias for {{{gridArgs.widgetPostfixCombined}}} - Overridable setting to force or prevent widget and postfix having their own sub-container
                              It is strongly encouraged to leave this alone in most cases. In Scipio standard markup,
                              the default is usually true unless prevented by other settings.
    totalColumns            = ((int)) CONVENIENCE alias for {{{gridArgs.totalColumns}}} - Total number of columns spanned by the outer container, including label area, widget and postfix
    labelColumns            = ((int)) CONVENIENCE alias for {{{gridArgs.labelColumns}}} - Number of grid columns to use as size for label area, IF one is to be rendered
                              If totalColumns is kept the same, any space removed from this value is removed from widget and postfix (combined).
    labelSmallDiffColumns   = ((int), fallback default: 1) CONVENIENCE alias for {{{gridArgs.labelSmallDiffColumns}}} - Difference between large and small columns of the label area (added to label area columns for "small" container)
                              By default, this setting is set to 1 so that on small screens the label area gets a slightly larger size.
                              Sometimes it is needed to set this to zero for custom markup.
    class                   = ((css-class)) CSS classes for the field element (NOT the cell container!)
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    containerClass          = ((css-class)) CSS classes, optional class for outer container 
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    widgetAreaClass         = ((css-class)) CSS classes, optional class for widget area container
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    labelAreaClass          = ((css-class)) CSS classes, optional class for label area container 
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    postfixAreaClass        = ((css-class)) CSS classes, optional class for postfix area container 
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    widgetPostfixAreaClass  = ((css-class)) CSS classes, optional class for combined widget and postfix parent container 
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    style                   = Legacy HTML style string for compatibility
                              WARN: This is currently only implemented on a few field types and sub-types.
                              TODO: Add to all (sometimes needed for display:none).
    maxlength               = ((int)) Max allowed length 
                              e.g. For text inputs, max number of characters.
    id                      = ((string), default: -auto-generated-) ID for the widget itself
                              If none specified, one may be auto-generated by the macro.
    containerId             = ((string), default: -auto-generated-) ID for the outer container
                              This defaults to {{{"${id}_container"}}}.
    containerStyle          = ((string)) Legacy HTML {{{style}}} attribute for outer container                          
    events                  = ((map)) Map of JS event names to script actions
                              event names can be specified with or without the "on" prefix ("click" or "onclick").
    onClick                 = Shortcut for: events={"click": onClick}
                              WARN: Beware of character case (due to Freemarker). It's onClick, not onclick!
    onChange                = Shortcut for: events={"change": onChange}
    onFocus                 = Shortcut for: events={"focus": onChange}
    disabled                = ((boolean), default: false) Whether field is disabled
    placeholder             = Field placeholder
    alert                   = ((css-class)) CSS classes, additional alert class
    mask                    = ((boolean)) Toggles jQuery mask plugin
    size                    = ((int), default: 20) Size attribute
    collapse                = ((boolean), default: false) Should the whole field (including label and postfix) be collapsing?
    collapsePostfix         = ((boolean), default: true) Should the postfix collapse with the field input?
                              this will not affect label unless collapse is also true (in which case this setting is ignored
                              and the whole field is collapse)
    collapsedInlineLabel    = ((boolean)) Special collapsed inline label control
                              Special function that will only apply in some cases. 
                              if this is set to true and the label does not get consumed
                              by the label area and becomes an inline label, this will cause an auto-implementation
                              of an inlined label using collapsing (instead of passing the inline label
                              down to the individual field type widget).
                              this may be needed for some field types.
    widgetOnly              = ((boolean), default: false) If true, renders only the widget element by default (no containers)
                              Implies {{{container}}} false and {{{labelArea}}} false by default.
                              NOTE: When there is no label area, the {{{label}}} arg trickles down into the widget's inline label area IF it supports one.
    inline                  = ((boolean), default: false) If true, forces container=false, marks the field with styles.field_inline, and forces inline labels (by disabling label area)
                              In other words, turns it into a logically inline element (traditionally, CSS "display: inline;").
                              Theme should act on this style to prevent taking up all the width.
                              In addition, this will force {{{labelArea}}} false and any label specified will use the inlined label (area).
                              NOTE: If you want to have a widget-only containerless field without inlining, use {{{widgetOnly=true}}}.
                                  If you want a regular field with containers where only the widget element is inlined (i.e. not full-width-spanning),
                                  use {{{class="+${styles.field_inline}"}}}.
    norows                  = ((boolean), default: false) If true, render without the rows-container
                              NOTE: This is a low-level control for advanced markup cases and global style presets (@fields).
    nocells                 = ((boolean), default: false) If true, render without the cells-container
                              NOTE: This is a low-level control for advanced markup cases and global style presets (@fields).
    container               = ((boolean), default: true) If false, sets norows=true and nocells=true
                              NOTE: This is a low-level control for advanced markup cases and global style presets (@fields).
                                  In most cases you should use {{{widgetOnly}}} parameter in templates if you want to omit container.
    ignoreParentField       = ((boolean), default: false) If true causes a child field to act as if it had no parent field. Rarely needed
    required                = ((boolean), default: false) Marks a required input
    requiredClass           = ((css-class)) CSS classes, default required class name
                              Does not support extended class +/= syntax.
    requiredTooltip         = tooltip to use when field is required. this is overridden by regular tooltip
                              for this, can prefix with "#LABEL:" string which indicates to take the named label from uiLabelMap.
    postfix                 = ((boolean), default: false) Controls whether an extra area is appended after widget area
    postfixColumns          = ((int), default: 1) Manual postfix size, in (large) grid columns
    postfixContent          = ((string)|(macro)) Manual postfix markup/content - set to boolean false to prevent any content (but not area container)
                              If macro, the macro must accept a single argument, {{{args}}}, a map of arguments.
                              NOTE: Not escaped by macro.
    postfixContentArgs      = ((map)) Optional map of arguments to pass to {{{postfixContent}}} macro, if macro
    preWidgetContent        = ((string)|(macro)) Text or text-generating macro that will be inserted in the widget area before widget content (low-level control)
                              If macro, the macro must accept a single argument, {{{args}}}, a map of arguments.
                              NOTE: Currently, the {{{args}}} map will be empty by default - pass using {{{prePostContentArgs}}}.
                              NOTE: Not escaped by macro.
    postWidgetContent       = ((string)|(macro)) Text or text-generating macro that will be inserted in the widget area after widget content (low-level control)
                              If macro, the macro must accept a single argument, {{{args}}}, a map of arguments.
                              NOTE: Currently, the {{{args}}} map will be empty by default - pass using {{{prePostContentArgs}}}.
                              NOTE: Not escaped by macro.
    preLabelContent         = ((string)|(macro)) Text or text-generating macro that will be inserted in the label area before label content (low-level control)
                              If macro, the macro must accept a single argument, {{{args}}}, a map of arguments.
                              NOTE: Currently, the {{{args}}} map will be empty by default - pass using {{{prePostContentArgs}}}.
                              NOTE: Not escaped by macro.
    postLabelContent        = ((string)|(macro)) Text or text-generating macro that will be inserted in the label area after label content (low-level control)
                              If macro, the macro must accept a single argument, {{{args}}}, a map of arguments.
                              NOTE: Currently, the {{{args}}} map will be empty by default - pass using {{{prePostContentArgs}}}.
                              NOTE: This is almost the same as labelDetail, except postLabelContent is lower level and will always occur at the specified position.
                              NOTE: Not escaped by macro.
    prePostfixContent       = ((string)|(macro)) Text or text-generating macro that will be inserted in the postfix area before postfix content (low-level control)
                              If macro, the macro must accept a single argument, {{{args}}}, a map of arguments.
                              NOTE: Currently, the {{{args}}} map will be empty by default - pass using {{{prePostContentArgs}}}.
                              NOTE: Not escaped by macro.
    postPostfixContent      = ((string)|(macro)) Text or text-generating macro that will be inserted in the postfix area after postfix content (low-level control)
                              If macro, the macro must accept a single argument, {{{args}}}, a map of arguments.
                              NOTE: Currently, the {{{args}}} map will be empty by default by default - pass using {{{prePostContentArgs}}}.
                              NOTE: Not escaped by macro.
    prePostContentArgs      = ((map)) Optional map of extra user-supplied args to be passed to the {{{prePostXxx}}} content macros as the {{{args}}} parameter.
    inverted                = ((boolean), default: false) If true, invert the widget and label area content and user-supplied and identifying classes
                              If this is set to true, the widget area content is swapped with the label area content and the user-supplied
                              classes and identifying classes are also swapped - {{{labelAreaClass}}} and {{{widgetAreaClass}}}. 
                              In addition, the top-level container gets a class to mark it as inverted.
                              However, the calculated default grid area classes are NOT swapped by default; this allows swapping content while
                              preserving grid alignment. Mostly useful for small field widgets such as checkboxes and radios.
                              NOTE: You may want to use {{{labelContent}}} arg to specify content.
    standardClass           = ((css-class)) CSS classes, default standard (non-inverted) class name, added to outer container
                              Does not support extended class +/= syntax.
                              Added for non-inverted fields.
    invertedClass           = ((css-class)) CSS classes, default inverted class name, added to outer container
                              Does not support extended class +/= syntax.
    autoValue               = ((boolean), default: -from globals-, fallback default: false) Fine-grained control to turn auto value lookups on or off
                              WARN: NOT FULLY IMPLEMENTED (2016-07-15)
                              @field has the ability to automatically lookup values from parameter, record and defaults maps,
                              through implicit calls to the #getAutoValue function.
                              By default, this auto value enabling is determined by the current globals as set by @fields
                              or #setAutoValueCfg. This boolean allows to disable per-field as a quick fix.
                              NOTE: This does not support the extensive arguments supported by @fields. It only supports 
                                  explicit boolean on/off to toggle for individual fields.
    autoValueArgs           = ((map)) Extra arguments that will be passed to #getAutoValue when auto values enabled.
                              Note that basics such as name, value, and type are already covered.
                              Some extras that may be specified are: overrideName, paramName, recordName, defaultName.
                              NOTE: suffix is reserved for use and should never be specified.
                              See #getAutoValue for a comprehensive list.
        
    * input (alias: text) *
    autoCompleteUrl         = If autocomplete function exists, specification of url will make it available
    postfix                 = ((boolean), default: false) If set to true, attach submit button
    
    * textarea *
    readonly                = ((boolean) Read-only
    rows                    = ((int)) Number of rows
    cols                    = ((int)) Number of columns
    wrap                    = HTML5 wrap attribute
    text, value             = Text/value, alternative to nested content
    
    * datetime *
    dateType                = (date-time|timestamp|date|time|month, default: date-time) Type of datetime
                              "date-time" and "timestamp" are synonymous.
    dateDisplayType         = (default|date|..., default: -same as dateType-). The visual display format of the date. Optional
                              If dateType is "date-time" (timestamp), it is possible to specify dateDisplayType="date" here.
                              This means the user will be presented with a short date only, but the data sent to the server
                              will be a full timestamp.
    datePostfix             = ((boolean), default: true) Override for date-widget-specific postfix
    datePostfixColumns      = ((int), default: 1) Size of date-widget-specific postfix
    manualInput             = ((boolean), default: true) Whether to allow manual input; if false, selection only through date picker
    title                   = Title
                              If empty, markup/theme decides what to show.
                              Can also be a special value in format {{{"#PROP:resource#propname"}}} (if no {{{resource}}}, taken from CommonUiLabels).
                              NOTE: tooltip has priority over title.
    
    * datefind *
    dateType                = (-same as datetime, except does not support month-)
    dateDisplayType         = (-same as datetime, except does not support month-)
    opValue                 = The selected operator (value)
    
    * textfind *
    opValue                 = The selected operator (value)
    ignoreCaseValue         = ((boolean), default: true) The ignore case checkbox current value
                              The default should be same as form widget default (text-find's "ignore-case" in widget-form.xsd).
    hideOptions             = ((boolean), default: false) If true, don't show select options
    hideIgnoreCase          = ((boolean), default: false) If true, hide case sensitivity boolean
    titleClass              = ((css-class)) CSS classes, extra classes for title
    
    * rangefind *
    opFromValue             = The selected "from" operator (value)
    opThruValue             = The selected "thru" operator (value)
    titleClass              = ((css-class)) CSS classes, extra classes for title
    
    * select *
    multiple                = ((boolean), default: false) Allow multiple select
    items                   = ((list)) List of maps; if specified, generates options from list of maps 
                              List of {"value": (value), "description": (label), "selected": (true/false)} maps
                              If items list not specified, manual nested content options can be specified instead.
                              NOTE: {{{selected}}} is currently ignored for non-multiple (uses {{{currentValue}}} instead).
    allowEmpty              = ((boolean), default: false) If true, will add an empty option
    currentValue            = currently selected value/key (only for non-multiple)
    currentFirst            = ((boolean), default: false) If true (and multiple false), will add a "first" item with current value selected, if there is one
    currentDescription      = If currentFirst true, this is used as first's description if specified
    defaultValue            = Optional selected option value for when none otherwise selected
                              NOTE: When auto-value is being used (#getAutoValue), this should be omitted.
    manualItemsOnly         = ((boolean)) Optional hint to say this select should contain exclusively manually generated items
                              By default, this is determined based on whether the items arg is specified or not.
    manualItems             = ((boolean)) Optional hint to say that nested content contains manual options (but not necessarily exclusively)
                              By default, this is determined based on whether the items arg is specified or not (NOT whether
                              there is any nested content or not).
                              If specifying both items arg AND nested content (discouraged), this should be manually set to true.
    dynSelectArgs           = ((map)) Optional map of args to pass to @dynamicSelectFieldScript to transform a multiple type select into a dynamic select
                              Usually only valid if multiple is true.
    asmSelectArgs           = ((map)) (deprecated) Old alias for dynSelectArgs        
    formName                = Name of form containing the field
    formId                  = ID of form containing the field
    title                   = Title attribute of <select> element
    
    * option *
    text                    = Option label 
                              May also be specified as nested.
    value                   = Value, sent to server upon submit
    selected                = ((boolean))
    
    * lookup *
    formName                = The name of the form that contains the lookup field
    fieldFormName           = Contains the lookup window form name
    
    * checkbox (single mode) *
    value                   = Value to submit with form when checkbox is selected
    currentValue            = Current value, used to check if should be checked
    useHidden               = ((boolean), default: false) If true, submits using a hidden field rather than checkbox itself
                              WARN: This affects javascript lookups, field name and id.
                              If this is true, the {{{name}}} of the checkbox itself receives the suffix {{{_visible}}},
                              such that the hidden input receives the passed name. Any javascript must adapt appropriately.
                              On the other hand, the {{{id}}} of the checkbox is unchanged, and the hidden field
                              receives an id with a {{{_hidden}}} suffix.
                              NOTE: 2017-04-20: An explicit true or false value for useHidden now overrides the automatic-enabling
                                  by the presence of altValue, so that useHidden=false guarantees no hidden even if you
                                  set an altValue; however, in this case altValue is ignored. This is to simplify some caller
                                  code language.
    altValue                = Value to submit with form when checkbox is unselected
                              WARN: This affects javascript lookups, field name and id; see {{{useHidden}}} parameter.
                              If this is specified (non-boolean, non-false), it automatically turns on {{{useHidden}}}
                              (without which implementation is impossible).
    checked                 = ((boolean)|, default: -empty-) Override checked state 
                              If set to boolean, overrides currentValue logic
    checkboxType            = (default|..., default: default)
                              Generic:
                              * {{{default}}}: default theme checkbox
                              Scipio standard theme:
                              * {{{simple}}}: guarantees a minimalistic checkbox
    valueType               = (|indicator, default: -empty-) Special and predefined value types
                              {{{indicator}}}: Same as passing {{{value="Y" altValue="N" useHidden=true}}}.
    
    * checkbox (multi mode) *
    items                   = ((list)) List of maps, if specified, multiple-items checkbox field generated
                              List of {"value": (value), "altValue": (value), "useHidden": (boolean), 
                              "description": (label), "tooltip": (tooltip), "events": (js event map), "checked": (true/false)} maps
                              NOTE: use of "checked" attrib is discouraged; is a manual override (both true and false override); prefer setting currentValue on macro
                              DEV NOTE: the names in this map cannot be changed easily; legacy ofbiz macro support
                              TODO: Currently the map does not support valueType
    inlineItems             = ((boolean), default: -from global styles-, fallback default: true) If true, radio items are many per line; if false, one per line
                              NOTE: this takes effect whether single-item or multiple-item radio.
                              the default can be overridden on a parent @field or @fields element.
    currentValue            = Current value, determines checked; this can be single-value string or sequence of value strings
    defaultValue            = Default value, determines checked (convenience parameter; used when currentValue empty; can also be sequence)
                              NOTE: When auto-value is being used (#getAutoValue), this should be omitted.
    allChecked              = ((boolean|), default: -empty-) Explicit false sets all to unchecked; leave empty "" for no setting (convenience parameter)
    value                   = Default value for any items which do not specify their own
    altValue                = Default alt (off) value for any items which do not specify their own
    useHidden               = ((boolean), default: false) Default useHidden for any items which do not specify their own
    
    * radio (single mode) *
    value                   = Y/N, only used if single radio item mode (items not specified)
    currentValue            = Current value, used to check if should be checked
    checked                 = ((boolean)|, default: -empty-) Override checked state 
                              If set to boolean, overrides currentValue logic
    radioType               = (default|..., default: default)
                              Generic:
                              * {{{default}}}: default theme radio
                              Scipio standard theme:
                              * See global styles.
    
    * radio (multi mode) *
    items                   = ((list)) List of maps, if specified, multiple-items radio generated with map entries in provided list as arguments
                              List of {"value": (value), "description": (label), "tooltip": (tooltip), "events": (js event map), "checked": (true/false)} maps
                              NOTE: use of "checked" attrib is discouraged; is a manual override (both true and false override); prefer setting currentValue on macro
                              DEV NOTE: the names in this map cannot be changed easily; legacy ofbiz macro support
    inlineItems             = ((boolean), default: -from global styles-, fallback default: true) If true, radio items are many per line; if false, one per line
                              NOTE: This takes effect whether single-item or multiple-item radio.
                              The default can be overridden on a parent @field or @fields element.
    currentValue            = Current value, determines checked
    defaultValue            = Default value, determines checked (convenience option; used when currentValue empty)
                              NOTE: When auto-value is being used (#getAutoValue), this should be omitted.
                              
    * file *
    autocomplete            = ((boolean), default: true) If false, prevents autocomplete
    
    * password *
    autocomplete            = ((boolean), default: true) If false, prevents autocomplete
    
    * submitarea *
    (nested)                = ((markup)) Button(s) to include
                              The buttons may be generated with {{{<@field type="submit">}}} or manual {{{<input>}}}, {{{<a>}}}, {{{<button>}}} elements.
    progressArgs            = ((map)) If this is an upload form, arguments to pass to @progress macro
                              See @progress and @progressScript macros. Should specify formSel, at least one of progBarId and progTextBoxId, and others.
    progressOptions         = ((map)) Progress options (convenience parameter)
                              Same as passing:
                                progressArgs={"enabled":true, "progressOptions":progressOptions}      
                      
    * submit *
    submitType              = (submit|link|button|image|input-button, default: submit) Submit element type
                              * {{{submit}}}: {{{<input type="submit" ... />}}}
                              * {{{input-button}}}: {{{<input type="button" ... />}}}
                              * {{{link}}}: {{{<a href="..." ...>...</a>}}}
                                NOTE: href should usually be specified for this, or explicitly set to boolean false if using onClick. 
                                    If not specified, generated href will cause form submit with form name (if found and not disabled).
                              * {{{button}}}: {{{<input type="button" ... />}}}
                                WARN: FIXME?: Currently this is same as input-button: {{{<input type="button" ... />}}}
                                  This could change to {{{<button...>...</button>}}} without notice...
                              * {{{image}}}: {{{<input type="image" src="..." .../>}}}
    text                    = ((string), default: -from global styles-) Display text, for text-link and button submits
                              If omitted, a default "Submit" text is used from global styles.
                              NOTE: Reserved values: If set to empty space " " or "_NO_BTN_MARKUP_", 
                                  button markup is not rendered (shows progress only) (the empty space is from a stock form widget convention).
                              NOTE: {{{value}}} arg is also accepted instead of {{{text}}}.
    href                    = href for submitType "link"  
                              NOTE: This parameter is automatically (re-)escaped for HTML and javascript (using #escapeFullUrl or equivalent) 
                                  to help prevent injection, as it is high-risk. It accepts pre-escaped query string delimiters for compatibility,
                                  but other characters should not be manually escaped (apart from URL parameter encoding).
    src                     = Image url for submitType "image"    
    confirmMsg              = Confirmation message     
    progressArgs            = Same as for submitarea, but only works if this is a top-level submit     
    progressOptions         = Same as for submitarea, but only works if this is a top-level submit
                      
    * reset *
    text                    = Label to show on reset button
                      
    * display *
    valueType               = (image|text|currency|date|date-time|timestamp|accounting-number|generic, default: generic)
                              "date-time" and "timestamp" are synonymous.
                              * {{{generic}}}: treated as arbitrary content, but text may still be interpreted
                              TODO: Currently all are handled as text/generic (because formatting done in java in stock ofbiz)
    value                   = Display value or image URL
    description             = For image type: image alt
    tooltip                 = Tooltip text
                              May result in extra wrapping container.
    formatText              = ((boolean), default: true) If true, translates newlines to HTML linebreaks (and potentially other transformations)
                              NOTE: 2017-08-03: The default for @field macro is currently {{{true}}}, which is the same as the Ofbiz form widget default, which is true.
                                  Prior to 2017-08-03, this {{{formatText}}} parameter documentation had been mistakenly changed 
                                  to suggest the default was {{{false}}} for {{{generic}}} (default) valueType for ftl templates (only). 
                                  However, the actual code was not completely changed and the effective default remained {{{true}}}. 
                                  Thus, currently, for compatibility reasons, the default is currently left to {{{true}}} 
                                  for all cases including {{{generic}}} valueType for ftl templates.
    
    * generic *
    tooltip                 = Tooltip text
                              May result in extra wrapping container.
                              
  * History *
    Modified for 1.14.4 (fixed documentation for "display" type "formatText" parameter).
-->
<#assign field_defaultArgs = {
  "type":"", "fieldsType":"", "label":"", "labelContent":false, "labelDetail":false, "name":"", "value":"", "valueType":"", 
  "currentValue":"", "altValue":false, "useHidden":"", "defaultValue":"", "class":"", "size":20, "maxlength":"", "id":"", 
  "onClick":"", "onChange":"", "onFocus":"",
  "disabled":false, "placeholder":"", "autoCompleteUrl":"", "mask":false, "alert":"false", "readonly":false, "rows":"4", 
  "cols":"50", "dateType":"date-time", "dateDisplayType":"",  "multiple":"", "checked":"", 
  "collapse":"", "collapsePostfix":"", "collapsedInlineLabel":"", "labelSmallDiffColumns":"",
  "tooltip":"", "gridArgs":{}, "totalColumns":"", "labelColumns":"", "widgetPostfixCombined":"", "norows":false, "nocells":false, "container":"", "widgetOnly":"", "containerId":"", "containerClass":"", "containerStyle":"",
  "fieldFormName":"", "formName":"", "formId":"", "postfix":false, "postfixColumns":"", "postfixContent":true, "required":false, "requiredClass":"", "requiredTooltip":true, "items":false, "autocomplete":true, "progressArgs":{}, "progressOptions":{}, 
  "labelType":"", "labelPosition":"", "labelArea":"", "labelAreaRequireContent":"", "labelAreaConsume":"", "inlineLabelArea":"", "inlineLabel":false,
  "description":"",
  "submitType":"input", "text":"", "href":"", "src":"", "confirmMsg":"", "inlineItems":"", 
  "selected":false, "allowEmpty":false, "currentFirst":false, "currentDescription":"",
  "manualItems":"", "manualItemsOnly":"", "dynSelectArgs":{}, "asmSelectArgs":false, "title":"", "allChecked":"", "checkboxType":"", "radioType":"", 
  "inline":"", "ignoreParentField":"",
  "opValue":"", "opFromValue":"", "opThruValue":"", "ignoreCaseValue":"", "hideOptions":false, "hideIgnoreCase":false,
  "titleClass":"", "formatText":"",
  "preWidgetContent":false, "postWidgetContent":false, "preLabelContent":false, "postLabelContent":false, "prePostfixContent":false, "postPostfixContent":false,
  "prePostContentArgs":{}, "postfixContentArgs":{}, "labelContentArgs":{}, "style":"",
  "widgetAreaClass":"", "labelAreaClass":"", "postfixAreaClass":"", "widgetPostfixAreaClass":"",
  "inverted":false, "invertedClass":"", "standardClass":"", "datePostfix":"", "datePostfixColumns":"",
  "manualInput":"",
  "events":{}, "wrap":"", 
  "autoValue":0, "autoValueArgs":{}, 
  "passArgs":{} 
}>
<#macro field args={} inlineArgs...> 
<#-- WARN: #compress must be used sparingly; using only around code parts -->
<#compress>
  <#-- TODO: Group arguments above so easier to read... -->

  <#-- parent @fields group elem info (if any; may be omitted in templates) -->
  <#local fieldsType = inlineArgs.fieldsType!args.fieldsType!field_defaultArgs.fieldsType>
  <#if fieldsType?has_content>
    <#local fieldsInfo = makeFieldsInfo({"type":fieldsType})>
  <#else>
    <#local fieldsInfo = readRequestStack("scipioFieldsInfoStack")!{}>
    <#if !fieldsInfo.type??>
      <#if !scipioDefaultFieldsInfo?has_content>
        <#-- optimization -->
        <#global scipioDefaultFieldsInfo = makeFieldsInfo({"type":"default"})>
      </#if>
      <#local fieldsInfo = scipioDefaultFieldsInfo>
    </#if>
  </#if>

  <#-- special default fields override -->
  <#local defaultArgs = scipioStdTmplLib.field_defaultArgs>
  <#if !fieldsInfo.fieldArgs?is_boolean>
    <#-- 2016-07-08: this is probably wrong because it makes fieldArgs keys part of localArgNames,
      so instead combine it with args map, though it requires a manual toSimpleMap, not ideal
    <#local defaultArgs = defaultArgs + fieldsInfo.fieldArgs>
    -->
    <#local args = fieldsInfo.fieldArgs + toSimpleMap(args)>
  </#if>

  <#-- standard args -->
  <#local argsMaps = mergeArgMapsEx(args, inlineArgs, defaultArgs)>
  <#local args = argsMaps.allArgs>
  <#local explArgs = argsMaps.explArgs>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
        
  <#-- other defaults -->      
  <#if !type?has_content>
    <#local type = "generic">
  <#elseif type == "text">
    <#local type = "input">
  </#if>
  <#if !valueType?has_content>
    <#local valueType = "generic">
  </#if>

  <#local fieldsType = (fieldsInfo.type)!"">

  <#if inline?is_boolean && inline == true>
    <#local container = false>
    <#local class = addClassArg(class, styles.field_inline!)>
    <#-- force label to be inline using our own user flags (easiest) -->
    <#if !labelArea?is_boolean>
      <#local labelArea = false>
    </#if>
  </#if>

  <#if widgetOnly?is_boolean && widgetOnly == true>
    <#local container = false>
    <#if !labelArea?is_boolean>
      <#local labelArea = false>
    </#if>
  </#if>

  <#if onClick?has_content>
    <#local events = events + {"click": onClick}>
  </#if>
  <#if onChange?has_content>
    <#local events = events + {"change": onChange}>
  </#if>
  <#if onFocus?has_content>
    <#local events = events + {"focus": onFocus}>
  </#if>
  
  <#-- Backward-compability - don't consider empty string (or empty anything) as having content -->
  <#if !labelContent?has_content && !labelContent?is_directive>
    <#local labelContent = false>
  </#if>
  <#if !labelDetail?has_content && !labelDetail?is_directive>
    <#local labelDetail = false>
  </#if>
  <#if !preWidgetContent?has_content && !preWidgetContent?is_directive>
    <#local preWidgetContent = false>
  </#if>
  <#if !postWidgetContent?has_content && !postWidgetContent?is_directive>
    <#local postWidgetContent = false>
  </#if>
  <#if !preLabelContent?has_content && !preLabelContent?is_directive>
    <#local preLabelContent = false>
  </#if>
  <#if !postLabelContent?has_content && !postLabelContent?is_directive>
    <#local postLabelContent = false>
  </#if>
  <#if !prePostfixContent?has_content && !prePostfixContent?is_directive>
    <#local prePostfixContent = false>
  </#if>
  <#if !postPostfixContent?has_content && !postPostfixContent?is_directive>
    <#local postPostfixContent = false>
  </#if>

  <#-- parent @field elem info (if any; is possible) -->
  <#local parentFieldInfo = readRequestStack("scipioFieldInfoStack")!{}>
  <#-- allow ignore parent -->
  <#if ignoreParentField?is_boolean>
    <#if ignoreParentField>
      <#local parentFieldInfo = {}>
    </#if>
  <#elseif fieldsInfo.ignoreParentField?is_boolean>
    <#if fieldsInfo.ignoreParentField>
      <#local parentFieldInfo = {}>
    </#if>
  </#if>
  <#local hasParentField = ((parentFieldInfo.type)!"")?has_content>
  <#local isTopLevelField = !hasParentField>
  <#local isChildField = hasParentField>
  
  <#local formInfo = readRequestStack("scipioFormInfoStack")!{}>
  
  <#-- get custom default for inlineItems -->
  <#if !inlineItems?has_content>
    <#if parentFieldInfo.inlineItems?has_content>
      <#local inlineItems = parentFieldInfo.inlineItems>
    <#elseif fieldsInfo.inlineItems?has_content>
      <#local inlineItems = fieldsInfo.inlineItems>
    </#if>
  </#if>
  
  <#-- get form name and id -->
  <#if !formName?has_content>
    <#if fieldsInfo.formName?has_content>
      <#local formName = fieldsInfo.formName>
    <#elseif formInfo.name?has_content>
      <#local formName = formInfo.name>
    </#if>
  </#if>
  <#if !formId?has_content>
    <#if fieldsInfo.formId?has_content>
      <#local formId = fieldsInfo.formName>
    <#elseif formInfo.id?has_content>
      <#local formId = formInfo.id>
    </#if>
  </#if>

  <#local fieldIdNum = getNextFieldIdNum()>
  <#if !id?has_content>
    <#local id = getNextFieldId(fieldIdNum)>
  </#if>
  <#if !containerId?has_content && id?has_content>
    <#local containerId = id + "_container">
  </#if>
  
  <#if required && (!requiredClass?is_boolean && requiredClass?has_content)>
    <#local class = addClassArg(class, requiredClass)>
  </#if>

  <#if required && !(tooltip?is_boolean && tooltip == false) && !(requiredTooltip?is_boolean && requiredTooltip == false)>
    <#if !requiredTooltip?is_boolean && requiredTooltip?has_content>
      <#-- 2016-04-21: This should ADD to the tooltip, not replace it
      <#local tooltip = getTextLabelFromExpr(requiredTooltip)>-->
      <#local tooltip = addStringToBoolStringVal(tooltip, getTextLabelFromExpr(requiredTooltip)!"", styles.tooltip_delim!" - ")>
    </#if>
  </#if>

  <#if inverted>
    <#if (!invertedClass?is_boolean && invertedClass?has_content)>
      <#local containerClass = addClassArg(containerClass, invertedClass)>
    </#if>
  <#else>
    <#if (!standardClass?is_boolean && standardClass?has_content)>
      <#local containerClass = addClassArg(containerClass, standardClass)>
    </#if>  
  </#if>

  <#-- treat tooltip and description (nearly) as synonyms for now -->
  <#if !tooltip?is_boolean && tooltip?has_content>
    <#if !(description?is_boolean && description == false) && !description?has_content>
      <#local description = tooltip>
    </#if>
  <#else>
    <#if (!description?is_boolean && description?has_content) && !(tooltip?is_boolean && tooltip == false)>
      <#local tooltip = description>
    </#if>
  </#if>

  <#-- the widgets do this now
  <#local class = compileClassArg(class)>-->
    
  <#if !container?is_boolean>
    <#if container?has_content>
      <#local container = container?boolean>
    <#elseif type == "hidden">
      <#-- 2017-02-24: no container for hidden, unless explicit specified for some reason... -->
      <#local container = false>
    <#elseif isChildField && (((styles.field_type_nocontainer_whenchild[type])!false) || 
      ((styles.field_type_nocontainer_whenhasparent[parentFieldInfo.type!])!false))>
      <#local container = false>
    <#else> 
      <#local container = true>
    </#if>
  </#if>
  
  <#-- label area logic
      NOTE: labelArea boolean logic does not determine "label type" or "label area type"; 
          only controls presence of. so labelArea logic and usage anywhere should not change
          if new label (area) type were to be added (e.g. on top instead of side by side). -->
  <#if labelArea?is_boolean>
    <#local labelAreaDefault = labelArea>
  <#elseif labelType == "none" || labelPosition == "none">
    <#local labelAreaDefault = false>
  <#elseif isChildField>
    <#local labelAreaDefault = false>
  <#else>
    <#local labelAreaDefault = (fieldsInfo.labelArea)!false>
    <#if (fieldsInfo.labelAreaExceptions)?has_content && (fieldsInfo.labelAreaExceptions)?is_sequence>
      <#if fieldsInfo.labelAreaExceptions?seq_contains(type)>
        <#local labelAreaDefault = !labelAreaDefault>
      <#elseif (type == "radio" || type == "checkbox")>
        <#if fieldsInfo.labelAreaExceptions?seq_contains(type + "-single") && !items?is_sequence>
          <#local labelAreaDefault = !labelAreaDefault>
        <#elseif fieldsInfo.labelAreaExceptions?seq_contains(type + "-multi") && items?is_sequence>
          <#local labelAreaDefault = !labelAreaDefault>
        </#if>
      </#if>
    </#if>
  </#if>
  
  <#if labelType?has_content>
    <#local effLabelType = labelType>
  <#else>
    <#local effLabelType = (fieldsInfo.labelType)!"">
  </#if>
  <#if labelPosition?has_content>
    <#local effLabelPosition = labelPosition>
  <#else>
    <#local effLabelPosition = (fieldsInfo.labelPosition)!"">
  </#if>

  <#if collapsedInlineLabel?is_boolean>
    <#local effCollapsedInlineLabel = collapsedInlineLabel>
  <#else>
    <#local effCollapsedInlineLabel = (fieldsInfo.collapsedInlineLabel)![]>
  </#if>

  <#if !labelAreaRequireContent?is_boolean>
    <#local labelAreaRequireContent = (fieldsInfo.labelAreaRequireContent)!false>
    <#if !labelAreaRequireContent?is_boolean>
      <#local labelAreaRequireContent = false>
    </#if>
  </#if>
  
  <#-- The way this now works is that labelArea boolean is the master control, and 
      by default, presence of label or labelDetail (with ?has_content) does NOT influence if label area
      will be present or not.
      
      It is now this way so that the code has the ability to "consume" (show) the label if a label
      area is present; if there's no label area, the label is passed down to the input widget as inlineLabel.
      This is needed for radio, checkbox and probably others later.
      
      There is another labelAreaRequireContent control that is separate from the consumation logic.
      In our default setup we want it set to false, but can be changed in styles and calls.
      -->
  <#local labelAreaConsumeLabel = (labelArea?is_boolean && labelArea == true) || 
      (!(labelArea?is_boolean && labelArea == false) && (labelAreaDefault))>
  <#if labelAreaConsume?is_boolean> <#-- this is the user setting -->
    <#-- user can prevent consuming by setting false -->
    <#local labelAreaConsumeLabel = labelAreaConsumeLabel && labelAreaConsume>
  <#else>
    <#-- check if @fields style prevents consuming -->
    <#if (fieldsInfo.labelAreaConsumeExceptions)?has_content && (fieldsInfo.labelAreaConsumeExceptions)?is_sequence>
      <#if fieldsInfo.labelAreaConsumeExceptions?seq_contains(type)>
        <#local labelAreaConsumeLabel = false>
      <#elseif (type == "radio" || type == "checkbox")>
        <#if fieldsInfo.labelAreaConsumeExceptions?seq_contains(type + "-single") && !items?is_sequence>
          <#local labelAreaConsumeLabel = false>
        <#elseif fieldsInfo.labelAreaConsumeExceptions?seq_contains(type + "-multi") && items?is_sequence>
          <#local labelAreaConsumeLabel = false>
        </#if>
      </#if>
    </#if>
  </#if>
  
  <#local origLabel = label>
  <#local effInlineLabel = false> <#-- this is really a string -->
  <#if !labelAreaConsumeLabel && !(inlineLabelArea?is_boolean && inlineLabelArea == false)>
    <#-- if there's no label area or if it's not set to receive the label, 
        label was not used up, so label arg becomes an inline label (used on radio and checkbox) 
        - unless caller overrides with his own inline label for whatever reason 
        - and unless he wants to prevent inline area with inlineLabelArea = false -->
    <#if !(inlineLabel?is_boolean && inlineLabel == false)>
      <#local effInlineLabel = inlineLabel>
    <#else>
      <#local effInlineLabel = label>
    </#if>
    <#local label = "">
  <#elseif inlineLabelArea?is_boolean && inlineLabelArea == true>
    <#-- caller wants a specific inline label -->
    <#local effInlineLabel = inlineLabel>
  </#if>

  <#-- NOTE: labelAreaRequireContent should not affect consume logic above -->
  <#local useLabelArea = (labelArea?is_boolean && labelArea == true) || 
    (!(labelArea?is_boolean && labelArea == false) && 
      (!labelAreaRequireContent || (label?has_content || !labelContent?is_boolean || !labelDetail?is_boolean)) && (labelAreaDefault))>
  
  <#-- FIXME?: labelContent currently does not follow the label inlining logic; only @field_markup_labelarea will render it -->
  
  <#-- Special case where inlineLabel is re-implemented using actual label area using collapsing. -->
  <#if !(effInlineLabel?is_boolean && effInlineLabel == false) && effInlineLabel?has_content && 
    ((effCollapsedInlineLabel?is_boolean && effCollapsedInlineLabel == true) ||
     (effCollapsedInlineLabel?is_sequence && effCollapsedInlineLabel?seq_contains(type)))>
    <#local useLabelArea = true>
    <#local label = effInlineLabel>
    <#if label?is_boolean>
      <#local label = "">
    </#if>
    <#local effInlineLabel = false> <#-- we're using it, so don't pass it down to widget anymore -->
    <#if !collapse?is_boolean>
      <#local collapse = true>
    </#if>
  </#if>
  
  <#if !collapse?is_boolean>
    <#local collapse = (fieldsInfo.collapse)!false>
    <#if !collapse?is_boolean>
      <#local collapse = false>
    </#if>
  </#if>
  <#if !collapsePostfix?is_boolean>
    <#local collapsePostfix = (fieldsInfo.collapsePostfix)!true>
    <#if !collapsePostfix?is_boolean>
      <#local collapsePostfix = true>
    </#if>
  </#if>
  
  <#-- TODO?: ensure boolean string because next calls don't yet support it as boolean -->
  <#if tooltip?is_boolean>
    <#local tooltip = "">
  </#if>
  <#if description?is_boolean>
    <#local description = "">
  </#if>
  
  <#-- push this field's info (popped at end) -->
  <#local fieldInfo = {"type":type, "inlineItems":inlineItems, "id":id}>
  <#local dummy = pushRequestStack("scipioFieldInfoStack", fieldInfo)>
  
  <#-- main markup begin -->
  <#local labelAreaContent = false>
  <#if useLabelArea>
    <#local labelAreaContent = fieldLabelAreaInvoker><#-- macro -->
    <#-- NOTE: origArgs is passed because in some cases it may be important for markup to know if the caller manually
        specified a certain parameter to @field or not - the other logical args don't record this info -->
    <#-- DEV NOTE: WARN: If you add any arguments here, they must also be added to @fieldLabelAreaInvoker macro below! 
        This pattern is used to get the @field_markup_labelarea invocation to occur at the correct time (within the label area) -->
    <#-- DEV NOTE: Also see @fieldLabelAreaInvoker - it recombines labelAreaContentArgs into labelContentArgs! -->
    <#local labelAreaContentArgs = {"labelType":effLabelType, "labelPosition":effLabelPosition, "label":label, "labelContent":labelContent, "labelDetail":labelDetail, 
        "fieldType":type, "fieldsType":fieldsType, "fieldId":id, "collapse":collapse, "required":required, "labelContentArgs":labelContentArgs, 
        "norows":norows, "nocells":nocells, "container":container,
        "origArgs":origArgs, "passArgs":passArgs}>
  </#if>
 
  <#-- auto value integration -->
  <#if !autoValue?is_boolean>
    <#local autoValue = scpAutoVal!false>
    <#if !autoValue?is_boolean><#-- TODO: REVIEW: without this there was a crash. probably shouldn't happen -->
      <#local autoValue = false>
    </#if>
  </#if>
  <#if autoValue>
    <#if type == "checkbox" || type == "radio">
      <#-- TODO: handle defaultValue? -->
      <#local autoValueArgsAll = {"name":name}>
      <#if explArgs.checked??>
        <#if (checked?is_boolean && checked == true) || (checked?is_string && checked == "checked")>
          <#local selectedVal = value>
        <#else>
          <#local selectedVal = altValue>
        </#if>
        <#local autoValueArgsAll = autoValueArgsAll + {"value":selectedVal}>
      <#elseif explArgs.currentValue??>
        <#if currentValue == value>
          <#local selectedVal = value>
        <#else>
          <#local selectedVal = altValue>
        </#if>
        <#local autoValueArgsAll = autoValueArgsAll + {"value":selectedVal}>
      </#if>
      <#local currentValue = getAutoValue(autoValueArgsAll + autoValueArgs)!>
      <#local checked = (currentValue == value)>
    <#elseif type == "option" || type == "submit" || type == "submitarea" || type == "reset">
      <#-- do nothing -->
    <#elseif type == "select">
      <#-- TODO: handle defaultValue? -->
      <#local autoValueArgsAll = {"name":name}>
      <#if explArgs.value??>
        <#local autoValueArgsAll = autoValueArgsAll + {"value":explArgs.value}>
      </#if>
      <#local currentValue = getAutoValue(autoValueArgsAll + autoValueArgs)!>
    <#else>
      <#-- types with extra inputs -->

      <#if type == "textfind">
        <#local autoValueArgsAll = {"name":name}>
        <#if explArgs.value??>
          <#local autoValueArgsAll = autoValueArgsAll + {"value":explArgs.value}>
        </#if>
        <#local value = getAutoValue(autoValueArgsAll + autoValueArgs)!>
      
        <#local autoValueArgsAll = {"name":name, "suffix":"_op"}>
        <#if explArgs.opValue??>
          <#local autoValueArgsAll = autoValueArgsAll + {"value":explArgs.opValue}>
        </#if>
        <#local opValue = getAutoValue(autoValueArgsAll + autoValueArgs)!>
        
        <#-- SPECIAL: checkbox needs to check presence of main value because is not 
            submitted when not checked -->
        <#local autoValueArgsAll = {"name":name, "suffix":"_ic",
          "submitFlagParam":autoValueArgs.paramName!name,
          "submitDefaultParamValue":"",
          "defaultValue":"Y"
        }>
        <#if explArgs.ignoreCaseValue??>
          <#local autoValueArgsAll = autoValueArgsAll + {"value":ignoreCaseValue?string("Y","")}>
        </#if>
        <#local ignoreCaseValueStr = getAutoValue(autoValueArgsAll + autoValueArgs)!>
        <#local ignoreCaseValue = (ignoreCaseValueStr == "Y")>

      <#elseif type == "rangefind">
        <#local autoValueArgsAll = {"name":name, "suffix":"_fld0_value"}>
        <#if explArgs.value??>
          <#local autoValueArgsAll = autoValueArgsAll + {"value":explArgs.value}>
        </#if>
        <#local value = getAutoValue(autoValueArgsAll + autoValueArgs)!>
      
        <#local autoValueArgsAll = {"name":name, "suffix":"_fld0_op"}>
        <#if explArgs.opFromValue??>
          <#local autoValueArgsAll = autoValueArgsAll + {"value":explArgs.opFromValue}>
        </#if>
        <#local opFromValue = getAutoValue(autoValueArgsAll + autoValueArgs)!>
        <#local autoValueArgsAll = {"name":name, "suffix":"_fld1_op"}>
        <#if explArgs.opThruValue??>
          <#local autoValueArgsAll = autoValueArgsAll + {"value":explArgs.opThruValue}>
        </#if>
        <#local opThruValue = getAutoValue(autoValueArgsAll + autoValueArgs)!>
        
      <#elseif type == "datefind">
        <#local autoValueArgsAll = {"name":name, "suffix":"_fld0_value"}>
        <#if explArgs.value??>
          <#local autoValueArgsAll = autoValueArgsAll + {"value":explArgs.value}>
        </#if>
        <#local value = getAutoValue(autoValueArgsAll + autoValueArgs)!>
      
        <#local autoValueArgsAll = {"name":name, "suffix":"_fld0_op"}>
        <#-- NOTE: this must match logic further below -->
        <#if explArgs.opFromValue?has_content>
          <#local autoValueArgsAll = autoValueArgsAll + {"value":explArgs.opFromValue}>
        <#elseif explArgs.opValue??>
          <#local autoValueArgsAll = autoValueArgsAll + {"value":explArgs.opValue}>
        </#if>
        <#local opFromValue = getAutoValue(autoValueArgsAll + autoValueArgs)!>
        <#local opValue = opFromValue>
        
      <#else>
        <#-- standard value input case -->
        <#local autoValueArgsAll = {"name":name}>
        <#if explArgs.value??>
          <#local autoValueArgsAll = autoValueArgsAll + {"value":explArgs.value}>
        </#if>
        <#local value = getAutoValue(autoValueArgsAll + autoValueArgs)!>
      </#if>

    </#if>
  </#if>
 
  <#local defaultGridArgs = {"totalColumns":totalColumns, "labelColumns":labelColumns, 
    "widgetPostfixCombined":widgetPostfixCombined, "labelArea":useLabelArea, 
    "labelInRow":(effLabelType != "vertical"), "postfix":postfix, "postfixColumns":postfixColumns,
    "fieldsType":fieldsType, "labelSmallDiffColumns":labelSmallDiffColumns}>
</#compress>     
  <@field_markup_container type=type fieldsType=fieldsType defaultGridArgs=defaultGridArgs gridArgs=gridArgs postfix=postfix  
    postfixContent=postfixContent labelArea=useLabelArea labelType=effLabelType labelPosition=effLabelPosition labelAreaContent=labelAreaContent 
    collapse=collapse collapsePostfix=collapsePostfix norows=norows nocells=nocells container=container containerId=containerId containerClass=containerClass containerStyle=containerStyle
    preWidgetContent=preWidgetContent postWidgetContent=postWidgetContent preLabelContent=preLabelContent postLabelContent=postLabelContent prePostfixContent=prePostfixContent postPostfixContent=postPostfixContent
    labelAreaContentArgs=labelAreaContentArgs postfixContentArgs=postfixContentArgs prePostContentArgs=prePostContentArgs
    widgetAreaClass=widgetAreaClass labelAreaClass=labelAreaClass postfixAreaClass=postfixAreaClass widgetPostfixAreaClass=widgetPostfixAreaClass
    inverted=inverted labelSmallDiffColumns=labelSmallDiffColumns origArgs=origArgs required=required passArgs=passArgs>
    <#switch type>
      <#case "input">
        <@field_input_widget name=name 
                              class=class 
                              style=style 
                              alert=alert 
                              value=value 
                              textSize=size 
                              maxlength=maxlength 
                              id=id 
                              events=events
                              disabled=disabled
                              readonly=readonly 
                              clientAutocomplete="" 
                              ajaxUrl=autoCompleteUrl 
                              ajaxEnabled="" 
                              mask=mask 
                              placeholder=placeholder 
                              tooltip=tooltip
                              inlineLabel=effInlineLabel
                              required=required
                              passArgs=passArgs/>
        <#break>
      <#case "textarea">
        <@field_textarea_widget name=name 
                              class=class 
                              alert=alert 
                              cols=cols 
                              style=style 
                              rows=rows 
                              id=id
                              readonly=readonly 
                              value=value 
                              placeholder=placeholder
                              tooltip=tooltip
                              inlineLabel=effInlineLabel
                              maxlength=maxlength
                              wrap=wrap
                              required=required
                              passArgs=passArgs>${escapeVal(text, 'htmlmarkup')}${escapeVal(value, 'htmlmarkup')}<#nested></@field_textarea_widget>
        <#break>
      <#case "datetime">
        <#if dateType == "date" || dateType == "time" || dateType == "month">
          <#-- leave as-is -->
        <#else> <#-- "date-time" -->
          <#local dateType = "timestamp">
        </#if>
        <@field_datetime_widget name=name 
                              class=class 
                              style=style 
                              alert=alert 
                              title=title 
                              value=value 
                              events=events
                              size=size 
                              maxlength=maxlength 
                              id=id 
                              dateType=dateType 
                              dateDisplayType=dateDisplayType 
                              formName=formName
                              tooltip=tooltip
                              origLabel=origLabel
                              manualInput=manualInput
                              postfix=datePostfix
                              postfixColumns=datePostfixColumns
                              inlineLabel=effInlineLabel
                              required=required
                              passArgs=passArgs/>                
        <#break>
      <#case "datefind">
        <#if dateType == "date" || dateType == "time">
          <#-- leave as-is -->
        <#else> <#-- "date-time" -->
          <#local dateType = "timestamp">
        </#if>
        <#if opFromValue?has_content>
          <#local datefindOpFromValue = opFromValue>
        <#else>
          <#local datefindOpFromValue = opValue>
        </#if>
        <@field_datefind_widget name=name 
                              class=class 
                              style=style 
                              alert=alert 
                              title=title 
                              value=value 
                              defaultOptionFrom=datefindOpFromValue
                              size=size 
                              maxlength=maxlength 
                              id=id 
                              dateType=dateType 
                              dateDisplayType=dateDisplayType 
                              formName=formName
                              tooltip=tooltip
                              origLabel=origLabel
                              inlineLabel=effInlineLabel
                              required=required
                              passArgs=passArgs/>                 
        <#break>
      <#case "textfind">
        <@field_textfind_widget name=name 
                              class=class 
                              style=style 
                              alert=alert 
                              title=title 
                              value=value 
                              defaultOption=opValue
                              ignoreCase=ignoreCaseValue
                              size=size 
                              maxlength=maxlength 
                              id=id 
                              formName=formName
                              tooltip=tooltip
                              hideOptions=hideOptions
                              hideIgnoreCase=hideIgnoreCase
                              titleClass=titleClass
                              origLabel=origLabel
                              inlineLabel=effInlineLabel
                              required=required
                              passArgs=passArgs/>                 
        <#break>
      <#case "rangefind">
        <@field_rangefind_widget name=name 
                              class=class 
                              style=style 
                              alert=alert 
                              title=title 
                              value=value 
                              defaultOptionFrom=opFromValue
                              defaultOptionThru=opThruValue
                              size=size 
                              maxlength=maxlength 
                              id=id 
                              formName=formName
                              tooltip=tooltip
                              titleClass=titleClass
                              origLabel=origLabel
                              inlineLabel=effInlineLabel
                              required=required
                              passArgs=passArgs/>                 
        <#break>
      <#case "select">
        <#if !manualItemsOnly?is_boolean>
          <#local manualItemsOnly = !items?is_sequence>
        </#if>
        <#if !manualItems?is_boolean>
          <#-- FIXME? this should be based on whether nested has content, but don't want to invoke #nested twice -->
          <#local manualItems = !items?is_sequence>
        </#if>
        <@field_select_widget name=name
                                class=class 
                                style=style 
                                alert=alert 
                                id=id
                                disabled=disabled 
                                multiple=multiple
                                formName=formName
                                formId=formId
                                otherFieldName="" 
                                events=events 
                                size=size
                                currentFirst=currentFirst
                                currentValue=currentValue 
                                allowEmpty=allowEmpty
                                options=items
                                fieldName=name
                                otherFieldName="" 
                                otherValue="" 
                                otherFieldSize=0 
                                inlineSelected=!currentFirst
                                ajaxEnabled=false
                                defaultValue=defaultValue
                                ajaxOptions=""
                                frequency=""
                                minChars=""
                                choices="" 
                                autoSelect=""
                                partialSearch=""
                                partialChars=""
                                ignoreCase=""
                                fullSearch=""
                                title=title
                                tooltip=tooltip
                                description=description
                                manualItems=manualItems
                                manualItemsOnly=manualItemsOnly
                                currentDescription=currentDescription
                                dynSelectArgs=dynSelectArgs
                                asmSelectArgs=asmSelectArgs
                                inlineLabel=effInlineLabel
                                required=required
                                passArgs=passArgs><#nested></@field_select_widget>
        <#break>
      <#case "option">
        <@field_option_widget value=value text=text selected=selected style=style passArgs=passArgs><#nested></@field_option_widget>
        <#break>
      <#case "lookup">
        <@field_lookup_widget name=name formName=formName fieldFormName=fieldFormName class=class style=style alert="false" value=value 
          size=size?string maxlength=maxlength id=id events=events title=title tooltip=tooltip required=required passArgs=passArgs/>
      <#break>
      <#case "checkbox">
        <#if valueType?is_string && valueType == "indicator">
          <#local value = "Y">
          <#local altValue = "N">
          <#local useHidden = true>
        </#if>
        <#if !checkboxType?has_content>
          <#local checkboxType = fieldsInfo.checkboxType>
        </#if>
        <#if !items?is_sequence>
          <#if !checked?is_boolean>
            <#if checked?has_content>
              <#if checked == "true" || checked == "Y" || checked == "checked">
                <#local checked = true>
              <#else>
                <#local checked = false>
              </#if>
            <#else>
              <#local checked = "">
            </#if>
          </#if>
          <#local description = effInlineLabel>
          <#if description?is_boolean>
            <#local description = "">
          </#if>
          <#local items=[{"value":value, "altValue":altValue, "useHidden":useHidden, "description":description, "tooltip":tooltip, "events":events, "checked":checked, "readonly":readonly, "disabled":disabled}]/>
          <@field_checkbox_widget multiMode=false items=items inlineItems=inlineItems id=id class=class style=style alert=alert 
            currentValue=currentValue defaultValue=defaultValue allChecked=allChecked name=name tooltip="" inlineLabel=effInlineLabel type=checkboxType 
                readonly=readonly disabled=disabled required=required passArgs=passArgs/>
        <#else>
          <@field_checkbox_widget multiMode=true items=items inlineItems=inlineItems id=id class=class style=style alert=alert 
            currentValue=currentValue defaultValue=defaultValue allChecked=allChecked name=name events=events tooltip=tooltip inlineLabel=effInlineLabel type=checkboxType 
            value=value altValue=altValue useHidden=useHidden
            readonly=readonly disabled=disabled required=required passArgs=passArgs/>
        </#if>
        <#break>
      <#case "radio">
        <#if !radioType?has_content>
          <#local radioType = fieldsInfo.radioType>
        </#if>
        <#if !items?is_sequence>
          <#-- single radio button item mode -->
          <#if !checked?is_boolean>
            <#if checked?has_content>
              <#if checked == "true" || checked == "Y" || checked == "checked">
                <#local checked = true>
              <#else>
                <#local checked = false>
              </#if>
            <#else>
              <#local checked = "">
            </#if>
          </#if>
          <#local description = effInlineLabel>
          <#if description?is_boolean>
            <#local description = "">
          </#if>
          <#local items=[{"key":value, "description":description, "tooltip":tooltip, "events":events, "checked":checked, "readonly":readonly, "disabled":disabled}]/>
          <@field_radio_widget multiMode=false items=items inlineItems=inlineItems id=id class=class style=style alert=alert 
            currentValue=currentValue defaultValue=defaultValue name=name tooltip="" inlineLabel=effInlineLabel type=radioType 
            readonly=readonly disabled=disabled required=required passArgs=passArgs/>
        <#else>
          <#-- multi radio button item mode -->
          <@field_radio_widget multiMode=true items=items inlineItems=inlineItems id=id class=class style=style alert=alert 
            currentValue=currentValue defaultValue=defaultValue name=name events=events tooltip=tooltip inlineLabel=effInlineLabel type=radioType 
            readonly=readonly disabled=disabled required=required passArgs=passArgs/>
        </#if>
        <#break>
      <#case "file">
        <@field_file_widget class=class alert=alert name=name value=value size=size maxlength=maxlength 
          autocomplete=autocomplete?string("", "off") id=id inlineLabel=effInlineLabel style=style required=required passArgs=passArgs/>
        <#break>
      <#case "password">
        <@field_password_widget class=class alert=alert name=name value=value size=size maxlength=maxlength 
          id=id autocomplete=autocomplete?string("", "off") placeholder=placeholder tooltip=tooltip inlineLabel=effInlineLabel style=style required=required passArgs=passArgs/>
        <#break> 
      <#case "reset">                    
        <@field_reset_widget class=class alert=alert name=name text=text fieldTitleBlank=false inlineLabel=effInlineLabel style=style passArgs=passArgs/>
        <#break>    
      <#case "submit">
        <#if !scipioSubmitFieldTypeButtonMap??>
          <#-- NOTE: currently button is same as input-button, maybe should be different? -->
          <#-- the logical button types (based on form widget types) -->
          <#global scipioSubmitFieldButtonTypeMap = {
            "submit":"button", "button":"button", "link":"text-link", "image":"image", "input-button":"button"
          }>
          <#-- the low-level input type attrib, within the logical button types -->
          <#global scipioSubmitFieldInputTypeMap = {
            "submit":"submit", "button":"button", "link":"", "image":"image", "input-button":"button"
          }>
        </#if>      
        <#local buttonType = scipioSubmitFieldButtonTypeMap[submitType]!"button">
        <#local inputType = scipioSubmitFieldInputTypeMap[submitType]!"submit">
        <#-- support legacy "value" for text as conversion help -->
        <#if !text?has_content && value?has_content> <#-- accept this for all types now because error-prone: inputType == "submit" -->
          <#local text = value>
        </#if>
        <@field_submit_widget buttonType=buttonType class=class id=id alert=alert formName=formName name=name events=events 
          imgSrc=src confirmation=confirmMsg containerId="" ajaxUrl="" text=text description=description showProgress=false 
          href=href inputType=inputType disabled=disabled progressArgs=progressArgs progressOptions=progressOptions inlineLabel=effInlineLabel 
          style=style passArgs=passArgs/>
        <#break>
      <#case "submitarea">
        <@field_submitarea_widget progressArgs=progressArgs progressOptions=progressOptions inlineLabel=effInlineLabel style=style passArgs=passArgs><#nested></@field_submitarea_widget>
        <#break>
      <#case "hidden">                    
        <@field_hidden_widget name=name value=value id=id class=class events=events inlineLabel=effInlineLabel style=style passArgs=passArgs/>
        <#break>        
      <#case "display">
        <#-- TODO?: may need formatting here based on valueType... not done by field_display_widget... done in java OOTB... 
            can also partially detect type of value with ?is_, but is not enough... -->
        <#if !valueType?has_content || (valueType == "generic")>
          <#local displayType = "text">
          <#if !formatText?is_boolean>
            <#-- SCIPIO: NOTE: 2017-08-03: there was an intent for this default to be false (for valueType == "generic" only, for ftl only), but
                the code was never committed; so for compatibility reasons, leaving to true for now; see doc
            <#local formatText = false>-->
            <#local formatText = true>
          </#if>
        <#else>
          <#local displayType = valueType>
        </#if>
        <@field_display_widget type=displayType value=value idName="" 
          title=title class=class id=id alert=alert inPlaceEditorUrl="" inPlaceEditorParams="" style=style 
          imageAlt=description tooltip=tooltip formatText=formatText inlineLabel=effInlineLabel required=required passArgs=passArgs><#nested></@field_display_widget>
        <#break> 
      <#default> <#-- "generic", empty or unrecognized -->
        <#if value?has_content>
          <@field_generic_widget class=class text=value title=title tooltip=tooltip inlineLabel=effInlineLabel style=style required=required passArgs=passArgs/>
        <#else>
          <@field_generic_widget class=class title=title tooltip=tooltip inlineLabel=effInlineLabel style=style required=required passArgs=passArgs><#nested /></@field_generic_widget>
        </#if>
    </#switch>
  </@field_markup_container>
  <#-- pop field info when done -->
  <#local dummy = popRequestStack("scipioFieldInfoStack")>
  <#local dummy = setRequestVar("scipioLastFieldInfo", fieldInfo)>
</#macro>

<#function getNextFieldIdNum> 
  <#local fieldIdNum = getRequestVar("scipioFieldIdNum")!0>
  <#local fieldIdNum = fieldIdNum + 1 />
  <#local dummy = setRequestVar("scipioFieldIdNum", fieldIdNum)>
  <#return fieldIdNum>
</#function>

<#function getNextFieldId fieldIdNum=true>
  <#if fieldIdNum?is_boolean>
    <#local fieldIdNum = getNextFieldIdNum()>
  </#if>
  <#-- FIXME? renderSeqNumber usually empty... where come from? should be as request attribute also? -->
  <#local id = "field_id_${renderSeqNumber!}_${fieldIdNum!0}">
  <#return id>
</#function>

<#-- @field container markup - theme override 
    nested content is the actual field widget (<input>, <select>, etc.). 
    WARN: origArgs may be empty -->
<#macro field_markup_container type="" fieldsType="" defaultGridArgs={} gridArgs={} 
    postfix=false postfixContent=true labelArea=true labelType="" labelPosition="" labelAreaContent="" collapse="" 
    collapseLabel="" collapsePostfix="" norows=false nocells=false container=true containerId="" containerClass="" containerStyle=""
    preWidgetContent=false postWidgetContent=false preLabelContent=false postLabelContent=false prePostfixContent=false postPostfixContent=false
    labelAreaContentArgs={} postfixContentArgs={} prePostContentArgs={}
    widgetAreaClass="" labelAreaClass="" postfixAreaClass="" widgetPostfixAreaClass="" inverted=false labelSmallDiffColumns=""
    origArgs={} passArgs={} required=false noLabelCell=false noInputCell=false catchArgs...>
  <#local rowClass = containerClass>

  <#local labelInRow = (labelType != "vertical")>
  
  <#local widgetPostfixCombined = gridArgs.widgetPostfixCombined!defaultGridArgs.widgetPostfixCombined!"">
  <#if !widgetPostfixCombined?is_boolean>
    <#-- We may have collapse==false but collapsePostfix==true, in which case
        we may want to collapse the postfix without collapsing the entire thing. 
        Handle this by making a combined sub-row if needed.
        2016-04-05: This container is also important for max field row width CSS workaround!
            Therefore, we will also omit the collapsePostfix requirement. -->
    <#if postfix && !collapse> <#-- previously: ((postfix && collapsePostfix) && !collapse) -->
      <#local widgetPostfixCombined = styles["fields_" + fieldsType + "_widgetpostfixcombined"]!styles["fields_default_widgetpostfixcombined"]!true>
    <#else>
      <#local widgetPostfixCombined = false>
    </#if>
  </#if>

  <#-- This is separated because some templates need access to the grid sizes to align things, and they
      can't be calculated statically in the styles hash -->
  <#local defaultGridStyles = getDefaultFieldGridStyles(defaultGridArgs + {"labelInRow": labelInRow,
    "widgetPostfixCombined":widgetPostfixCombined} + gridArgs)>
  <#-- NOTE: For inverted, we don't swap the defaultGridStyles grid classes, only the user-supplied and identifying ones -->

  <#local fieldEntryTypeClass = "field-entry-type-" + mapScipioFieldTypeToStyleName(type)>
  <#local labelAreaClass = addClassArg(labelAreaClass, "field-entry-title " + fieldEntryTypeClass)>
  <#local widgetAreaClass = addClassArg(widgetAreaClass, "field-entry-widget " + fieldEntryTypeClass)>
  <#local postfixAreaClass = addClassArg(postfixAreaClass, "field-entry-postfix " + fieldEntryTypeClass)>
  <#local widgetPostfixAreaClass = addClassArg(widgetPostfixAreaClass, "field-entry-widgetpostfix " + fieldEntryTypeClass)>

  <#local rowClass = addClassArg(rowClass, "form-field-entry " + fieldEntryTypeClass)>
  <@row class=compileClassArg(rowClass) collapse=collapse!false norows=(norows || !container) id=containerId style=containerStyle>
    <#if labelType == "vertical">
      <@cell>
        <#if labelArea && labelPosition == "top" || labelArea && labelPosition == "left">
          <@row collapse=collapse norows=(norows || !container)>
            <#if inverted>
              <#local widgetAreaClass = addClassArg(widgetAreaClass, "field-entry-widget-top")>
            <#else>
              <#local labelAreaClass = addClassArg(labelAreaClass, "field-entry-title-top")>
            </#if>
            <@cell class=compileClassArg(inverted?string(widgetAreaClass, labelAreaClass), defaultGridStyles.labelArea) nocells=(nocells || !container)>
              <#if inverted>
                <#if !preWidgetContent?is_boolean><@contentArgRender content=preWidgetContent args=prePostContentArgs /></#if>
                <#nested>
                <#if !postWidgetContent?is_boolean><@contentArgRender content=postWidgetContent args=prePostContentArgs /></#if>
              <#else>
                <#if !preLabelContent?is_boolean><@contentArgRender content=preLabelContent args=prePostContentArgs /></#if>
                <#if !labelAreaContent?is_boolean><@contentArgRender content=labelAreaContent args=labelAreaContentArgs /></#if>
                <#if !postLabelContent?is_boolean><@contentArgRender content=postLabelContent args=prePostContentArgs /></#if>
              </#if>
            </@cell>
          </@row>
        </#if>
          <@row collapse=(collapse || (postfix && collapsePostfix)) norows=(norows || !container)>
            <#if inverted>
                <@cell class=compileClassArg(labelAreaClass, defaultGridStyles.widgetArea) nocells=(nocells || !container || noLabelCell)>
                    <#if !preLabelContent?is_boolean><@contentArgRender content=preLabelContent args=prePostContentArgs /></#if>
                    <#if !labelAreaContent?is_boolean><@contentArgRender content=labelAreaContent args=labelAreaContentArgs /></#if>
                    <#if !postLabelContent?is_boolean><@contentArgRender content=postLabelContent args=prePostContentArgs /></#if>
                </@cell>
            <#else>
                <@cell class=compileClassArg(widgetAreaClass, defaultGridStyles.widgetArea) nocells=(nocells || !container || noInputCell)>                
                    <#if !preWidgetContent?is_boolean><@contentArgRender content=preWidgetContent args=prePostContentArgs /></#if>
                    <#nested>
                    <#if !postWidgetContent?is_boolean><@contentArgRender content=postWidgetContent args=prePostContentArgs /></#if>
                </@cell>
            </#if>
            <#if postfix>
              <#if !nocells && container>
              <@cell class=compileClassArg(postfixAreaClass, defaultGridStyles.postfixArea)>
                <#if !prePostfixContent?is_boolean><@contentArgRender content=prePostfixContent args=prePostContentArgs /></#if>
                <#if (postfixContent?is_boolean && postfixContent == true) || !postfixContent?has_content>
                  <span class="${styles.postfix!}"><input type="submit" class="${styles.icon!} ${styles.icon_button!}" value="${styles.icon_button_value!}"/></span>
                <#elseif !postfixContent?is_boolean> <#-- boolean false means prevent markup -->
                  <#if !postfixContent?is_boolean><@contentArgRender content=postfixContent args=postfixContentArgs /></#if>
                </#if>
                <#if !postPostfixContent?is_boolean><@contentArgRender content=postPostfixContent args=prePostContentArgs /></#if>
              </@cell>
              <#else>
                <#if !prePostfixContent?is_boolean><@contentArgRender content=prePostfixContent args=prePostContentArgs /></#if>
                <#if (postfixContent?is_boolean && postfixContent == true) || !postfixContent?has_content>
                  <span class="${styles.postfix!}"><input type="submit" class="${styles.icon!} ${styles.icon_button!}" value="${styles.icon_button_value!}"/></span>
                <#elseif !postfixContent?is_boolean> <#-- boolean false means prevent markup -->
                  <#if !postfixContent?is_boolean><@contentArgRender content=postfixContent args=postfixContentArgs /></#if>
                </#if>
                <#if !postPostfixContent?is_boolean><@contentArgRender content=postPostfixContent args=prePostContentArgs /></#if>
              </#if>
            </#if>
          </@row>
      </@cell>
    <#else> <#-- elseif labelType == "horizontal" -->
      <#-- TODO: support more label configurations (besides horizontal left) -->
      <#if labelArea && labelPosition == "left" || labelArea && labelPosition == "top">
        <#if inverted>
          <#-- NOTE: (per comment above) when inverted, we use user's widgetAreaClass BUT we use the calculated grid
              class for label area (defaultGridStyles.labelArea) -->
          <#local widgetAreaClass = addClassArg(widgetAreaClass, "field-entry-widget-left")>
          <@cell class=compileClassArg(widgetAreaClass, defaultGridStyles.labelArea) nocells=(nocells || !container || noInputCell)>
            <#if !preWidgetContent?is_boolean><@contentArgRender content=preWidgetContent args=prePostContentArgs /></#if>
            <#nested>
            <#if !postWidgetContent?is_boolean><@contentArgRender content=postWidgetContent args=prePostContentArgs /></#if>
          </@cell>
        <#else>
          <#local labelAreaClass = addClassArg(labelAreaClass, "field-entry-title-left")>
          <@cell class=compileClassArg(labelAreaClass, defaultGridStyles.labelArea) nocells=(nocells || !container || noLabelCell)>
            <#if !preLabelContent?is_boolean><@contentArgRender content=preLabelContent args=prePostContentArgs /></#if>
            <#if !labelAreaContent?is_boolean><@contentArgRender content=labelAreaContent args=labelAreaContentArgs /></#if>
            <#if !postLabelContent?is_boolean><@contentArgRender content=postLabelContent args=prePostContentArgs /></#if>
          </@cell>
        </#if>
      </#if>

      <#-- need this surrounding cell/row for collapsePostfix (only if true and collapse false) -->
      <@cell class=compileClassArg(widgetPostfixAreaClass, defaultGridStyles.widgetPostfixArea) open=widgetPostfixCombined close=widgetPostfixCombined>
        <@row open=widgetPostfixCombined close=widgetPostfixCombined collapse=(collapse || (postfix && collapsePostfix))>
          <#-- NOTE: here this is the same as doing 
                 class=("=" + compileClassArg(class, defaultGridStyles.widgetArea))
               as we know the compiled class will never be empty. -->
            <#if inverted>
              <#-- NOTE: (per comment above) when inverted, we use user's labelAreaClass BUT we use the calculated grid
                class for widget area (defaultGridStyles.widgetArea) -->
              <@cell class=compileClassArg(labelAreaClass, defaultGridStyles.widgetArea) nocells=(nocells || !container || noLabelCell)>
                <#if !preLabelContent?is_boolean><@contentArgRender content=preLabelContent args=prePostContentArgs /></#if>
                <#if !labelAreaContent?is_boolean><@contentArgRender content=labelAreaContent args=labelAreaContentArgs /></#if>
                <#if !postLabelContent?is_boolean><@contentArgRender content=postLabelContent args=prePostContentArgs /></#if>
              </@cell>
            <#else>
              <@cell class=compileClassArg(widgetAreaClass, defaultGridStyles.widgetArea) nocells=(nocells || !container || noInputCell)>          
                <#if !preWidgetContent?is_boolean><@contentArgRender content=preWidgetContent args=prePostContentArgs /></#if>
                <#nested>
                <#if !postWidgetContent?is_boolean><@contentArgRender content=postWidgetContent args=prePostContentArgs /></#if>
              </@cell>
            </#if>
          <#if postfix>
            <#if !nocells && container>
              <@cell class=compileClassArg(postfixAreaClass, defaultGridStyles.postfixArea)>
                <#if !prePostfixContent?is_boolean><@contentArgRender content=prePostfixContent args=prePostContentArgs /></#if>
                <#if (postfixContent?is_boolean && postfixContent == true) || !postfixContent?has_content>
                  <span class="${styles.postfix!}"><input type="submit" class="${styles.icon!} ${styles.icon_button!}" value="${styles.icon_button_value!}"/></span>
                <#elseif !postfixContent?is_boolean> <#-- boolean false means prevent markup -->
                  <#if !postfixContent?is_boolean><@contentArgRender content=postfixContent args=postfixContentArgs /></#if>
                </#if>
                <#if !postPostfixContent?is_boolean><@contentArgRender content=postPostfixContent args=prePostContentArgs /></#if>
              </@cell>
            <#else>
              <#if !prePostfixContent?is_boolean><@contentArgRender content=prePostfixContent args=prePostContentArgs /></#if>
              <#if (postfixContent?is_boolean && postfixContent == true) || !postfixContent?has_content>
                <span class="${styles.postfix!}"><input type="submit" class="${styles.icon!} ${styles.icon_button!}" value="${styles.icon_button_value!}"/></span>
              <#elseif !postfixContent?is_boolean> <#-- boolean false means prevent markup -->
                <#if !postfixContent?is_boolean><@contentArgRender content=postfixContent args=postfixContentArgs /></#if>
              </#if>
              <#if !postPostfixContent?is_boolean><@contentArgRender content=postPostfixContent args=prePostContentArgs /></#if>
            </#if>
          </#if>
        </@row>
      </@cell>
    </#if>
  </@row>
</#macro>

<#-- This is a helper macro needed to get @field_markup_labelarea to render in the right spot. Themes should not override this. -->
<#macro fieldLabelAreaInvoker args={}>
  <#-- NOTE: Special case for labelContentArgs -->
  <@field_markup_labelarea labelType=args.labelType labelPosition=args.labelPosition label=args.label labelContent=args.labelContent labelDetail=args.labelDetail 
        fieldType=args.fieldType fieldsType=args.fieldsType fieldId=args.fieldId collapse=args.collapse required=args.required 
        labelContentArgs=(args + args.labelContentArgs) norows=args.norows nocells=args.nocells container=args.container
        origArgs=args.origArgs passArgs=args.passArgs/><#t>
</#macro>

<#-- @field label area markup - theme override 
    WARN: origArgs may be empty -->
<#macro field_markup_labelarea labelType="" labelPosition="" label="" labelContent=false labelDetail=false fieldType="" fieldsType="" fieldId="" collapse="" 
    required=false labelContentArgs={} norows=false nocells=false container=true origArgs={} passArgs={} catchArgs...>
  <#-- the label must be escaped by default. caller can prevent using #wrapAsRaw
  <#local label = label?trim>-->
  <#local label = escapeVal(label, 'htmlmarkup')?trim>
  <#if !labelContent?is_boolean>
    <@contentArgRender content=labelContent args=labelContentArgs doTrim=true />
    <#-- don't show this here, let macro handle it
    <#if required>*</#if>-->
  <#elseif label?has_content>
    <#if collapse>
      <span class="${styles.prefix!} form-field-label">${label}<#if required> *</#if></span>
    <#else>
      <label class="form-field-label"<#if fieldId?has_content> for="${escapeVal(fieldId, 'html')}"</#if>>${label}<#if required> *</#if></label>
    </#if>
  <#-- only show this if there's a label, otherwise affects inline fields too in ugly way, and there are other indications anyhow
  <#else>
    <#if required>*</#if>-->
  </#if> 
  <#if !labelDetail?is_boolean><@contentArgRender content=labelDetail args=labelContentArgs doTrim=true /></#if>
  <#-- This was nbsp to prevent collapsing empty cells in foundation, now replaced by a CSS hack (see _base.scss)
  <#if container && !nocells>
    <#if !label?has_content && labelDetail?is_boolean && labelContent?is_boolean>
      &nbsp;
    </#if>
  </#if>-->
</#macro>

<#-- 
*************
* getDefaultFieldGridStyles
************
Returns the classes that @field would put on the label, widget and postfix area containers, given the requirements.
Caller may override any.

NOTE: This is used both internally by @field and in some cases is also needed in templates.
                    
TODO: This (and @field args) do not currently provide enough control over large vs small,
    and medium is omitted entirely.
                    
  * Parameters *
    fieldsType              = ((string), default: default) The @fields type
                              Used for calculating the defaults of some of the other parameters.
    widgetPostfixCombined   = ((boolean), default: false) Whether the calculation should consider widget and postfix having an extra container around them together
                              NOTE: The hardcoded default for this is {{{false}}} and must always be {{{false}}}.
                                  The hardcoding is part of this function's interface. This is because structure depends highly
                                  on what the caller decides is appropriate and there is not enough information to decide it here.
                              NOTE: Even though the default for this is false, in many cases generally we end up using true.
    totalColumns            = ((int), default: -from global styles-) The logical total columns for a field row
                              NOTE: This does not have to be 12.
    totalLarge              = ((int), default: -value of totalColumns-) Total columns override for large
    totalSmall              = ((int), default: -value of totalColumns-) Total columns override for small
    labelColumns            = ((int), default: -from global styles-) The columns size of label area, if {{{labelArea}}} and {{{labelInRow}}} are true
    labelLarge              = ((int), default: -value of labelColumns-) Label area columns override for large              
    labelSmallLarge         = ((int), default: -value of labelColumns-)  Label area columns override for small              
    postfixColumns          = ((int), default: -from global styles-) Postfix columns
    postfixLarge            = ((int), default: -value of postfixColumns-) Postfix columns override for large
    postfixSmall            = ((int), default: -value of postfixColumns-) Postfix columns override for small
-->
<#assign getDefaultFieldGridStyles_defaultArgs = {
  "widgetPostfixCombined":"", "fieldsType":"",
  "labelArea":"", "labelInRow":"",
  "postfix":"", "isLargeParent":"", "labelSmallDiffColumns":"",
  "totalColumns":"", "totalLarge":"", "totalSmall":"", 
  "labelColumns":"", "labelLarge":"", "labelSmall":"",
  "postfixColumns":"", "postfixLarge":"", "postfixSmall":""
}>
<#function getDefaultFieldGridStyles args={} catchArgs...>
  <#local args = mergeArgMapsBasic(args, {}, scipioStdTmplLib.getDefaultFieldGridStyles_defaultArgs)>
  <#local dummy = localsPutAll(args)> 
  
  <#-- TODO: rewrite default case in java for speed -->
  
  <#-- basic arg defaults -->

  <#if !fieldsType?has_content>
    <#local fieldsType = "default">
  </#if>
  <#if !postfix?is_boolean>
    <#local postfix = false>
  </#if>
  <#if !labelArea?is_boolean>
    <#local labelArea = true>
  </#if>
  <#if !labelInRow?is_boolean>
    <#local labelInRow = true>
  </#if>
  <#if !widgetPostfixCombined?is_boolean>
    <#local widgetPostfixCombined = false>
  </#if>
  <#if !isLargeParent?is_boolean>
    <#local largeContainerFactor = styles["large_container_factor"]!6>
    <#-- get estimate of the current absolute column widths (with all parent containers, as much as possible) -->
    <#local absColSizes = getAbsContainerSizeFactors()>
    <#-- if parent container is large, then we'll include the large grid sizes; otherwise only want small to apply -->
    <#local isLargeParent = (absColSizes.large > largeContainerFactor)>  
  </#if>
  
  
  <#-- global styles lookups  -->
  
  <#-- TODO?: All these global styles lookups don't really have to happen per-field, should optimize to cache results so less map lookups -->
  <#if !totalColumns?has_content>
    <#local totalColumns = styles["fields_" + fieldsType + "_totalcolumns"]!styles["fields_default_totalcolumns"]!12>
  </#if>
  <#if labelArea && labelInRow>
    <#if !labelColumns?has_content>
      <#local labelColumns = styles["fields_" + fieldsType + "_labelcolumns"]!styles["fields_default_labelcolumns"]!2>
    </#if>
  <#else>
    <#local labelColumns = totalColumns>
  </#if>
  <#if postfix>
    <#if !postfixColumns?has_content>
      <#local postfixColumns = styles["fields_" + fieldsType + "_postfixcolumns"]!styles["fields_default_postfixcolumns"]!1>
    </#if>
  <#else>
    <#local postfixColumns = 0>
  </#if>
  <#if !labelSmallDiffColumns?has_content>
    <#local labelSmallDiffColumns = styles["fields_" + fieldsType + "_labelsmallcoldiff"]!styles["fields_default_labelsmallcoldiff"]!1>
  </#if>

  <#if !totalLarge?has_content>
    <#local totalLarge = styles["fields_" + fieldsType + "_totallarge"]!styles["fields_default_totallarge"]!totalColumns>
  </#if>
  <#if labelArea && labelInRow>
    <#if !labelLarge?has_content>
      <#local labelLarge = styles["fields_" + fieldsType + "_labellarge"]!styles["fields_default_labellarge"]!labelColumns>
    </#if>
  <#else>
    <#local labelLarge = totalLarge>
  </#if>
  <#if postfix>
    <#if !postfixLarge?has_content>
      <#local postfixLarge = styles["fields_" + fieldsType + "_postfixlarge"]!styles["fields_default_postfixlarge"]!postfixColumns>
    </#if>
  <#else>
    <#local postfixLarge = 0>
  </#if>

  <#if !totalSmall?has_content>
    <#local totalSmall = styles["fields_" + fieldsType + "_totalsmall"]!styles["fields_default_totalsmall"]!totalColumns>
  </#if>
  <#if labelArea && labelInRow>
    <#if !labelSmall?has_content>
      <#local labelSmall = styles["fields_" + fieldsType + "_labelsmall"]!styles["fields_default_labelsmall"]!(labelColumns+labelSmallDiffColumns)>
    </#if>
  <#else>
    <#local labelSmall = totalSmall>
  </#if>
  <#if postfix>
    <#if !postfixSmall?has_content>
      <#local postfixSmall = styles["fields_" + fieldsType + "_postfixsmall"]!styles["fields_default_postfixsmall"]!postfixColumns>
    </#if>
  <#else>
    <#local postfixSmall = 0>
  </#if>
  
  <#-- resolve unspecified values -->

  <#if labelArea && labelInRow>
    <#local widgetPostfixLarge = totalLarge - labelLarge>
    <#local widgetPostfixSmall = totalSmall - labelSmall>
  <#else>
    <#-- Either no label or the label is on top and gets its own row -->
    <#local widgetPostfixLarge = totalLarge>
    <#local widgetPostfixSmall = totalSmall>
  </#if>
  
  <#if widgetPostfixCombined>
    <#-- widget area will be child of a separate container. 
        Currently total columns MUST be hardcoded as 12 here. -->
    <#local widgetLarge = 12 - postfixLarge>
    <#local widgetSmall = 12 - postfixSmall>
  <#else>
    <#local widgetLarge = widgetPostfixLarge - postfixLarge>
    <#local widgetSmall = widgetPostfixSmall - postfixSmall>
  </#if>

  <#local labelAreaClass><#if labelArea>${styles.grid_small!}${labelSmall}<#if isLargeParent> ${styles.grid_large!}${labelLarge}</#if></#if></#local>
  <#local widgetPostfixAreaClass>${styles.grid_small!}${widgetPostfixSmall}<#if isLargeParent> ${styles.grid_large!}${widgetPostfixLarge}</#if></#local>
  <#local widgetAreaClass>${styles.grid_small!}${widgetSmall}<#if isLargeParent> ${styles.grid_large!}${widgetLarge}</#if></#local>
  <#local postfixAreaClass><#if postfix>${styles.grid_small!}${postfixSmall}<#if isLargeParent> ${styles.grid_large!}${postfixLarge}</#if></#if></#local>
  
  <#-- This is last if in separate row -->
  <#if labelArea && !labelInRow>
    <#local labelAreaClass = labelAreaClass + " " + styles.grid_end!>
  </#if>

  <#-- This is last in all cases where no postfix -->
  <#if !postfix>
    <#local widgetAreaClass = widgetAreaClass + " " + styles.grid_end!>
  </#if>

  <#-- This is always last (when used) -->
  <#local widgetPostfixAreaClass = widgetPostfixAreaClass + " " + styles.grid_end!>

  <#-- This is always last (when used) -->
  <#if postfix>
    <#local postfixAreaClass = postfixAreaClass + " " + styles.grid_end!>
  </#if>
  
  <#return {
    "labelArea" : labelAreaClass,
    "widgetPostfixArea" : widgetPostfixAreaClass,
    "widgetArea" : widgetAreaClass,
    "postfixArea" : postfixAreaClass
  }>
</#function>

<#-- 
*************
* getAutoValue
************
Returns an appropriate field value (typically for use with @field) based on current values in request
and context, following a certain scheme type specified directly or previously through globals.

WARN: NOT FULLY IMPLEMENTED (2016-07-15) - EXPERIMENTAL

Typically the value is looked up in a set of global maps (parameters, a record or entity, defaults, etc.)
following some predefined priority.

The schema type may be specified directly, but in most cases it should have been specified for a group of 
getAutoValue calls using @fields, where the specific maps to use when looking up the value may also be specified. 
It is also possible to manually call #setAutoValueCfg to set them, which should rarely be needed.

NOTE: This method conditionally acts on the {{{isError}}} boolean context field. Usually it is
    set depending on the result of the last controller event. In non-standard cases you may have
    to set it manually.

TODO: We need more options (and/or types) to make tweakable the special handling in cases of error, new records, etc.

  * Parameters *
    type                    = (standard|..., default: -from globals-, fallback default: standard) The value scheme type
                              * {{{params}}}: looks for value in overrides map, then parameters map, then defaults map
                              * {{{record}}}: looks for value in overrides map, then record map, then defaults map
                              * {{{defaults-only}}}: looks for value in overrides map, then defaults map
                              * {{{params-record}}}: looks for value in overrides map, then parameters map, then record, then defaults map.
                                This may be
                              * {{{params-or-record}}}: looks for value in overrides map, then EITHER parameters map OR record map, then defaults map
                                At current time (2016-07-08), the selection of parameters or record map default behavior is based on whether an event
                                error occurred ({{{isError}}} boolean context field).
                              * {{{standard}}}: In scipio standard API, currently (2016-07-08), this is the same params-or-record, currently considered the standard behavior.
    name                    = Main field name, used for all maps that does not have more specific names (overrideName, paramName, etc.)
                              NOTE: As a convenience, name can be passed as single parameter instead of the {{{args}}} map.
    overrideName            = Field name for overrides map
    paramName               = Field name for parameters map
    recordName              = Field name for record map
    defaultName             = Field name for defaults map
    suffix                  = Optional suffix added to each of the name parameters
    overrideValue           = An override values, which takes priority over all
    paramValue              = A param value, which takes immediate priority over the params map
    value                   = A value, which takes immediate priority over the record values
                              It is ignored in all cases where the record map is also ignored (such as {{{type="params"}}}).
    defaultValue            = A default value, which takes immediate priority over the default map values
    submitDetectMethod      = (default|flag|post|none, default: default) Submit detection method
                              * {{{default}}}: use {{{flag}}} if {{{submitFlagParam}}}, otherwise use {{{post}}}
                              * {{{flag}}}: use {{{submitFlagParam}}}
                              * {{{post}}}: use POST request check
                              * {{{none}}}: none
    submitFlagParam         = Optional name of a parameter from params map whose presence determines if a form was submitted or not.
                              Automatically implies {{{submitDetectMethod="flag"}}}.
                              This parameter is checked using {{{??}}} operator, by simple presence.
                              This is needed for parameters whose HTML inputs don't always submit a value.
                              If a submit happened and the field is missing, then the field is given the value specified in {{{submitDefaultParamValue}}}.
                              By default, submission is detected using presence of POST request, but in most cases,
                              it is better to have submitFlagParam specified, easiest using @fields.
    submitDefaultParamValue = ((string), default: ""/[]/{}) Default param value to use if submitFlagParam checks out
    submitError             = ((boolean)|"", default: "") Explicit success/error flag
                              If not specified as boolean (empty string), uses isError context variable.
                              
  * Related *
    @fields
    #setAutoValueCfg
-->
<#assign getAutoValue_defaultArgs = {
  "type":"", "name":"", "overrideName":"", "paramName":"", "recordName":"",
  "defaultName":"", "suffix":"", "overrideValue":"", "paramValue":"", 
  "value":"", "defaultValue":"", "submitFlagParam":"", "submitDefaultParamValue":"",
  "submitDetectMethod":"default", "submitError":""
}>
<#function getAutoValue args={}>
  <#if isObjectType("string", args)><#-- shorthand -->
    <#local args = {"name": args}>
  </#if>
  <#local type = args.type!scpAutoValType!"params-or-record">
  <#if !type?has_content || type == "standard">
    <#local type = "params-or-record">
  </#if>

  <#local overrides = scpAutoValOverrides!{}>
  <#local params = getAutoValueEffParamsMap()>
  <#local recordOrBool = scpAutoValRecord!false>
  <#if !recordOrBool?is_hash>
    <#local recordOrBool = false>
  </#if>
  <#local record = scpAutoValRecord!{}>
  <#local defaults = scpAutoValDefaults!{}>

  <#local suffix = args.suffix!"">
  <#local overrideName = (args.overrideName!args.name) + suffix>
  <#local paramName = (args.paramName!args.name) + suffix>
  <#local recordName = (args.defaultName!args.name) + suffix>
  <#local defaultName = (args.defaultName!args.name) + suffix>
  
  <#local submitFlagParam = args.submitFlagParam!scpAutoValSubFlagParam!"">
  <#local submitDetectMethod = args.submitDetectMethod!scpAutoValFlagMethod!"">
  
  <#local submitError = args.submitError!scpAutoValSubError!"">
  <#if !submitError?is_boolean>
    <#local submitError = isError!false>
  </#if>
  
  <#-- Submit detection: 
    we detect submits using submitFlagParam first. If it's not there, then fallback
    on POST check.
  -->
  <#local submitted = (submitFlagParam?has_content && params[args.submitFlagParam]??) ||
    (submitDetectMethod != "none" && (request.getMethod()!?upper_case) == "POST")>
    
  <#-- default param values for non-submitted inputs after a form submit -->
  <#if !params[paramName]?? && submitted>
    <#-- this concat is awful, but best we can do here -->
    <#local params = params + {paramName:args.submitDefaultParamValue!}>
  </#if>

  <#-- 
    DEV NOTE: We need the behavior of "params-or-record" to be similar to the code in:
      org.ofbiz.widget.model.ModelFormField.getEntry(Map, String, boolean)
    but can ignore "useRequestParameters" because our type substitutes for it.
    
    The stock behavior is that params are only considered if there was an error
    in the last create/update operation. This usually is fairly sane, it means
    that user input will be preserved on errors but discarded and reloaded from DB on success.
    NOTE: the main limitation of this is that it's not possible to call screen with helper-like
        pre-filled values. But this seems rare.
        TODO?: we could maybe allow this by detecting if a controller event was run or not...
            but that is making some assumptions that won't always hold.
    Special case: if there was no record map, then we'll use parameters. This is close
    to the "create by default from parameters passed in" special case noted in stock.
    
    TODO: Currently the whole form/record must use either parameters or record.
        We are limiting behavior to prevent problems with some fields such as checkboxes,
        and because this is stock behavior currently.
  -->
  <#if type == "params" || (type == "params-or-record" && (submitError || recordOrBool?is_boolean))>
    <#return args.overrideValue!overrides[overrideName]!args.paramValue!params[paramName]!args.defaultValue!defaults[defaultName]!>
  <#elseif type == "record" || (type == "params-or-record")>
    <#return args.overrideValue!overrides[overrideName]!args.value!record[recordName]!args.defaultValue!defaults[defaultName]!>
  <#elseif type == "params-record">
    <#return args.overrideValue!overrides[overrideName]!args.paramValue!params[paramName]!args.value!record[recordName]!args.defaultValue!defaults[defaultName]!>
  <#elseif type == "defaults-only">
    <#return args.overrideValue!overrides[overrideName]!args.defaultValue!defaults[defaultName]!>
  </#if>
</#function>

<#-- 
*************
* setAutoValueCfg
************
Sets the current global value configuration (maps and settings) used by #getAutoValue, and can also be used 
to disable auto value lookups for @field.

NOTE: Any parameters not specified leave the existing corresponding globals unchanged.

NOTE: The globals specified by this function currently do not survive screen render boundaries; they
    have page/template scope, not request scope.

  * Parameters *
    autoValue               = ((boolean)) Determines if auto value lookups are enabled for macros such as @field
                              NOTE: Unlike some other macros and functions (such as @fields), for this function, if this
                                  parameter is omitted, the function will not turn on auto values. It is a manual
                                  call and requires explicit true.
    type                    = ((string)) The value scheme type
                              See #getAutoValue for possible values.
    overrides               = ((map)) Map to use as overrides map for lookups
    params                  = ((map)|(boolean)) Map to use as parameters map for lookups
                              Normally, if this is not specified anywhere, the Ofbiz parameters map is used.
                              If this is set to boolean false (special value), no parameters map will be used.
    record                  = ((map)) Map to use as record map for lookups
                              Usually this is something like an entity value.
    defaults                = ((map)) Map to use as defaults map for lookups
    submitDetectMethod      = ((string)) Submit detection method
                              See #getAutoValue for possible values.
    submitFlagParam         = Name of a parameter from params map whose presence can be used to detect whether a form submit occurred
                              See #getAutoValue for info.
    submitError             = ((boolean)|"", default: "") Explicit success/error flag
                              If not specified as boolean (empty string), uses isError context variable.
    
  * Related *
    #getAutoValueCfg
    #getAutoValue                       
-->
<#assign setAutoValueCfg_defaultArgs = {
  "autoValue":"", "type":"", "overrides":{}, "params":{}, "record":{}, "defaults":{}, 
  "submitDetectMethod":"", "submitFlagParam":"", "submitError":""
}>
<#function setAutoValueCfg args={}>
  <#if args.autoValue??>
    <#global scpAutoVal = args.autoValue>
  </#if>
  <#if args.type??>
    <#global scpAutoValType = args.type>
  </#if>
  <#if args.overrides??>
    <#global scpAutoValOverrides = args.overrides>
  </#if>
  <#if args.params??>
    <#global scpAutoValParams = args.params>
  </#if>
  <#if args.record??>
    <#global scpAutoValRecord = args.record>
  </#if>
  <#if args.defaults??>
    <#global scpAutoValDefaults = args.defaults>
  </#if>
  <#if args.submitDetectMethod??>
    <#global scpAutoValSubDetect = args.submitDetectMethod>
  </#if>
  <#if args.submitFlagParam??>
    <#global scpAutoValSubFlagParam = args.submitFlagParam>
  </#if>
  <#if args.submitError??>
    <#global scpAutoValSubError = args.submitError>
  </#if>
  <#return "">
</#function>

<#--
*************
* getAutoValueCfg
************
Returns the current global auto value configuration (settings and maps).

  * Related *
    #setAutoValueCfg
    #getAutoValue
-->
<#assign getAutoValueCfg_defaultArgs = {
  "dummy":""
}>
<#function getAutoValueCfg args={}>
  <#return {"autoValue":scpAutoVal!"", "type":scpAutoValType!"", 
    "overrides":scpAutoValOverrides!false, "params":scpAutoValParams!false, 
    "record":scpAutoValRecord!false, "defaults":scpAutoValDefaults!false,
    "submitDetectMethod":scpAutoValSubDetect!"", "submitFlagParam":scpAutoValSubFlagParam!"",
    "submitError":scpAutoValSubError!""}>
</#function>

<#function getAutoValueEffParamsMap>
  <#if scpAutoValParams?? && !(scpAutoValParams?is_boolean && scpAutoValParams == true)>
    <#local params = scpAutoValParams>
    <#if params?is_boolean>
      <#local params = {}>
    </#if>
  <#else>
    <#-- by default, use parameters map -->
    <#local params = parameters!{}>
  </#if>
  <#return params>
</#function>

<#-- 
*************
* getAutoValueMap
************
Returns a field value autodetermining helper map.

TODO: NOT IMPLEMENTED

  * Parameters *
    type                    = (standard|..., default: -from globals-, fallback default: standard) The value scheme type
                              * {{{params}}}: looks for value in overrides map, then parameters map, then defaults map
                              * {{{record}}}: looks for value in overrides map, then record map, then defaults map
                              * {{{defaults-only}}}: looks for value in overrides map, then defaults map
                              * {{{params-record}}}: looks for value in overrides map, then parameters map, then record, then defaults map.
                                This may be
                              * {{{params-or-record}}}: looks for value in overrides map, then EITHER parameters map OR record map, then defaults map
                                At current time (2016-07-08), the selection of parameters or record map default behavior is based on whether an event
                                error occurred ({{{isError}}} boolean context field).
                              * {{{standard}}}: In scipio standard API, currently (2016-07-08), this is the same params-or-record, currently considered the standard behavior.
   
-->
<#assign getAutoValueMap_defaultArgs = {
  "type":""
}>
<#function getAutoValueMap args={}>
  <#-- TODO: NOT IMPLEMENTED. through: com.ilscipio.scipio.ce.webapp.ftl.template.standard.FieldValueMap -->
</#function>
