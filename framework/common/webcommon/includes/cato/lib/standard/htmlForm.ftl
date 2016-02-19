<#--
* 
* Form and field HTML template include, standard Cato markup.
*
* Included by htmlTemplate.ftl.
*
* NOTE: May have implicit dependencies on other parts of Cato API.
*
-->

<#include "htmlFormFieldWidget.ftl">

<#-- 
*************
* Form
************
An HTML form element.

  * Usage Example *  
    <@form name="myform">
      <@fields>
        <input type="hidden" ... />
        <@field ... />
        <@field ... />
      </@fields>
    </@form>            
                    
  * Parameters *
    type                = [input|display], default input
                          DEV NOTE: "display" is special for time being, probably rare or unused;
                                    maybe it should cause to omit <form> element
    class               = classes on form element itself 
    attribs             = hash of attributes for HTML <form> element (needed for names with dashes)
    inlineAttribs       = other attributes for HTML <form> element
                          NOTE: camelCase names are automatically converted to dash-separated-lowercase-names.
-->
<#assign form_defaultArgs = {
  "type":"input", "name":"", "id":"", "class":"", "open":true, "close":true, 
  "attribs":{}, "passArgs":{}
}>
<#macro form args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.form_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local attribs = makeAttribMapFromArgMap(args)>
  <#local origArgs = args>

  <#if open>
    <#local formInfo = {"type":type, "name":name, "id":id}>
    <#local dummy = pushRequestStack("catoFormInfoStack", formInfo)>
  </#if>

  <#if open && !close>
    <#local dummy = pushRequestStack("catoFormMarkupStack", {
      "type":type, "name":name, "id":id, "class":class, "attribs":attribs, "origArgs":origArgs, "passArgs":passArgs
    })>
  <#elseif close && !open>
    <#local stackValues = popRequestStack("catoFormMarkupStack")!{}>
    <#local dummy = localsPutAll(stackValues)>
  </#if>
  <@form_markup type=type name=name id=id class=class open=open close=close attribs=attribs origArgs=origArgs passArgs=passArgs><#nested></@form_markup>
  <#if close>
    <#local dummy = popRequestStack("catoFormInfoStack")>
  </#if>
</#macro>

<#-- @form main markup - theme override -->
<#macro form_markup type="" name="" id="" class="" open=true close=true attribs={} origArgs={} passArgs={} catchArgs...>
  <#if open>
    <form<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#rt>
      <#lt><#if name?has_content> name="${name}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs /></#if>>
  </#if>
      <#nested>
  <#if close>
    </form>
  </#if>
</#macro>

<#-- 
*************
* Progress Script
************
Generates script data and markup needed to make an instance to initialize upload progress 
javascript anim for a form, with progress bar and/or text.

Server-side upload event for the form must register a FileUploadProgressListener in session
for getFileUploadProgressStatus AJAX calls.
                    
  * Parameters *
    enabled         = boolean, default true (helper macro arg)
    progressOptions = elem IDs and options passed to CatoUploadProgress javascript class
                      in addition, supports: 
                        submitHook - one of: "formSubmit" (default), "validate" (jquery validate), "none" (caller does manually) 
                        validateObjScript - if submitHook is "validate", add this script text to jquery validate({...}) object body.
                      see CatoUploadProgress javascript class for available options.
    htmlwrap        = if true, wrap in @script (default true)
-->
<#assign progressScript_defaultArgs = {
  "enabled":true, "htmlwrap":true, "progressOptions":{}, "passArgs":{}
}>
<#macro progressScript args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.progressScript_defaultArgs)>
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
                  uploadProgress = new CatoUploadProgress(<@objectAsScript lang="js" object=progressOptions />);
                  uploadProgress.reset();
              });
              
            <#if (progressOptions.submitHook!) == "validate">
              jQuery("${progressOptions.formSel}").validate({
                  submitHandler: function(form) {
                      var goodToGo = uploadProgress.initUpload();
                      if (goodToGo) {
                          form.submit();
                      }
                  },
                  ${progressOptions.validateObjScript!""}
              });
            <#elseif (progressOptions.submitHook!) != "none" >
              jQuery("${progressOptions.formSel}").submit(function(event) {
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

  * Usage Example *  
    <@progress value=40/>             
    
   Can be animated using js, example: 
   
   $('#${id}_meter').css("width", "78%");
    
   Can also be animated automatically using progressOptions which activates use of CatoUploadProgress
   script for this progress bar by linking it to a form submit.
                    
  * Parameters *
    value          = Percentage done
    id             = custom id; can also be specified as progressOptions.progBarId instead
                     if omitted will not make a progress bar, but script still generated for progressOptions.progTextBoxId
    type           = (alert|info|success) default: info
    class          = Adds classes - please use "(small|medium|large)-block-grid-#"
                     supports prefixes:
                       "+": causes the classes to append only, never replace defaults (same logic as empty string "")
                       "=": causes the class to replace non-essential defaults (same as specifying a class name directly)
    containerClass = classes added only on container
    showValue      = Display value inside bar
    wrapClass      = classes on outer wrapper only
    progressArgs      = if present, attaches progress bar to an upload form with javascript-based progress and 
                        attaches results to page using elem IDs and options specified via these arguments,
                        which are passed to @progress macro (see @progress macro for supported options)
    progressOptions   = convenience parameter; same as passing:
                        progressArgs={"enabled":true, "progressOptions":progressOptions}
-->
<#assign progress_defaultArgs = {
  "value":0, "id":"", "type":"", "class":"", "showValue":false, "containerClass":"", "progressArgs":{}, 
  "progressOptions":{}, "passArgs":{}
}>
<#macro progress args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.progress_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>

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
      <#local opts = concatMaps(opts, {"progBarId":"${id}"})>
    </#if>
    <#-- inlines always override args map -->
    <@progressScript progressOptions=opts htmlwrap=true args=progressArgs passArgs=passArgs />
  </#if>
</#macro>

<#-- @progress main markup - theme override -->
<#macro progress_markup value=0 id="" class="" showValue=false containerClass="" stateClass="" origArgs={} passArgs={} catchArgs...>
  <#local classes = compileClassArg(class)>
  <#local containerClasses = compileClassArg(containerClass)>
  <div class="${styles.progress_container}<#if !styles.progress_wrap?has_content && classes?has_content> ${classes}</#if><#if stateClass?has_content> ${stateClass}</#if><#if containerClasses?has_content> ${containerClasses}</#if>"<#if id?has_content> id="${id}"</#if>>
    <#if styles.progress_wrap?has_content><div class="${styles.progress_wrap!}<#if classes?has_content> ${classes}</#if>"<#if id?has_content> id="${id!}_meter"</#if> role="progressbar" aria-valuenow="${value!}" aria-valuemin="0" aria-valuemax="100" style="width: ${value!}%"></#if>
      <span class="${styles.progress_bar!}"<#if !styles.progress_wrap?has_content> style="width: ${value!}%"<#if id?has_content> id="${id!}_meter"</#if></#if>><#if showValue>${value!}</#if></span>
    <#if styles.progress_wrap?has_content></div></#if>
  </div>
</#macro>

<#-- 
*************
* asmSelectScript
************
Generates script data and markup needed to turn a multiple-select form field into
dynamic jquery asmselect.
IMPL NOTE: this must support legacy ofbiz parameters.
                    
  * Parameters *
    * general *
    enabled             = boolean true/false, default true (helper for macro args)
    id                  = select elem id
    title               = select title
    sortable            = boolean
    formId              = form ID
    formName            = form name
    asmSelectOptions    = optional, a map of overriding options to pass to asmselect
    asmSelectDefaults   = boolean, default true, if false will not include any defaults and use asmSelectOptions only
    relatedFieldId      = related field ID (optional)
    htmlwrap            = if true, wrap in @script (default true)
    
    * needed only if relatedFieldId specified *
    relatedTypeName       = related type, name
    relatedTypeFieldId    = related type field ID
    paramKey              = param key 
    requestName           = request name
    responseName          = response name
-->
<#assign asmSelectScript_defaultArgs = {
  "enabled":true, "id":"", "title":false, "sortable":false, "formId":"", "formName":"", "asmSelectOptions":{}, 
  "asmSelectDefaults":true, "relatedFieldId":"", "relatedTypeName":"", "relatedTypeFieldId":"", "paramKey":"", 
  "requestName":"", "responseName":"", "htmlwrap":true, "passArgs":{}
}>
<#macro asmSelectScript args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.asmSelectScript_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#if enabled>
    <#-- MIGRATED FROM component://common/webcommon/includes/setMultipleSelectJs.ftl -->
    <#if id?has_content>
    <@script htmlwrap=htmlwrap>
    jQuery(document).ready(function() {
        multiple = jQuery("#${id!}");
    
      <#if !(title?is_boolean && title == false)>
        <#if title?is_boolean>
          <#local title = "">
        </#if>
        // set the dropdown "title" if??
        multiple.attr('title', '${title}');
      </#if>
      
        <#if asmSelectDefaults>
          <#-- Cato: get options from styles -->
          <#local defaultAsmSelectOpts = {
            "addItemTarget": 'top',
            "sortable": sortable!false,
            "removeLabel": uiLabelMap.CommonRemove
            <#--, debugMode: true-->
          }>
          <#local asmSelectOpts = defaultAsmSelectOpts + styles.field_select_asmselect!{} + asmSelectOptions>
        <#else>
          <#local asmSelectOpts = asmSelectOptions>
        </#if>
        // use asmSelect in Widget Forms
        multiple.asmSelect(<@objectAsScript lang="js" object=asmSelectOpts />);
          
      <#if relatedFieldId?has_content> <#-- can be used without related field -->
        // track possible relatedField changes
        // on initial focus (focus-field-name must be relatedFieldId) or if the field value changes, select related multi values. 
        typeValue = jQuery('#${relatedTypeFieldId}').val();
        jQuery("#${relatedFieldId}").one('focus', function() {
          selectMultipleRelatedValues('${requestName}', '${paramKey}', '${relatedFieldId}', '${id}', '${relatedTypeName}', typeValue, '${responseName}');
        });
        jQuery("#${relatedFieldId}").change(function() {
          selectMultipleRelatedValues('${requestName}', '${paramKey}', '${relatedFieldId}', '${id}', '${relatedTypeName}', typeValue, '${responseName}');
        });
        selectMultipleRelatedValues('${requestName}', '${paramKey}', '${relatedFieldId}', '${id}', '${relatedTypeName}', typeValue, '${responseName}');
      </#if>
      });  
    </@script>
    </#if>
  </#if>
</#macro>

<#-- 
*************
* Fieldset
************
A visible fieldset, including the HTML element.

  * Usage Example *  
    <@fieldset title="">
        Inner Content
    </@fieldset>            
                    
  * Parameters *
    class           = css classes 
                      supports prefixes:
                        "+": causes the classes to append only, never replace defaults (same logic as empty string "")
                        "=": causes the class to replace non-essential defaults (same as specifying a class name directly)
    containerClass  = class for wrapper 
                      (includes width in columns, or append only with "+")
    id              = set id
    title           = fieldset-title
    collapsed       = show/hide the fieldset
-->
<#assign fieldset_defaultArgs = {
  "id":"", "title":"", "class":"", "containerClass":"", "collapsed":false, "open":true, "close":true, "passArgs":{}
}>
<#macro fieldset args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.fieldset_defaultArgs)>
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
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.fieldset_core_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>

  <#if id?has_content>
    <#local containerId = "${id}_wrapper">
  <#else>
    <#local containerId = "">
  </#if>

  <#if open && !close>
    <#local dummy = pushRequestStack("catoFieldsetCoreMarkupStack", {
      "class":class, "containerClass":containerClass, "id":id, "containerId":containerId, "title":title, 
      "collapsed":collapsed, "collapsibleAreaId":collapsibleAreaId, "expandToolTip":expandToolTip, 
      "collapseToolTip":collapseToolTip, "collapsible":collapsible, 
      "origArgs":origArgs, "passArgs":passArgs
    })>
  <#elseif close && !open>
    <#local stackValues = popRequestStack("catoFieldsetCoreMarkupStack")!{}>
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
        <fieldset<#if classes?has_content> class="${classes!}"</#if><#if id?has_content> id="${id}"</#if>>
      <#--<#if collapsible>
        <ul>
          <li class="<#if collapsed>${styles.collapsed!}">
                      <a onclick="javascript:toggleCollapsiblePanel(this, '${collapsibleAreaId}', '${expandToolTip}', '${collapseToolTip}');">
                    <#else>expanded">
                      <a onclick="javascript:toggleCollapsiblePanel(this, '${collapsibleAreaId}', '${expandToolTip}', '${collapseToolTip}');">
                    </#if>
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<#if title?has_content>${title}</#if></a>
          </li>
        </ul>
      <#else>
        <#if title?has_content>${title}</#if>
      </#if><#rt/>
    </div>
    <div id="${collapsibleAreaId}" class="fieldgroup-body" <#if collapsed && collapsible> style="display: none;"</#if>>
    -->
          <#if title?has_content><legend><#if collapsible || collapsed>[ <i class="${styles.icon!} ${styles.icon_arrow!}"></i> ] </#if>${title}</legend></#if>
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

  * Usage Example * 
    <@fields>
      <@field attr="" />
      <@field attr="" />
    </@field>
     
    <@fields type="default-nolabels">
      <@field attr="" />
      <@field attr="" />
    </@field>
    
    <@fields type="generic" labelArea=false>
      <@field attr="" />
      <@field attr="" />
    </@field>
    
  * Parameters *
    type            = [default|default-nolabels|default-compact|default-manual|generic], default default. the type of fields arrangement. affects layout and styling of contained fields.
                      default: default cato field arrangement. this is the type assumed when no @fields element is present.
                          currently, it mostly influences the label area (present for all @field types except submit).
                      default-nolabels: default cato field arrangement for common sets of fields with no labels.
                          it expects that @field entries won't be passed any labels.
                      default-compact: default cato field arrangement for fields that are in limited space.
                          by default, this means the labels will be arranged vertically with the fields.
                      default-manual: manual field arrangement. means field arrangement is custom and field macro and theme should not impose
                          any layout, but may still apply minor low-level default styling choices and non-optional layout fallbacks. caller determines arrangement/layout/label type/etc.
                      generic: generic field arrangement of no specific pattern and no specific styling. means field arrangement is custom and field macro and theme should not
                          make any assumptions except where a default is required. caller determines arrangement/layout/label type/etc.
                      NOTE: For default-manual, generic and similar where styles hash does not specify a label area by default, 
                          to show a label area for a field, it is NOT sufficient to specify label="xxx".
                          You must specify both labelArea=true and label="xxx". label arg does not influence presence of label area.
                          This is explicitly intended, as the label arg is general-purpose in nature and is not associated only with the label area (and anything else will break logic);
                          generally, @field specifies label as pure data and theme decides where and how to display it.
                          In the majority of cases, this should rarely be used anyway; use another more appropriate @fields type instead.
    labelType       = [horizontal|vertical|none], defaults specified in styles variables based on fields type. override for type of the field labels themselves.
                      horizontal: a label area added to the left (or potentially to the right) a field, horizontally. 
                          the implementation decides how to do this.
                          DEV NOTE: previously this was called "gridarea". but in the bootstrap code, this no longer makes sense.
                              It would be perfectly valid for us to add an extra type here called "gridarea" that specifically requires
                              a grid (TODO?). "horizontal" simply delegates the choice to the implementation.
                      vertical: a label area added before (or potentially after) a field, vertically. 
                          the implementation decides how to do this.
                      none: no labels or label areas. expects the @field macro won't be passed any.
                      TODO: we should have types here that specifically request that either "gridarea" or "inline" are used for markup:
                          gridarea-horizontal, gridarea-vertical, inline-horizontal, inline-vertical
                          The current implementation is unspecific.
    labelPosition   = [left|right|top|bottom|none], defaults specified in styles variables based on fields type. override for layout/positioning of the labels.
                      some values only make sense for some arrangements.
    labelArea       = boolean, defaults specified in styles variables based on fields type. overrides whether fields are expected to have a label area or not, mainly when label omitted. 
                      logic is influenced by other arguments.
                      NOTE: This does not determine label area type (horizontal, etc.); only labelType does that (in current code).
                          They are decoupled. This only controls presence of it.
                      NOTE: This is weaker than labelArea arg of @field macro, but stronger than other args of this macro.
    labelAreaExceptions = string of space-delimited @field type names or list of names, defaults specified in styles variables based on fields type  
                          NOTE: radio and checkbox support special names: radio-single, radio-multi, checkbox-single, checkbox-multi
    labelAreaRequireContent = boolean, if true, the label area will only be included if label or labelDetail have content.
                              this is generally independent of labelArea boolean and other settings. 
                              NOTE: This will not affect
                              the fallback logic of labels to inline labels (a.k.a. whether the label area "consumes" the label for itself),
                              otherwise that would mean labels would always be forced into the label area and never inline.
    labelAreaConsumeExceptions = string of space-delimited @field type names or list of names, defaults specified in styles variables based on fields type  
                                 list of field types that should never have their label appear in the main label area.
                                 for these, the label will trickle down into the field's inline area, if it has any (otherwise no label).
                                 NOTE: radio and checkbox support special names: radio-single, radio-multi, checkbox-single, checkbox-multi
    formName            = the form name the child fields should assume  
    formId              = the form ID the child fields should assume   
    inlineItems         = change default for @field inlineItems parameter (true/false)    
    checkboxType        = default checkbox type
    radioType           = default radio type  
    open/close          = advanced structure logic
    ignoreParentField   = default false. If true, causes all fields within to ignore their parent and behave as if no parent.
    fieldArgs           = A map of @field parameters that will be used as new defaults for each field call.
                          This is an automated mechanism. The map will be blended over the standard @field defaults before the invocation.
                          In addition, contrary to the parameters, a map passed directly to @fields will be blended over both the @field defaults
                          AND over any defaults set in the styles for the given @fields type: 
                            {@field regular defaults} + {fieldargs from styles hash} + {@fields fieldArgs direct arg}
                          NOTES:
                            * This may overlap with some of the existing parameters above. Covers other cases not made explicit above.
                            * If set to boolean false, will prevent all custom default field args and prevent using those set in styles hash. Probably never needed.
                          EXAMPLE:
                            <@fields type="default" fieldArgs={"labelArea":false}>
-->
<#assign fields_defaultArgs = {
  "type":"default", "open":true, "close":true, "labelType":"", "labelPosition":"", "labelArea":"", "labelAreaExceptions":true, "labelAreaRequireContent":"", "labelAreaConsumeExceptions":true,
  "formName":"", "formId":"", "inlineItems":"", "collapse":"", "collapsePostfix":"", "collapsedInlineLabel":"", "checkboxType":"", "radioType":"", "ignoreParentField":"", 
  "fieldArgs":true, "passArgs":{}
}>
<#macro fields args={} inlineArgs...>
  <#-- NOTE: this is non-standard args usage -->
  <#local fieldsInfo = makeFieldsInfo(mergeArgMapsBasic(args, inlineArgs))>
  <#if (fieldsInfo.open!true) == true>
    <#local dummy = pushRequestStack("catoFieldsInfoStack", fieldsInfo)>
  </#if>
    <#nested>
  <#if (fieldsInfo.close!true) == true>
    <#local dummy = popRequestStack("catoFieldsInfoStack")>
  </#if>
</#macro>

<#function makeFieldsInfo args={}>
  <#local args = mergeArgMapsBasic(args, {}, catoStdTmplLib.fields_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  
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

  <#local fieldArgsFromStyles = styles[stylesPrefix + "_fieldargs"]!styles["fields_default_fieldargs"]!false>
  <#if fieldArgs?is_boolean>
    <#if fieldArgs == true>
      <#local fieldArgs = fieldArgsFromStyles>
    </#if>
  <#else>
    <#local fieldArgs = toSimpleMap(fieldArgs)>
    <#if !fieldArgsFromStyles?is_boolean>
      <#local fieldArgs = fieldArgsFromStyles + fieldArgs>
    </#if>
  </#if>

  <#return {"type":type, "stylesType":stylesType, "stylesPrefix":stylesPrefix, "labelType":labelType, "labelPosition":labelPosition, 
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
* mapCatoFieldTypeToStyleName
************ 
Maps a cato field type to a style name representing the type.

Should be coordinated with mapOfbizFieldTypeToStyleName to produce common field type style names.
-->
<#function mapCatoFieldTypeToStyleName fieldType>
  <#local res = (styles.field_type_stylenames_cato[fieldType])!(styles.field_type_stylenames_cato["default"])!"">
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

Should be coordinated with mapCatoFieldTypeToStyleName to produce common field type style names.
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
* mapOfbizFieldTypeToCatoFieldType
************ 
Maps an Ofbiz field type to a Cato field type.
-->
<#function mapOfbizFieldTypeToCatoFieldType fieldType>
  <#if !ofbizFieldTypeToCatoFieldTypeMap??>
    <#global ofbizFieldTypeToCatoFieldTypeMap = {
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
  <#return ofbizFieldTypeToCatoFieldTypeMap[fieldType]!ofbizFieldTypeToCatoFieldTypeMap["default"]!"">
</#function>

<#-- 
*************
* Field
************ 
A form field input widget with optional label and post-input (postfix) content.

@field can be used as a low-level field control (similar to original Ofbiz
form widget macros, but with friendlier parameters) and for high-level declarations
of fields (similar to the actual <field> elements in Ofbiz form widget definitions, but friendlier
and more configurable). This versaility is the main reason for its implementation complexity.

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

This system can accodomate custom @fields types, but a default set are provided in the cato
standard markup.

NOTE: All @field arg defaults can be overridden by the @fields fieldArgs argument.

  * Usage Example *  
    <@field attr="" /> <#- single field using default look ->
    
    <@fields type="default"> <#- single field using default look, same as previous ->
      <@field attr="" />
    </@fields>

    <@fields type="default-nolabels"> <#- specific arrangement needed ->
      <@field attr="" />
    </@fields>
    
    <@fields type="default-manual"> <#- use @field as low-level control ->
      <@field attr="" labelArea=true label="My Label" />
    </@fields>    
    
  * Parameters *
    * General *
    type            = form element type - supported values and their parameters listed below, between asterix.
                      default "generic", which means input defined manually with #nested.
                      generic is mostly for grouping multiple sub-fields, but can be used anywhere.
                      (specific field types should be preferred to manually defining content, where possible)
    label           = field label
                      for top-level @field elements and and parent fields, normally the label will get consumed
                      by the label area and shown there. for child fields and some other circumstances, or whenever there is
                      no label area, the label will instead be passed down as an "inline label" to the input
                      widget implementation. in some cases, this "inline label" is
                      re-implemented using the label area - see collapsedInlineLabel parameter.
                      NOTE: Presence of label arg does not guarantee a label area will be shown; this is controlled
                          by labelArea (and labelType) and its defaults, optionally coming from @fields container.
                          label arg is mainly to provide data; theme and other flags decide what to do with it.
                          For generic parent fields, label type must be specified explicitly, e.g.
                            <@fields type="generic"><@field labelType="horizontal" label="mylabel">...</@fields> 
                      NOTE: label area behavior may also be influenced by containing macros such as @fields
    labelDetail     = extra content (HTML) inserted with (after) label
    labelType       = explicit label type (see @fields)
    labelPosition   = explicit label layout (see @fields)
    labelArea       = boolean, default empty string (use @fields type default).
                      if true, forces a label area.
                      if false, prevents a label area.
                      NOTE: This does not determine label area type (horizontal, etc.); only labelType does that (in current code).
                          They are decoupled. This only controls presence of it.
    labelAreaRequireContent = boolean, if true, the label area will only be included if label or labelDetail have content.
                              by default, this is empty string (use @fields type default), and if no styles defaults,
                              default is false.
    labelAreaConsume        = boolean, default true. if set to false, will prevent the label area from consuming (displaying) the label and the
                              label will trickle down into an inline area if one exists for the field type.
    inlineLabelArea     = manual override for inline label logic. in general can be left to macro (and @fields types stylable via global styles hash). logical default: false (or "").
    inlineLabel         = manual override for inline label logic. in general can be left to macro (and @fields types stylable via global styles hash). logical default: "" (false on interface).
                          NOTE: often if you specify this it means you might want to set inlineLabelArea=true as well.
    tooltip         = Small field description - to be displayed to the customer
                      May be set to boolean false to manually prevent tooltip defaults.
    description     = Field description.
                      NOTE: currently this is treated as an alternative arg for tooltip
                      TODO?: DEV NOTE: this should probably be separate from tooltip in the end...
    name            = field name
    value           = field value
    columns         = number of grid columns to use as size for widget + postfix area (combined)
                      DEV NOTE: this value now includes the postfix area because this usually easier
                          to use this way and because the widget + postfix configuration is variable
    class           = css classes for the field element (NOT the cell container!)
                      supports prefixes:
                        "+": causes the classes to append only, never replace defaults (same logic as empty string "")
                        "=": causes the class to replace non-essential defaults (same as specifying a class name directly)
    containerClass  = optional class for outer container (prefix with +)
    maxlength       = max allowed length (e.g. max number of characters for text inputs)
    id              = field id
    containerId     = id for the outer container (optional)
    events          = map of JS event names to script actions. 
                      event names can be specified with or without the "on" prefix ("click" or "onclick").
    onClick         = shortcut for: events={"click": onClick}
                      WARN: Beware of character case. It's onClick, not onclick!
    onChange        = shortcut for: events={"change": onChange}
    onFocus         = shortcut for: events={"focus": onChange}
    disabled        = field disabled
    placeholder     = field placeholder
    alert           = adds additional css alert class
    mask            = toggles jQuery mask plugin
    size            = size attribute (default: 20)
    collapse        = should the whole field (including label and postfix) be collapsing? (default: false)
    collapsePostfix = should the postfix collapse with the field input? (default: true)
                      this will not affect label unless collapse is also true (in which case this setting is ignored
                      and the whole field is collapse)
    collapsedInlineLabel  = this is a special function that will only apply in some cases. 
                            if this is set to true and the label does not get consumed
                            by the label area and becomes an inline label, this will cause an auto-implementation
                            of an inlined label using collapsing (instead of passing the inline label
                            down to the individual field type widget).
                            this may be needed for some field types.
    norows          = render without the rows-container
    nocells         = render without the cells-container
    container       = defaul true. If false, sets norows=true and nocells=true.
    inline          = default false. If true, forces container=false and marks the field with styles.field_inline, or
                      in other words, turns it into a logically inline element (traditionally, CSS "display: inline;").
                      Theme should act on this style to prevent taking up all the width.
    ignoreParentField    = default false. If true causes a child field to act as if it had no parent field. Rarely needed.
    required        = boolean, default false. Marks a required input.
    requiredClass   = default required class name. Does not support extended class +/= syntax.
    requiredTooltip = tooltip to use when field is required. this is overridden by regular tooltip.
                      for this, can prefix with "#LABEL:" string which indicates to take the named label from uiLabelMap.
    postfix         = boolean true/false, controls whether an extra area is appended after widget area
    postfixSize     = manual postfix size, in (large) grid columns
    postfixContent  = manual postfix markup/content - set to boolean false to prevent any content (but not area container)
        
    * input (alias: text) *
    autoCompleteUrl = if autocomplete function exists, specification of url will make it available
    postfix         = if set to true, attach submit button (default:false)
    
    * textArea *
    readonly        = readonly
    rows            = number of rows
    cols            = number of columns
    wrap            = HTML5 wrap attribute
    
    * dateTime *
    dateType        = [date-time|date|time] (default: date-time) type of datetime
                      NOTE: date-time is equivalent to "timestamp" in form widgets
    title           = shows requested title.
                      If empty, markup/theme decides what to show.
                      Can also be a special value in format "#PROP:resource#propname". If no resource, taken from CommonUiLabels.
                      NOTE: Tooltip has priority over title.
    
    * select *
    multiple        = allow multiple select true/false
    items           = if specified, generates options from list of maps; 
                      list of {"value": (value), "description": (label), "selected": (true/false)} maps
                      NOTE: selected is currently ignored for non-multiple (uses currentValue instead)
                      if items list not specified, manual #nested content options can be specified instead.
    allowEmpty      = default false; if true, will add an empty option
    currentValue    = currently selected value/key (only for non-multiple)
    currentFirst    = default false; if true (and multiple false), will add a "first" item with current value selected, if there is one
    currentDescription    = if currentFirst true, this is used as first's description if specified
    defaultValue          = optional selected option value for when none otherwise selected
    manualItemsOnly = optional boolean hint caller may specify to say that this select should
                      contain exclusively manually generated items. 
                      by default, this is based on whether the items arg is specified or not.
    manualItems     = optional boolean hint caller may sometimes need to give macro to say that #nested contains
                      manual options, but not exclusively. 
                      by default, this is based on whether the items arg is specified or not (NOT whether
                      there is any nested content or not).
                      if specifying both items arg AND #nested content (discouraged), this should be manually set to true.
    asmSelectArgs   = optional map of args passed to @asmSelectScript to transform a multiple type select into
                      a jquery asmselect. usually only valid if multiple is true.
    formName        = name of form containing the field
    formId          = id of form containing the field
    title           = title attribute of <select> element
    
    * option *
    text            = option label (can also specify as #nested)
    value           = value (sent to server)
    selected        = boolean
    
    * lookup *
    formName        = The name of the form that contains the lookup field.
    fieldFormName   = Contains the lookup window form name.
    
    * Checkbox (single mode) *
    value           = Y/N
    currentValue    = current value, used to check if should be checked
    checked         = override checked state (true/false/"") - if set to boolean, overrides currentValue logic
    checkboxType    = [default|...], default default
                      Generic:
                        default: default theme checkbox
                      Cato standard theme:
                        simple: guarantees a minimalistic checkbox
    
    * Checkbox (multi mode) *
    items           = if specified, multiple-items checkbox field generated; 
                      list of {"value": (value), "description": (label), "tooltip": (tooltip), "events": (js event map), "checked": (true/false)} maps
                      NOTE: use of "checked" attrib is discouraged; is a manual override (both true and false override); prefer setting currentValue on macro
                      DEV NOTE: the names in this map cannot be changed easily; legacy ofbiz macro support
    inlineItems     = if true (default), radio items are many per line; if false, one per line
                      note this takes effect whether single-item or multiple-item radio.
                      the default can be overridden on a parent @field or @fields element.
    currentValue    = current value, determines checked; this can be single-value string or sequence of value strings
    defaultValue    = default value, determines checked (convenience option; used when currentValue empty; can also be sequence)
    allChecked      = true/false/"", convenience option (explicit false sets all to unchecked; leave empty "" for no setting)
    
    * radio (single mode) *
    value           = Y/N, only used if single radio item mode (items not specified)
    currentValue    = current value, used to check if should be checked
    checked         = override checked state (true/false/"") - if set to boolean, overrides currentValue logic
    radioType       = [default], default default
                      Generic:
                        default: default theme radio
    
    * radio (multi mode) *
    items           = if specified, multiple-items radio generated; 
                      list of {"value": (value), "description": (label), "tooltip": (tooltip), "events": (js event map), "checked": (true/false)} maps
                      NOTE: use of "checked" attrib is discouraged; is a manual override (both true and false override); prefer setting currentValue on macro
                      DEV NOTE: the names in this map cannot be changed easily; legacy ofbiz macro support
    inlineItems     = if true (default), radio items are many per line; if false, one per line
                      note this takes effect whether single-item or multiple-item radio.
                      the default can be overridden on a parent @field or @fields element.
    currentValue    = current value, determines checked
    defaultValue    = default value, determines checked (convenience option; used when currentValue empty)
    
    * file *
    autocomplete    = true/false, default true (false to prevent)
    
    * password *
    autocomplete    = true/false, default true (false to prevent)
    
    * submitarea *
    <#nested>       = button(s) (<@field type="submit"> or manual <input>, <a>, <button>) to include
    progressArgs    = if this is an upload form, arguments to pass to @progress macro. 
                      see @progress[Script] macro[s]. should specify formSel, (progBarId and/or progTextBoxId), and others.
    progressOptions = convenience parameter; same as passing:
                      progressArgs={"enabled":true, "progressOptions":progressOptions}      
                      
    * submit *
    submitType      = [submit|link|button|image|input-button], default submit (<input type="submit"/>)  
                      submit: <input type="submit" ... />
                      input-button: <input type="button" ... />
                      link: <a href="..." ...>...</a>
                          NOTE: href should usually be specified for this, or explicitly set to boolean false if using onClick. 
                              If not specified, generated href will cause form submit with form name (if found and not disabled).
                      button: WARN/FIXME?: currently this is same as input-button: <input type="button" ... />
                          This could change to <button...>...</button> without notice...
                      image: <input type="image" src="..." .../>
    text            = display text (also value for submitType "submit")                
    href            = href for submitType "link"  
    src             = image url for submitType "image"    
    confirmMsg      = confirmation message     
    progressArgs    = same as for submitarea, but only works if this is a top-level submit     
    progressOptions = same as for submitarea, but only works if this is a top-level submit
                      
    * reset *
    text            = label to show on reset button
                      
    * display *
    valueType       = [image|text|currency|date|date-time|accounting-number|generic], default generic (treated as text)
                      TODO: currently all are handled as text/generic (because formatting done in java in stock ofbiz)
    value           = display value or image URL
    description     = for image type: image alt
-->
<#assign field_defaultArgs = {
  <#-- TODO: group these arguments so easier to read... -->
  "type":"", "label":"", "labelDetail":"", "name":"", "value":"", "valueType":"", "currentValue":"", "defaultValue":"", "class":"", "size":20, "maxlength":"", "id":"", 
  "onClick":"", "onChange":"", "onFocus":"",
  "disabled":false, "placeholder":"", "autoCompleteUrl":"", "mask":false, "alert":"false", "readonly":false, "rows":"4", 
  "cols":"50", "dateType":"date-time", "multiple":"", "checked":"", 
  "collapse":"", "collapsePostfix":"", "collapsedInlineLabel":"",
  "tooltip":"", "columns":"", "norows":false, "nocells":false, "container":"", "containerId":"", "containerClass":"",
  "fieldFormName":"", "formName":"", "formId":"", "postfix":false, "postfixSize":1, "postfixContent":true, "required":false, "requiredClass":"", "requiredTooltip":true, "items":false, "autocomplete":true, "progressArgs":{}, "progressOptions":{}, 
  "labelType":"", "labelPosition":"", "labelArea":"", "labelAreaRequireContent":"", "labelAreaConsume":"", "inlineLabelArea":"", "inlineLabel":false,
  "description":"",
  "submitType":"input", "text":"", "href":"", "src":"", "confirmMsg":"", "inlineItems":"", 
  "selected":false, "allowEmpty":false, "currentFirst":false, "currentDescription":"",
  "manualItems":"", "manualItemsOnly":"", "asmSelectArgs":{}, "title":"", "allChecked":"", "checkboxType":"", "radioType":"", 
  "inline":"", "ignoreParentField":"",
  "events":{}, "wrap":"", "passArgs":{} 
}>
<#macro field args={} inlineArgs...> 

  <#-- parent @fields group elem info (if any; may be omitted) -->
  <#local fieldsInfo = readRequestStack("catoFieldsInfoStack")!{}>
  <#if !fieldsInfo.type??>
    <#if !catoDefaultFieldsInfo?has_content>
      <#-- optimization -->
      <#global catoDefaultFieldsInfo = makeFieldsInfo({"type":"default"})>
    </#if>
    <#local fieldsInfo = catoDefaultFieldsInfo>
  </#if>

  <#-- special default fields override -->
  <#local defaultArgs = catoStdTmplLib.field_defaultArgs>
  <#if !fieldsInfo.fieldArgs?is_boolean>
    <#local defaultArgs = defaultArgs + fieldsInfo.fieldArgs>
  </#if>

  <#-- standard args -->
  <#local args = mergeArgMaps(args, inlineArgs, defaultArgs)>
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

  <#if inline?is_boolean && inline == true>
    <#local container = false>
    <#local class = addClassArg(class, styles.field_inline!)>
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

  <#if onClick?has_content>
    <#local events = events + {"click": onClick}>
  </#if>
  <#if onChange?has_content>
    <#local events = events + {"change": onChange}>
  </#if>
  <#if onFocus?has_content>
    <#local events = events + {"focus": onFocus}>
  </#if>
  
  <#-- parent @field elem info (if any; is possible) -->
  <#local parentFieldInfo = readRequestStack("catoFieldInfoStack")!{}>
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
  
  <#local formInfo = readRequestStack("catoFormInfoStack")!{}>
  
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

  <#-- fieldIdNum will always increment throughout the page 
       now stored in request attributes so survived screens.render though still accessible as a global -->
  <#local fieldIdNum = getRequestVar("catoFieldIdNum")!0>
  <#local fieldIdNum = fieldIdNum + 1 />
  <#local dummy = setRequestVar("catoFieldIdNum", fieldIdNum)>
  
  <#if !id?has_content>
    <#-- FIXME? renderSeqNumber usually empty... where come from? should be as request attribute also? -->
    <#local id = "field_id_${renderSeqNumber!}_${fieldIdNum!0}">
  </#if>
  
  <#if required && (!requiredClass?is_boolean && requiredClass?has_content)>
    <#local class = addClassArg(class, requiredClass)>
  </#if>
  <#if required && ((tooltip?is_boolean && tooltip == true) || (!tooltip?is_boolean && !tooltip?has_content)) && !(requiredTooltip?is_boolean && requiredTooltip == false)>
    <#if !requiredTooltip?is_boolean && requiredTooltip?has_content>
      <#local tooltip = getTextLabelFromExpr(requiredTooltip)>
    </#if>
  </#if>
  <#-- the widgets do this now
  <#local class = compileClassArg(class)>-->
    
  <#if !container?is_boolean>
    <#if container?has_content>
      <#local container = container?boolean>
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
      (!labelAreaRequireContent || (label?has_content || labelDetail?has_content)) && (labelAreaDefault))>
  
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
  
  <#-- TODO? ensure boolean string because next calls don't yet support it as boolean -->
  <#if tooltip?is_boolean>
    <#local tooltip = "">
  </#if>
  <#if description?is_boolean>
    <#local description = "">
  </#if>
  
  <#-- push this field's info (popped at end) -->
  <#local dummy = pushRequestStack("catoFieldInfoStack", 
    {"type":type, "inlineItems":inlineItems})>
  
  <#-- main markup begin -->
  <#local labelAreaContent = "">
  <#if useLabelArea>
    <#-- NOTE: origArgs is passed because in some cases it may be important for markup to know if the caller manually
        specified a certain parameter to @field or not - the other logical args don't record this info -->
    <#local labelAreaContent><@field_markup_labelarea labelType=effLabelType labelPosition=effLabelPosition label=label labelDetail=labelDetail 
        fieldType=type fieldId=id collapse=collapse required=required origArgs=origArgs passArgs=passArgs/></#local>
  </#if>
      
  <@field_markup_container type=type columns=columns postfix=postfix postfixSize=postfixSize 
    postfixContent=postfixContent labelArea=useLabelArea labelType=effLabelType labelPosition=effLabelPosition labelAreaContent=labelAreaContent 
    collapse=collapse collapsePostfix=collapsePostfix norows=norows nocells=nocells container=container containerId=containerId containerClass=containerClass origArgs=origArgs passArgs=passArgs>
    <#switch type>
      <#case "input">
        <@field_input_widget name=name 
                              class=class 
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
                              passArgs=passArgs/>
        <#break>
      <#case "textarea">
        <@field_textarea_widget name=name 
                              class=class 
                              alert=alert 
                              cols=cols 
                              rows=rows 
                              id=id 
                              readonly=readonly 
                              value=value 
                              placeholder=placeholder
                              tooltip=tooltip
                              inlineLabel=effInlineLabel
                              wrap=wrap
                              passArgs=passArgs><#nested></@field_textarea_widget>
        <#break>
      <#case "datetime">
        <#if dateType == "date">
          <#local shortDateInput=true/>
        <#elseif dateType == "time">
          <#local shortDateInput=false/>
        <#else> <#-- "date-time" -->
          <#local dateType = "timestamp">
          <#local shortDateInput=false/>
        </#if>
        <@field_datetime_widget name=name 
                              class=class 
                              alert=alert 
                              title=title 
                              value=value 
                              size=size 
                              maxlength=maxlength 
                              id=id 
                              dateType=dateType 
                              shortDateInput=shortDateInput 
                              timeDropdownParamName="" 
                              defaultDateTimeString="" 
                              localizedIconTitle="" 
                              timeDropdown="" 
                              timeHourName="" 
                              classString="" 
                              hour1="" 
                              hour2="" 
                              timeMinutesName="" 
                              minutes="" 
                              isTwelveHour="" 
                              ampmName="" 
                              amSelected="" 
                              pmSelected="" 
                              compositeType="" 
                              formName=""
                              tooltip=tooltip
                              origLabel=origLabel
                              inlineLabel=effInlineLabel
                              passArgs=passArgs/>                
        <#break>
      <#case "select">
        <#if !manualItemsOnly?is_boolean>
          <#local manualItemsOnly = !items?is_sequence>
        </#if>
        <#if !manualItems?is_boolean>
          <#-- FIXME? this should be based on whether #nested has content, but don't want to invoke #nested twice -->
          <#local manualItems = !items?is_sequence>
        </#if>
        <@field_select_widget name=name
                                class=class 
                                alert=alert 
                                id=id 
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
                                asmSelectArgs=asmSelectArgs
                                inlineLabel=effInlineLabel
                                passArgs=passArgs><#nested></@field_select_widget>
        <#break>
      <#case "option">
        <@field_option_widget value=value text=text selected=selected passArgs=passArgs><#nested></@field_option_widget>
        <#break>
      <#case "lookup">
        <@field_lookup_widget name=name formName=formName fieldFormName=fieldFormName class=class alert="false" value=value 
          size=size?string maxlength=maxlength id=id events=events passArgs=passArgs/>
      <#break>
      <#case "checkbox">
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
          <#local items=[{"value":value, "description":description, "tooltip":tooltip, "events":events, "checked":checked}]/>
          <@field_checkbox_widget multiMode=false items=items inlineItems=inlineItems id=id class=class alert=alert 
            currentValue=currentValue defaultValue=defaultValue allChecked=allChecked name=name tooltip="" inlineLabel=effInlineLabel type=checkboxType passArgs=passArgs/>
        <#else>
          <@field_checkbox_widget multiMode=true items=items inlineItems=inlineItems id=id class=class alert=alert 
            currentValue=currentValue defaultValue=defaultValue allChecked=allChecked name=name events=events tooltip=tooltip inlineLabel=effInlineLabel type=checkboxType passArgs=passArgs/>
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
          <#local items=[{"key":value, "description":description, "tooltip":tooltip, "events":events, "checked":checked}]/>
          <@field_radio_widget multiMode=false items=items inlineItems=inlineItems id=id class=class alert=alert 
            currentValue=currentValue defaultValue=defaultValue name=name tooltip="" inlineLabel=effInlineLabel type=radioType passArgs=passArgs/>
        <#else>
          <#-- multi radio button item mode -->
          <@field_radio_widget multiMode=true items=items inlineItems=inlineItems id=id class=class alert=alert 
            currentValue=currentValue defaultValue=defaultValue name=name events=events tooltip=tooltip inlineLabel=effInlineLabel type=radioType passArgs=passArgs/>
        </#if>
        <#break>
      <#case "file">
        <@field_file_widget class=class alert=alert name=name value=value size=size maxlength=maxlength 
          autocomplete=autocomplete?string("", "off") id=id inlineLabel=effInlineLabel passArgs=passArgs/>
        <#break>
      <#case "password">
        <@field_password_widget class=class alert=alert name=name value=value size=size maxlength=maxlength 
          id=id autocomplete=autocomplete?string("", "off") placeholder=placeholder tooltip=tooltip inlineLabel=effInlineLabel passArgs=passArgs/>
        <#break> 
      <#case "reset">                    
        <@field_reset_widget class=class alert=alert name=name text=text fieldTitleBlank=false inlineLabel=effInlineLabel passArgs=passArgs/>
        <#break>    
      <#case "submit">
        <#if !catoSubmitFieldTypeButtonMap??>
          <#-- NOTE: currently button is same as input-button, maybe should be different? -->
          <#-- the logical button types (based on form widget types) -->
          <#global catoSubmitFieldButtonTypeMap = {
            "submit":"button", "button":"button", "link":"text-link", "image":"image", "input-button":"button"
          }>
          <#-- the low-level input type attrib, within the logical button types -->
          <#global catoSubmitFieldInputTypeMap = {
            "submit":"submit", "button":"button", "link":"", "image":"image", "input-button":"button"
          }>
        </#if>      
        <#local buttonType = catoSubmitFieldButtonTypeMap[submitType]!"button">
        <#local inputType = catoSubmitFieldInputTypeMap[submitType]!"submit">
        <#-- support legacy "value" for text as conversion help -->
        <#if inputType == "submit" && !text?has_content && value?has_content>
          <#local text = value>
        </#if>
        <@field_submit_widget buttonType=buttonType class=class alert=alert formName=formName name=name events=events 
          imgSrc=src confirmation=confirmMsg containerId="" ajaxUrl="" text=text description=description showProgress=false 
          href=href inputType=inputType disabled=disabled progressArgs=progressArgs progressOptions=progressOptions inlineLabel=effInlineLabel passArgs=passArgs/>
        <#break>
      <#case "submitarea">
        <@field_submitarea_widget progressArgs=progressArgs progressOptions=progressOptions inlineLabel=effInlineLabel passArgs=passArgs><#nested></@field_submitarea_widget>
        <#break>
      <#case "hidden">                    
        <@field_hidden_widget name=name value=value id=id events=events inlineLabel=effInlineLabel passArgs=passArgs/>
        <#break>        
      <#case "display">
        <#-- TODO? may need formatting here based on valueType... not done by field_display_widget... done in java OOTB... 
            can also partially detect type of value with ?is_, but is not enough... -->
        <#if !valueType?has_content || (valueType=="generic")>
          <#local displayType = "text">
        <#else>
          <#local displayType = valueType>
        </#if>
        <#if !value?has_content>
          <#local value><#nested></#local>
        </#if>
        <#if displayType == "image">
          <#local imageLocation = value>
          <#local desc = "">
        <#else>
          <#local imageLocation = "">
          <#local desc = value>
        </#if>
        <@field_display_widget type=displayType imageLocation=imageLocation idName="" description=desc 
          title="" class=class alert=alert inPlaceEditorUrl="" inPlaceEditorParams="" 
          imageAlt=description tooltip=tooltip inlineLabel=effInlineLabel passArgs=passArgs/>
        <#break> 
      <#default> <#-- "generic", empty or unrecognized -->
        <#if value?has_content>
          <@field_generic_widget text=value tooltip=tooltip inlineLabel=effInlineLabel passArgs=passArgs/>
        <#else>
          <@field_generic_widget tooltip=tooltip inlineLabel=effInlineLabel passArgs=passArgs><#nested /></@field_generic_widget>
        </#if>
    </#switch>
  </@field_markup_container>
  <#-- pop field info when done -->
  <#local dummy = popRequestStack("catoFieldInfoStack")>
</#macro>

<#-- @field container markup - theme override 
    labelContent is generated by field_markup_labelarea.
    #nested is the actual field widget (<input>, <select>, etc.). 
    WARN: origArgs may be empty -->
<#macro field_markup_container type="" class="" columns="" postfix=false postfixSize=0 postfixContent=true labelArea=true labelType="" labelPosition="" labelAreaContent="" collapse="" collapseLabel="" collapsePostfix="" norows=false nocells=false container=true containerId="" containerClass="" origArgs={} passArgs={} catchArgs...>
  <#local rowClass = containerClass>
  <#local labelAreaClass = "">  
  <#local postfixClass = "">

  <#local labelInRow = (labelType != "vertical")>
  
  <#-- we may have collapse==false but collapsePostfix==true, in which case
      we may want to collapse the postfix without collapsing the entire thing 
      handle this by making a combined sub-row if needed -->
  <#local widgetPostfixCombined = ((postfix && collapsePostfix) && !collapse)>

  <#-- this is separated because some templates need access to the grid sizes to align things, and they
      can't be calculated statically in the styles hash -->
  <#local defaultGridStyles = getDefaultFieldGridStyles({"columns":columns, "labelArea":labelArea, 
    "labelInRow":labelInRow, "postfix":postfix, "postfixSize":postfixSize, "widgetPostfixCombined":widgetPostfixCombined })>

  <#local fieldEntryTypeClass = "field-entry-type-" + mapCatoFieldTypeToStyleName(type)>
  <#local labelAreaClass = addClassArg(labelAreaClass, "field-entry-title " + fieldEntryTypeClass)>
  <#local class = addClassArg(class, "field-entry-widget " + fieldEntryTypeClass)>
  <#local postfixClass = addClassArg(postfixClass, "field-entry-postfix " + fieldEntryTypeClass)>

  <#local rowClass = addClassArg(rowClass, "form-field-entry " + fieldEntryTypeClass)>
  <@row class=rowClass collapse=collapse!false norows=(norows || !container) id=containerId>
    <#if labelType == "vertical">
      <@cell>
        <#if labelArea && labelPosition == "top">
          <@row collapse=collapse norows=(norows || !container)>
            <#local labelAreaClass = addClassArg(labelAreaClass, "field-entry-title-top")>
            <@cell class=compileClassArg(labelAreaClass, defaultGridStyles.labelArea) nocells=(nocells || !container)>
              ${labelAreaContent}        
            </@cell>
          </@row>
        </#if>
          <@row collapse=(collapse || (postfix && collapsePostfix)) norows=(norows || !container)>
            <@cell class=compileClassArg(class, defaultGridStyles.widgetArea) nocells=(nocells || !container)>
              <#nested>
            </@cell>
            <#if postfix && !nocells && container>
              <@cell class=compileClassArg(postfixClass, defaultGridStyles.postfixArea)>
                <#if (postfixContent?is_boolean && postfixContent == true) || !postfixContent?has_content>
                  <span class="postfix"><input type="submit" class="${styles.icon!} ${styles.icon_button!}" value="${styles.icon_button_value!}"/></span>
                <#elseif !postfixContent?is_boolean> <#-- boolean false means prevent markup -->
                  ${postfixContent}
                </#if>
              </@cell>
            </#if>
          </@row>
      </@cell>
    <#else> <#-- elseif labelType == "horizontal" -->
      <#-- TODO: support more label configurations (besides horizontal left) -->
      <#if labelArea && labelPosition == "left">
        <#local labelAreaClass = addClassArg(labelAreaClass, "field-entry-title-left")>
        <@cell class=compileClassArg(labelAreaClass, defaultGridStyles.labelArea) nocells=(nocells || !container)>
            ${labelAreaContent}
        </@cell>
      </#if>

      <#-- need this surrounding cell/row for collapsePostfix (only if true and collapse false) -->
      <@cell class=compileClassArg("", defaultGridStyles.widgetPostfixArea) open=widgetPostfixCombined close=widgetPostfixCombined>
        <@row open=widgetPostfixCombined close=widgetPostfixCombined collapse=(collapse || (postfix && collapsePostfix))>
          <#-- NOTE: here this is the same as doing 
                 class=("=" + compileClassArg(class, defaultGridStyles.widgetArea))
               as we know the compiled class will never be empty. -->
          <@cell class=compileClassArg(class, defaultGridStyles.widgetArea) nocells=(nocells || !container)>
            <#nested>
          </@cell>
          <#if postfix && !nocells && container>
            <@cell class=compileClassArg(postfixClass, defaultGridStyles.postfixArea)>
              <#if (postfixContent?is_boolean && postfixContent == true) || !postfixContent?has_content>
                <span class="postfix"><input type="submit" class="${styles.icon!} ${styles.icon_button!}" value="${styles.icon_button_value!}"/></span>
              <#elseif !postfixContent?is_boolean> <#-- boolean false means prevent markup -->
                ${postfixContent}
              </#if>
            </@cell>
          </#if>
        </@row>
      </@cell>
    </#if>
  </@row>
</#macro>

<#-- @field label area markup - theme override 
    This generates labelContent passed to @field_markup_container. 
    WARN: origArgs may be empty -->
<#macro field_markup_labelarea labelType="" labelPosition="" label="" labelDetail="" fieldType="" fieldId="" collapse="" required=false origArgs={} passArgs={} catchArgs...>
  <#local label = label?trim>
  <#if label?has_content>
    <#if collapse>
      <span class="${styles.prefix!} form-field-label">${label}<#if required> *</#if></span>
    <#else>
      <label class="form-field-label"<#if fieldId?has_content> for="${fieldId}"</#if>>${label}<#if required> *</#if></label>
    </#if>  
  <#else>
    <#if required>*</#if>
  </#if> 
  <#local labelDetail = labelDetail?trim>
  <#if labelDetail?has_content>
    ${labelDetail}
  </#if>  
  <#-- FIXME?: nbsp workaround is to prevent a foundation "bug" where empty cells sometimes go to zero width -->
  <#if !label?has_content && !labelDetail?has_content>
    &nbsp;
  </#if>
</#macro>

<#-- calculates the default @field grid styles - used unless overridden by @field's caller 
     TODO: support more columns values -->
<#function getDefaultFieldGridStyles args={} catchArgs...>
  <#local args = mergeArgMapsBasic(args, {}, {
    <#-- parameters: defaults -->
    "columns" : "",
    "labelArea" : true,
    "labelInRow" : true,
    "postfix" : false,
    "postfixSize" : 0,
    "isLarge" : "",
    "labelSmallColDiff" : 1,
    "widgetPostfixCombined" : false
  })>
  <#local dummy = localsPutAll(args)> 
  
  <#if !isLarge?is_boolean>
    <#-- get estimate of the current absolute column widths (with all parent containers, as much as possible) -->
    <#local absColSizes = getAbsContainerSizeFactors()>
    <#-- if parent container is large, then we'll include the large grid sizes; otherwise only want small to apply -->
    <#local isLarge = (absColSizes.large > 6)>  
  </#if>

  <#if postfix>
    <#local columnspostfix = postfixSize/>
  <#else>
    <#local columnspostfix = 0/>
  </#if>
  <#if !columns?has_content>
    <#if labelArea && labelInRow>
      <#local widgetAndPostfixColumns = 10>
    <#else>
      <#local widgetAndPostfixColumns = 12>
    </#if>
  <#else>
    <#-- DEV NOTE: WARN: variable name swap here -->
    <#local widgetAndPostfixColumns = columns>
  </#if>
  
  <#if widgetPostfixCombined>
    <#-- widget area will be child of a separate container -->
    <#local columns = 12 - columnspostfix>
  <#else>
    <#local columns = widgetAndPostfixColumns - columnspostfix>
  </#if>

  <#if labelInRow>
    <#local columnslabelarea = 12 - widgetAndPostfixColumns>
  <#else>
    <#local columnslabelarea = 12>
  </#if>

  <#local defaultLabelAreaClass><#if labelArea>${styles.grid_small!}<#if labelInRow>${columnslabelarea + labelSmallColDiff}<#else>${columnslabelarea}</#if><#if isLarge> ${styles.grid_large!}${columnslabelarea}</#if></#if></#local>
  <#local defaultWidgetAndPostfixClass><#if labelArea && labelInRow>${styles.grid_small!}${widgetAndPostfixColumns - labelSmallColDiff}<#else>${styles.grid_small!}${widgetAndPostfixColumns}</#if><#if isLarge> ${styles.grid_large!}${widgetAndPostfixColumns}</#if></#local>
  <#local defaultWidgetAreaClass><#if labelArea && labelInRow && !widgetPostfixCombined>${styles.grid_small!}${columns - labelSmallColDiff}<#else>${styles.grid_small!}${columns}</#if><#if isLarge> ${styles.grid_large!}${columns}</#if></#local>
  <#local defaultPostfixClass><#if postfix>${styles.grid_small!}${columnspostfix}<#if isLarge> ${styles.grid_large!}${columnspostfix}</#if></#if></#local>
  
  <#return {
    "labelArea" : defaultLabelAreaClass,
    "widgetPostfixArea" : defaultWidgetAndPostfixClass,
    "widgetArea" : defaultWidgetAreaClass,
    "postfixArea" : defaultPostfixClass
  }>
</#function>

