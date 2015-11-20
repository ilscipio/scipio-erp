<#--
* 
* Form and field HTML template include, default Cato markup.
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
-->
<#macro form type="input" name="" id="" class="" openOnly=false closeOnly=false nestedOnly=false attribs={} inlineAttribs...>
  <#local attribs = concatMaps(attribs, inlineAttribs)>
  <#local open = !(nestedOnly || closeOnly)>
  <#local close = !(nestedOnly || openOnly)>
  <#if open>
    <#local formInfo = {"type":type, "name":name, "id":id}>
    <#local dummy = pushRequestStack("catoCurrentFormInfo", formInfo)>
    <form<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if name?has_content> name="${name}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "name", "id"]/></#if>>
  </#if>
      <#nested>
  <#if close>
    </form>
    <#local dummy = popRequestStack("catoCurrentFormInfo")>
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
<#macro progressScript args={} inlineArgs...>
  <#local args = concatMaps(args, inlineArgs)>
  <#local enabled = args.enabled!true>
  <#if enabled>
  <#local htmlwrap = args.htmlwrap!htmlwrap>
  <#local progressOptions = args.progressOptions!{}>
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
    type           = (warning|info|success) default: success
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
<#macro progress value=0 id="" type="" class="" showValue=false containerClass="" progressArgs={} progressOptions={}>
  <#local progressOptions = progressArgs.progressOptions!progressOptions>
  <#local explicitId = id?has_content>
  <#if !id?has_content>
    <#local id = (progressOptions.progBarId)!"">
  </#if>

    <#switch type>
      <#case "alert">
        <#local color=styles.color_alert!/>
      <#break>
      <#case "info">
        <#local color=styles.color_info!/>
      <#break>
      <#case "warning">
        <#local color=styles.color_warning!/>
      <#break>
      <#default>
        <#local color=styles.color_success!/>
    </#switch>

    <@progress_markup value=value id=id class=class showValue=showValue containerClass=containerClass color=color />
    
  <#if progressOptions?has_content>
    <#local opts = progressOptions>
    <#if explicitId>
      <#local opts = concatMaps(opts, {"progBarId":"${id}"})>
    </#if>
    <#-- inlines always override args map -->
    <@progressScript progressOptions=opts htmlwrap=true args=progressArgs />
  </#if>
</#macro>

<#-- @progress main markup - theme override -->
<#macro progress_markup value=0 id="" class="" showValue=false containerClass="" color="" extraArgs...>
    <#local classes = compileClassArg(class)>
    <#local containerClasses = compileClassArg(containerClass)>
    <div class="${styles.progress_container}<#if !styles.progress_wrap?has_content && classes?has_content> ${classes}</#if><#if color?has_content> ${color!}</#if><#if containerClasses?has_content> ${containerClasses}</#if>"<#if id?has_content> id="${id}"</#if>>
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
<#macro asmSelectScript args={} inlineArgs...>
  <#local args = concatMaps(args, inlineArgs)>
  <#local enabled = args.enabled!true>
  <#if enabled>
  <#local id = args.id!"">
  <#local title = args.title!false>
  <#local sortable = args.sortable!false>
  <#local formId = args.formId!"">
  <#local formName = args.formName!"">
  <#local asmSelectOptions = args.asmSelectOptions!{}>
  <#local asmSelectDefaults = args.asmSelectDefaults!true>
  <#local relatedFieldId = args.relatedFieldId!"">
  <#local relatedTypeName = args.relatedTypeName!"">
  <#local relatedTypeFieldId = args.relatedTypeFieldId!"">
  <#local paramKey = args.paramKey!"">
  <#local requestName = args.requestName!"">
  <#local responseName = args.responseName!"">
  <#local htmlwrap = args.htmlwrap!true>
  
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
<#macro fieldset id="" title="" class="" containerClass="" collapsed=false openOnly=false closeOnly=false nestedOnly=false>
    <@fieldset_core class=class containerClass=containerClass id=id title=title collapsed=collapsed collapsibleAreaId="" collapsible=false expandToolTip="" collapseToolTip="" openOnly=openOnly closeOnly=closeOnly nestedOnly=nestedOnly>
        <#nested />
    </@fieldset_core>
</#macro>

<#-- DEV NOTE: see @section_core for details on pattern 
     migrated from @renderFieldGroupOpen/Close form widget macro -->
<#macro fieldset_core class="" containerClass="" id="" title="" collapsed=false collapsibleAreaId="" expandToolTip="" collapseToolTip="" collapsible=false openOnly=false closeOnly=false nestedOnly=false>
  <#local open = !(nestedOnly || closeOnly)>
  <#local close = !(nestedOnly || openOnly)>
  <@fieldset_markup open=open close=close class=class containerClass=containerClass id=id title=title collapsed=collapsed collapsibleAreaId=collapsibleAreaId expandToolTip=expandToolTip collapseToolTip=collapseToolTip collapsible=collapsible><#nested></@fieldset_markup>
</#macro>

<#-- @fieldset main markup - theme override -->
<#macro fieldset_markup open=true close=true class="" containerClass="" id="" title="" collapsed=false collapsibleAreaId="" expandToolTip="" collapseToolTip="" collapsible=false extraArgs...>
  <#if open>
    <#local classes = compileClassArg(class)>
    <#local containerClasses = compileClassArg(containerClass, "${styles.grid_large!}12")>
    <div class="${styles.grid_row!}">
      <div class="fieldgroup ${styles.grid_cell!}<#if containerClasses?has_content> ${containerClasses}</#if><#if collapsible || collapsed> toggleField<#if collapsed> ${styles.collapsed!}</#if></#if>"<#if id?has_content> id="${id}_wrapper"</#if>>
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
      </div>
    </div>
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
    type            = [default|default-nolabels|generic], default default. the type of fields arrangement.
                      default: default cato field arrangement. this is the type assumed when no @fields element is present.
                          currently, it mostly influences the label area (present for all @field types except submit).
                      default-nolabels: default cato field arrangement for common sets of fields with no labels.
                          it expects that @field entries won't be passed any labels.
                      generic: generic field arrangement of no specific pattern. means field arrangement is custom and field macro should not
                          make any assumptions except where a default is required. caller determines arrangement/layout/label type/etc.
    labelType       = [gridarea|none], defaults specified in styles variables based on fields type. override for type of the field labels themselves.
                      gridarea: a grid area reserved for label.
                      none: no labels or label areas. expects the @field macro won't be passed any.
    labelLayout     = [left|right|none], defaults specified in styles variables based on fields type. override for layout/positioning of the labels.
    labelArea       = boolean, defaults specified in styles variables based on fields type. overrides whether fields are expected to have a label area or not, mainly when label omitted. 
                      logic is influenced by other arguments.
                      NOTE: This does not determine label area type (gridarea, etc.); only labelType does that (in current code).
                          They are decoupled. This only controls presence of it.
                      NOTE: This is weaker than labelArea arg of @field macro, but stronger than other args of this macro.
    labelAreaExceptions = string of space-delimited @field type names or list of names, defaults specified in styles variables based on fields type  
    formName            = the form name the child fields should assume  
    formId              = the form ID the child fields should assume   
    inlineItems     = change default for @field inlineItems parameter (true/false)     
-->
<#macro fields type="default" labelType="" labelLayout="" labelArea="" labelAreaExceptions=true formName="" formId="" inlineItems="">
    <#local fieldsInfo = makeFieldsInfo(type, labelType, labelLayout, labelArea, labelAreaExceptions, formName, formId, inlineItems)>
    <#local dummy = pushRequestStack("catoCurrentFieldsInfo", fieldsInfo)>
    <#nested>
    <#local dummy = popRequestStack("catoCurrentFieldsInfo")>
</#macro>

<#function makeFieldsInfo type labelType="" labelLayout="" labelArea="" labelAreaExceptions=true formName="" formId="" inlineItems="">
    <#local stylesType = type?replace("-","_")>
    <#local stylesPrefix = "fields_" + stylesType + "_">
    <#if !styles[stylesPrefix + "labeltype"]??>
      <#local stylesType = "default">
      <#local stylesPrefix = "fields_default_">
    </#if>

    <#if !labelArea?is_boolean>
      <#local stylesLabelArea = styles[stylesPrefix + "labelarea"]!"">
      <#if stylesLabelArea?is_boolean>
        <#local labelArea = stylesLabelArea>
      </#if>
    </#if>
    <#if !labelType?has_content>
      <#local labelType = styles[stylesPrefix + "labeltype"]!"gridarea">
    </#if>
    <#if !labelLayout?has_content>
      <#local labelLayout = styles[stylesPrefix + "labellayout"]!"left">
    </#if>
    <#if !labelArea?is_boolean>
      <#local labelArea = (labelType != "none" && labelLayout != "none")>
    </#if>

    <#if !labelAreaExceptions?is_sequence && !labelAreaExceptions?is_string>
      <#if labelAreaExceptions?is_boolean && labelAreaExceptions == false>
        <#local labelAreaExceptions = []>
      <#else>
        <#local labelAreaExceptions = styles[stylesPrefix + "labelareaexceptions"]!"">
      </#if>
    </#if>
    <#if labelAreaExceptions?is_string>
      <#if labelAreaExceptions?has_content>
        <#local labelAreaExceptions = labelAreaExceptions?split(" ")>
      <#else>
        <#local labelAreaExceptions = []>
      </#if>
    </#if>
    <#return {"type":type, "labelType":labelType, "labelLayout":labelLayout, 
        "labelArea":labelArea, "labelAreaExceptions":labelAreaExceptions, "formName":formName, "formId":formId,
        "inlineItems":inlineItems}>
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
* Field
************ 
  * Usage Example *  
    <@field attr="" />
    
  * Parameters *
    * General *
    type            = form element type - supported values and their parameters listed below, between asterix.
                      default "generic", which means input defined manually with #nested.
                      generic is mostly for grouping multiple sub-fields, but can be used anywhere.
                      (specific field types should be preferred to manually defining content, where possible)
    label           = field label
                      NOTE: Presence of label arg does not guarantee a label will be shown; this is controlled
                          by labelArea (and labelType) and its defaults, optionally coming from @fields container.
                          For generic parent fields, label type must be specified explicitly, e.g.
                            <@fields type="generic"><@field labelType="gridarea" label="mylabel">...</@fields> 
                      NOTE: label area behavior may also be influenced by containing macros such as @fields
    labelDetail     = extra content (HTML) inserted with (after) label
    labelType       = explicit label type (see @fields)
    labelLayout     = explicit label layout (see @fields)
    labelArea       = boolean, default empty string (use @fields type default).
                      if true, forces a label area.
                      if false, prevents a label area.
                      NOTE: This does not determine label area type (gridarea, etc.); only labelType does that (in current code).
                          They are decoupled. This only controls presence of it.
    tooltip         = Small field description - to be displayed to the customer
    description     = alternative to tooltip
    name            = field name
    value           = field value
    columns         = int value for columns for field (overrides classes)
    class           = css classes for the field element (NOT the cell container!)
                      supports prefixes:
                        "+": causes the classes to append only, never replace defaults (same logic as empty string "")
                        "=": causes the class to replace non-essential defaults (same as specifying a class name directly)
    maxlength       = maxLength
    id              = field id
    events          = map of JS event names to script actions. 
                      event names can be specified with or without the "on" prefix ("click" or "onclick").
    onClick         = shortcut for: events={"click": onClick}
    disabled        = field disabled
    placeholder     = field placeholder
    alert           = adds additional css alert class
    mask            = toggles jQuery mask plugin
    size            = size attribute (default: 20)
    collapse        = should the field be collapsing? (default: false)
    norows          = render without the rows-container
    nocells         = render without the cells-container
    required        = required input
        
    * input *
    autoCompleteUrl = if autocomplete function exists, specification of url will make it available
    postfix          = if set to true, attach submit button (default:false)
    
    * textArea *
    readonly        = readonly
    rows            = number of rows
    cols            = number of columns
    
    * dateTime *
    dateType        = type of datetime [date,time] (default: date)
    
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
    fieldForName    = Contains the lookup window form name.
    
    * Checkbox (single mode) *
    value           = Y/N
    currentValue    = current value, used to check if should be checked
    checked         = override checked state (true/false/"") - if set to boolean, overrides currentValue logic
    
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
    submitType      = [submit|link|button|image], default submit (<input type="submit">)  
    text            = display text (also value for submitType=="submit")                
    href            = href for submitType=="link"  
    src             = image url for submitType=="image"    
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
<#macro field type="" label="" labelDetail="" name="" value="" valueType="" currentValue="" defaultValue="" class="" size=20 maxlength="" id="" onClick="" 
        disabled=false placeholder="" autoCompleteUrl="" mask=false alert="false" readonly=false rows="4" 
        cols="50" dateType="date" multiple="" checked="" collapse="" tooltip="" columns="" norows=false nocells=false container=""
        fieldFormName="" formName="" formId="" postfix=false postfixSize=1 required=false items=false autocomplete=true progressArgs={} progressOptions={} 
        labelType="" labelLayout="" labelArea="" description=""
        submitType="input" text="" href="" src="" confirmMsg="" inlineItems="" 
        selected=false allowEmpty=false currentFirst=false currentDescription=""
        manualItems="" manualItemsOnly="" asmSelectArgs={} title="" allChecked="" events={}>
  <#if !type?has_content>
    <#local type = "generic">
  </#if>
  <#if !valueType?has_content>
    <#local valueType = "generic">
  </#if>
  <#-- treat tooltip and description as synonyms for now -->
  <#if tooltip?has_content>
    <#if !description?has_content>
      <#local description = tooltip>
    </#if>
  <#else>
    <#if description?has_content>
      <#local tooltip = description>
    </#if>
  </#if>

  <#if onClick?has_content>
    <#local events = events + {"click": onClick}>
  </#if>
  
  <#-- parent @fields group elem info (if any; may be omitted) -->
  <#local fieldsInfo = readRequestStack("catoCurrentFieldsInfo")!{}>
  <#if !fieldsInfo.type??>
    <#if !catoDefaultFieldsInfo?has_content>
      <#-- optimization -->
      <#global catoDefaultFieldsInfo = makeFieldsInfo("default")>
    </#if>
    <#local fieldsInfo = catoDefaultFieldsInfo>
  </#if>
  
  <#-- parent @field elem info (if any; is possible) -->
  <#local parentFieldInfo = readRequestStack("catoCurrentFieldInfo")!{}>
  <#local hasParentField = ((parentFieldInfo.type)!"")?has_content>
  <#local isTopLevelField = !hasParentField>
  <#local isChildField = hasParentField>
  
  <#local formInfo = readRequestStack("catoCurrentFormInfo")!{}>
  
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
  
  <#if required && (!containsStyleName(class, styles.required!""))>
      <#local class = addClassArg(class, styles.required!"")>
  </#if>
  <#-- the widgets do this now
  <#local class = compileClassArg(class)>-->
    
  <#if !catoFieldNoContainerChildren??>
    <#global catoFieldNoContainerChildren = {
     <#-- "submit":true -->   <#-- only if parent is submitarea (below) -->
      "radio":true,
      "checkbox":true
    }>
    <#global catoFieldNoContainerParent = {
      "submitarea":true
    }>
  </#if>
  <#if !container?is_boolean>
    <#if container?has_content>
      <#local container = container?boolean>
    <#elseif isChildField && (catoFieldNoContainerChildren[type]?? || catoFieldNoContainerParent[parentFieldInfo.type!]??)>
      <#local container = false>
    <#else> 
      <#local container = true>
    </#if>
  </#if>
  
  <#-- label area logic
      TODO: right now most of the fieldsInfo parameters are not fully exploited.
          assumes labelType=="gridarea" (unless "none" which influences labelArea) and 
          labelLayout="left" (unless "none" which influences labelArea). 
      NOTE: labelArea boolean logic does not determine "label type" or "label area type"; 
          only controls presence of. so labelArea logic and usage anywhere should not change
          if new label (area) type were to be added (e.g. on top instead of side by side). -->
  <#if labelArea?is_boolean>
    <#local labelAreaDefault = labelArea>
  <#elseif labelType == "none" || labelLayout == "none">
    <#local labelAreaDefault = false>
  <#elseif isChildField>
    <#-- based on current usage, a child field should never really have a label area by default (requires explicit)... -->
    <#local labelAreaDefault = false>
  <#else>
    <#local labelAreaDefault = (fieldsInfo.labelArea)!false>
    <#if (fieldsInfo.labelAreaExceptions)?has_content>
      <#if fieldsInfo.labelAreaExceptions?seq_contains(type)>
        <#local labelAreaDefault = !labelAreaDefault>
      </#if>
    </#if>
  </#if>
  
  <#if labelType?has_content>
    <#local effLabelType = labelType>
  <#else>
    <#local effLabelType = (fieldsInfo.labelType)!"">
  </#if>
  <#if labelLayout?has_content>
    <#local effLabelLayout = labelLayout>
  <#else>
    <#local effLabelLayout = (fieldsInfo.labelLayout)!"">
  </#if>
  
  <#-- NOTE: I've changed
      (label?has_content || labelDetail?has_content || labelAreaDefault)
      to
      (labelAreaDefault)
      because this gives caller more control over label and simplifies logic; and label presence alone 
      can't determine well what kind of label gets used (label area or inline) unless adding more per-field checks.
      this way, if label area present, it "consumes" the label; otherwise it becomes an inlineLabel (or potentially something else first).  -->
  <#local useLabelArea = (labelArea?is_boolean && labelArea == true) || 
           (!(labelArea?is_boolean && labelArea == false) && (labelAreaDefault))>
  
  
  <#-- push this field's info (popped at end) -->
  <#local dummy = pushRequestStack("catoCurrentFieldInfo", 
      {"type":type, "inlineItems":inlineItems})>
  
  <#-- main markup begin -->
  <#local labelAreaContent = "">
  <#local inlineLabel = "">
  <#if useLabelArea>
      <#local labelAreaContent><@field_markup_labelarea labelType=effLabelType labelLayout=effLabelLayout label=label labelDetail=labelDetail fieldType=type fieldId=id collapse=collapse required=required /></#local>
  <#else>
      <#-- if there's no label area, label was not used up, so label arg becomes an inline label (used on radio and checkbox) -->
      <#local inlineLabel = label>
  </#if>
      
  <@field_markup_container type=type columns=columns postfix=postfix postfixSize=postfixSize labelArea=useLabelArea labelType=effLabelType labelLayout=effLabelLayout labelAreaContent=labelAreaContent collapse=collapse norows=norows nocells=nocells container=container>
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
                              tooltip=tooltip/>
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
                              tooltip=tooltip><#nested></@field_textarea_widget>
        <#break>
      <#case "datetime">
        <#if dateType == "date"><#local shortDateInput=true/><#else><#local shortDateInput=false/></#if>
        <@field_datetime_widget name=name 
                              class=class 
                              alert=alert 
                              title=label 
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
                              tooltip=tooltip/>                
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
                                asmSelectArgs=asmSelectArgs><#nested></@field_select_widget>
        <#break>
      <#case "option">
        <@field_option_widget value=value text=text selected=selected><#nested></@field_option_widget>
        <#break>
      <#case "lookup">
        <@field_lookup_widget name=name formName=formName fieldFormName=fieldFormName class=class alert="false" value=value 
          size=size?string maxlength=maxlength id=id events=events />
      <#break>
      <#case "checkbox">
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
          <#local items=[{"value":value, "description":inlineLabel, "tooltip":tooltip, "events":events, "checked":checked}]/>
          <@field_checkbox_widget multiMode=false items=items inlineItems=inlineItems id=id class=class alert=alert 
            currentValue=currentValue defaultValue=defaultValue allChecked=allChecked name=name tooltip="" />
        <#else>
          <@field_checkbox_widget multiMode=true items=items inlineItems=inlineItems id=id class=class alert=alert 
            currentValue=currentValue defaultValue=defaultValue allChecked=allChecked name=name events=events tooltip=tooltip />
        </#if>
        <#break>
      <#case "radio">
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
          <#local items=[{"key":value, "description":inlineLabel, "tooltip":tooltip, "events":events, "checked":checked}]/>
          <@field_radio_widget multiMode=false items=items inlineItems=inlineItems id=id class=class alert=alert 
            currentValue=currentValue defaultValue=defaultValue name=name tooltip="" />
        <#else>
          <#-- multi radio button item mode -->
          <@field_radio_widget multiMode=true items=items inlineItems=inlineItems id=id class=class alert=alert 
            currentValue=currentValue defaultValue=defaultValue name=name events=events tooltip=tooltip />
        </#if>
        <#break>
      <#case "file">
        <@field_file_widget class=class alert=alert name=name value=value size=size maxlength=maxlength 
          autocomplete=autocomplete?string("", "off") id=id />
        <#break>
      <#case "password">
        <@field_password_widget class=class alert=alert name=name value=value size=size maxlength=maxlength 
          id=id autocomplete=autocomplete?string("", "off") placeholder=placeholder tooltip=tooltip/>
        <#break> 
      <#case "reset">                    
        <@field_reset_widget class=class alert=alert name=name text=text fieldTitleBlank=false />
        <#break>    
      <#case "submit">
        <#if !catoSubmitFieldTypeButtonMap??>
          <#-- the logical button types (based on form widget types) -->
          <#global catoSubmitFieldButtonTypeMap = {
            "submit":"button", "button":"button", "link":"text-link", "image":"image"
          }>
          <#-- the low-level input type attrib, within the logical button types -->
          <#global catoSubmitFieldInputTypeMap = {
            "submit":"submit", "button":"button", "link":"", "image":"image"
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
          href=href inputType=inputType disabled=disabled progressArgs=progressArgs progressOptions=progressOptions />
        <#break>
      <#case "submitarea">
        <@field_submitarea_widget progressArgs=progressArgs progressOptions=progressOptions><#nested></@field_submitarea_widget>
        <#break>
      <#case "hidden">                    
        <@field_hidden_widget name=name value=value id=id events=events />
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
            imageAlt=description tooltip=tooltip />
        <#break> 
      <#default> <#-- "generic", empty or unrecognized -->
        <#if value?has_content>
            <@field_generic_widget text=value tooltip=tooltip/>
        <#else>
            <@field_generic_widget tooltip=tooltip><#nested /></@field_generic_widget>
        </#if>
    </#switch>
  </@field_markup_container>
  <#-- pop field info when done -->
  <#local dummy = popRequestStack("catoCurrentFieldInfo")>
</#macro>

<#-- @field container markup - theme override -->
<#macro field_markup_container type="" class="" columns="" postfix=false postfixSize=0 labelArea=true labelType="" labelLayout="" labelAreaContent="" collapse="" norows=false nocells=false container=true extraArgs...>
  <#local rowClass = "">
  <#local labelAreaClass = "">  
  <#local postfixClass = "">
  <#if postfix>
      <#local columnspostfix = postfixSize/>
      <#if !collapse?has_content>
          <#local collapse = true/> <#-- explicit collapse param overrides postfix setting, but collapse by default -->
      </#if>
      <#local defaultClass = "${styles.grid_small!}${12-columnspostfix} ${styles.grid_large!}${12-columnspostfix}"/>
  <#else>
      <#local defaultClass = "${styles.grid_large!}12"/>
      <#local columnspostfix = 0/>
  </#if>
  <#if !collapse?has_content>
      <#local collapse = false/>
  </#if>

  <#local fieldEntryTypeClass = "field-entry-type-" + mapCatoFieldTypeToStyleName(type)>
  
  <#local rowClass = addClassArg(rowClass, "form-field-entry " + fieldEntryTypeClass)>
  <@row class=rowClass collapse=collapse!false norows=(norows || !container)>
    <#-- TODO: support more label configurations (besides gridarea left) -->
    <#if labelArea && labelType == "gridarea" && labelLayout == "left">
        <#local defaultLabelAreaClass="${styles.grid_small!}3 ${styles.grid_large!}2"/>
        <#local defaultClass="${styles.grid_small!}${9-columnspostfix} ${styles.grid_large!}${10-columnspostfix}"/>
        <#if columns?has_content>
            <#local defaultLabelAreaClass="${styles.grid_small!}${12-columns+1} ${styles.grid_large!}${12-columns}"/>
            <#local defaultClass="${styles.grid_small!}${columns-columnspostfix-1} ${styles.grid_large!}${columns-columnspostfix}"/>
        </#if>
        <#local labelAreaClass = addClassArg(labelAreaClass, "field-entry-title " + fieldEntryTypeClass)>
        <@cell class=compileClassArg(labelAreaClass, defaultLabelAreaClass) nocells=(nocells || !container)>
            ${labelAreaContent}
        </@cell>
    </#if>
    <#local class = addClassArg(class, "field-entry-widget " + fieldEntryTypeClass)>
    <#-- NOTE: here this is the same as doing 
           class=("=" + compileClassArg(class, defaultClass))
         as we know the compiled class will never be empty. -->
    <@cell class=compileClassArg(class, defaultClass) nocells=(nocells || !container)>
        <#nested>
    </@cell>
    <#if postfix && !nocells && container>
        <#local defaultPostfixClass = "${styles.grid_small!}${postfixSize} ${styles.grid_large!}${postfixSize}">
        <#local postfixClass = addClassArg(postfixClass, "field-entry-postfix " + fieldEntryTypeClass)>
        <@cell class=compileClassArg(postfixClass, defaultPostfixClass)>
            <span class="postfix"><input type="submit" class="${styles.icon!} ${styles.icon_button!}" value="${styles.icon_button_value!}"/></span>
        </@cell>
    </#if>
  </@row>
</#macro>

<#-- @field label area markup - theme override -->
<#macro field_markup_labelarea labelType="" labelLayout="" label="" labelDetail="" fieldType="" fieldId="" collapse="" required=false extraArgs...>
  <#if !collapse?has_content>
      <#local collapse = false/>
  </#if>
  <#if label?has_content>
    <#if fieldType=="checkbox" || collapse==false>
        <label class="form-field-label"<#if fieldId?has_content> for="${fieldId}"</#if>>${label}<#if required> *</#if></label>
    <#else>
        <span class="${styles.prefix!} form-field-label">${label}<#if required> *</#if></span>
    </#if>  
  </#if> 
  <#if labelDetail?has_content>
    ${labelDetail}
  </#if>  
</#macro>
