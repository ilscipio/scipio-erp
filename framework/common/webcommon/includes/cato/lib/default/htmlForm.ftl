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
* Progress Script
************
Generates script data and markup needed to make an instance to initialize upload progress 
javascript anim for a form, with progress bar and/or text.

Server-side upload event for the form must register a FileUploadProgressListener in session
for getFileUploadProgressStatus AJAX calls.

TODO: document better if needed
                    
  * Parameters *
    options = elem IDs and options passed to CatoUploadProgress javascript class
              in addition, supports: 
                submitHook - one of: "formSubmit" (default), "validate" (jquery validate), "none" (caller does manually) 
                validateObjScript - if submitHook is "validate", add this script text to jquery validate({...}) object body.
-->
<#macro progressScript options={} htmlwrap=false>
  <#if options?has_content && options.formSel?has_content>
    <#if htmlwrap>
    <script type="text/javascript">
    //<![CDATA[
    </#if>
    
    <#-- This belongs here, but due to Ofbiz bug, moved to commonScripts.ftl
    <@requireScriptOfbizUrl uri="getFileUploadProgressStatus" />
    -->
    
    (function() {
        var uploadProgress = null;
    
        jQuery(document).ready(function() {
          <#if options.successRedirectUrl??>
            <#-- shouldn't have &amp; in script tag... but code may escape and should support... -->
            <#local options = concatMaps(options, {"successRedirectUrl":options.successRedirectUrl?replace("&amp;", "&")})>
          </#if>
            uploadProgress = new CatoUploadProgress(<@objectAsScript lang="js" object=options />);
            uploadProgress.reset();
        });
        
      <#if (options.submitHook!) == "validate">
        jQuery("${options.formSel}").validate({
            submitHandler: function(form) {
                var goodToGo = uploadProgress.initUpload();
                if (goodToGo) {
                    form.submit();
                }
            },
            ${options.validateObjScript!""}
        });
      <#elseif (options.submitHook!) != "none" >
        jQuery("${options.formSel}").submit(function(event) {
            var goodToGo = uploadProgress.initUpload();
            if (!goodToGo) {
                event.preventDefault();
            }
        });
      </#if>
    })();
    
    <#if htmlwrap>
    //]]>
    </script>
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
    progressOptions = if present, attaches progress bar to an upload form with javascript-based progress and 
                      attaches results to page using elem IDs and options in this map - 
                      see CatoUploadProgress javascript class for options; mostly same
-->
<#macro progress value=0 id="" type="" class="" showValue=false containerClass="" progressOptions={}>
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
    <@progressScript options=opts htmlwrap=true />
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
<#macro form type="input" name="" id="" class="" attribs={} openOnly=false closeOnly=false wrapIf=true inlineAttribs...>
  <#local open = wrapIf && !closeOnly>
  <#local close = wrapIf && !openOnly>
  <#if open>
    <#local formInfo = {"type":type, "name":name, "id":id}>
    <#local dummy = pushRequestStack("catoCurrentFormInfo", formInfo)>
    <form<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if name?has_content> name="${name}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "name", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
  </#if>
      <#nested>
  <#if close>
    </form>
    <#local dummy = popRequestStack("catoCurrentFormInfo")>
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
<#macro fieldset id="" title="" class="" containerClass="" collapsed=false openOnly=false closeOnly=false wrapIf=true>
    <@fieldset_core class=class containerClass=containerClass id=id title=title collapsed=collapsed collapsibleAreaId="" collapsible=false expandToolTip="" collapseToolTip="" openOnly=openOnly closeOnly=closeOnly wrapIf=wrapIf>
        <#nested />
    </@fieldset_core>
</#macro>

<#-- DEV NOTE: see @section_core for details on pattern 
     migrated from @renderFieldGroupOpen/Close form widget macro -->
<#macro fieldset_core class="" containerClass="" id="" title="" collapsed=false collapsibleAreaId="" expandToolTip="" collapseToolTip="" collapsible=false openOnly=false closeOnly=false wrapIf=true>
  <#local open = wrapIf && !closeOnly>
  <#local close = wrapIf && !openOnly>
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
                      note that this is weaker than labelArea arg of @field macro, but stronger than other args of this macro.
    labelAreaExceptions = string of space-delimited @field type names or list of names, defaults specified in styles variables based on fields type  
    formName            = the form name the child fields should assume  
    formId              = the form ID the child fields should assume             
-->
<#macro fields type="default" labelType="" labelLayout="" labelArea="" labelAreaExceptions=true formName="" formId="">
    <#local fieldsInfo = makeFieldsInfo(type, labelType, labelLayout, labelArea, labelAreaExceptions, formName, formId)>
    <#local dummy = pushRequestStack("catoCurrentFieldsInfo", fieldsInfo)>
    <#nested>
    <#local dummy = popRequestStack("catoCurrentFieldsInfo")>
</#macro>

<#function makeFieldsInfo type labelType="" labelLayout="" labelArea="" labelAreaExceptions=true formName="" formId="">
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
        "labelArea":labelArea, "labelAreaExceptions":labelAreaExceptions, "formName":formName, "formId":formId}>
</#function>

<#-- 
*************
* mapCatoFieldTypeToStyleName
************ 
Maps a cato field type to a style name representing the type.

Should be coordinated with mapWidgetFieldTypeToStyleName to produce common field type style names.
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
* mapWidgetFieldTypeToStyleName
************ 
Maps an Ofbiz field type to a style name representing the type.

Should be coordinated with mapCatoFieldTypeToStyleName to produce common field type style names.
-->
<#function mapWidgetFieldTypeToStyleName fieldType>
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
    type            = [input|textarea|datetime|select|checkbox|radio|display|password|generic], form element type
                      default generic meaning input defined manually with #nested
                      (discouraged; prefer specific; but sometimes required and useful
                      for transition)
    label           = field label
                      note: label area behavior may also be influenced by containing macros such as @fields
    labelDetail     = extra content (HTML) inserted with (after) label
    labelType       = explicit label type (see @fields)
    labelLayout     = explicit label layout (see @fields)
    labelArea       = boolean, default empty string (use @fields type default).
                      if true, forces a label area.
                      if false, prevents a label area.
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
    onClick         = JS Event
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
    currentValue    = currently selected value
    
    * lookup *
    formName        = The name of the form that contains the lookup field.
    fieldForName    = Contains the lookup window form name.
    
    * Checkbox *
    value           = Y/N
    checked         = checked (true/false)
    
    * radio (single mode) *
    value           = Y/N, only used if single radio item mode (items not specified)
    checked         = determines if checked (true/false)
    
    * radio (multi mode) *
    items           = if specified, multiple-items radio generated; 
                      list of {"key": value, "description": label, "event": html-dom-event-attrib, "action": event-js} hashes
    inlineItems     = if true (default), radio items are many per line; if false, one per line
                      note this takes effect whether single-item or multiple-item radio.
    currentValue    = current value, determines checked
    defaultValue    = default value, determines checked
    
    * file *
    autocomplete    = true/false, default true (false to prevent)
    
    * password *
    autocomplete    = true/false, default true (false to prevent)
    
    * submitarea *
    <#nested>       = button(s) (<@field type="submit"> or manual <input>, <a>, <button>) to include
    progressOptions = if this is an upload form, specify progress upload options, enables progress next to buttons. 
                      see @progress[Script] macro[s]. should specify formSel, (progBarId and/or progTextBoxId), and others.
                      
    * submit *
    submitType      = [submit|link|button|image], default submit (<input type="submit">)  
    text            = display text (also value for submitType=="submit")                
    href            = href for submitType=="link"  
    src             = image url for submitType=="image"    
    confirmMsg      = confirmation message            
                      
    * display *
    valueType       = [image|text|currency|date|date-time|accounting-number|generic], default generic (treated as text)
                      TODO: currently all are handled as text/generic (because formatting done in java in stock ofbiz)
    value           = display value or image URL
    description     = for image type: image alt
-->
<#macro field type="" label="" labelDetail="" name="" value="" valueType="" currentValue="" defaultValue="" class="" size=20 maxlength="" id="" onClick="" 
        disabled=false placeholder="" autoCompleteUrl="" mask=false alert="false" readonly=false rows="4" 
        cols="50" dateType="date" multiple="" checked=false collapse="" tooltip="" columns="" norows=false nocells=false container=""
        fieldFormName="" formName="" formId="" postfix=false postfixSize=1 required=false items=[] autocomplete=true progressOptions={} 
        labelType="" labelLayout="" labelArea="" description=""
        submitType="input" text="" href="" src="" confirmMsg="" inlineItems="">
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
  
  <#-- this field's info (popped at end) -->
  <#local dummy = pushRequestStack("catoCurrentFieldInfo", 
      {"type":type})>
  
  <#-- get form name and id -->
  <#if !formName?has_content>
    <#if fieldsInfo.formName?has_content>
      <#local formName = fieldsInfo.formName>
    <#elseif formInfo.name?has_content>
      <#local formName = formInfo.formName>
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
  
  <#local radioSingle = (type=="radio" && !items?has_content)>
  <#-- special case: for radioSingle, the label is passed to its macro instead...
      note this doesn't automatically prevent the container label area (otherwise inconsistent with everything else) -->
  <#local inlineLabel = "">
  <#if radioSingle>
    <#local inlineLabel = label>
    <#local label = "">
  </#if>
  
  <#if !catoFieldNoContainerChildren??>
    <#global catoFieldNoContainerChildren = {
     <#-- "submit":true -->   <#-- only if parent is submitarea -->
      "radio":true    <#-- child radio will pretty much always imply radioSingle -->
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
          labelLayout="left" (unless "none" which influences labelArea). -->
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
  
  <#local useLabelArea = (labelArea?is_boolean && labelArea == true) || 
           (!(labelArea?is_boolean && labelArea == false) && (label?has_content || labelDetail?has_content || labelAreaDefault))>
  
  <#-- main markup begin -->
  <#local labelContent = "">
  <#if useLabelArea>
      <#local labelContent><@field_markup_labelarea label=label labelDetail=labelDetail fieldType=type fieldId=id collapse=collapse required=required /></#local>
  </#if>
  <@field_markup_container type=type columns=columns postfix=postfix postfixSize=postfixSize useLabelArea=useLabelArea labelContent=labelContent collapse=collapse norows=norows nocells=nocells container=container>
    <#switch type>
      <#case "input">
        <@field_input_widget name=name 
                              class=class 
                              alert=alert 
                              value=value 
                              textSize=size 
                              maxlength=maxlength 
                              id=id 
                              event="onClick" 
                              action=onClick 
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
        <#-- TODO: Currently, select only supports manual items, not auto-generated items
             Must set manualItems to true especially, otherwise extra empty options generated -->
        <#local manualItems = true>
        <#local manualItemsOnly = true>
        
        <@field_select_widget name=name
                                class=class 
                                alert=alert 
                                id=id 
                                multiple=multiple
                                formName=""
                                otherFieldName="" 
                                event="onClick" 
                                action=onClick  
                                size=size
                                firstInList="" 
                                currentValue=currentValue 
                                explicitDescription="" 
                                allowEmpty=""
                                options=[]
                                fieldName=name
                                otherFieldName="" 
                                otherValue="" 
                                otherFieldSize=0 
                                dDFCurrent=""
                                ajaxEnabled=false
                                noCurrentSelectedKey=""
                                ajaxOptions=""
                                frequency=""
                                minChars=""
                                choices="" 
                                autoSelect=""
                                partialSearch=""
                                partialChars=""
                                ignoreCase=""
                                fullSearch=""
                                tooltip=tooltip
                                manualItems=manualItems
                                manualItemsOnly=manualItemsOnly><#nested></@field_select_widget>
        <#break>
      <#case "lookup">
        <@field_lookup_widget name=name formName=formName fieldFormName=fieldFormName class=class alert="false" value=value 
          size=size?string maxlength=maxlength id=id event="onClick" action=onClick />
      <#break>
      <#case "checkbox">
            <@field_checkbox_widget id=id currentValue=value checked=checked name=name action=action />
        <#break>
      <#case "radio">
            <#if radioSingle>
                <#-- single radio button item mode -->
                <#local items=[{"key":value, "description":inlineLabel!""}]/>
                <@field_radio_widget multiMode=false items=items inlineItems=inlineItems class=class alert=alert 
                  currentValue=(checked?string(value,"")) noCurrentSelectedKey="" name=name event="" action="" tooltip=tooltip />
            <#else>
                <#-- multi radio button item mode -->
                <@field_radio_widget multiMode=true items=items inlineItems=inlineItems class=class alert=alert 
                  currentValue=currentValue noCurrentSelectedKey=defaultValue name=name event="" action="" tooltip=tooltip />
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
        <@field_submit_widget buttonType=buttonType class=class alert=alert formName=formName name=name event="" 
          action="" imgSrc=src confirmation=confirmMsg containerId="" ajaxUrl="" title=text showProgress=false 
          onClick=onClick href=href inputType=inputType disabled=disabled progressOptions=progressOptions />
        <#break>
      <#case "submitarea">
        <@field_submitarea_widget progressOptions=progressOptions><#nested></@field_submitarea_widget>
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
<#macro field_markup_container type="" class="" columns="" postfix=false postfixSize=0 useLabelArea=true labelContent="" collapse="" norows=false nocells=false container=true extraArgs...>
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
    <#if useLabelArea>
        <#local defaultLabelAreaClass="${styles.grid_small!}3 ${styles.grid_large!}2"/>
        <#local defaultClass="${styles.grid_small!}${9-columnspostfix} ${styles.grid_large!}${10-columnspostfix}"/>
        <#if columns?has_content>
            <#local defaultLabelAreaClass="${styles.grid_small!}${12-columns+1} ${styles.grid_large!}${12-columns}"/>
            <#local defaultClass="${styles.grid_small!}${columns-columnspostfix-1} ${styles.grid_large!}${columns-columnspostfix}"/>
        </#if>
        <#local labelAreaClass = addClassArg(labelAreaClass, "field-entry-title " + fieldEntryTypeClass)>
        <@cell class=compileClassArg(labelAreaClass, defaultLabelAreaClass) nocells=(nocells || !container)>
            ${labelContent}
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
<#macro field_markup_labelarea label="" labelDetail="" fieldType="" fieldId="" collapse="" required=false extraArgs...>
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
