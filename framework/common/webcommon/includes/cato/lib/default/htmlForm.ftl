<#--
* 
* Form and field HTML template include, default Cato markup.
*
* Included by htmlTemplate.ftl.
*
* NOTE: May have implicit dependencies on other parts of Cato API.
*
-->

<#-- 
*************
* Progress Script Macro
************
Generates script data and markup needed to make an instance to initialize upload progress 
javascript anim for a form, with progress bar and/or text.

Server-side upload event for the form must register a FileUploadProgressListener in session
for getFileUploadProgressStatus AJAX calls.

TODO: document better if needed
                    
   * General Attributes *
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
* Progress Bar Macro
************

    Usage example:  
    <@progress value=40/>             
    
   Can be animated using js, example: 
   
   $('#${id}_meter').css("width", "78%");
    
   Can also be animated automatically using progressOptions which activates use of CatoUploadProgress
   script for this progress bar by linking it to a form submit.
                    
   * General Attributes *
    value          = Percentage done
    id             = custom id; can also be specified as progressOptions.progBarId instead
                     if omitted will not make a progress bar, but script still generated for progressOptions.progTextBoxId
    type           = (warning|info|success) default: success
    class          = Adds classes - please use "(small|medium|large)-block-grid-#"
                     (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    showValue      = Display value inside bar
    wrapClass      = classes on outer wrapper only
    progressOptions = if present, attaches progress bar to an upload form with javascript-based progress and 
                      attaches results to page using elem IDs and options in this map - 
                      see CatoUploadProgress javascript class for options; mostly same
-->
<#macro progress value=0 id="" type="" class=true showValue=false wrapperClass=true progressOptions={}>
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
    <#local classes = makeClassesArg(class, "")>
    <#local wrapperClasses = makeClassesArg(wrapperClass, "")>
    <div class="${styles.progress_container}<#if !styles.progress_wrap?has_content && classes?has_content> ${classes}</#if><#if color?has_content> ${color!}</#if><#if wrapperClasses?has_content> ${wrapperClasses}</#if>"<#if id?has_content> id="${id}"</#if>>
      <#if styles.progress_wrap?has_content><div class="${styles.progress_wrap!}<#if classes?has_content> ${classes}</#if>"<#if id?has_content> id="${id!}_meter"</#if> role="progressbar" aria-valuenow="${value!}" aria-valuemin="0" aria-valuemax="100" style="width: ${value!}%"></#if>
            <span class="${styles.progress_bar!}"<#if !styles.progress_wrap?has_content> style="width: ${value!}%"<#if id?has_content> id="${id!}_meter"</#if></#if>><#if showValue>${value!}</#if></span>
      <#if styles.progress_wrap?has_content></div></#if>
    </div>
    
  <#if progressOptions?has_content>
    <#local opts = progressOptions>
    <#if explicitId>
      <#local opts = concatMaps(opts, {"progBarId":"${id}"})>
    </#if>
    <@progressScript options=opts htmlwrap=true />
  </#if>
</#macro>

<#-- 
*************
* Form Macro
************
HTML form.

    Usage example:  
    <@form name="myform">
      <@fields>
        <input type="hidden" ... />
        <@field ... />
        <@field ... />
      </@fields>
    </@form>            
                    
   * General Attributes *
    type                = [input|display], default input
                          DEV NOTE: "display" is special for time being, probably rare or unused;
                                    maybe it should cause to omit <form> element
    class               = classes on form element itself 
    attribs             = hash of attributes for HTML <form> element (needed for names with dashes)
    inlineAttribs       = other attributes for HTML <form> element
-->
<#macro form type="input" name="" id="" class=true attribs={} inlineAttribs...>
    <#local formInfo = {"type":type, "name":name, "id":id}>
    <#local dummy = pushRequestStack("catoCurrentFormInfo", formInfo)>
    <#local classes = makeClassesArg(class, "")>
    <form<#if classes?has_content> class="${classes}</#if><#if id?has_content> id="${id}"</#if><#if name?has_content> name="${name}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "name", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
      <#nested>
    </form>
    <#local dummy = popRequestStack("catoCurrentFormInfo")>
</#macro>

<#-- 
*************
* Fieldset Macro
************
A visible fieldset, including the HTML element.

    Usage example:  
    <@fieldset title="">
        Inner Content
    </@fieldset>            
                    
   * General Attributes *
    class           = css classes 
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    id              = set id
    title           = fieldset-title
    collapsed       = show/hide the fieldset
-->
<#macro fieldset id="" title="" class=true collapsed=false>
    <#local classes = makeClassesArg(class, "")>
    <@fieldset_core style=classes id=id title=title collapsed=collapsed collapsibleAreaId="" collapsible=false expandToolTip="" collapseToolTip="">
        <#nested />
    </@fieldset_core>
</#macro>

<#-- DEV NOTE: see @section_core for details on pattern 
     migrated from @renderFieldGroupOpen/Close form widget macro -->
<#macro fieldset_core style="" id="" title="" collapsed=false collapsibleAreaId="" expandToolTip="" collapseToolTip="" collapsible=false openOnly=false closeOnly=false wrapIf=true>
  <#local open = wrapIf && !closeOnly>
  <#local close = wrapIf && !openOnly>
  <@fieldset_markup open=open close=close style=style id=id title=title collapsed=collapsed collapsibleAreaId=collapsibleAreaId expandToolTip=expandToolTip collapseToolTip=collapseToolTip collapsible=collapsible><#nested></@fieldset_markup>
</#macro>

<#macro fieldset_markup open=true close=true style="" id="" title="" collapsed=false collapsibleAreaId="" expandToolTip="" collapseToolTip="" collapsible=false>
<#if open>
<div class="${styles.grid_row!}">
  <div class="fieldgroup ${styles.grid_large!}12 ${styles.grid_cell!}<#if style?has_content> ${style}</#if><#if collapsible || collapsed> toggleField<#if collapsed> ${styles.collapsed!}</#if></#if>"<#if id?has_content> id="${id}"</#if>>
    <fieldset<#if style?has_content> class="${style!}"</#if>>
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
* Fields Macro
************ 
Fields helper that helps modify a set of @field definitions, or group of fields.
Not associated with a visible element, as is @fieldset.
Can be omitted.
May sometimes need multiple of these per form (so @form insufficient for this purpose),
or even multiple per fieldset. 

    Usage example: 
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
    
    * General Attributes *
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
* mapCatoFieldTypeToStyleName function
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
* mapWidgetFieldTypeToStyleName function
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
* Field Macro
************ 
    Usage example:  
    <@field attr="" />
    
    * General Attributes *
    type            = form element of type [input,textarea,datetime,select,checkbox,radio,display,password,generic],
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
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
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
    items           = if specified, multiple-items radio generated; list of {"key": value, "description": label, "event": html-dom-event-attrib, "action": event-js} hashes
    currentValue    = current value, determines checked
    defaultValue    = default value, determines checked
    
    * file *
    autocomplete    = true/false, default true (false to prevent)
    
    * password *
    autocomplete    = true/false, default true (false to prevent)
    
    * submitarea *
    <#nested>       = button(s) (<@field type="submit"> or manual <input, <a, <button) to include
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
<#macro field type="" label="" labelDetail="" name="" value="" valueType="" currentValue="" defaultValue="" class=true size=20 maxlength="" id="" onClick="" 
        disabled=false placeholder="" autoCompleteUrl="" mask=false alert="false" readonly=false rows="4" 
        cols="50" dateType="date" multiple="" checked=false collapse=false tooltip="" columns="" norows=false nocells=false nocontainer=""
        fieldFormName="" formName="" formId="" postfix=false postfixSize=1 required=false items=[] autocomplete=true progressOptions={} 
        labelType="" labelLayout="" labelArea="" description=""
        submitType="input" text="" href="" src="" confirmMsg="">
<#if !type?has_content>
  <#local type = "generic">
</#if>
<#if !valueType?has_content>
  <#local valueType = "generic">
</#if>
<#-- treat these as synonyms for now -->
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

<#local radioSingle = (type=="radio" && !items?has_content)>

<#if !id?has_content>
    <#-- FIXME? renderSeqNumber usually empty... where come from? should be as request attribute also? -->
    <#local id = "field_id_${renderSeqNumber!}_${fieldIdNum!0}">
</#if>

<#-- NOTE: here, "classes" is for @cell container; "class" is for input elem (not same as other macros)! 
     can't allow specify @cell container classes as-is because can't calculate from it, would need columns param as int -->
<#local classes = "${styles.grid_large!}12"/>
<#local columnspostfix = 0/>
<#if postfix>
    <#local columnspostfix = postfixSize/>
    <#local collapse = true/>
    <#local classes = "${styles.grid_small!}${12-columnspostfix} ${styles.grid_large!}${12-columnspostfix}"/>
</#if>

<#local class = makeClassesArg(class, "")>
<#if required && (!containsStyleName(class, "required"))>
    <#local class = (class + " required")?trim>
</#if>

<#if !catoFieldNoContainerChildren??>
  <#global catoFieldNoContainerChildren = {
   <#-- "submit":true --> <#-- only if parent is submitarea -->
  }>
  <#global catoFieldNoContainerParent = {
    "submitarea":true
  }>
</#if>
<#if !nocontainer?is_boolean>
  <#if nocontainer?has_content>
    <#local nocontainer = nocontainer?boolean>
  <#elseif isChildField && (catoFieldNoContainerChildren[type]?? || catoFieldNoContainerParent[parentFieldInfo.type!]??)>
    <#local nocontainer = true>
  <#else> 
    <#local nocontainer = false>
  </#if>
</#if>

<#local fieldEntryTypeClass = "field-entry-type-" + mapCatoFieldTypeToStyleName(type)>
<@row collapse=collapse!false norows=(norows || nocontainer) class=("+form-field-entry "+fieldEntryTypeClass)>

    <#-- TODO: right now most of the fieldsInfo parameters are not fully exploited.
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

    <#if (labelArea?is_boolean && labelArea == true) || 
         (!(labelArea?is_boolean && labelArea == false) && (label?has_content || labelDetail?has_content || labelAreaDefault))>
        <#local subclasses="${styles.grid_small!}3 ${styles.grid_large!}2"/>
        <#local classes="${styles.grid_small!}${9-columnspostfix} ${styles.grid_large!}${10-columnspostfix}"/>
        
        <#if columns?has_content>
            <#local subclasses="${styles.grid_small!}${12-columns+1} ${styles.grid_large!}${12-columns}"/>
            <#local classes="${styles.grid_small!}${columns-columnspostfix-1} ${styles.grid_large!}${columns-columnspostfix}"/>
        </#if>
        
        <#if !radioSingle>
            <@cell class=(subclasses+" field-entry-title "+fieldEntryTypeClass)?trim nocells=(nocells || nocontainer)>
              <#if label?has_content>
                <#if type=="checkbox" || collapse==false>
                    <label class="form-field-label"<#if id?has_content> for="${id}"</#if>>${label}</label>
                <#else>
                    <span class="${styles.prefix!} form-field-label">${label}</span>
                </#if>  
                <#-- TODO: probably remove asterix later; need for dev -->
                <#if required>*</#if>
              </#if> 
              <#if labelDetail?has_content>
                ${labelDetail}
              </#if>    
            </@cell>
        </#if>
    </#if>
    <@cell class=("${classes!}"+" field-entry-widget "+fieldEntryTypeClass)?trim nocells=(nocells || nocontainer)>
        <#switch type>
          <#case "input">
            <@field_input_widget name=name 
                                  className=class 
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
                                  className=class 
                                  alert=alert 
                                  cols=cols 
                                  rows=rows 
                                  id=id 
                                  readonly=readonly 
                                  value=value 
                                  placeholder=placeholder
                                  tooltip=tooltip/>
            <#break>
          <#case "datetime">
            <#if dateType == "date"><#local shortDateInput=true/><#else><#local shortDateInput=false/></#if>
            <@field_datetime_widget name=name 
                                  className=class 
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
                                    className=class 
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
            <@field_lookup_widget name=name formName=formName fieldFormName=fieldFormName className=class alert="false" value=value size=size?string maxlength=maxlength id=id event="onClick" action=onClick />
          <#break>
          <#case "checkbox">
                <@field_checkbox_widget id=id currentValue=value checked=checked name=name action=action />
            <#break>
          <#case "radio">
                <#if radioSingle>
                    <#-- single radio button item mode -->
                    <#local items=[{"key":value, "description":label!""}]/>
                    <@field_radio_widget items=items className=class alert=alert currentValue=(checked?string(value,"")) noCurrentSelectedKey="" name=name event="" action="" tooltip=tooltip />
                <#else>
                    <#-- multi radio button item mode -->
                    <div<@fieldClassStr class=class alert=alert />>
                      <@field_radio_widget items=items className="" alert=alert currentValue=currentValue noCurrentSelectedKey=defaultValue name=name event="" action="" tooltip=tooltip />
                    </div>
                </#if>
            <#break>
          <#case "file">
            <@field_file_widget className=class alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete?string("", "off") id=id />
            <#break> 
          <#case "password">
            <@field_password_widget className=class alert=alert name=name value=value size=size maxlength=maxlength id=id autocomplete=autocomplete?string("", "off") placeholder=placeholder tooltip=tooltip/>
            <#break> 
          <#case "submit">
          <#case "submitarea">
            <#local hasProgress = (progressOptions.formSel)?has_content>
            <#local content>
              <#if type == "submit">
                <#if !catoSubmitFieldTypeButtonMap??>
                  <#global catoSubmitFieldButtonTypeMap = {
                    "submit":"button", "button":"button", "link":"text-link", "image":"image"
                  }>
                  <#global catoSubmitFieldInputTypeMap = {
                    "submit":"submit", "button":"button", "link":"", "image":"image"
                  }>
                </#if>
                <#local buttonType = catoSubmitFieldButtonTypeMap[submitType]!"button">
                <#local inputType = catoSubmitFieldInputTypeMap[submitType]!"">
                <@field_submit_widget buttonType=buttonType className=class alert=alert formName=formName name=name event="" action="" imgSrc=src confirmation=confirmMsg containerId="" ajaxUrl="" title=text showProgress=false onClick=onClick href=href inputType=inputType disabled=disabled />
              <#else>
                <#nested>
              </#if>
            </#local>
            <#if hasProgress>
              <@fieldSubmitAreaProgress progressOptions=progressOptions nestedContent=content />
            <#else>
              ${content}
            </#if>
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
                <@field_display_widget type=displayType imageLocation=imageLocation idName="" description=desc title="" class=class alert=alert inPlaceEditorUrl="" inPlaceEditorParams="" imageAlt=description/>
                <#-- FIXME: tooltip too crappy -->
              <#if tooltip?has_content>
                <span class="tooltip">${tooltip}</span>
              </#if>
            <#break> 
          <#default> <#-- "generic", empty or unrecognized -->
            <#if value?has_content>
                <@field_generic_widget text=value/>
            <#else>
                <#nested />
            </#if>
            <#-- FIXME: tooltip too crappy -->
            <#if tooltip?has_content>
              <span class="tooltip">${tooltip}</span>
            </#if>
        </#switch>
     </@cell>
     <#if postfix && !nocells>
         <@cell class="${styles.grid_small!}${postfixSize} ${styles.grid_large!}${postfixSize}">
                <span class="postfix"><input type="submit" class="${styles.icon!} ${styles.icon_button!}" value="${styles.icon_button_value!}"/></span>
         </@cell>
     </#if>
</@row>
<#-- pop field info when done -->
<#local dummy = popRequestStack("catoCurrentFieldInfo")>
</#macro>

<#-- migrated from @renderClass form widget macro -->
<#macro fieldClassStr class alert=false>
  <#if class?has_content || alert?string == "true"> class="${class!}<#if alert?string == "true"> alert</#if>"</#if><#t>
</#macro>

<#-- migrated from @renderSubmitFieldAreaProgress form widget macro -->
<#macro fieldSubmitAreaProgress progressOptions nestedContent=true>
  <#if !nestedContent?is_string>
    <#if nestedContent?is_boolean && nestedContent == false>
      <#local nestedContent = "">
    <#else>
      <#local nestedContent><#nested></#local>
    </#if>
  </#if>

  <#local rowClass>submit-progress-row<#if buttonMarkup?has_content> has-submit-button<#else> no-submit-button</#if></#local>
  <@row class=("+" + rowClass)>
    <#if nestedContent?has_content>
      <@cell class="${styles.grid_small!}3 ${styles.grid_large!}2">
        ${nestedContent}
      </@cell>
    </#if>
    <#if progressOptions.progBarId?has_content>
      <#-- with progress bar, optional text -->
      <#local subclasses = progressOptions.progTextBoxId?has_content?string("${styles.grid_small!}6 ${styles.grid_large!}6", "${styles.grid_small!}9 ${styles.grid_large!}10 ${styles.grid_end!}")>
      <@cell class=subclasses>
        <@progress id=progressOptions.progBarId type="info" wrapperClass="+${styles.hidden!}" progressOptions=progressOptions/>
      </@cell>
      <#if progressOptions.progTextBoxId?has_content>
        <#local subclasses = "${styles.grid_small!}3 ${styles.grid_large!}4 ${styles.grid_end!}">
        <@cell class=subclasses id=progressOptions.progTextBoxId>
        </@cell>
      </#if>
    <#elseif progressOptions.progTextBoxId?has_content>
       <#-- text progress only -->
       <#local subclasses = "${styles.grid_small!}9 ${styles.grid_large!}10 ${styles.grid_end!}">
       <@cell class=subclasses id=progressOptions.progTextBoxId>
       </@cell>
       <@progressScript options=progressOptions htmlwrap=true />
    </#if>
  </@row>
</#macro>


<#-- INDIVIDUAL FIELD IMPLEMENTATIONS
     TODO: the parameters on these should be refined to be less ofbiz-like and more cato-like; but must
        be flexible enough to 100% allow calls from ofbiz widget lib macros.
     TODO: the _widget macros must further be split up into
        _widget_markup macros, which explicitly contain as little markup as possible, and should also
        try to avoid all of the macro argument ofbiz-isms.
        the _widget are called by the Ofbiz macro libs and may need to contain logic code, so they're
        not suited to be markup macros, and their args are too ofbiz-ish. -->

<#-- migrated from @renderTextField form widget macro -->
<#macro field_input_widget name="" className="" alert="" value="" textSize="" maxlength="" id="" event="" action="" disabled=false ajaxUrl="" ajaxEnabled=false 
    mask=false clientAutocomplete="" placeholder="" tooltip="" collapse=false readonly=false fieldTitleBlank=false>
  <@field_input_markup_widget name=name className=className alert=alert value=value textSize=textSize maxlength=maxlength id=id event=event action=action disabled=disabled ajaxUrl=ajaxUrl ajaxEnabled=ajaxEnabled 
    mask=mask clientAutocomplete=clientAutocomplete placeholder=placeholder tooltip=tooltip collapse=collapse readonly=readonly fieldTitleBlank=fieldTitleBlank><#nested></@field_input_markup_widget>
</#macro>

<#macro field_input_markup_widget name="" className="" alert="" value="" textSize="" maxlength="" id="" event="" action="" disabled=false ajaxUrl="" ajaxEnabled=false 
    mask=false clientAutocomplete="" placeholder="" tooltip="" collapse=false readonly=false fieldTitleBlank=false>
  <#if tooltip?has_content> 
     <#local className = (className + " has-tip tip-right")/>  
  </#if>
  <#if mask?has_content && mask>
    <script type="text/javascript">
      jQuery(function($){jQuery("#${id}").mask("${mask!}");});
    </script>
  </#if>
  <input type="text" name="${name?default("")?html}"<#t/>
    <#if tooltip?has_content> 
     data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="${tooltip!}"<#rt/>
     </#if><#rt/>
    <@fieldClassStr class=className alert=alert />
    <#if value?has_content> value="${value}"</#if><#rt/>
    <#if textSize?has_content> size="${textSize}"</#if><#rt/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#rt/>
    <#if disabled?has_content && disabled> disabled="disabled"</#if><#rt/>
    <#if readonly?has_content && readonly> readonly="readonly"</#if><#rt/>
    <#if id?has_content> id="${id}"</#if><#rt/>
    <#if event?has_content && action?has_content> ${event}="${action}"</#if><#rt/>
    <#if clientAutocomplete?has_content && clientAutocomplete=="false"> autocomplete="off"</#if><#rt/>
    <#if placeholder?has_content> placeholder="${placeholder}"</#if><#rt/>
  /><#t/>
  <#if ajaxUrl?has_content>
    <#local defaultMinLength = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultMinLength")>
    <#local defaultDelay = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultDelay")>
    <script language="JavaScript" type="text/javascript">ajaxAutoCompleter('${ajaxUrl}', false, ${defaultMinLength!2}, ${defaultDelay!300});</script><#lt/>
  </#if>
</#macro>

<#-- migrated from @renderTextareaField form widget macro -->
<#macro field_textarea_widget name="" className="" alert="" cols="" rows="" id="" readonly="" value="" visualEditorEnable=true 
    buttons="" language="" placeholder="" tooltip="" title="" fieldTitleBlank=false collapse=false>
  <@field_textarea_markup_widget name=name className=className alert=alert cols=cols rows=rows id=id readonly=readonly value=value visualEditorEnable=visualEditorEnable 
    buttons=buttons language=language placeholder=placeholder tooltip=tooltip title=title fieldTitleBlank=fieldTitleBlank collapse=collapse><#nested></@field_textarea_markup_widget>
</#macro>

<#macro field_textarea_markup_widget name="" className="" alert="" cols="" rows="" id="" readonly="" value="" visualEditorEnable=true 
    buttons="" language="" placeholder="" tooltip="" title="" fieldTitleBlank=false collapse=false>
  <#if tooltip?has_content> 
     <#local className = (className+ " has-tip tip-right")/>  
  </#if>
  <textarea name="${name}"<#t/>
    <#if tooltip?has_content> data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
    <@fieldClassStr class=className alert=alert />
    <#if cols?has_content> cols="${cols}"</#if><#rt/>
    <#if rows?has_content> rows="${rows}"</#if><#rt/>
    <#if id?has_content> id="${id}"</#if><#rt/>
    <#if readonly?has_content> readonly="readonly"</#if><#rt/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#rt/>
    <#if placeholder?has_content> placeholder="${placeholder}"</#if><#t/>
    ><#t/>
    <#if value?has_content>${value}</#if>
  </textarea><#lt/>
  
  <#--
  ToDo: Remove
  <#if visualEditorEnable?has_content>
    <script language="javascript" src="/images/jquery/plugins/elrte-1.3/js/elrte.min.js" type="text/javascript"></script><#rt/>
    <#if language?has_content && language != "en">
      <script language="javascript" src="/images/jquery/plugins/elrte-1.3/js/i18n/elrte.${language!"en"}.js" type="text/javascript"></script><#rt/>
    </#if>
    <link href="/images/jquery/plugins/elrte-1.3/css/elrte.min.css" rel="stylesheet" type="text/css">
    <script language="javascript" type="text/javascript">
      var opts = {
         cssClass : 'el-rte',
         lang     : '${language!"en"}',
         toolbar  : '${buttons?default("maxi")}',
         doctype  : '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">', //'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN">',
         cssfiles : ['/images/jquery/plugins/elrte-1.3/css/elrte-inner.css']
      }
      jQuery('#${id?default("")}').elrte(opts);
    </script>
  </#if>
  -->
</#macro>

<#-- Merges a yyyy-MM-dd into a full timestamp
     TODO: move this out to js file -->
<#assign mergeStdDateTimeJs>
                var mergeStdDateTime = function(oldDate, newDate) {
                    var result;
                    if (oldDate.match(/^\d\d\d\d-\d\d-\d\d\s/)) {
                       if (newDate.length >= oldDate.length) {
                           result = newDate;
                       }
                       else {
                           <#-- preserve everything after days -->
                           result = newDate + oldDate.substr(newDate.length);
                       }
                    }
                    else {
                       var zeroPat = "0000-00-00 00:00:00.000";
                       if (newDate.length >= zeroPat.length) {
                           result = newDate;
                       }
                       else {
                           <#-- append zeroes -->
                           result = newDate + zeroPat.substr(newDate.length);
                       }
                    }
                    return result;
                };
</#assign>

<#-- migrated from @renderDateTimeField form widget macro -->
<#macro field_datetime_widget name="" className="" title="" value="" size="" maxlength="" id="" dateType="" shortDateInput=false 
    timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" 
    hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName="" 
    alert=false mask="" event="" action="" step="" timeValues="" tooltip="" collapse=false fieldTitleBlank=false>
  <@field_datetime_markup_widget name=name className=className title=title value=value size=size maxlength=maxlength id=id dateType=dateType shortDateInput=shortDateInput 
    timeDropdownParamName=timeDropdownParamName defaultDateTimeString=defaultDateTimeString localizedIconTitle=localizedIconTitle timeDropdown=timeDropdown timeHourName=timeHourName classString=classString 
    hour1=hour1 hour2=hour2 timeMinutesName=timeMinutesName minutes=minutes isTwelveHour=isTwelveHour ampmName=ampmName amSelected=amSelected pmSelected=pmSelected compositeType=compositeType formName=formName 
    alert=alert mask=mask event=event action=action step=step timeValues=timeValues tooltip=tooltip collapse=false fieldTitleBlank=fieldTitleBlank><#nested></@field_datetime_markup_widget>
</#macro>

<#macro field_datetime_markup_widget name="" className="" title="" value="" size="" maxlength="" id="" dateType="" shortDateInput=false 
    timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" 
    hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName="" 
    alert=false mask="" event="" action="" step="" timeValues="" tooltip="" collapse=false fieldTitleBlank=false>
  
  <#local fdatepickerOptions>{format:"yyyy-mm-dd", forceParse:false}</#local>
  <#-- Note: ofbiz never handled dateType=="date" here because it pass shortDateInput=true in renderer instead-->
  <#-- These should be ~uiLabelMap.CommonFormatDate/Time/DateTime -->
  <#local dateFormat><#if (shortDateInput!false) == true>yyyy-MM-dd<#elseif dateType=="time">HH:mm:ss.SSS<#else>yyyy-MM-dd HH:mm:ss.SSS</#if></#local>
  <#local useTsFormat = (((shortDateInput!false) == false) && dateType!="time")>

  <div class="${styles.grid_row!} ${styles.collapse!} date" data-date="" data-date-format="${dateFormat}">
        <div class="${styles.grid_small!}11 ${styles.grid_cell!}">
          <#if dateType == "time">
            <input type="text" name="${name}"<@fieldClassStr class=className alert=alert /><#rt/>
            <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
            <#if title?has_content> title="${title}"</#if>
            <#if value?has_content> value="${value}"</#if>
            <#if size?has_content> size="${size}"</#if><#rt/>
            <#if maxlength?has_content>  maxlength="${maxlength}"</#if>
            <#if id?has_content> id="${id}"</#if> class="${styles.grid_small!}3 ${styles.grid_cell!}"/><#rt/>
          <#else>
            <input type="text" name="${name}_i18n"<@fieldClassStr class=className alert=alert /><#rt/>
            <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
            <#if title?has_content> title="${title}"</#if>
            <#if value?has_content> value="${value}"</#if>
            <#if size?has_content> size="${size}"</#if><#rt/>
            <#if maxlength?has_content>  maxlength="${maxlength}"</#if>
            <#if id?has_content> id="${id}_i18n"</#if> class="${styles.grid_small!}3 ${styles.grid_cell!}"/><#rt/>

            <input type="hidden" name="${name}"<#if id?has_content> id="${id}"</#if><#if value?has_content> value="${value}"</#if> />
          </#if>
        </div>
        <div class="${styles.grid_small!}1 ${styles.grid_cell!}">
        <span class="postfix"><i class="${styles.icon!} ${styles.icon_calendar!}"></i></span>
        </div>
      <#if dateType != "time">
        <script type="text/javascript">
            $(function() {

                var dateI18nToNorm = function(date) {
                    <#-- TODO (note: depends on dateType) -->
                    return date;
                };
                
                var dateNormToI18n = function(date) {
                    <#-- TODO (note: depends on dateType) -->
                    return date;
                };
            
                jQuery("#${id}_i18n").change(function() {
                    jQuery("#${id}").val(dateI18nToNorm(this.value));
                });
                
              <#if useTsFormat>
                ${mergeStdDateTimeJs}
              </#if>
                
                var oldDate = "";
                var onFDatePopup = function(ev) {
                    oldDate = dateI18nToNorm(jQuery("#${id}_i18n").val());
                };
                var onFDateChange = function(ev) {
                  <#if useTsFormat>
                    jQuery("#${id}_i18n").val(dateNormToI18n(mergeStdDateTime(oldDate, dateI18nToNorm(jQuery("#${id}_i18n").val()))));
                  </#if>
                };
                
                <#if name??>
                    <#local dateElemJs>$("input[name='${name?html}_i18n']")</#local>
                <#else>
                    <#local dateElemJs>$("input")</#local>
                </#if>
                ${dateElemJs}.fdatepicker(${fdatepickerOptions}).on('changeDate', onFDateChange).on('show', onFDatePopup);
            });
        </script>
      </#if>
  </div>
</#macro>

<#-- migrated from @renderDateFindField form widget macro -->
<#macro field_datefind_widget className="" alert="" name="" localizedInputTitle="" value="" value2="" size="" maxlength="" dateType="" 
    formName="" defaultDateTimeString="" imgSrc="" localizedIconTitle="" titleStyle="" defaultOptionFrom="" defaultOptionThru="" 
    opEquals="" opSameDay="" opGreaterThanFromDayStart="" opGreaterThan="" opGreaterThan="" opLessThan="" opUpToDay="" opUpThruDay="" opIsEmpty="">
  <@field_datefind_markup_widget className=className alert=alert name=name localizedInputTitle=localizedInputTitle value=value value2=value2 size=size maxlength=maxlength dateType=dateType 
    formName=formName defaultDateTimeString=defaultDateTimeString imgSrc=imgSrc localizedIconTitle=localizedIconTitle titleStyle=titleStyle defaultOptionFrom=defaultOptionFrom defaultOptionThru=defaultOptionThru 
    opEquals=opEquals opSameDay=opSameDay opGreaterThanFromDayStart=opGreaterThanFromDayStart opGreaterThan=opGreaterThan opGreaterThan=opGreaterThan opLessThan=opLessThan opUpToDay=opUpToDay opUpThruDay=opUpThruDay opIsEmpty=opIsEmpty><#nested></@field_datefind_markup_widget>
</#macro>

<#macro field_datefind_markup_widget className="" alert="" name="" localizedInputTitle="" value="" value2="" size="" maxlength="" dateType="" 
    formName="" defaultDateTimeString="" imgSrc="" localizedIconTitle="" titleStyle="" defaultOptionFrom="" defaultOptionThru="" 
    opEquals="" opSameDay="" opGreaterThanFromDayStart="" opGreaterThan="" opGreaterThan="" opLessThan="" opUpToDay="" opUpThruDay="" opIsEmpty="">

  <#local fdatepickerOptions>{format:"yyyy-mm-dd", forceParse:false}</#local>
  <#-- note: values of localizedInputTitle are: uiLabelMap.CommonFormatDate/Time/DateTime -->
  <#local dateFormat><#if dateType == "date">yyyy-MM-dd<#elseif dateType=="time">HH:mm:ss.SSS<#else>yyyy-MM-dd HH:mm:ss.SSS</#if></#local>
  <#local useTsFormat = (dateType != "date" && dateType != "time")>
  
  <div class="${styles.grid_row!} ${styles.collapse!} date" data-date="" data-date-format="${dateFormat}">
        <div class="${styles.grid_small!}5 ${styles.grid_cell!}">
        <input class="${styles.grid_small!}3 ${styles.grid_cell!}" id="${name?html}_fld0_value" type="text"<@fieldClassStr class=className alert=alert /><#if name?has_content> name="${name?html}_fld0_value"</#if><#if localizedInputTitle?has_content> title="${localizedInputTitle}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if>/><#rt/>
        </div>
        <div class="${styles.grid_small!}1 ${styles.grid_cell!}">
        <span class="postfix"><i class="${styles.icon} ${styles.icon_calendar!}"></i></span>
        </div>
        <div class="${styles.grid_small!}5 ${styles.grid_cell!} ${styles.grid_small!}offset-1">
        <select<#if name?has_content> name="${name}_fld0_op"</#if> class="selectBox"><#rt/>
          <option value="equals"<#if defaultOptionFrom=="equals"> selected="selected"</#if>>${opEquals}</option><#rt/>
          <option value="sameDay"<#if defaultOptionFrom=="sameDay"> selected="selected"</#if>>${opSameDay}</option><#rt/>
          <option value="greaterThanFromDayStart"<#if defaultOptionFrom=="greaterThanFromDayStart"> selected="selected"</#if>>${opGreaterThanFromDayStart}</option><#rt/>
          <option value="greaterThan"<#if defaultOptionFrom=="greaterThan"> selected="selected"</#if>>${opGreaterThan}</option><#rt/>
        </select><#rt/>
        </div>
      <#if dateType != "time">
        <script type="text/javascript">
            $(function() {
            
              <#if useTsFormat>
                ${mergeStdDateTimeJs}
              </#if>
                
                var oldDate = "";
                var onFDatePopup = function(ev) {
                    oldDate = jQuery("#${name?html}_fld0_value").val();
                };
                var onFDateChange = function(ev) {
                  <#if useTsFormat>
                    jQuery("#${name?html}_fld0_value").val(mergeStdDateTime(oldDate, jQuery("#${name?html}_fld0_value").val()));
                  </#if>
                };
            
                <#if name??>
                    <#local dateElemJs>$('#${name?html}_fld0_value')</#local>
                <#else>
                    <#local dateElemJs>$('input')</#local>
                </#if>
                ${dateElemJs}.fdatepicker(${fdatepickerOptions}).on('changeDate', onFDateChange).on('show', onFDatePopup);
            });
        </script>
      </#if>
  </div>
</#macro>

<#-- migrated from @renderDropDownField form widget macro -->
<#macro field_select_widget name="" className="" alert="" id="" multiple="" formName="" otherFieldName="" size="" firstInList="" 
    currentValue="" explicitDescription="" allowEmpty="" options="" fieldName="" otherFieldName="" otherValue="" otherFieldSize="" 
    dDFCurrent="" noCurrentSelectedKey="" ajaxOptions="" frequency="" minChars="" choices="" autoSelect="" partialSearch="" partialChars="" 
    ignoreCase="" fullSearch="" event="" action="" ajaxEnabled=false tooltip="" manualItems=false manualItemsOnly=false 
    collapse=false fieldTitleBlank=false>
  <@field_select_markup_widget name=name className=className alert=alert id=id multiple=multiple formName=formName otherFieldName=otherFieldName size=size firstInList=firstInList 
    currentValue=currentValue explicitDescription=explicitDescription allowEmpty=allowEmpty options=options fieldName=fieldName otherFieldName=otherFieldName otherValue=otherValue otherFieldSize=otherFieldSize 
    dDFCurrent=dDFCurrent noCurrentSelectedKey=noCurrentSelectedKey ajaxOptions=ajaxOptions frequency=frequency minChars=minChars choices=choices autoSelect=autoSelect partialSearch=partialSearch partialChars=partialChars 
    ignoreCase=ignoreCase fullSearch=fullSearch event=event action=action ajaxEnabled=ajaxEnabled tooltip=tooltip manualItems=manualItems manualItemsOnly=manualItemsOnly 
    collapse=collapse fieldTitleBlank=fieldTitleBlank><#nested></@field_select_markup_widget>
</#macro>

<#macro field_select_markup_widget name="" className="" alert="" id="" multiple="" formName="" otherFieldName="" size="" firstInList="" 
    currentValue="" explicitDescription="" allowEmpty="" options="" fieldName="" otherFieldName="" otherValue="" otherFieldSize="" 
    dDFCurrent="" noCurrentSelectedKey="" ajaxOptions="" frequency="" minChars="" choices="" autoSelect="" partialSearch="" partialChars="" 
    ignoreCase="" fullSearch="" event="" action="" ajaxEnabled=false tooltip="" manualItems=false manualItemsOnly=false 
    collapse=false fieldTitleBlank=false>

    <select name="${name!""}<#rt/>"<@fieldClassStr class=className alert=alert /><#if id?has_content> id="${id}"</#if><#if multiple?has_content> multiple="multiple"</#if><#if otherFieldSize gt 0> onchange="process_choice(this,document.${formName}.${otherFieldName})"</#if><#if event?has_content> ${event}="${action}"</#if><#--<#if size?has_content> size="${size}"</#if>-->
    <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>>
    <#if !manualItemsOnly>  
      <#if firstInList?has_content && currentValue?has_content && !multiple?has_content>
        <option selected="selected" value="${currentValue}">${explicitDescription}</option><#rt/>
        <option value="${currentValue}">---</option><#rt/>
      </#if>
      <#if allowEmpty?has_content || (!manualItems && !options?has_content)>
        <option value="">&nbsp;</option>
      </#if>
      <#list options as item>
        <#if multiple?has_content>
          <option<#if currentValue?has_content && item.selected?has_content> selected="${item.selected}" <#elseif !currentValue?has_content && noCurrentSelectedKey?has_content && noCurrentSelectedKey == item.key> selected="selected" </#if> value="${item.key}">${item.description}</option><#rt/>
        <#else>
          <option<#if currentValue?has_content && currentValue == item.key && dDFCurrent?has_content && "selected" == dDFCurrent> selected="selected"<#elseif !currentValue?has_content && noCurrentSelectedKey?has_content && noCurrentSelectedKey == item.key> selected="selected"</#if> value="${item.key}">${item.description}</option><#rt/>
        </#if>
      </#list>
    </#if>
      <#nested>
    </select>
  <#if otherFieldName?has_content>
    <noscript><input type='text' name='${otherFieldName}' /></noscript>
    <script type='text/javascript' language='JavaScript'><!--
      disa = ' disabled';
      if(other_choice(document.${formName}.${fieldName}))
        disa = '';
      document.write("<input type='text' name='${otherFieldName}' value='${otherValue?js_string}' size='${otherFieldSize}'"+disa+" onfocus='check_choice(document.${formName}.${fieldName})' />");
      if(disa && document.styleSheets)
      document.${formName}.${otherFieldName}.styles.visibility  = 'hidden';
    //--></script>
  </#if>

  <#if ajaxEnabled>
    <script language="JavaScript" type="text/javascript">
      ajaxAutoCompleteDropDown();
      jQuery(function() {
        jQuery("#${id}").combobox();
      });
    </script>
  </#if>
</#macro>

<#-- migrated from @renderLookupField form widget macro -->
<#macro field_lookup_widget name="" formName="" fieldFormName="" className="" alert="false" value="" size="" 
    maxlength="" id="" event="" action="" readonly=false autocomplete="" descriptionFieldName="" 
    targetParameterIter="" imgSrc="" ajaxUrl="" ajaxEnabled=javaScriptEnabled presentation="layer" width="" 
    height="" position="" fadeBackground="true" clearText="" showDescription="" initiallyCollapsed="" 
    lastViewName="main" title="" fieldTitleBlank=false>
  <@field_lookup_markup_widget name=name formName=formName fieldFormName=fieldFormName className=className alert=alert value=value size=size 
    maxlength=maxlength id=id event=event action=action readonly=readonly autocomplete=autocomplete descriptionFieldName=descriptionFieldName 
    targetParameterIter=targetParameterIter imgSrc=imgSrc ajaxUrl=ajaxUrl ajaxEnabled=ajaxEnabled presentation=presentation width=width 
    height=height position=position fadeBackground=fadeBackground clearText=clearText showDescription=showDescription initiallyCollapsed=initiallyCollapsed 
    lastViewName=lastViewName title=title fieldTitleBlank=fieldTitleBlank><#nested></@field_lookup_markup_widget>
</#macro>

<#macro field_lookup_markup_widget name="" formName="" fieldFormName="" className="" alert="false" value="" size="" 
    maxlength="" id="" event="" action="" readonly=false autocomplete="" descriptionFieldName="" 
    targetParameterIter="" imgSrc="" ajaxUrl="" ajaxEnabled=javaScriptEnabled presentation="layer" width="" 
    height="" position="" fadeBackground="true" clearText="" showDescription="" initiallyCollapsed="" 
    lastViewName="main" title="" fieldTitleBlank=false>

  <#if Static["org.ofbiz.widget.model.ModelWidget"].widgetBoundaryCommentsEnabled(context)>
  </#if>
  <#if (!ajaxUrl?has_content) && ajaxEnabled?has_content && ajaxEnabled>
    <#local ajaxUrl = requestAttributes._REQUEST_HANDLER_.makeLink(request, response, fieldFormName)/>
    <#local ajaxUrl = id + "," + ajaxUrl + ",ajaxLookup=Y" />
  </#if>
  <#if (!showDescription?has_content)>
    <#local showDescriptionProp = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.lookup.showDescription", "N")>
    <#if "Y" == showDescriptionProp>
      <#local showDescription = "true" />
    <#else>
      <#local showDescription = "false" />
    </#if>
  </#if>
  <#if ajaxEnabled?has_content && ajaxEnabled>
    <script type="text/javascript">
      jQuery(document).ready(function(){
        if (!jQuery('form[name="${formName}"]').length) {
          alert("Developer: for lookups to work you must provide a form name!")
        }
      });
    </script>
  </#if>
  <span class="field-lookup">
    <#if size?has_content && size=="0">
      <input type="hidden" <#if name?has_content> name="${name}"/></#if>
    <#else>
      <input type="text"<@fieldClassStr class=className alert=alert /><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if>
        <#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if id?has_content> id="${id}"</#if><#rt/>
        <#if readonly?has_content && readonly> readonly="readonly"</#if><#rt/><#if event?has_content && action?has_content> ${event}="${action}"</#if><#rt/>
        <#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/></#if>
    <#if presentation?has_content && descriptionFieldName?has_content && presentation == "window">
      <a href="javascript:call_fieldlookup3(document.${formName?html}.${name?html},document.${formName?html}.${descriptionFieldName},'${fieldFormName}', '${presentation}'<#rt/>
      <#if targetParameterIter?has_content>
        <#list targetParameterIter as item>
          ,document.${formName}.${item}.value<#rt>
        </#list>
      </#if>
      );"></a><#rt>
    <#elseif presentation?has_content && presentation == "window">
      <a href="javascript:call_fieldlookup2(document.${formName?html}.${name?html},'${fieldFormName}', '${presentation}'<#rt/>
      <#if targetParameterIter?has_content>
        <#list targetParameterIter as item>
          ,document.${formName}.${item}.value<#rt>
        </#list>
      </#if>
      );"></a><#rt>
    <#else>
      <#if ajaxEnabled?has_content && ajaxEnabled>
        <#local defaultMinLength = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultMinLength")>
        <#local defaultDelay = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultDelay")>
        <#local ajaxUrl = ajaxUrl + "&amp;_LAST_VIEW_NAME_=" + lastViewName />
        <#if !ajaxUrl?contains("searchValueFieldName=")>
          <#if descriptionFieldName?has_content && showDescription == "true">
            <#local ajaxUrl = ajaxUrl + "&amp;searchValueFieldName=" + descriptionFieldName />
          <#else>
            <#local ajaxUrl = ajaxUrl + "&amp;searchValueFieldName=" + name />
          </#if>
        </#if>
      </#if>
      <script type="text/javascript">
        jQuery(document).ready(function(){
          var options = {
            requestUrl : "${fieldFormName}",
            inputFieldId : "${id}",
            dialogTarget : document.${formName?html}.${name?html},
            dialogOptionalTarget : <#if descriptionFieldName?has_content>document.${formName?html}.${descriptionFieldName}<#else>null</#if>,
            formName : "${formName?html}",
            width : "${width}",
            height : "${height}",
            position : "${position}",
            modal : "${fadeBackground}",
            ajaxUrl : <#if ajaxEnabled?has_content && ajaxEnabled>"${ajaxUrl}"<#else>""</#if>,
            showDescription : <#if ajaxEnabled?has_content && ajaxEnabled>"${showDescription}"<#else>false</#if>,
            presentation : "${presentation!}",
            defaultMinLength : "${defaultMinLength!2}",
            defaultDelay : "${defaultDelay!300}",
            args :
              <#rt/>
                <#if targetParameterIter?has_content>
                  <#local isFirst = true>
                  <#lt/>[<#rt/>
                  <#list targetParameterIter as item>
                    <#if isFirst>
                      <#lt/>document.${formName}.${item}<#rt/>
                      <#local isFirst = false>
                    <#else>
                      <#lt/> ,document.${formName}.${item}<#rt/>
                    </#if>
                  </#list>
                  <#lt/>]<#rt/>
                <#else>[]
                </#if>
                <#lt/>
          };
          new Lookup(options).init();
        });
      </script>
    </#if>
    <#if readonly?has_content && readonly>
      <a id="${id}_clear" 
        style="background:none;margin-left:5px;margin-right:15px;" 
        class="clearField" 
        href="javascript:void(0);" 
        onclick="javascript:document.${formName}.${name}.value='';
          jQuery('#' + jQuery('#${id}_clear').next().attr('id').replace('_button','') + '_${id}_lookupDescription').html('');
          <#if descriptionFieldName?has_content>document.${formName}.${descriptionFieldName}.value='';</#if>">
          <#if clearText?has_content>${clearText}<#else>${uiLabelMap.CommonClear}</#if>
      </a>
    </#if>
  </span>
  <#if ajaxEnabled?has_content && ajaxEnabled && (presentation?has_content && presentation == "window")>
    <#if ajaxUrl?index_of("_LAST_VIEW_NAME_") < 0>
      <#local ajaxUrl = ajaxUrl + "&amp;_LAST_VIEW_NAME_=" + lastViewName />
    </#if>
    <script language="JavaScript" type="text/javascript">ajaxAutoCompleter('${ajaxUrl}', ${showDescription}, ${defaultMinLength!2}, ${defaultDelay!300});</script><#t/>
  </#if>
</#macro>

<#-- migrated from @renderCheckBox form widget macro -->
<#macro field_checkbox_widget id="" checked=false currentValue="N" name="" action="" tooltip="" fieldTitleBlank=false>
  <@field_checkbox_markup_widget id=id checked=checked currentValue=currentValue name=name action=action tooltip=tooltip fieldTitleBlank=fieldTitleBlank><#nested></@field_checkbox_markup_widget>
</#macro>

<#macro field_checkbox_markup_widget id="" checked=false currentValue="N" name="" action="" tooltip="" fieldTitleBlank=false>
    <div class="switch small">
    <input type="checkbox" id="<#if id?has_content>${id}<#else>${name!}</#if>"<#rt/>
      <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
      <#if (checked?is_boolean && checked) || (checked?is_string && checked == "Y")> checked="checked"
      <#elseif currentValue?has_content && currentValue=="Y"> checked="checked"</#if> 
      name="${name!""?html}" value="${currentValue!}"<#if action?has_content> onClick="${action}"</#if>/><#rt/>
      <label for="<#if id?has_content>${id}<#else>${name!}</#if>"></label>
    </div>
</#macro>

<#-- migrated from @renderRadioField form widget macro -->
<#macro field_radio_widget items="" className="" alert="" currentValue="" noCurrentSelectedKey="" name="" event="" action="" tooltip="">
  <@field_radio_markup_widget items=items className=className alert=alert currentValue=currentValue noCurrentSelectedKey=noCurrentSelectedKey name=name event=event action=action tooltip=tooltip><#nested></@field_radio_markup_widget>
</#macro>

<#macro field_radio_markup_widget items="" className="" alert="" currentValue="" noCurrentSelectedKey="" name="" event="" action="" tooltip="">
  <#list items as item>
    <span<@fieldClassStr class=className alert=alert />><#rt/>
      <input type="radio"<#if currentValue?has_content><#if currentValue==item.key> checked="checked"</#if>
        <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
        <#elseif noCurrentSelectedKey?has_content && noCurrentSelectedKey == item.key> checked="checked"</#if> 
        name="${name!""?html}" value="${item.key!""?html}"<#if item.event?has_content> ${item.event}="${item.action!}"<#elseif event?has_content> ${event}="${action!}"</#if>/><#rt/>
      ${item.description}
    </span>
  </#list>
</#macro>

<#-- migrated from @renderFileField form widget macro -->
<#macro field_file_widget className="" alert="" name="" value="" size="" maxlength="" autocomplete="" id="" title="" fieldTitleBlank=false>
  <@field_file_markup_widget className=className alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete id=id title=title fieldTitleBlank=fieldTitleBlank><#nested></@field_file_markup_widget>
</#macro>

<#macro field_file_markup_widget className="" alert="" name="" value="" size="" maxlength="" autocomplete="" id="" title="" fieldTitleBlank=false>
  <input type="file"<@fieldClassStr class=className alert=alert /><#if id?has_content> id="${id}"</#if><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
</#macro>

<#-- migrated from @renderPasswordField form widget macro -->
<#macro field_password_widget className="" alert="" name="" value="" size="" maxlength="" id="" autocomplete="" title="" placeholder="" fieldTitleBlank=false tooltip="">
  <@field_password_markup_widget className=className alert=alert name=name value=value size=size maxlength=maxlength id=id autocomplete=autocomplete title=title placeholder=placeholder fieldTitleBlank=fieldTitleBlank tooltip=tooltip><#nested></@field_password_markup_widget>
</#macro>

<#macro field_password_markup_widget className="" alert="" name="" value="" size="" maxlength="" id="" autocomplete="" title="" placeholder="" fieldTitleBlank=false tooltip="">
  <#if tooltip?has_content> 
     <#local className = (className+ " has-tip tip-right")/>  
  </#if> 
  <input type="password"<@fieldClassStr class=className alert=alert /><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if>
  <#if maxlength?has_content> maxlength="${maxlength}"</#if><#if id?has_content> id="${id}"</#if><#if autocomplete?has_content> autocomplete="off"</#if> 
  <#if placeholder?has_content> placeholder="${placeholder}"</#if>
  <#if tooltip?has_content> data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
  <#if className?has_content> class="${className}"</#if><#rt/>/>
</#macro>

<#-- migrated from @renderSubmitField form widget macro -->
<#macro field_submit_widget buttonType="" className="" alert="" formName="" name="" event="" action="" imgSrc="" confirmation="" 
    containerId="" ajaxUrl="" title="" fieldTitleBlank=false showProgress="" href="" onClick="" inputType="" disabled=false progressOptions={}>
  <@field_submit_markup_widget buttonType=buttonType className=className alert=alert formName=formName name=name event=event action=action imgSrc=imgSrc confirmation=confirmation 
    containerId=containerId ajaxUrl=ajaxUrl title=title fieldTitleBlank=fieldTitleBlank showProgress=showProgress href=href onClick=onClick inputType=inputType disabled=disabled progressOptions=progressOptions><#nested></@field_submit_markup_widget>
</#macro>

<#macro field_submit_markup_widget buttonType="" className="" alert="" formName="" name="" event="" action="" imgSrc="" confirmation="" 
    containerId="" ajaxUrl="" title="" fieldTitleBlank=false showProgress="" href="" onClick="" inputType="" disabled=false progressOptions={}>
  <#-- Cato: FIXME?: factor out default submit class somewhere so configurable -->
  <#if buttonType!="image">
    <#if !className?has_content || className=="smallSubmit">
      <#local className = "${styles.button_default!}">
    </#if>
  </#if>

  <#-- Cato: to omit button (show progress only), we use empty title hack " " similar to what ofbiz does with hyperlinks with no label -->
  <#if (buttonType=="text-link" || buttonType!="image") && !(title?trim?has_content)>
  <#local buttonMarkup = "">
  <#else>
  <#local buttonMarkup>
  <#if buttonType=="text-link">
    <a<@fieldClassStr class=className alert=alert />href="<#if href?has_content>${href}<#elseif formName?has_content>javascript:document.${formName}.submit()<#else>javascript:void(0)</#if>"<#if disabled> disabled="disabled"<#else><#if onClick?has_content> onclick="${onClick}"<#elseif confirmation?has_content> onclick="return confirm('${confirmation?js_string}');"</#if></#if>><#if title?has_content>${title}</#if></a>
  <#elseif buttonType=="image">
    <input type="<#if inputType?has_content>${inputType}<#else>image</#if>" src="${imgSrc}"<@fieldClassStr class=className alert=alert /><#if name?has_content> name="${name}"</#if>
    <#if title?has_content> alt="${title}"</#if><#if event?has_content> ${event}="${action}"</#if>
    <#if disabled> disabled="disabled"<#else>
      <#if onClick?has_content> onclick="${onClick}"<#elseif confirmation?has_content>onclick="return confirm('${confirmation?js_string}');"</#if>
    </#if>/>
  <#else>
    <input type="<#if inputType?has_content>${inputType}<#elseif containerId?has_content>button<#else>submit</#if>"<@fieldClassStr class=className alert=alert />
    <#if name?has_content> name="${name}"</#if><#if title?has_content> value="${title}"</#if><#if event?has_content> ${event}="${action}"</#if>
    <#if disabled> disabled="disabled"<#else>
      <#if onClick?has_content> onclick="${onClick}"<#else>
        <#if containerId?has_content> onclick="<#if confirmation?has_content>if (confirm('${confirmation?js_string}')) </#if>ajaxSubmitFormUpdateAreas('${containerId}', '${ajaxUrl}')"<#else>
        <#if confirmation?has_content> onclick="return confirm('${confirmation?js_string}');"</#if>
        </#if>
      </#if>
    </#if>/>
  </#if>
  </#local>
  </#if>
  <#if progressOptions?has_content>
      <@fieldSubmitAreaProgress progressOptions=progressOptions nestedContent=buttonMarkup />
  <#else>
      ${buttonMarkup}
  </#if>
</#macro>

<#-- migrated from @renderDisplayField form widget macro -->
<#macro field_display_widget type="" imageLocation="" idName="" description="" title="" class="" alert="" inPlaceEditorUrl="" 
    inPlaceEditorParams="" imageAlt="" collapse=false fieldTitleBlank=false>
  <@field_display_markup_widget type=type imageLocation=imageLocation idName=idName description=description title=title class=class alert=alert inPlaceEditorUrl=inPlaceEditorUrl 
    inPlaceEditorParams=inPlaceEditorParams imageAlt=imageAlt collapse=false fieldTitleBlank=fieldTitleBlank><#nested></@field_display_markup_widget>
</#macro>

<#macro field_display_markup_widget type="" imageLocation="" idName="" description="" title="" class="" alert="" inPlaceEditorUrl="" 
    inPlaceEditorParams="" imageAlt="" collapse=false fieldTitleBlank=false>
  <#if type?has_content && type=="image">
    <img src="${imageLocation}" alt="${imageAlt}"><#lt/>
  <#else>
    <#--
    <#if inPlaceEditorUrl?has_content || class?has_content || alert=="true" || title?has_content>
      <span<#if idName?has_content> id="cc_${idName}"</#if><#if title?has_content> title="${title}"</#if><@fieldClassStr class=class alert=alert />><#t/>
    </#if>
    -->
    <#if description?has_content>
      ${description?replace("\n", "<br />")}<#t/>
    <#else>
      &nbsp;<#t/>
    </#if>
    <#--
    <#if inPlaceEditorUrl?has_content || class?has_content || alert=="true">
      </span><#lt/>
    </#if>
    <#if inPlaceEditorUrl?has_content && idName?has_content>
      <script language="JavaScript" type="text/javascript"><#lt/>
        ajaxInPlaceEditDisplayField('cc_${idName}', '${inPlaceEditorUrl}', ${inPlaceEditorParams});<#lt/>
      </script><#lt/>
    </#if>-->
    </#if>
</#macro>

<#-- migrated from @renderRangeFindField form widget macro -->
<#macro field_textfind_widget name="" value="" defaultOption="" opEquals="" opBeginsWith="" opContains="" 
    opIsEmpty="" opNotEqual="" className="" alert="" size="" maxlength="" autocomplete="" titleStyle="" 
    hideIgnoreCase="" ignCase="" ignoreCase="" title="" fieldTitleBlank=false>
  <@field_textfind_markup_widget name=name value=value defaultOption=defaultOption opEquals=opEquals opBeginsWith=opBeginsWith opContains=opContains 
    opIsEmpty=opIsEmpty opNotEqual=opNotEqual className=className alert=alert size=size maxlength=maxlength autocomplete=autocomplete titleStyle=titleStyle 
    hideIgnoreCase=hideIgnoreCase ignCase=ignCase ignoreCase=ignoreCase title=title fieldTitleBlank=fieldTitleBlank><#nested></@field_textfind_markup_widget>
</#macro>

<#macro field_textfind_markup_widget name="" value="" defaultOption="" opEquals="" opBeginsWith="" opContains="" 
    opIsEmpty="" opNotEqual="" className="" alert="" size="" maxlength="" autocomplete="" titleStyle="" 
    hideIgnoreCase="" ignCase="" ignoreCase="" title="" fieldTitleBlank=false>

  <@row collapse=collapse!false>
  <#if opEquals?has_content>
            <#local class1="${styles.grid_small!}3 ${styles.grid_large!}3"/>
            <#local class2="${styles.grid_small!}6 ${styles.grid_large!}6"/>
            <#local class3="${styles.grid_small!}3 ${styles.grid_large!}3"/>
            
        <#else>
            <#local class1=""/>
            <#local class2="${styles.grid_small!}9 ${styles.grid_large!}9"/>
            <#local class3="${styles.grid_small!}3 ${styles.grid_large!}3"/>
      </#if>      
      <#if opEquals?has_content>
        <#local newName = "${name}"/>
        <@cell class="${class1!}">
    <select <#if name?has_content>name="${name}_op"</#if>    class="selectBox"><#rt/>
      <option value="equals"<#if defaultOption=="equals"> selected="selected"</#if>>${opEquals}</option><#rt/>
      <option value="like"<#if defaultOption=="like"> selected="selected"</#if>>${opBeginsWith}</option><#rt/>
      <option value="contains"<#if defaultOption=="contains"> selected="selected"</#if>>${opContains}</option><#rt/>
      <option value="empty"<#rt/><#if defaultOption=="empty"> selected="selected"</#if>>${opIsEmpty}</option><#rt/>
      <option value="notEqual"<#if defaultOption=="notEqual"> selected="selected"</#if>>${opNotEqual}</option><#rt/>
    </select>
        </@cell>
  <#else>
    <input type="hidden" name=<#if name?has_content> "${name}_op"</#if>    value="${defaultOption}"/><#rt/>
  </#if>
      <@cell class="${class2!}">
    <input type="text"<@fieldClassStr class=className alert=alert />name="${name}"<#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
       
      </@cell>
      <@cell class="${class3!}"> 
    <#if hideIgnoreCase>
      <input type="hidden" name="${name}_ic" value=<#if ignCase>"Y"<#else> ""</#if>/><#rt/>
    <#else>
            <div class="">
                <label for="${name}_ic"><input type="checkbox" id="${name}_ic" name="${name}_ic" value="Y" <#if ignCase> checked="checked"</#if> />
                ${ignoreCase!}</label>
                <#rt/>
            </div>
    </#if>
      </@cell>
  </@row>
</#macro>

<#-- migrated from @renderRangeFindField form widget macro -->
<#macro field_rangefind_widget className="" alert="" name="" value="" size="" maxlength="" autocomplete="" titleStyle="" defaultOptionFrom="" opEquals="" opGreaterThan="" opGreaterThanEquals="" opLessThan="" opLessThanEquals="" value2="" defaultOptionThru="">
  <@field_rangefind_markup_widget className=className alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete titleStyle=titleStyle defaultOptionFrom=defaultOptionFrom opEquals=opEquals opGreaterThan=opGreaterThan opGreaterThanEquals=opGreaterThanEquals opLessThan=opLessThan opLessThanEquals=opLessThanEquals value2=value2 defaultOptionThru=defaultOptionThru><#nested></@field_rangefind_markup_widget>
</#macro>

<#macro field_rangefind_markup_widget className="" alert="" name="" value="" size="" maxlength="" autocomplete="" titleStyle="" defaultOptionFrom="" opEquals="" opGreaterThan="" opGreaterThanEquals="" opLessThan="" opLessThanEquals="" value2="" defaultOptionThru="">
  <#local class1="${styles.grid_small!}9 ${styles.grid_large!}9"/>
  <#local class2="${styles.grid_small!}3 ${styles.grid_large!}3"/>
  <@row collapse=collapse!false>
    <@cell class=class1>
      <input type="text"<@fieldClassStr class=className alert=alert /><#if name?has_content>name="${name}_fld0_value"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
    </@cell>
    <@cell class=class2>
      <#if titleStyle?has_content>
        <span class="${titleStyle}"><#rt/>
      </#if>
      <select<#if name?has_content> name="${name}_fld0_op"</#if> class="selectBox"><#rt/>
        <option value="equals"<#if defaultOptionFrom=="equals"> selected="selected"</#if>>${opEquals}</option><#rt/>
        <option value="greaterThan"<#if defaultOptionFrom=="greaterThan"> selected="selected"</#if>>${opGreaterThan}</option><#rt/>
        <option value="greaterThanEqualTo"<#if defaultOptionFrom=="greaterThanEqualTo"> selected="selected"</#if>>${opGreaterThanEquals}</option><#rt/>
      </select><#rt/>
      <#if titleStyle?has_content>
        </span><#rt/>
      </#if>
    </@cell>
  </@row><#rt/>
  <@row>
    <@cell class=class1>
      <input type="text"<@fieldClassStr class=className alert=alert /><#if name?has_content>name="${name}_fld1_value"</#if><#if value2?has_content> value="${value2}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
    </@cell>
    <@cell class=class2>
      <#if titleStyle?has_content>
        <span class="${titleStyle}"><#rt/>
      </#if>
      <select name=<#if name?has_content>"${name}_fld1_op"</#if> class="selectBox"><#rt/>
        <option value="lessThan"<#if defaultOptionThru=="lessThan"> selected="selected"</#if>>${opLessThan?html}</option><#rt/>
        <option value="lessThanEqualTo"<#if defaultOptionThru=="lessThanEqualTo"> selected="selected"</#if>>${opLessThanEquals?html}</option><#rt/>
      </select><#rt/>
      <#if titleStyle?has_content>
        </span>
      </#if>
    </@cell>
  </@row>
</#macro>

<#-- migrated from @renderField form widget macro -->
<#macro field_generic_widget text="">
  <@field_generic_markup_widget text=text><#nested></@field_generic_markup_widget>
</#macro>

<#macro field_generic_markup_widget text="">
  <#if text??>
    ${text}<#lt/>
  </#if>
</#macro>
