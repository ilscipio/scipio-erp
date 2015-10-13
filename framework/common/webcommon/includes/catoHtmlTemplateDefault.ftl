<#--
* 
* A set of HTML templating macros, part of standard Cato Freemarker API.
* Automatically included at all times.
* Intended to be swappable.
*
* Default Cato markup (NOTE: currently targeted toward Foundation CSS).
*
* NOTE: In general, macros expect to be called using named arguments (not supported for functions),
*     except where otherwise noted.
*
* IMPL NOTE: Macros should avoid using "request" directly (use setRequestVar/getRequestVar/other).
*
* DEV NOTE: although intended as swappable, the way to reuse/extend this file needs clarified.
*     currently contains much logic code and includes all the widget macros (see FIXMEs). can #include this file
*     and override macros, but widget macro includes makes that very heavy, and still logic reuse problems.
*
* DEV NOTE: some macros use attribs and inlineAttribs args to specify extra HTML attribs.
*     even though it would be convenient, we can't allow a "attribString" arg because no way
*     for macro to get attribs out of it if it needs them, cause problems.
*     FIXME: not all macros currently properly check attribMap for duplicate attribs
*       of args and inlineAttribs (priority should be: args - inlineAttribs - attribMap).
*
* TODO?: there could be a catoHtmlTemplateHelpers.ftl to help separate template macro-related logic from markup (less copy-paste).
*     Do not put functions closely related to these macros in CatoUtilities.
*     For now, try to keep markup generalized and parametrizable via catoHtmlVariablesDefault.html as much as possible.
*
-->

<#-- note: assumes catoUtilities.ftl included. see end of file for additional includes. -->

<#-- 
*************************************
* API TEMPLATE MACROS *
*************************************
* Intended for use in production templates.
-->

<#--
*************
* HTML Head open
************
Opens an HTML document and header section.

    Usage example:  
    <@htmlHeadOpen />            
                    
   * General Attributes *
    includeDocType      = boolean, default false (included by screen renderer, @renderScreenBegin)
-->
<#macro htmlHeadOpen includeDocType=false>
<#if includeDocType><!DOCTYPE html></#if><#t>
<#if locale??><#local docLangAttr = locale.toString()?replace("_", "-")></#if>
<#local langDir = "ltr">
<#if docLangAttr?? && "ar.iw"?contains(docLangAttr?substring(0, 2))>
    <#local langDir = "rtl">
</#if>
<!--[if IE 9]><html class="lt-ie10" lang="${docLangAttr!"en"}" <#if langDir??>dir="${langDir}"</#if>> <![endif]-->
<html class="no-js" lang="${doctLangAttr!"en"}"<#if langDir?has_content> dir="${langDir!}"</#if>>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
</#macro>

<#--
*************
* Row Macro
************
    Usage example:  
    <@row attr="" >
        <@cell attr=""/>
    </@row>              
                    
   * General Attributes *
    class           = css classes 
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    alt             = boolean, if true alternate row (odd), if false regular (even)
    selected        = boolean, if true row is marked selected
-->
<#macro row class=true id="" collapse=false norows=false alt="" selected="">
    <#if !norows>
      <#local classes = makeClassesArg(class, "")>
      <#local altClass = "">
      <#if alt?is_boolean>
        <#local altClass = alt?string(styles.row_alt!, styles.row_reg!)>
      </#if>
      <#if selected?is_boolean && selected == true>
        <#local altClass = (altClass + " " + styles.row_selected!)?trim>
      </#if>
    <div class="${styles.grid_row!}<#if classes?has_content> ${classes}</#if><#if collapse> collapse</#if><#if altClass?has_content> ${altClass}</#if>"<#if id?has_content> id="${id}"</#if>><#rt/>
    </#if>
        <#nested />
    <#if !norows>    
    </div>
    </#if>    
</#macro>

<#-- 
*************
* Cell Macro
************
    Usage examples:  
    <@row attr="" >
        <@cell attr="">
            cell content goes in here!
        </@cell>
    </@row>  
    
    <@row>
        <@cell>
            cell content goes in here!
        </@cell>
    </@row>   
    
    <@row>
        <@cell columns=3>
            cell content goes in here!
        </@cell>
    </@row> 
    
    <@row>
        <@cell columns=3 small=2 class="mycell">
            cell content goes in here!
        </@cell>
    </@row>            
                    
   * General Attributes *
    class           = css classes (if column sizes specified, adds classes; if no column sizes specified, expected to contain manual column sizes and overrides columns size default)
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    columns         = expected number of columns to be rendered (specify as number, default 12, default only used if class empty and no column sizes specified)
    small           = specific number of small columns (specify as number), overrides small part general columns value above
    large           = specific number of large columns (specify as number), overrides large part of general columns value above
    medium          = specific number of medium columns (specify as number), overrides medium part of general columns value above
    offset          = offset in number of columns
    smallOffset     = specific offset for small columns
    mediumOffset    = specific offset for medium columns
    largeOffset     = specific offset for large columns
    last            = boolean, usually optional, if true indicate last cell in row 
-->
<#macro cell columns=-1 small=-1 medium=-1 large=-1 offset=-1 smallOffset=-1 mediumOffset=-1 largeOffset=-1 class=true id="" collapse=false nocells=false last=false>
    <#local addClass = parseAddClassArg(class)>
    <#local class = parseClassArg(class, "")>  
     
    <#local columns = columns?number>
    <#local small = small?number>
    <#local medium = medium?number>
    <#local large = large?number>
    <#local offset = offset?number>
    <#local smallOffset = smallOffset?number>
    <#local mediumOffset = mediumOffset?number>
    <#local largeOffset = largeOffset?number>
    
    <#if !nocells>
        <#local specColsClasses><#if (small > 0)> ${styles.grid_small!}${small}</#if><#if (medium > 0)> ${styles.grid_medium!}${medium}</#if><#if (large > 0)> ${styles.grid_large!}${large}<#elseif (large != 0) && (columns > 0)> ${styles.grid_large!}${columns}</#if></#local>
        <#if class?has_content>
            <#local colSizeClasses = (class + specColsClasses)?trim>
        <#else>
            <#local colSizeClasses = specColsClasses?trim>
        </#if>
        <#if !colSizeClasses?has_content>
            <#local colSizeClasses = "${styles.grid_large!}12">
        </#if>
        <#local specOffsetClassesStr><#if (smallOffset > 0)> ${styles.grid_small_offset!}${smallOffset}</#if><#if (mediumOffset > 0)> ${styles.grid_medium_offset!}${mediumOffset}</#if><#if (largeOffset > 0)> ${styles.grid_large_offset!}${largeOffset}<#elseif (largeOffset != 0) && (offset > 0)> ${styles.grid_large_offset!}${offset}</#if></#local>
        <div class="${colSizeClasses}${specOffsetClassesStr} ${styles.grid_cell!}<#if last> ${styles.grid_end!}</#if><#if addClass?has_content> ${addClass}</#if>" <#if id?has_content> id="${id}"</#if>><#rt/>
    </#if>
        <#nested />
    <#if !nocells></div></#if>
</#macro>

<#-- 
*************
* Section Macro
************
Creates a logical section with optional title and menu. Automatically handles heading sizes
and keeps track of section nesting for whole request, even across screens.render calls.

Note: use getCurrentHeadingLevel and getCurrentSectionLevel functions if need to get current
levels manually, but most often should let @section menu handle them.

    Usage example:  
    <@section attr="">
        Inner Content
    </@section>            
                    
   * General Attributes *
    type                = [generic], default generic
    class               = css classes, on outer columns element (affects title)
                          (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
                          note: boolean false has no effect here
    id                  = set id
    title               = section title
    titleClass          = section title class (supports complex expressions; rarely needed, usually headingLevel enough)
    padded              = 
    autoHeadingLevel    = auto increase heading level when title present (enabled by default)
    headingLevel        = force this heading level for title. if autoHeadingLevel true, also influences nested elems (even if no title here, but if no title won't consume a size).
    relHeadingLevel     = increase heading level by this number
    defaultHeadingLevel = default heading level (same as headingLevel if autoHeadingLevel false)
                          if empty string, use getDefaultHeadingLevel()
    menuHtml            = optional HTML menu data, li elements only (ul auto added)
                          FIXME: need to rework the menu options. could have a menu "type"
                              that causes lookup in styles (see @fields and @table type arg)
                              for menuClass and menuLayout, but these args are all redundant
                              with ones on @menu macro, BUT because this is inlined HTML the 
                              @menu args are lost and would need to be passed twice... 
                              need to define @menu "type" arg better and figure out how to integrate
                              here to avoid having to specify menuClass/menuLayout everywhere...
                              and somehow avoid the arg redundancy (FTL issue)...
    menuClass           = menu class, default is section menu (FIXME: doesn't really mean anything at the moment). "none" prevents class. this is a low-level control; avoid if possible (FIXME: currently impossible to avoid).
    menuLayout          = [post-title|pre-title|inline-title], default post-title. this is a low-level control; avoid if possible (FIXME: currently impossible to avoid).
    menuRole            = "nav-menu" (default), "paginate-menu"
    requireMenu         = if true, add menu elem even if empty
    forceEmptyMenu      = if true, always add menu and must be empty
    hasContent          = minor hint, optional, default true, when false, to add classes to indicate content is empty or treat as logically empty (workaround for no css :blank and possibly other)
-->
<#macro section type="generic" id="" title="" class=true padded=false autoHeadingLevel=true headingLevel="" relHeadingLevel="" defaultHeadingLevel="" menuHtml="" menuClass="" menuLayout="" menuRole="nav-menu" requireMenu=false forceEmptyMenu=false hasContent=true titleClass="">
    <#local addClass = parseAddClassArg(class)>
    <#local class = parseClassArg(class, "")>
    <#if id?has_content>
        <#local contentId = id + "_content">
        <#local menuId = id + "_menu">
    <#else>
        <#local contentId = "">
        <#local menuId = "">
    </#if>
    <#-- note: addClass logic is only partially implemented (doesn't support booleans and "" means use default; otherwise may conflict with stock API?), but good enough for now -->
    <#-- note: autoHeadingLevel logic now implemented in renderScreenletBegin -->
    <@screenlib.renderScreenletBegin id=id collapsibleAreaId=contentId title=title classes=class padded=padded menuString=menuHtml fromWidgets=false menuClass=menuClass menuId=menuId menuLayout=menuLayout menuRole=menuRole requireMenu=requireMenu 
        forceEmptyMenu=forceEmptyMenu hasContent=hasContent autoHeadingLevel=autoHeadingLevel headingLevel=headingLevel relHeadingLevel=relHeadingLevel defaultHeadingLevel=defaultHeadingLevel titleStyle=titleClass addClasses=addClass />
        <#nested />
    <@screenlib.renderScreenletEnd />
</#macro>

<#-- 
*************
* Heading
************
    Usage example:  
    <@heading>My Title</@heading>         
                                 
   * General Attributes *
    elemType       = [heading|h|p|span|div|raw], default heading (note: do not specify h1-h6 here - use level)
                     boolean true means use default, false same as raw (none)
    level          = specific level (1-6). If not specified, current heading level returned by
                     getCurrentHeadingLevel() function is used. 
                     note: does not consume a level.
    relLevel       = for level, uses level of current heading returned by getCurrentHeadingLevel()
                     plus this number of levels. default: 0 (current level)
    class          = heading elem classes (simple)
                     (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    levelClassPrefix = default "heading-level-", prefix for class that will be appended level number
    id             = heading id
    consumeLevel   = boolean, default currently always false (DEV NOTE: could be made to depend on calculated level).
                     if true, the global heading level is set to (calculated level for this heading) + 1.
                     note this is better handled through use of the @section macro. mostly useful for h1.
    containerElemType = [div|] default empty. if present, adds container around title or this elem type
    containerClass    = container elem classes (simple)       
    containerId       = container id  
    attribs              = hash of other legacy h1-h6 attributes (mainly for those with dash in name)
    [inlineAttribs...]   = other legacy h1-h6 attributes, inlined
-->
<#macro heading elemType=true level="" relLevel="" class=true id="" levelClassPrefix="heading-level-" consumeLevel="" containerElemType=false containerClass=true attribs={} inlineAttribs...>
  <#if !level?has_content>
    <#local level = getCurrentHeadingLevel()>
  </#if>
  <#if relLevel?has_content>
    <#local level = level + relLevel>
  </#if>
  <#if levelClassPrefix?has_content>
    <#local headingLevelClass = levelClassPrefix + level?string>
  <#else>
    <#local headingLevelClass = "">
  </#if>
  <#local classes = makeClassesArg(class, headingLevelClass)>
  <#local containerClasses = makeClassesArg(containerClass, headingLevelClass)>
  <#if (consumeLevel?is_boolean && consumeLevel == true)>
    <#local dummy = setCurrentHeadingLevel(level + 1)>
  </#if>
  <#-- local adjustment to prevent invalid <hX> elem. true value always reflected in headingLevelClass. -->
  <#if (level < 1)>
    <#local level = 1>
  <#elseif (level > 6)>
    <#local level = 6>
  </#if>
  <#if elemType?is_boolean>
    <#if elemType>
      <#local hElem = "h" + level?string>
    <#else>
      <#local hElem = "">
    </#if>
  <#elseif elemType == "heading" || elemType == "h">
    <#local hElem = "h" + level?string>
  <#elseif elemType == "raw" || !elemType?has_content>
    <#local hElem = "">
  <#else>
    <#local hElem = elemType>
  </#if>
  <#if containerElemType?is_boolean>
    <#local cElem = "">
  <#elseif containerElemType == "raw" || !containerElemType?has_content>
    <#local cElem = "">
  <#else>
    <#local cElem = containerElemType>
  </#if>
  <#if cElem?has_content>
    <${cElem}<#if containerClasses?has_content> class="${containerClasses}"</#if><#if containerId?has_content> id="${containerId}"</#if>>
  </#if>
  <#if hElem?has_content><${hElem}<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#rt>
    <#lt><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id", "style"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>></#if><#nested><#if hElem?has_content></${hElem}></#if>
  <#if cElem?has_content>
    </${cElem}>
  </#if>
</#macro>

<#-- 
*************
* Fields Macro
************ 
Fields container that helps modify a set of @field definitions, or group of fields.
Can be omitted.
May sometimes need multiple of these per form (so @form insufficient for this purpose). 
Not associated with an HTML element as is @fieldset.

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
* Field Macro
************ 
    Usage example:  
    <@field attr="" />
    
    * General Attributes *
    type            = form element of type [input,textarea,datetime,select,checkbox,radio,display,generic],
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
<#macro field type="generic" label="" labelDetail="" name="" value="" valueType="generic" currentValue="" defaultValue="" class=true size=20 maxlength="" id="" onClick="" 
        disabled=false placeholder="" autoCompleteUrl="" mask=false alert="false" readonly=false rows="4" 
        cols="50" dateType="date" multiple="" checked=false collapse=false tooltip="" columns="" norows=false nocells=false nocontainer=false
        fieldFormName="" formName="" formId="" postfix=false postfixSize=1 required=false items=[] autocomplete=true progressOptions={} 
        labelType="" labelLayout="" labelArea="" description=""
        submitType="input" text="" href="" src="" confirmMsg="">
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
    "submit":true
  }>
</#if>
<#if isChildField && catoFieldNoContainerChildren[type]??>
  <#local nocontainer = true>
</#if>

<@row collapse=collapse!false norows=(norows || nocontainer) class="form-field-entry">

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
            <@cell class=(subclasses+" field-entry-title")?trim nocells=(nocells || nocontainer)>
              <#if label?has_content>
                <#if type=="checkbox" || collapse==false>
                    <label class="form-field-label"<#if id?has_content> for="${id}"</#if>>${label}</label>
                <#else>
                    <span class="prefix form-field-label">${label}</span>
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
    <@cell class=("${classes!}"+" field-entry-widget")?trim nocells=(nocells || nocontainer)>
        <#switch type>
          <#case "input">
            <@formlib.renderTextField name=name 
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
            <@formlib.renderTextareaField name=name 
                                  className=class 
                                  alert=alert 
                                  cols=cols 
                                  rows=rows 
                                  id=id 
                                  readonly=readonly 
                                  value=value 
                                  tooltip=tooltip/>
            <#break>
          <#case "datetime">
            <#if dateType == "date"><#local shortDateInput=true/><#else><#local shortDateInput=false/></#if>
            <@formlib.renderDateTimeField name=name 
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
            
            <@formlib.renderDropDownField name=name
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
                                    manualItemsOnly=manualItemsOnly><#nested></@formlib.renderDropDownField>
            <#break>
          <#case "lookup">
            <@formlib.renderLookupField name=name formName=formName fieldFormName=fieldFormName className=class alert="false" value=value size=size?string maxlength=maxlength id=id event="onClick" action=onClick />
          <#break>
          <#case "checkbox">
                <@formlib.renderCheckBox id=id currentValue=value checked=checked name=name action=action />
            <#break>
          <#case "radio">
                <#if radioSingle>
                    <#-- single radio button item mode -->
                    <#local items=[{"key":value, "description":label!""}]/>
                    <@formlib.renderRadioField items=items className=class alert=alert currentValue=(checked?string(value,"")) noCurrentSelectedKey="" name=name event="" action="" tooltip=tooltip />
                <#else>
                    <#-- multi radio button item mode -->
                    <div<@formlib.renderClass class alert />>
                      <@formlib.renderRadioField items=items className="" alert=alert currentValue=currentValue noCurrentSelectedKey=defaultValue name=name event="" action="" tooltip=tooltip />
                    </div>
                </#if>
            <#break>
          <#case "file">
            <@formlib.renderFileField className=class alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete?string("", "off") id=id />
            <#break> 
          <#case "password">
            <@formlib.renderPasswordField className=class alert=alert name=name value=value size=size maxlength=maxlength id=id autocomplete=autocomplete?string("", "off") />
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
                <@formlib.renderSubmitField buttonType=buttonType className=class alert=alert formName=formName name=name event="" action="" imgSrc=src confirmation=confirmMsg containerId="" ajaxUrl="" title=text showProgress=false onClick=onClick href=href inputType=inputType disabled=disabled />
              <#else>
                <#nested>
              </#if>
            </#local>
            <#if hasProgress>
              <@formlib.renderSubmitFieldAreaProgress progressOptions=progressOptions nestedContent=content />
            <#else>
              ${content}
            </#if>
            <#break> 
          <#case "display">
            <#-- TODO? may need formatting here based on valueType... not done by renderDisplayField... done in java OOTB... 
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
            <#-- FIXME: maybe use div or span with a class instead of p, but this blends nicely for now -->
            <p>
                <@formlib.renderDisplayField type=displayType imageLocation=imageLocation idName="" description=desc title="" class=class alert=alert inPlaceEditorUrl="" inPlaceEditorParams="" imageAlt=description/>
                <#-- FIXME: tooltip too crappy -->
              <#if tooltip?has_content>
                <span class="tooltip">${tooltip}</span>
              </#if>
            </p>
            <#break> 
          <#default> <#-- "generic", empty or unrecognized -->
            <#if value?has_content>
                <@formlib.renderField text=value/>
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

<#-- 
*************
* Fieldset Macro
************
A fieldset including the HTML element.

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
    <@formlib.renderFieldGroupOpen style=classes id=id title=title collapsed=collapsed collapsibleAreaId="" collapsible=false expandToolTip="" collapseToolTip=""/>
        <#nested />
    <@formlib.renderFieldGroupClose style="" id="" title=""/>
</#macro>

<#-- 
*************
* Form Macro
************
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
* Modal Macro
************
    Usage example:  
    <@modal id="dsadsa" attr="" >
    modal Content 
    </@modal>                
   * General Attributes *
    id              = set id (required)
    label           = set anchor text (required)
-->
<#macro modal id label href="">
    <a href="#" data-reveal-id="${id}_modal" <#if href?has_content>data-reveal-ajax="${href!}"</#if>>${label}</a>
    <div id="${id}_modal" class="${styles.modal_wrap!}" data-reveal>
        <#nested>
        <a class="close-reveal-modal">&#215;</a>
    </div>
</#macro>

<#-- 
*************
* Pagination Macro
************
    Usage example:  
    <@paginate mode="single" ... />
    <@paginate mode="content">
      <@table>
        ...
      </@table>
    </@paginate>            
                    
   * General Attributes *
   mode            = [single|content], default single
                     single: produces a single pagination menu
                     content: decorates the nested content with one or more pagination menus
   type            = [default], default default, type of the pagination menu itself
                     default: default cato pagination menu (currently @renderNextPrev numbered menu)
   layout          = [default|top|bottom|both], default default, type of layout, only meaningful for "content" mode
                     default: cato default layout (currently "both")
                     top: no more than one menu, always at top
                     bottom: no more than one menu, always at bottom
                     both: always two menus, top and bottom
   noResultsMode   = [default|hide|disable], default default (default may depend on mode)
                     default: cato default (currently "hide", for both modes)
                     hide: hide menu when no results
                     disable: disable but show controls when no results (TODO?: not implemented)
   paginateOn      = indicates whether pagination is currently on or off (use with paginateToggle)
   url             = Base Url to be used for pagination
   class           = css classes 
                     (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
   listSize        = size of the list in total
   viewIndex       = page currently displayed
   viewSize        = maximum number of items displayed
   altParam        = Use viewIndex/viewSize as parameters, instead of VIEW_INDEX / VIEW_SIZE
   forcePost       = Always use POST for non-ajax browsing (note: even if false, large requests are coerced to POST)
   paramStr        = Extra URL parameters in string format, escaped (param1=val1&amp;param2=val2)
   viewIndexFirst  = First viewIndex value number (0 or 1, only affects param values, not display)
   showCount       = If true show "Displaying..." count or string provided in countMsg; if false don't
   countMsg        = Custom message for count, optional
   paginateToggle  = if true, include a control to toggle pagination on/off 
                     (specify current state with paginateOn and tweak using paginateToggle* arguments)
-->
<#macro paginate mode="single" type="default" layout="default" noResultsMode="default" paginateOn=true url="" class=true viewIndex=0 listSize=0 viewSize=1 altParam=false 
    forcePost=false paramStr="" viewIndexFirst=0 showCount=true countMsg=""
    paginateToggle=false paginateToggleString="" paginateToggleOnValue="Y" paginateToggleOffValue="N">
    <#local classes = makeClassesArg(class, "nav-pager")>
    
    <#-- these errors apparently happen a lot, enforce here cause screens never catch, guarantee other checks work -->
    <#if (!viewSize?is_number)>
        ${Static["org.ofbiz.base.util.Debug"].logError("pagination: viewSize was not a number type: " + viewSize!, "htmlUtilitiesPaginate")!}<#t>
        <#local viewSize = viewSize?number>
    </#if>
    <#local viewSize = viewSize?floor>
    <#if (!viewIndex?is_number)>
        ${Static["org.ofbiz.base.util.Debug"].logError("pagination: viewIndex was not a number type: " + viewIndex!, "htmlUtilitiesPaginate")!}<#t>
        <#local viewIndex = viewIndex?number>
    </#if>
    <#local viewIndex = viewIndex?floor>
    
    <#local viewIndexLast = viewIndexFirst + ((listSize/viewSize)?ceiling-1)>
    <#if (viewIndexLast < viewIndexFirst)>
      <#local viewIndexLast = viewIndexFirst>
    </#if>
    <#if (viewIndex < viewIndexFirst) || (viewIndex > viewIndexLast)>
        ${Static["org.ofbiz.base.util.Debug"].logError("pagination: viewIndex was out of bounds: " + viewIndex, "htmlUtilitiesPaginate")!}<#t>
        <#if (viewIndex < viewIndexFirst)>
            <#local viewIndex = viewIndexFirst>
        <#else>
            <#local viewIndex = viewIndexLast>
        </#if>
    </#if>
    
    <#local lowIndex = (viewIndex - viewIndexFirst) * viewSize/>
    <#local highIndex = ((viewIndex - viewIndexFirst) + 1) * viewSize/>
    <#if (listSize < highIndex)>
        <#local highIndex = listSize/>
    </#if>
    <#if altParam>
        <#local viewIndexString = "viewIndex">
        <#local viewSizeString = "viewSize">
        <#if !paginateToggleString?has_content>
            <#local paginateToggleString = "paging">
        </#if>
    <#else>
        <#local viewIndexString = "VIEW_INDEX">
        <#local viewSizeString = "VIEW_SIZE">
        <#if !paginateToggleString?has_content>
            <#local paginateToggleString = "PAGING">
        </#if>
    </#if>
    <#if (viewIndexLast > (viewIndex))>
        <#local viewIndexNext = (viewIndex+1)>
    <#else>
        <#local viewIndexNext = viewIndex>
    </#if>
    <#if (viewIndex > viewIndexFirst)>
        <#local viewIndexPrevious = (viewIndex-1)>
    <#else>
        <#local viewIndexPrevious = viewIndex>
    </#if>


        <#local commonUrl = addParamDelimToUrl(url, "&amp;")>
        <#if paramStr?has_content>
            <#local commonUrl = commonUrl + trimParamStrDelims(paramStr) + "&amp;">
        </#if>
        
        <#local firstUrl = "">
        <#if (!firstUrl?has_content)>
            <#local firstUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndexFirst}"/>
        </#if>
        <#local previousUrl = "">
        <#if (!previousUrl?has_content)>
             <#local previousUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndexPrevious}"/>
        </#if>
        <#local nextUrl="">
        <#if (!nextUrl?has_content)>
            <#local nextUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndexNext}"/>
        </#if>
        <#local lastUrl="">
        <#if (!lastUrl?has_content)>
            <#local lastUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndexLast}"/>
        </#if>
        <#local selectUrl="">
        <#if (!selectUrl?has_content)>
            <#local selectUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}="/>
        </#if>
        <#local selectSizeUrl="">
        <#if (!selectSizeUrl?has_content)>
            <#local selectSizeUrl=commonUrl+"${viewSizeString}='+this.value+'&amp;${viewIndexString}=${viewIndexFirst}"/>
        </#if>

        <#local paginateOnUrl="">
        <#if (!paginateOnUrl?has_content)>
            <#local paginateOnUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndex}&amp;${paginateToggleString}=${paginateToggleOnValue}"/>
        </#if>
        <#local paginateOffUrl="">
        <#if (!paginateOffUrl?has_content)>
            <#local paginateOffUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndex}&amp;${paginateToggleString}=${paginateToggleOffValue}"/>
        </#if>
    
    <#if showCount && (!countMsg?has_content)>
       <#local messageMap = {"lowCount": lowIndex+1, "highCount": highIndex, "total": listSize}>
       <#local countMsg = Static["org.ofbiz.base.util.UtilProperties"].getMessage("CommonUiLabels", "CommonDisplaying", messageMap, locale)!"">
    </#if>
    
  <#-- TODO: noResultsMode should be delegated to @renderNextPrev... but generally hiding everything for now... -->
  <#if noResultsMode == "default">
    <#local noResultsMode = "hide">
  </#if>
  <#local showNextPrev = (noResultsMode != "hide") || (listSize > 0)>  
  <#-- DEV NOTE: make sure all @renderNextPrev calls same (DO NOT use #local capture; risks duplicate IDs) -->
  <#if mode == "single">
      <#if showNextPrev>
        <@formlib.renderNextPrev ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateStyle=classes paginateFirstStyle="nav-first" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel=uiLabelMap.CommonFirst paginatePreviousStyle="nav-previous" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel=uiLabelMap.CommonPrevious pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl commonDisplaying=showCount?string(countMsg,"") paginateNextStyle="nav-next" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel=uiLabelMap.CommonNext paginateLastStyle="nav-last" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel=uiLabelMap.CommonLast paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst paginate=paginateOn paginateToggle=paginateToggle ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnStyle="" paginateOnLabel=uiLabelMap.CommonPagingOn ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffStyle="" paginateOffLabel=uiLabelMap.CommonPagingOff />
      </#if>
  <#else>
      <#if showNextPrev && layout != "bottom">
        <@formlib.renderNextPrev ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateStyle=classes paginateFirstStyle="nav-first" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel=uiLabelMap.CommonFirst paginatePreviousStyle="nav-previous" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel=uiLabelMap.CommonPrevious pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl commonDisplaying=showCount?string(countMsg,"") paginateNextStyle="nav-next" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel=uiLabelMap.CommonNext paginateLastStyle="nav-last" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel=uiLabelMap.CommonLast paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst paginate=paginateOn paginateToggle=paginateToggle ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnStyle="" paginateOnLabel=uiLabelMap.CommonPagingOn ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffStyle="" paginateOffLabel=uiLabelMap.CommonPagingOff />
      </#if>
        <#nested>
      <#if showNextPrev && layout != "top">
        <@formlib.renderNextPrev ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateStyle=classes paginateFirstStyle="nav-first" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel=uiLabelMap.CommonFirst paginatePreviousStyle="nav-previous" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel=uiLabelMap.CommonPrevious pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl commonDisplaying=showCount?string(countMsg,"") paginateNextStyle="nav-next" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel=uiLabelMap.CommonNext paginateLastStyle="nav-last" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel=uiLabelMap.CommonLast paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst paginate=paginateOn paginateToggle=paginateToggle ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnStyle="" paginateOnLabel=uiLabelMap.CommonPagingOn ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffStyle="" paginateOffLabel=uiLabelMap.CommonPagingOff />
      </#if>
  </#if>
</#macro>



<#-- 
*************
* Script
************
Inline script wrapper.

    Usage example:  
    <@script>
        jQuery(document).ready(function() {
            alert("Page loaded.");
        });
    </@script>         
                    
   * General Attributes *
    type            = script type identifier
    language        = language identifier
    src             = source
    forceInline     = if true, the script must be inlined in the markup where the macro is used
                      and should never be delegated. in most cases this should be omitted.
-->
<#macro script type="text/javascript" language="" src="" ofbizContentSrc="" forceInline=false>
<#if ofbizContentSrc?has_content>
  <script type="${type}"<#if language?has_content> language="${language}"</#if> src="<@ofbizContentUrl>${ofbizContentSrc}</@ofbizContentUrl>"></script>
<#elseif src?has_content>
  <script type="${type}"<#if language?has_content> language="${language}"</#if> src="${src}"></script>
<#else>
  <script type="${type}"<#if language?has_content> language="${language}"</#if>>
  //<![CDATA[
    <#nested>
  //]]>
  </script>
</#if>
</#macro>

<#-- 
*************
* Alert box
************
Alert box for messages that should grab user attention.
NOTE: Should avoid using this for regular, common inlined message results such as "no records found" unless
it's an unexpected result, error or one that requires user action. See other macros.

    Usage example:  
    <@alert type="info">
        <#nested>
    </@alert>            
                    
   * General Attributes *
    type           = (info|success|warning|secondary|alert|error), default info
    class          = classes or additional classes for nested container
                     (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
-->
<#macro alert type="info" class=true id="">
<#local classes = makeClassesArg(class, "${styles.grid_large!}12")>
<#if type="error"><#local type = "alert"></#if>
<div class="${styles.grid_row!}"<#if id?has_content> id="${id}"</#if>>
   <div class="${styles.grid_large!}12 ${styles.grid_cell!}">
       <div data-alert class="${styles.alert_wrap!} ${styles.alert_prefix_type!}${type}">
           <div class="${styles.grid_row!}">
              <div class="<#if classes?has_content>${classes} </#if>${styles.grid_cell!}">
                  <#nested>
                  <a href="#" class="close">&times;</a>
                  </div>
              </div>
           </div>
       </div>
   </div>
</#macro>

<#-- 
*************
* Query result message
************
Common query result message.
Note: this is ONLY for expected, non-error messages, such as no records found in a query.
Other messages such as for missing params/record IDs are usually errors.

    Usage example:  
    <@resultMsg>${uiLabelMap.CommonNoRecordFound}.</@resultMsg>            
                    
   * General Attributes *
    class       = classes or additional classes for nested container
                  (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
-->
<#macro resultMsg class=true id="">
  <#local classes = makeClassesArg(class, "result-msg")>
  <p<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if>><#nested></p>
</#macro>

<#-- 
*************
* Error result message
************
Common error result message.
Abstracts/centralizes method used to display error, since of no consequence to most
templates: currently @alert.

    Usage example:  
    <@errorMsg type="permission">${uiLabelMap.CommonNoPermission}.</@errorMsg>            
                    
   * General Attributes *
    type           = [permission|security|error], default error
    class          = classes or additional classes for nested container
                     (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
-->
<#macro errorMsg type="error" class=true id="">
  <@alert type="error" class=class id=id><#nested></@alert>
</#macro>

<#--
*************
* panel box
************
    Usage example:  
    <@panel type="">
        <#nested>
    </@panel>            
                    
   * General Attributes *
    type           = (callout|) default:empty
    title          = Title
-->
<#macro panel type="" title="">
<div class="${styles.panel_wrap!} ${type}">
  <div class="${styles.panel_head!}"><#if title?has_content><h5 class="${styles.panel_title!}">${title!}</h5></#if></div>
  <div class="${styles.panel_body!}"><p><#nested></p></div>
</div>
</#macro>

<#-- 
*************
* Pricing table
************
Since this is very foundation specific, this function may be dropped in future installations.

    Usage example:  
    <@pul >
        <#pli>Text or <a href="">Anchor</a></#pli>
    </@pul>            
                    
   * General Attributes *
    title           = fieldset-title
    
-->

<#macro pul title="">
          <ul class="${styles.pricing_wrap!}">
              <@pli type="title">${title!}</@pli>
              <#nested>
          </ul>
</#macro>

<#macro pli type="">
    <#switch type>
          <#case "price">
              <li class="${styles.pricing_price!}"><#nested></li>
          <#break>
          <#case "description">
              <li class="${styles.pricing_description!}"><#nested></li>
          <#break>
          <#case "title">
              <li class="${styles.pricing_title!}"><#nested></li>
          <#break>
          <#case "button">
              <li class="${styles.pricing_cta!}"><#nested></li>
          <#break>        
          <#default>
              <li class="${styles.pricing_bullet!}"><#nested></li>
          <#break>
    </#switch>
</#macro>

<#-- 
*************
* Grid list
************
Since this is very foundation specific, this function may be dropped in future installations

    Usage example:  
    <@grid>
        <li>Text or <a href="">Anchor</a></li>
    </@grid>            
                    
   * General Attributes *
    class           = Adds classes - please use "(small|medium|large)-${styles.grid_block!}#"
                      (FIXME: prepend with "+" to append only, i.e. never replace non-essential defaults)
    columns         = Number of columns (default 5)
    type            = (tiles|) default:empty
    
-->
<#macro grid type="" class=true columns=4>
    <#if type=="tiles" || type="freetiles">
        <#local freewallNum = getRequestVar("catoFreewallIdNum")!0>
        <#local freewallNum = freewallNum + 1 />
        <#local dummy = setRequestVar("catoFreewallIdNum", freewallNum)>
        <#local id="freewall_id_${freewallNum!0}">
        <#-- FIXME: the "class" arg is not even used... 
        <#local classes = makeClassesArg(class, "...")>
        -->
        <div class="${styles.tile_container!}" id="${id!}">
            <#nested>
        </div>
        <script type="text/javascript">
        //<![CDATA[
         $(function() {
            $('#${id}').freetile({
                selector: '.${styles.tile_wrap!}'
            });
            <#--
            Alternative implementation of gridster.js
            $('#${id}').gridster({
                widget_selector: '.${styles.tile_wrap!}',
                min_cols:${columns},
                autogenerate_stylesheet:false
            }).disable();
            -->
         });
        //]]>
        </script>
    <#else>
        <#-- FIXME: the "class" arg is not even used...
        <#local classes = makeClassesArg(class, "...")>
        -->
        <#local defaultClass="${styles.grid_small!}${styles.grid_block!}2 ${styles.grid_medium!}${styles.grid_block!}4 ${styles.grid_large!}${styles.grid_block!}5">
            <#if (columns-2 > 0)>
                <#local class="${styles.grid_small!}${styles.grid_block!}${columns-2} ${styles.grid_medium!}${styles.grid_block!}${columns-1} ${styles.grid_large!}${styles.grid_block!}${columns}"/>
            <#else>
                <#local class="${styles.grid_large!}${styles.grid_block!}${columns}"/>
            </#if>
          <ul class="${class!defaultClass!}">
              <#nested>
          </ul>
    </#if>
</#macro>

<#-- 
*************
* Nav list
************
Since this is very foundation specific, this function may be dropped in future installations

    Usage example:  
    <@nav type="">
        <li>Text or <a href="">Anchor</a></li>
    </@nav>
    
    Or:
    <@nav type="magellan">
        <@mli arrival="MyTargetAnchor">Text or <a href="">Anchor</a></@mli>
    </@nav>
    
    <h3 ${mtarget("id")}>Jump Destination</h3>           
                    
   * General Attributes *
    type            = (inline|magellan|breadcrumbs) (default:inline)
    class           = Adds classes - please use "(small|medium|large)-block-grid-#"    
-->
<#macro nav type="inline">
    <#switch type>
        <#case "magellan">
            <div data-magellan-expedition="fixed">
              <dl class="sub-nav">
                <#nested>
              </dl>
            </div>
        <#break>
        <#case "breadcrumbs">
            <ul class="${styles.nav_breadcrumbs!}">
                <#nested>
            </ul>
        <#break>
        <#default>
            <ul class="${styles.list_inline!} ${styles.nav_subnav!}">
              <#nested>
            </ul>
        <#break>
    </#switch>
</#macro>

<#macro mli arrival="">
    <dd data-magellan-arrival="${arrival!}"><#nested></dd>
</#macro>

<#function mtarget id>
  <#local returnValue="data-magellan-destination=\"${id}\""/>
  <#return returnValue>
</#function>

<#function makeMagTargetAttribMap id>
  <#return {"data-magellan-destination":id}>
</#function>


<#-- 
*************
* Code Block
************
Creates a very basic wrapper for code blocks
    Usage example:  
    <@code type="java">
       //Some java content
    </@code>
                    
   * General Attributes *
    type            = (html|java|css|javascript|log) (default:html) 
-->
<#macro code type="html">
    <pre><code data-language="${type!}"><#compress>
    <#nested>
    </#compress>
    </code></pre>
</#macro>

<#-- 
*************
* Table
************
Helps define table. Required wrapper for all table sub-elem macros.

    Usage example:  
    <@table type="data-list" class="basic-table" id="my-table">
      <@thead>
        <@tr>
          <@th width="15%">col 1</@th>
          <@th width="85%">col 2</@th>
        </@tr>
      </@thead>
      <@tbody>
        <@tr class="my-row-class" valign="middle">
          <@td>data value 1</@td>
          <@td>data value 2</@td>
        </@tr>
      </@tbody>
    </@table>
                    
   * General Attributes *
    type            = [generic, data-list, data-complex, summary, fields], default generic
                      DEV NOTE: TODO: WARN: these are WIP types, may not be enough (important part is to label things for easy search)
                      generic: generic html table (free-form, complex), no features enabled by default
                      data-list: record-containing table, one data record per row (but row cells may be complex and may have tfoot)
                                 similar to a form widget "list" or "multi" table; intended to resemble these, to unify them.
                      data-complex: record-containing table, but with complex structure (more than one row per record, separators, etc.)
                                    there is no form widget equivalent of these and usually need some custom alt-row work.
                      summary: usually table with one or a few set rows of summary totals
                               e.g. order grand totals.
                      fields: label-value pairs for display, side-by-side, usually no header, roughly
                              this is especially for legacy Ofbiz code. it is somewhat still valid for display-only fields.
                              legacy Ofbiz code tables may be assigned this for input forms formatted with tables, but they
                              ultimately belong as @field and @row/@cell.
    class           = manual classes to add, as string, default "basic-table" for data, 
                      if specified as string replaces defaults (class=false prevents class)
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    id              = table id
    autoAltRows     = default false for now (temporarily false for type="data-list" as well, tbd)
    firstRowAlt     = default false
    inheritAltRows  = only for nested tables: if true, all rows in nested tables will inherit alt from parent table row
    useFootAltRoots = whether use alt row logic in foot or not
    cellspacing     = cellspacing, default 0 for most types except generic, set to "" to prevent setting.
    wrapIf/openOnly/closeOnly = advanced structure control, for esoteric cases
    attribs               = hash of other legacy <table attributes (mainly for those with dash in name)
    [inlineAttribs...]    = other legacy <table attributes and values, inlined
-->
<#macro table type="generic" class=true id="" cellspacing=true scrollable=false autoAltRows="" firstRowAlt="" inheritAltRows=false useFootAltRows=false wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#if open>
  <#-- save previous globals, for nesting -->
  <#local prevTableInfo = getRequestVar("catoCurrentTableInfo")!{}>
  <#local prevSectionInfo = getRequestVar("catoCurrentTableSectionInfo")!{}>
  <#local prevRowAltFlag = getRequestVar("catoCurrentTableRowAltFlag")!""> <#-- used to keep track of state (always boolean) -->
  <#local prevCurrentRowAlt = getRequestVar("catoCurrentTableCurrentRowAlt")!""> <#-- the actual alt value of current row (may be empty) -->
  <#local prevLastRowAlt = getRequestVar("catoCurrentTableLastRowAlt")!""> <#-- the actual alt value of "last" row (may be empty) -->
  <#if !autoAltRows?is_boolean>
    <#-- don't enable for all data-list tables by default for now, not sure wanted...
    <#local autoAltRows = (type == "data-list") || inheritAltRows>-->
    <#local autoAltRows = inheritAltRows>
  </#if>
  <#local styleName = type?replace("-","_")>
  <#if (!styleName?has_content) || styleName == "generic" || (!(styles["table_" + styleName]!false)?is_string)>
    <#local styleName = "default">
  </#if>
  <#local defaultClass = styles["table_" + styleName]!"">
  <#local classes = makeClassesArg(class, defaultClass)>
  <#if cellspacing?is_boolean>
    <#if cellspacing>
      <#local cellspacing = styles["table_" + styleName + "_cellspacing"]!"">
    <#else>
      <#local cellspacing = "">
    </#if>
  </#if>
  <#local catoCurrentTableInfo = {"type": type, "autoAltRows": autoAltRows,
    "inheritAltRows": inheritAltRows, "parentRowAlt": prevCurrentRowAlt, "useFootAltRows": useFootAltRows}>
  <#local dummy = setRequestVar("catoCurrentTableInfo", catoCurrentTableInfo)!>
  <#local catoCurrentTableSectionInfo = {"type": "body", "cellElem": "td"}>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", catoCurrentTableSectionInfo)!>
  <#-- note: catoCurrentTableRowAltFlag should always be boolean
       note: catoCurrentTableCurrentRowAlt probably doesn't need to be set here, but playing it safe -->
  <#if firstRowAlt?is_boolean>
    <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", firstRowAlt)!>
    <#local dummy = setRequestVar("catoCurrentTableCurrentRowAlt", firstRowAlt)!>
  <#elseif inheritAltRows>
    <#if prevCurrentRowAlt?is_boolean>
      <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", prevCurrentRowAlt)!>
    <#else>
      <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", false)!>
    </#if>
    <#local dummy = setRequestVar("catoCurrentTableCurrentRowAlt", prevCurrentRowAlt)!>
  <#else>
    <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", false)!>
    <#local dummy = setRequestVar("catoCurrentTableCurrentRowAlt", false)!>
  </#if>
  <#-- note: this var may be empty string (none) -->
  <#local dummy = setRequestVar("catoCurrentTableLastRowAlt", prevCurrentRowAlt)!>
  <#local style = "">
  <#-- need to save values on a stack if open-only! -->
  <#if !close>
    <#local dummy = pushRequestStack("catoCurrentTableStack", 
        {"prevTableInfo":prevTableInfo, "prevSectionInfo":prevSectionInfo, "prevRowAltFlag":prevRowAltFlag, 
         "prevCurrentRowAlt":prevCurrentRowAlt, "prevLastRowAlt":prevLastRowAlt, "scrollable":scrollable})>
  </#if>
  <#if scrollable>
  <#-- TODO: change this to something more foundation-like.
       this is a custom workaround to get scrolling, nothing else working. -->
  <div class="scrollable-table-container">
  </#if>
  <table<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#rt>
    <#lt><#if cellspacing?has_content> cellspacing="${cellspacing}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id", "cellspacing"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
</#if>
    <#nested>
<#if close>
  <#-- need to get values back from stack if close-only! -->
  <#if !open>
    <#local stackValues = popRequestStack("catoCurrentTableStack")!{}>
    <#local prevTableInfo = stackValues.prevTableInfo>
    <#local prevSectionInfo = stackValues.prevSectionInfo>
    <#local prevRowAltFlag = stackValues.prevRowAltFlag>
    <#local prevCurrentRowAlt = stackValues.prevCurrentRowAlt>
    <#local prevLastRowAlt = stackValues.prevLastRowAlt>
    <#local scrollable = stackValues.scrollable>
  </#if>
  </table>
  <#if scrollable>
  </div>
  </#if>
  <#local dummy = setRequestVar("catoCurrentTableInfo", prevTableInfo)!>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", prevSectionInfo)!>
  <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", prevRowAltFlag)!>
  <#local dummy = setRequestVar("catoCurrentTableCurrentRowAlt", prevCurrentRowAlt)!>
  <#local dummy = setRequestVar("catoCurrentTableLastRowAlt", prevLastRowAlt)!>
</#if>
</#macro>

<#macro thead class=true id="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#if open>
  <#local classes = makeClassesArg(class, "")>
  <#local prevTableSectionInfo = getRequestVar("catoCurrentTableSectionInfo")!{}>
  <#local catoCurrentTableSectionInfo = {"type": "head", "cellElem": "th"}>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", catoCurrentTableSectionInfo)!>
  <#-- need to save values on a stack if open-only! -->
  <#if !close>
    <#local dummy = pushRequestStack("catoCurrentTableHeadStack", 
        {"prevTableSectionInfo":prevTableSectionInfo})>
  </#if>
  <thead<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
</#if>
    <#nested>
<#if close>
  <#-- need to get values back from stack if close-only! -->
  <#if !open>
    <#local stackValues = popRequestStack("catoCurrentTableHeadStack")!{}>
    <#local prevTableSectionInfo = stackValues.prevTableSectionInfo>
  </#if>
  </thead>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", prevTableSectionInfo)!>
</#if>
</#macro>

<#macro tbody class=true id="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#if open>
  <#local classes = makeClassesArg(class, "")>
  <#local prevTableSectionInfo = getRequestVar("catoCurrentTableSectionInfo")!{}>
  <#local catoCurrentTableSectionInfo = {"type": "body", "cellElem": "td"}>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", catoCurrentTableSectionInfo)!>
  <#-- need to save values on a stack if open-only! -->
  <#if !close>
    <#local dummy = pushRequestStack("catoCurrentTableBodyStack", 
        {"prevTableSectionInfo":prevTableSectionInfo})>
  </#if>
  <tbody<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
</#if>
    <#nested>
<#if close>
  <#-- need to get values back from stack if close-only! -->
  <#if !open>
    <#local stackValues = popRequestStack("catoCurrentTableBodyStack")!{}>
    <#local prevTableSectionInfo = stackValues.prevTableSectionInfo>
  </#if>
  </tbody>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", prevTableSectionInfo)!>
</#if>
</#macro>

<#macro tfoot class=true id="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#if open>
  <#local classes = makeClassesArg(class, "")>
  <#local prevTableSectionInfo = getRequestVar("catoCurrentTableSectionInfo")!{}>
  <#local catoCurrentTableSectionInfo = {"type": "foot", "cellElem": "td"}>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", catoCurrentTableSectionInfo)!>
  <#-- need to save values on a stack if open-only! -->
  <#if !close>
    <#local dummy = pushRequestStack("catoCurrentTableFootStack", 
        {"prevTableSectionInfo":prevTableSectionInfo})>
  </#if>
  <tfoot<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
</#if>
    <#nested>
<#if close>
  <#-- need to get values back from stack if close-only! -->
  <#if !open>
    <#local stackValues = popRequestStack("catoCurrentTableFootStack")!{}>
    <#local prevTableSectionInfo = stackValues.prevTableSectionInfo>
  </#if>
  </tfoot>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", prevTableSectionInfo)!>
</#if>
</#macro>

<#-- 
*************
* Table row
************
Helps define table rows. takes care of alt row styles. must have a parent @table wrapper. 
                     
   * General Attributes *
    type            = [generic|content|meta|util], default generic or content (depends on table type)
                      generic: free-form row with no assumptions on content.
                               default for "generic" tables".
                      content: normal data or content row. exact meaning depends on table type.
                               default for all non-"generic" tables.
                               note that for "data-complex" this definition is currently relaxed.
                      meta: indicates this is a special info/status row (e.g. "No Records Found" message), not an actual content row.
                            meta rows are treated differently by default as are thead and tfoot rows.
                            exact meaning depends on table type.
                      util: indicates this is a special utility-only row meant to hold no real data, 
                            such as: spacer rows (<@tr type="util"><@td colspan=3><hr /></@td></@tr>)
                            TODO: this isn't handled yet but SHOULD be used in templates anyhow.
    class           = css classes
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    id              = row id
    useAlt          = boolean, if specified, can manually enable/disable whether alternate row code runs per-row
    alt             = boolean, if specified, override the automatic auto-alt styling to specific value true or false (manual mode)
                      note: at current time, alt on non-body rows (except foot rows if enabled in @table) does not affect
                            next row's alt (unless groupLast used explicit on next) logic
    groupLast       = boolean, if specified, considers row logically grouped with last row;
                      sets alt to exact same as last row
    groupParent     = boolean, nested tables only, if specified, considers row logically grouped with parent row;
                      sets alt to exact same as parent row
    selected        = boolean, if specified and true marked as selected
    wrapIf/openOnly/CloseOnly = advanced structure control, for esoteric cases (should omit nested for openOnly/closeOnly)
    attribs               = hash of other legacy <tr attributes (mainly for those with dash in name)
    [inlineAttribs...]    = other legacy <tr attributes and values, inlined
-->
<#macro tr type="" class=true id="" useAlt="" alt="" groupLast="" groupParent="" selected="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#local catoCurrentTableInfo = getRequestVar("catoCurrentTableInfo")!{}>
<#local catoCurrentTableSectionInfo = getRequestVar("catoCurrentTableSectionInfo")!{}>
<#local catoCurrentTableRowAltFlag = getRequestVar("catoCurrentTableRowAltFlag")!false>
<#local catoCurrentTableLastRowAlt = getRequestVar("catoCurrentTableLastRowAlt")!"">
<#if open>
  <#local tableType = (catoCurrentTableInfo.type)!"generic">
  <#local sectionType = (catoCurrentTableSectionInfo.type)!"body">
  <#if !type?has_content>
    <#local type = (!tableType?has_content || tableType == "generic")?string("generic", "content")>
  </#if>
  <#local metaRow = (type == "meta")>
  <#local isRegAltRow = !metaRow && ((sectionType == "body") || (sectionType == "foot" && ((catoCurrentTableInfo.useFootAltRows)!)==true))>
  <#if !(useAlt?is_boolean && useAlt == false)>
    <#if !alt?is_boolean>
      <#if groupLast?is_boolean && groupLast == true>
        <#local alt = catoCurrentTableLastRowAlt!""> <#-- may be empty string (none) -->
      <#elseif groupParent?is_boolean && groupParent == true>
        <#local alt = (catoCurrentTableInfo.parentRowAlt)!"">
      <#elseif (isRegAltRow && ((catoCurrentTableInfo.autoAltRows)!false)==true)>
        <#if ((catoCurrentTableInfo.inheritAltRows)!false)==true>
          <#local alt = (catoCurrentTableInfo.parentRowAlt)!"">
        <#else>
          <#local alt = catoCurrentTableRowAltFlag!false> <#-- always boolean -->
        </#if>
      <#elseif useAlt?is_boolean && useAlt == true>
        <#-- forced -->
        <#local alt = catoCurrentTableRowAltFlag!false>
      </#if>
    </#if>
  </#if>
  <#-- save the "effective" or "real" current row alt -->
  <#local catoCurrentTableCurrentRowAlt = alt>
  <#local dummy = setRequestVar("catoCurrentTableCurrentRowAlt", catoCurrentTableCurrentRowAlt)!>
  <#-- need to save values on a stack if open-only! -->
  <#if !close>
    <#local dummy = pushRequestStack("catoCurrentTableRowStack", 
        {"type":type, "useAlt":useAlt, "alt":alt, "isRegAltRow":isRegAltRow})>
  </#if>
  <#local classes = makeClassesArg(class, "")>
  <#if alt?is_boolean>
    <#local classes = (classes + " " + alt?string(styles.row_alt!, styles.row_reg!))?trim>
  </#if>
  <#if selected?is_boolean && selected == true>
    <#local classes = (classes + " " + styles.row_selected!)?trim>
  </#if>
  <tr<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
</#if>    
    <#nested>
<#if close>
  <#-- need to get values back from stack if close-only! -->
  <#if !open>
    <#local stackValues = popRequestStack("catoCurrentTableRowStack")!{}>
    <#local type = stackValues.type>
    <#local useAlt = stackValues.useAlt>
    <#local alt = stackValues.alt>
    <#local isRegAltRow = stackValues.isRegAltRow>
  </#if>
  </tr>
  <#if !(useAlt?is_boolean && useAlt == false)>
    <#-- note: isRegAltRow check here could be removed but maybe better to keep? only auto-toggle for regular rows... -->
    <#if alt?is_boolean && isRegAltRow> <#-- not needed:  && ((catoCurrentTableInfo.inheritAltRows)!)==false -->
      <#local catoCurrentTableRowAltFlag = !alt>
      <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", catoCurrentTableRowAltFlag)!>
    </#if>
  </#if>
  <#-- note: may be empty string, that's ok, will record if last was disabled so groupLast always makes sense -->
  <#local catoCurrentTableLastRowAlt = alt>
  <#local dummy = setRequestVar("catoCurrentTableLastRowAlt", catoCurrentTableLastRowAlt)!>
</#if>
</#macro>

<#-- 
*************
* Table cell
************
Helps define table cells.
                    
   * General Attributes *
    class           = css classes 
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    id              = cell id
    wrapIf/openOnly/CloseOnly = advanced structure control, for esoteric cases (should omit nested for openOnly/closeOnly)
    attribs               = hash of other legacy <th and <td attributes (mainly for those with dash in name)
    [inlineAttribs...]    = other legacy <th and <td attributes and values
-->
<#macro th class=true id="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
  <#local classes = makeClassesArg(class, "")>
  <#if open><th<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>></#if><#nested><#if close></th></#if>
</#macro>

<#macro td class=true id="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
  <#local classes = makeClassesArg(class, "")>
  <#if open><td<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>></#if><#nested><#if close></td></#if>
</#macro>

<#-- 
*************
* Data row class string
************
Helps build common data/table row class string (odd, even, etc.). Common pattern.
Using @table macro is preferred.
    Usage example:  
    <tr<@dataRowClassStr class="myClass" alt=false/>>
                    
   * General Attributes *
    class           = css classes 
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    alt             = boolean, if true is alternate row (odd), if false regular (even)
    selected        = boolean, if true marked as selected
-->
<#macro dataRowClassStr class=true alt="" selected="">
  <#local classes = makeClassesArg(class, "")>
  <#if alt?is_boolean>
    <#local classes = (classes + " " + alt?string(styles.row_alt!, styles.row_reg!))?trim>
  </#if>
  <#if selected?is_boolean && selected == true>
    <#local classes = (classes + " " + styles.row_selected!)?trim>
  </#if>
  <#if classes?has_content> class="${classes}"</#if>
</#macro>

<#-- 
*************
* Tile
************
Creates a very basic wrapper for tiles (can be used in metro designs).
Please be aware that this is neither based on standard bootstrap, nor foundation. 
It is loosely based on http://metroui.org.ua/tiles.html 

    Usage example:  
    <@tile type="small">
       // content
    </@tile>
                    
   * General Attributes *
    type            = (small|normal|wide|large|big|super) (default:normal)
    title           = Title
    class           = css classes 
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    link            = Link URL
    id              = field id
    color           = (0|1|2|3|4|5|6|7) defaul:0 (empty)   
    icon            = Set icon code (http://zurb.com/playground/foundation-icon-fonts-3)
    image           = Set a background image-url (icon won't be shown if not empty)
-->
<#macro tile type="normal" title="" class=true id="" link="" color=0 icon="" image="">
    <#local classes = makeClassesArg(class, "")>
    <#local nested><#nested></#local>
    <div class="${styles.tile_wrap!} ${styles.tile_wrap!}-${type!}<#if classes?has_content> ${classes}</#if> ${styles.tile_color!}${color!}"<#if id?has_content> id="${id!}"</#if> data-sizex="${calcTileSize("x",type!)}" data-sizey="${calcTileSize("y",type!)}">
        <#if image?has_content><div class="${styles.tile_image!}" style="background-image: url(${image!})"></div></#if>
        <div class="${styles.tile_content!}">
            <#if link?has_content><a href="${link!}"></#if>
            <#if icon?has_content && !icon?starts_with("AdminTileIcon") && !image?has_content><span class="${styles.tile_icon!}"><i class="${icon!}"></i></span></#if>
            <#if nested?has_content><span class="${styles.tile_overlay!}"><#nested></span></#if>
            <#if title?has_content><span class="${styles.tile_title!}">${title!}</span></#if>
            <#if link?has_content></a></#if>
        </div>
    </div>  
</#macro>

<#function calcTileSize type="x" value="normal">
    <#local tileSizeX={"small":0,"normal":1,"wide":2,"large":2,"big":3,"super":4}/>
    <#local tileSizeY={"small":0,"normal":1,"wide":1,"large":2,"big":3,"super":4}/>
    <#if type="x">
        <#return tileSizeX[value]/>
    <#else>
        <#return tileSizeY[value]/>
    </#if>
</#function>

<#-- 
*************
* Chart Macro
************
Foundation Pizza: http://zurb.com/playground/pizza-amore-charts-and-graphs (customization through _base.scss)
Chart.js: http://www.chartjs.org/docs/ (customization through _charsjs.scss)

    Usage example:  
    <@chart type="bar" >
        <@chartdata value="36" title="Peperoni"/> 
    </@chart>              
                    
   * General Attributes *
    type           = (pie|bar|line) (default:pie)
    library        = (foundation|chart) (default:foundation)
    title          = Data Title  (default:empty)
-->
<#macro chart type="pie" library="foundation" title="">
    <#local fieldIdNum = getRequestVar("catoFieldIdNum")!0>
    <#local fieldIdNum = fieldIdNum + 1 />
    <#local dummy = setRequestVar("catoFieldIdNum", fieldIdNum)>
    <#global chartLibrary = library!"foundation"/>
    <#if chartLibrary=="foundation">
        <@row>
        <@cell columns=3>    
        <ul data-${type!}-id="chart_${renderSeqNumber!}_${fieldIdNum!}" class="${styles.chart_legend!}">
            <#nested/>
        </ul>
        </@cell>
        <@cell columns=9><div id="chart_${renderSeqNumber!}_${fieldIdNum!}" style="height:300px;"></div></@cell>
        </@row>
    <#else>
        <#global chartId = "chart_${renderSeqNumber!}_${fieldIdNum!}"/>
        <#global chartType = type/>
        <canvas id="${chartId!}" class="${styles.grid_large!}12 chart-data" height="300" style="height:300px;"></canvas>
        <script type="text/javascript">
        //<![CDATA[
            $(function(){
                var chartDataEl = $('.chart-data:first-of-type');
                var chartData = chartDataEl.sassToJs({pseudoEl:":before", cssProperty: "content"});
                var options ={
                    animation: false,
                    responsive: true,
                    maintainAspectRatio: true,
                    scaleLineColor: chartData.scaleLineColor,
                    scaleFontFamily: chartData.scaleFontFamily,
                    scaleFontSize: chartData.scaleFontSize,
                    scaleFontColor: chartData.scaleFontColor,
                    scaleShowLabels: chartData.scaleShowLabels,
                    scaleShowLabels: chartData.scaleShowLabels,
                    scaleShowLine : chartData.scaleShowLine,
                    angleShowLineOut : chartData.angleShowLineOut,
                    scaleBeginAtZero : chartData.scaleBeginAtZero,
                    showTooltips: chartData.showTooltips,
                    tooltipFillColor: chartData.tooltipFillColor,
                    tooltipFontFamily: chartData.tolltipFontFamily,
                    tooltipFontSize: chartData.tooltipFontSize,
                    tooltipFontStyle: chartData.tooltipFontStyle,
                    tooltipFontColor: chartData.tooltipFontColor,
                    tooltipTitleFontFamily: chartData.tooltipTitleFontFamily,
                    tooltipTitleFontSize: chartData.tooltipTitleFontSize,
                    tooltipTitleFontStyle: chartData.tooltipTitleFontStyle,
                    tooltipTitleFontColor: chartData.tooltipTitleFontColor,
                    tooltipYPadding: chartData.tooltipYPadding,
                    tooltipXPadding: chartData.tooltipXPadding,
                    tooltipCaretSize: chartData.tooltipCaretSize,
                    tooltipCornerRadius: chartData.tooltipCornerRadius,
                    datasetFill : chartData.datasetFill,
                    tooltipTemplate: "<%if (label){%><%=label%>: <%}%><%= value %>",
                    multiTooltipTemplate: "<%= value %>",
                    pointDot : chartData.pointDot,
                    pointHitDetectionRadius : chartData.pointHitDetectionRadius,
                    pointDotRadius : chartData.pointDotRadius,
                    pointDotStrokeWidth : chartData.pointDotStrokeWidth,
                    <#if type=="line">
                    bezierCurve : chartData.bezierCurve,
                    bezierCurveTension : chartData.bezierCurveTension,
                    legendTemplate : "<ul class=\"legend <%=name.toLowerCase()%>-legend\"><% for (var i=0; i<datasets.length; i++){%><li style=\"color:<%=datasets[i].strokeColor%> !important \"><span style=\"color:#efefef !important \"><%=datasets[i].value%>  <%if(datasets[i].label){%><%=datasets[i].label%><%}%></span></li><%}%></ul>",
                    dataLabels: chartData.dataLabels
                    <#elseif type=="pie">
                    legendTemplate : "<ul class=\"legend <%=name.toLowerCase()%>-legend\"><% for (var i=0; i<segments.length; i++){%><li style=\"color:<%=segments[i].fillColor%> !important \"><span style=\"color:#efefef !important \"><%=segments[i].value%>  <%if(segments[i].label){%><%=segments[i].label%><%}%></span></li><%}%></ul>"
                    <#else>
                    legendTemplate : "<ul class=\"<%=name.toLowerCase()%>-legend\"><% for (var i=0; i<datasets.length; i++){%><li><span style=\"background-color:<%=datasets[i].strokeColor%>\"></span><%if(datasets[i].label){%><%=datasets[i].label%><%}%></li><%}%></ul>"
                    </#if>
                    };
                var ctx_${renderSeqNumber!}_${fieldIdNum!} = $('#${chartId!}').get(0).getContext("2d");
                <#if type=="pie">
                var data = [];
                <#else>
                var data = {
                        labels :[],
                        datasets: [
                            {
                              fillColor: chartData.fillColor,
                              strokeColor: chartData.strokeColor,
                              pointColor: chartData.pointColor,
                              pointStrokeColor: chartData.pointStrokeColor,
                              pointHighlightFill: chartData.pointHighlightFill,
                              pointHighlightStroke: chartData.pointHighlightStroke,
                              label: "",
                              data: []
                            }
                            ]
                    };
                </#if>
                var ${chartId!} = new Chart(ctx_${renderSeqNumber!}_${fieldIdNum!})<#if type=="bar">.Bar(data,options);</#if><#if type=="line">.Line(data,options);</#if><#if type=="pie">.Pie(data,options);</#if>
                <#nested/>
            });
        //]]>
        </script>
    </#if>
</#macro>

<#macro chartdata title value value2="">
    <#if chartLibrary=="foundation">
        <li <#if value2?has_content>data-y="${value!}" data-x="${value2!}"<#else>data-value="${value!}"</#if>>${title!}</li>
    <#else>
        <#if chartType="line" || chartType="bar">
            ${chartId!}.addData([<#if value?has_content>${value!}</#if>]<#if title?has_content>,"${title!}"</#if>);
        <#else>
            ${chartId!}.addData({value:${value!},color:chartData.color,highlight: chartData.highlight<#if title?has_content>,label:"${title!}"</#if>});
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
      <#local opts = opts+{"progBarId":"${id}"}>
    </#if>
    <@progressScript options=opts htmlwrap=true />
  </#if>
</#macro>

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
            <#local options = options + {"successRedirectUrl":options.successRedirectUrl?replace("&amp;", "&")}>
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
* Menu
************
Menu macro, mainly intended for small inline menu definitions in templates, but can substitute for widget menu
definitions if needed.

It can be used in two forms:
  <#assign items = [{"type":"link", ...}, {"type":"link", ...}, ...]>
  <@menu ... items=items />
  OR
  <@menu ...>
    <@menuitem type="link" ... />
    <@menuitem type="link" ... />
    ...
  </@menu>
  
In the first, each hash of the items list represents a menu item with the exact same arguments as the @menuitem macro.
The first method gives the @menu macro more control over the items, and to delegate the definitions, while 
second is cleaner to express.

Note that both macros support arguments passed in a hash (or map) using the "args" argument, so the entire menu definition
can be delegated in infinite ways (even to data prep). The inline args have priority over the hash args, as would be expected.
                  
FIXME? doesn't survive screens.render (uses #globals only), but probably doesn't need to.                  
                    
   * General Attributes *
    type            = menu type: [generic|section|section-inline|main|tab|subtab|button|...], default generic (but discouraged; prefer specific)
    inlineItems     = boolean, if true, generate only items, not menu container
    class           = menu class style. can be boolean true/false or string, if string
                      starts with "+" the classes are in addition to defaults, otherwise replace defaults.
    id              = menu id
    style           = legacy menu style (for <ul element)
    attribs         = hash of other menu attribs (for <ul element, especially those with dashes)
    items           = list of hashes, where each hash contains arguments representing a menu item,
                      same as @menuitem macro parameters.
                      alternatively, the items can be specified as nested content.
    preItems        = special-case list of hashes of items, added before items and #nested.
                      excluded from sorting.
                      avoid use unless specific need; may be needed by cato menu handling.
                      **SPECIAL**: this can also be specified as the #global variable catoMenuPreItems,
                          which if present is read and unset by macro (does not work on nested menus).
                          force-disabled if preItems==false.
    postItems       = special-case list of hashes of items, added after items and #nested
                      excluded from sorting.
                      avoid use unless specific need; may be needed by cato menu handling.
                      **SPECIAL**: this can also be specified as the #global variable catoMenuPostItems,
                          which if present is read and unset by macro (does not work on nested menus).
                          force-disabled if postItems==false.
    sort,
    sortBy,
    sortDesc        = items sorting behavior; will only work if items are specified
                      through items list of hashes, currently does not apply to 
                      nested items. by default, sorts by text, or sortBy can specify a menu item arg to sort by.
                      normally case-insensitive.
    nestedFirst     = default false, if true, use nested items before items list, otherwise items list always first.
                      usually use only one of alternatives but versatile.
-->
<#macro menu args={} inlineArgs...>
  <#local type = inlineArgs.type!args.type!"generic">
  <#local inlineItems = inlineArgs.inlineItems!args.inlineItems!false>
  <#local class = inlineArgs.class!args.class!true>
  <#local id = inlineArgs.id!args.id!"">
  <#local style = inlineArgs.style!args.style!"">
  <#local attribs = inlineArgs.attribs!args.attribs!"">
  <#local items = inlineArgs.items!args.items!true>
  <#local preItems = inlineArgs.preItems!args.preItems!true>
  <#local postItems = inlineArgs.postItems!args.postItems!true>
  <#local sort = inlineArgs.sort!args.sort!false>
  <#local sortBy = inlineArgs.sortBy!args.sortBy!"">
  <#local sortDesc = inlineArgs.sortDesc!args.sortDesc!false>
  <#local nestedFirst = inlineArgs.nestedFirst!args.nestedFirst!false>
  <#t>
  <#local preItemsGlobal = catoMenuPreItems!"">
  <#global catoMenuPreItems = "">
  <#local postItemsGlobal = catoMenuPostItems!"">
  <#global catoMenuPostItems = "">
  <#t>
  <#local prevMenuInfo = catoCurrentMenuInfo!>
  <#local prevMenuItemIndex = catoCurrentMenuItemIndex!>
  <#local styleName = type?replace("-","_")>
  <#if (!styleName?has_content) || styleName == "generic" || (!(styles["menu_" + styleName]!false)?is_string)>
    <#local styleName = "default">
  </#if>
  <#local menuInfo = {"type":type, "styleName":styleName, 
    "inlineItems":inlineItems, "class":class, "id":id, "style":style, "attribs":attribs,
    "preItems":preItems, "postItems":postItems, "sort":sort, "sortBy":sortBy, "sortDesc":sortDesc, "nestedFirst":nestedFirst}>
  <#global catoCurrentMenuInfo = menuInfo>
  <#global catoCurrentMenuItemIndex = 0>
  <#t>
  <#local classes = makeClassesArg(class, styles["menu_" + styleName]!"")>
  <#t>
  <#if !inlineItems>
    <ul<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id", "style"]/></#if>>
  </#if>
  <#if !(preItems?is_boolean && preItems == false)>
    <#if preItems?is_sequence>
      <#list preItems as item>
        <@menuitem args=item />
      </#list>
    <#elseif preItemsGlobal?is_sequence>
      <#list preItemsGlobal as item>
        <@menuitem args=item />
      </#list>      
    </#if>
  </#if>
  <#if !(items?is_boolean && items == false)>
    <#if nestedFirst>
        <#nested>
    </#if>
    <#if items?is_sequence>
      <#if sort && (!sortBy?has_content)>
        <#local sortBy = "text">
      </#if>
      <#if sortBy?has_content>
        <#local items = items?sort_by(sortBy)>
        <#if sortDesc>
          <#local items = items?reverse>
        </#if>
      </#if>
      <#list items as item>
        <@menuitem args=item />
      </#list>
    </#if>
    <#if !nestedFirst>
        <#nested>
    </#if>
  </#if>
  <#if !(postItems?is_boolean && postItems == false)>
    <#if postItems?is_sequence>
      <#list postItems as item>
        <@menuitem args=item />
      </#list>
    <#elseif postItemsGlobal?is_sequence>
      <#list postItemsGlobal as item>
        <@menuitem args=item />
      </#list>
    </#if>
  </#if>
  <#if !inlineItems>
    </ul>
  </#if>
  <#t>
  <#global catoCurrentMenuInfo = prevMenuInfo>
  <#global catoCurrentMenuItemIndex = prevMenuItemIndex>
  <#global catoLastMenuInfo = menuInfo>
</#macro>

<#-- 
*************
* Menu Item
************
Menu item macro. Must ALWAYS be enclosed in a @menu macro (see @menu options if need to generate items only).
             
   * General Attributes *
    type            = menu item (content) type: [generic|link|text|submit], default generic (but discouraged; prefer specific)
    class           = menu item class (for <li> element)
    id              = menu item id
    style           = legacy menu item style (for <li> element)
    attribs         = other menu item attributes (for <li> element, especially those with dashes in names)
    contentClass    = menu item content class (for <a>, <span> or <input> element)
    contentId       = menu item content id
    contentStyle    = legacy menu item content style
    contentAttribs  = other menu item content attributes (for <a>, <span> or <input> element, especially those with dashes in names)
    text            = text to use as content. for now ALWAYS use this argument to specify
                      text, not nested content.
                      TODO: clarify nested content usage (because may have nested menus?)
    href            = content link, for "link" type
                      Also supports ofbiz request URLs using the notation: ofbizUrl:// (see interpretRequestUri function)
    onClick         = onClick (for content elem)
    title           = logical title attribute of content (link)
    disabled        = whether disabled, default false
    selected        = whether selected or not, default false
    active          = whether active or not, default false
    nestedHtml      = alternative to #nested content, so can be passed in @menu items hash list
    nestedMenu      = alternative to nestedHtml and #nested content, is a hash of @menu attribs
                      for menu to use as sub-menu.
    wrapNested      = if true, nested content is wrapped in link or span element. default false (nested outside, following).
    nestedFirst     = if true, nested content comes before content elem. default false (comes after content elem/text).
-->
<#macro menuitem args={} inlineArgs...>
  <#local type = inlineArgs.type!args.type!"generic">
  <#local class = inlineArgs.class!args.class!true>
  <#local id = inlineArgs.id!args.id!"">
  <#local style = inlineArgs.style!args.style!"">
  <#local attribs = inlineArgs.attribs!args.attribs!"">
  <#local contentClass = inlineArgs.contentClass!args.contentClass!true>
  <#local contentId = inlineArgs.contentId!args.contentId!"">
  <#local contentStyle = inlineArgs.contentStyle!args.contentStyle!"">
  <#local contentAttribs = inlineArgs.contentAttribs!args.contentAttribs!"">
  <#local text = inlineArgs.text!args.text!"">
  <#local href = inlineArgs.href!args.href!true>
  <#local fullPath = inlineArgs.fullPath!args.fullPath!false>
  <#local secure = inlineArgs.secure!args.secure!false>
  <#local encode = inlineArgs.encode!args.encode!true>
  <#local onClick = inlineArgs.onClick!args.onClick!"">
  <#local disabled = inlineArgs.disabled!args.disabled!false>
  <#local selected = inlineArgs.selected!args.selected!false>
  <#local active = inlineArgs.active!args.active!false>
  <#local target = inlineArgs.target!args.target!"">
  <#local nestedHtml = inlineArgs.nestedHtml!args.nestedHtml!true>
  <#local nestedMenu = inlineArgs.nestedMenu!args.nestedMenu!false>
  <#local wrapNested = inlineArgs.wrapNested!args.wrapNested!false>
  <#local nestedFirst = inlineArgs.nestedFirst!args.nestedFirst!false>
  <#t>
  <#local menuType = (catoCurrentMenuInfo.type)!"">
  <#local menuStyleName = (catoCurrentMenuInfo.styleName)!"">
  <#t>
  <#local classes = makeClassesArg(class, styles["menu_" + menuStyleName + "_item"]!"")>
  <#t>
  <#if type == "link">
    <#local defaultContentClass = styles["menu_" + menuStyleName + "_itemlink"]!"">
  <#elseif type == "text">
    <#local defaultContentClass = "text-entry">
  <#elseif type == "submit">
    <#local defaultContentClass = "">
  <#else>
    <#local defaultContentClass = "">
  </#if>
  <#local contentClasses = makeClassesArg(contentClass, defaultContentClass)>
  <#t>
  <#if disabled>
    <#local classes = (classes + " disabled")?trim>
    <#local contentClasses = (contentClasses + " disabled")?trim>
    <#local href = "javascript:void(0);">
  </#if>
  <#if selected>
    <#local classes = (classes + " selected")?trim>
    <#local contentClasses = (contentClasses + " selected")?trim>
  </#if>
  <#if active>
    <#local classes = (classes + " active")?trim>
    <#local contentClasses = (contentClasses + " active")?trim>
  </#if>
  <li<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id", "style"]/></#if>><#rt>
    <#if !nestedHtml?is_boolean>
      <#-- use nestedHtml -->
    <#elseif !nestedMenu?is_boolean>
      <#local nestedHtml><@menu args=nestedMenu /></#local>
    <#else>
      <#local nestedHtml><#nested></#local>
    </#if>
    <#if !wrapNested && nestedFirst>${nestedHtml}</#if>
    <#if type == "link">
      <#if !href?is_string>
        <#local href = "javascript:void(0);">
      </#if>
      <#local href = interpretRequestUri(href)>
      <a href="${href}"<#if onClick?has_content> onclick="${onClick}"</#if><#if contentClasses?has_content> class="${contentClasses}"</#if><#if contentId?has_content> id="${contentId}"</#if><#if contentStyle?has_content> style="${contentStyle}"</#if><#if contentAttribs?has_content><@elemAttribStr attribs=contentAttribs exclude=["class","id","style","href","onclick","target","title"]/></#if><#if target?has_content> target="${target}"</#if><#if title?has_content> title="${title}"</#if>><#if wrapNested && nestedFirst>${nestedHtml}</#if><#if text?has_content>${text}</#if><#if wrapNested && !nestedFirst>${nestedHtml}</#if></a>
    <#elseif type == "text">
      <span<#if contentClasses?has_content> class="${contentClasses}"</#if><#if contentId?has_content> id="${contentId}"</#if><#if contentStyle?has_content> style="${contentStyle}"</#if><#if contentAttribs?has_content><@elemAttribStr attribs=contentAttribs exclude=["class","id","style","onclick"]/></#if><#if onClick?has_content> onclick="${onClick}"</#if>><#if wrapNested && nestedFirst>${nestedHtml}</#if><#if text?has_content>${text}</#if><#if wrapNested && !nestedFirst>${nestedHtml}</#if></span>
    <#elseif type == "submit">
      <#if wrapNested && nestedFirst>${nestedHtml}</#if><input type="submit"<#if contentClasses?has_content> class="${contentClasses}"</#if><#if contentId?has_content> id="${contentId}"</#if><#if contentStyle?has_content> style="${contentStyle}"</#if><#if contentAttribs?has_content><@elemAttribStr attribs=contentAttribs exclude=["class","id","style","value","onclick","disabled","type"]/></#if> value="<#if text?has_content>${text}</#if>"<#if onClick?has_content> onclick="${onClick}"</#if><#if disabled> disabled="disabled"</#if> /><#if wrapNested && !nestedFirst>${nestedHtml}</#if>
    <#else>
      <#if text?has_content>${text}</#if><#if wrapNested>${nestedHtml}</#if>
    </#if>
    <#if !wrapNested && !nestedFirst>${nestedHtml}</#if>
  </li><#lt>
  <#global catoCurrentMenuItemIndex = catoCurrentMenuItemIndex + 1>
</#macro>



<#-- 
*************************************
* DEV MACROS *
*************************************
* For development and debugging purposes.
-->

<#-- 
*************
* printVars macro
************
Iterates over all variable attributes & functions and prints in table; useful for determining current vars in context

Usage example:  
    <@printVars />           
                    
   * General Attributes *
    var           = Custom var to be printed (default:context)
-->
<#macro printVars var=context>
    <table>
    <#list var?keys as key>
        <@printVar key=key value=var.get(key)/>
    </#list>
    </table>
</#macro>

<#macro printVar key value="">
  <tr><td style="width:200px;">${key}</td>
  <td>
  <#local var = value/>
  <#if var?has_content>
      <#attempt><#compress>
         <#if var?is_string>
            ${var}
         </#if>
        
        <#if var?is_boolean]
            ${var?string}
        </#if>
    
        <#if var?is_date>
            ${var?time}
        </#if>
    
        <#if var?is_number>
            ${var?string}
        </#if>
        
        <#if var?is_collection>
            <table>
            <#list var?sort()?keys as key>
                <tr><td>${key}</td><td><@printVar var=var[key]/></td></tr>
            </#list>
            </table>
        </#if>
        
        <#if var?is_sequence>
            <ol>
            <#list var?sort() as i>
                <li><@printVar var=i/></li>
            </#list>
            </ol>
        </#if>
        
      </#compress>
      <#recover>
        <@alert type="error">${(.error)!"(generic)"}</@alert>
      </#attempt>
      </td>
    </tr>
  <#else>
  </#if>
</#macro>



<#-- 
*************************************
* END OF FILE INCLUDES *
*************************************
* Files that must be included at end.
-->

<#-- Macro library imports. These must be included at end due to circular dependencies (FIXME?).
     In a sense the widget macros depend on Cato markup, but Cato macros currently call widget macros. 
     
     TODO?: to be confirmed, but I think these could remain static includes even if ofbiz is to support
     per-site macro library overrides; the implementations in this file depend on the 
     implementation details of the default stock macros, and this is an implementation detail itself. 
     instead we would have per-site cato lib overrides. debatable... 
     EDIT: this will on depend on the implementation of macros like @field. if it's split up,
     then static includes make sense. but if it's left as-is and the stock macros are the only way
     to have per-field-type markup overrides, then might want a per-site macro lib loading here... -->
<#-- use the stock htmlTemplate.ftl file to import all; changes nothing, simply faster
<#import "component://widget/templates/htmlFormMacroLibrary.ftl" as formlib>
<#import "component://widget/templates/htmlScreenMacroLibrary.ftl" as screenlib> 
<#import "component://widget/templates/htmlMenuMacroLibrary.ftl" as menulib>-->
<#import "component://common/webcommon/includes/htmlTemplate.ftl" as formlib>
<#import "component://common/webcommon/includes/htmlTemplate.ftl" as screenlib> 
<#import "component://common/webcommon/includes/htmlTemplate.ftl" as menulib>


