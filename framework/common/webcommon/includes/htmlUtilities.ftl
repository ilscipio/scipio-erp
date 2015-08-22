<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#compress>
<#--
* 
* A set of utility macros to be used for page rendering.
* Automatically included at all times
*
-->
<#include "component://widget/templates/htmlFormMacroLibrary.ftl"/>
<#include StringUtil.wrapString("component://widget/templates/htmlScreenMacroLibrary.ftl") /> 
<#include StringUtil.wrapString("component://widget/templates/htmlMenuMacroLibrary.ftl") />
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

<#global requiredScriptOfbizUrls = []>
<#macro requireScriptOfbizUrl uri htmlwrap=false>
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
    <#global requiredScriptOfbizUrls = requiredScriptOfbizUrls + [uri]>
  </#if>
</#macro>

<#-- 
******************
* UTILITY MACROS *
******************
-->
<#-- 
*************
* Field Macro
************ 
    Usage example:  
    <@field attr="" />
    
    * General Attributes *
    type            = form element of type [input,textarea,datetime,select,checkbox,radio]
    label           = form label
    columns         = int value for columns for field (overrides classes)
    tooltip         = Small field description - to be displayed to the customer
    name            = field name
    value           = field value
    class           = css classes
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
    addClass        = css classes in addition to default and class param
        
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
    <#nested>       = button(s) (<input, <a, <button) to include
    progressOptions = if this is an upload form, specify progress upload options, enables progress next to buttons. 
                      see @progress[Script] macro[s]. should specify formSel, (progBarId and/or progTextBoxId), and others.
-->
<#macro field type="" label="" name="" value="" currentValue="" defaultValue="" class="${styles.grid_large!}12" size=20 maxlength="" id="" onClick="" 
        disabled=false placeholder="" autoCompleteUrl="" mask=false alert="false" readonly=false rows="4" 
        cols="50" dateType="date" multiple="" checked=false collapse=false tooltip="" columns="" norows=false nocells=false
        fieldFormName="" formName="" postfix=false required=false addClass="" items=[] autocomplete=true progressOptions={}>
<#-- fieldIdNum will always increment throughout the page -->
<#global fieldIdNum=(fieldIdNum!0)+1 />

<#local radioSingle = (type=="radio" && !items?has_content)>

<#if !id?has_content>
    <#local id="field_id_${renderSeqNumber!}_${fieldIdNum!0}">
</#if>
<#local classes = class/>
<#local columnspostfix=0/>
<#if postfix>
    <#local columnspostfix=1/>
    <#local collapse=true/>
    <#local classes="${styles.grid_small!}${12-columnspostfix}"/>
</#if>

<#if required && (!containsStyleName(class, "required"))>
    <#local class = class + " required">
</#if>

<#if addClass?has_content>
    <#local class = class + " " + addClass>
</#if>

<@row collapse=collapse!false norows=norows class="form-field-entry">
    <#if label?has_content && type != "submitarea">
        <#local subclasses="${styles.grid_small!}3 ${styles.grid_large!}2"/>
        <#local classes="${styles.grid_small!}${9-columnspostfix} ${styles.grid_large!}${10-columnspostfix}"/>
        
        <#if columns?has_content>
            <#local subclasses="${styles.grid_small!}${12-columns+1} ${styles.grid_large!}${12-columns}"/>
            <#local classes="${styles.grid_small!}${columns-columnspostfix-1} ${styles.grid_large!}${columns-columnspostfix}"/>
        </#if>
        
        <#if !radioSingle>
            <@cell class=subclasses+" field-entry-title" nocells=nocells>
                <#if type=="checkbox" || collapse==false>
                    <label class="form-field-label"<#if id?has_content> for="${id}"</#if>>${label}</label>
                <#else>
                    <span class="prefix form-field-label">${label}</span>
                </#if>           
            </@cell>
        </#if>
    </#if>
    <@cell class="${classes!}"+" field-entry-widget" nocells=nocells>
        <#switch type>
          <#case "input">
                <@renderTextField name=name 
                                  className=class 
                                  alert=alert 
                                  value=value 
                                  textSize=size 
                                  maxlength=maxlength 
                                  id=id 
                                  event="onClick" 
                                  action=onClick 
                                  disabled=disabled 
                                  clientAutocomplete="" 
                                  ajaxUrl=autoCompleteUrl 
                                  ajaxEnabled="" 
                                  mask=mask 
                                  placeholder=placeholder 
                                  tooltip=tooltip/>
            <#break>
          <#case "textarea">
            <@renderTextareaField name=name 
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
            <@renderDateTimeField name=name 
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
            
            <@renderDropDownField name=name
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
                                    manualItemsOnly=manualItemsOnly><#nested></@renderDropDownField>
            <#break>
          <#case "lookup">
            <@renderLookupField name=name formName=formName fieldFormName=fieldFormName className=class alert="false" value=value size=size?string maxlength=maxlength id=id event="onClick" action=onClick />
          <#break>
          <#case "checkbox">
                <@renderCheckBox id=id currentValue=value checked=checked name=name action=action />
            <#break>
          <#case "radio">
                <#if radioSingle>
                    <#-- single radio button item mode -->
                    <#local items=[{"key":value, "description":label!""}]/>
                    <@renderRadioField items=items className=class alert=alert currentValue=(checked?string(value,"")) noCurrentSelectedKey="" name=name event="" action="" tooltip=tooltip />
                <#else>
                    <#-- multi radio button item mode -->
                    <div <@renderClass class alert />>
                      <@renderRadioField items=items className="" alert=alert currentValue=currentValue noCurrentSelectedKey=defaultValue name=name event="" action="" tooltip=tooltip />
                    </div>
                </#if>
            <#break>
          <#case "file">
            <@renderFileField className=class alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete?string("", "off") />
            <#break> 
          <#case "password">
            <@renderPasswordField className=class alert=alert name=name value=value size=size maxlength=maxlength id=id autocomplete=autocomplete?string("", "off") />
            <#break> 
          <#case "submitarea">
            <@row>
              <#local hasProgress = (progressOptions.formSel)?has_content>
              <#if hasProgress>
                <#local subclasses="${styles.grid_small!}3 ${styles.grid_large!}2"/>
              <#else>
                <#local subclasses="${styles.grid_small!}12 ${styles.grid_large!}12"/>
              </#if>
              <@cell class=subclasses>
                <#nested>
              </@cell>
              <#if hasProgress>
                <#if progressOptions.progBarId?has_content>
                  <#-- with progress bar, optional text -->
                  <#local subclasses = progressOptions.progTextBoxId?has_content?string("${styles.grid_small!}6 ${styles.grid_large!}6", "${styles.grid_small!}9 ${styles.grid_large!}10")>
                  <@cell class=subclasses>
                    <@progress id=progressOptions.progBarId type="info" addWrapClass="${styles.hidden!}" progressOptions=progressOptions/>
                  </@cell>
                  <#if progressOptions.progTextBoxId?has_content>
                    <#local subclasses = "${styles.grid_small!}3 ${styles.grid_large!}4">
                    <@cell class=subclasses id=progressOptions.progTextBoxId>
                    </@cell>
                  </#if>
                <#elseif progressOptions.progTextBoxId?has_content>
                   <#-- text progress only -->
                   <#local subclasses = "${styles.grid_small!}9 ${styles.grid_large!}10">
                   <@cell class=subclasses id=progressOptions.progTextBoxId>
                   </@cell>
                   <@progressScript options=progressOptions htmlwrap=true />
                </#if>
              </#if>
            </@row>
            <#break> 
          <#default>
            <#if value?has_content>
                <@renderField text=value/>
            <#else>
                <#nested />
            </#if>
        </#switch>
     </@cell>
     <#if postfix && !nocells>
         <@cell class="${styles.grid_small!}1">
                <span class="postfix"><input type="submit" class="${styles.icon!} ${styles.icon_button!}" value="${styles.icon_button_value!}"/></span>
         </@cell>
     </#if>
</@row>
</#macro>

<#-- 
*************
* Fieldset Macro
************
    Usage example:  
    <@fieldset title="">
        Inner Content
    </@fieldset>            
                    
   * General Attributes *
    class           = css classes
    id              = set id
    title           = fieldset-title
    collapsed       = show/hide the fieldset
-->
<#macro fieldset id="" title="" class="" collapsed=false>
    <@renderFieldGroupOpen style=class id=id title=title collapsed=collapsed collapsibleAreaId="" collapsible=false expandToolTip="" collapseToolTip=""/>
        <#nested />
    <@renderFieldGroupClose style="" id="" title=""/>
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
    alt             = boolean, if true alternate row (odd), if false regular (even)
    selected        = boolean, if true row is marked selected
-->
<#macro row class="" id="" collapse=false norows=false alt="" selected="">
    <#if !norows>
      <#local altClass = "">
      <#if alt?is_boolean>
        <#local altClass = alt?string(styles.row_alt!, styles.row_reg!)>
      </#if>
      <#if selected?is_boolean && selected == true>
        <#local altClass = (altClass + " " + styles.row_selected!)?trim>
      </#if>
    <div class="${styles.grid_row!}<#if class?has_content> ${class}</#if><#if collapse> collapse</#if><#if altClass?has_content> ${altClass}</#if>"<#if id?has_content> id="${id}"</#if>><#rt/>
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
    addClass        = additional class that never affects defaults
    columns         = expected number of columns to be rendered (specify as number, default 12, default only used if class empty and no column sizes specified)
    small           = specific number of small columns (specify as number), overrides small part general columns value above
    large           = specific number of large columns (specify as number), overrides large part of general columns value above
    medium          = specific number of medium columns (specify as number), overrides medium part of general columns value above
    offset          = offset in number of columns
    TODO: smallOffset, mediumOffset, largeOffset
    last            = boolean, usually optional, if true indicate last cell in row 
-->
<#macro cell columns=0 small=0 medium=0 large=0 offset=0 class="" addClass="" id="" collapse=false nocells=false last=false>
    <#local columns = columns?number>
    <#local small = small?number>
    <#local medium = medium?number>
    <#local large = large?number>
    <#local offset = offset?number>
    
    <#if !nocells>
        <#local specColsClasses><#if (small > 0)> ${styles.grid_small!}${small}</#if><#if (medium > 0)> ${styles.grid_medium!}${medium}</#if><#if (large > 0)> ${styles.grid_large!}${large}<#elseif (columns > 0)> ${styles.grid_large!}${columns}</#if></#local>
        <#if class?has_content>
            <#local colSizeClasses = (class + specColsClasses)?trim>
        <#else>
            <#local colSizeClasses = specColsClasses?trim>
        </#if>
        <#if !colSizeClasses?has_content>
            <#local colSizeClasses = "${styles.grid_large!}12">
        </#if>
        <div class="${colSizeClasses}<#if (offset > 0)> ${styles.grid_offset!}${offset!}</#if> ${styles.grid_cell!}<#if last> ${styles.grid_end!}</#if><#if addClass?has_content> ${addClass}</#if>" <#if id?has_content> id="${id}"</#if>><#rt/>
    </#if>
        <#nested />
    <#if !nocells></div></#if>
</#macro>


<#-- 
*************
* Section Macro
************
    Usage example:  
    <@section attr="">
        Inner Content
    </@section>            
                    
   * General Attributes *
    class               = css classes
    id                  = set id
    title               = section title
    titleClass          = section title class (rarely needed, usually headerLevel enough)
    padded              = 
    autoHeaderLevel     = auto increase header level when title present (enabled by default)
    headerLevel         = force this header level for title. if autoHeaderLevel true, also influences nested elems (even if no title here, but if no title won't consume a size).
    defaultHeaderLevel  = default header level (same as headerLevel if autoHeaderLevel false)
    menuHtml            = optional HTML menu data, li elements only (ul auto added)
    menuClass           = menu class, default is buttons class. "none" prevents class.
    menuRole            = "nav-menu" (default), "paginate-menu"
    requireMenu         = if true, add menu elem even if empty
    forceEmptyMenu      = if true, always add menu and must be empty
    hasContent          = minor hint, optional, default true, when false, to add classes to indicate content is empty or treat as logically empty (workaround for no css :blank and possibly other)
-->
<#macro section id="" title="" classes="" padded=false autoHeaderLevel=true headerLevel="" defaultHeaderLevel=2 menuHtml="" menuClass="" menuRole="nav-menu" requireMenu=false forceEmptyMenu=false hasContent=true titleClass="">
    <#if id?has_content>
        <#local contentId = id + "_content">
        <#local menuId = id + "_menu">
    <#else>
        <#local contentId = "">
        <#local menuId = "">
    </#if>
    <#-- note: autoHeaderLevel logic now implemented in renderScreenletBegin -->
    <@renderScreenletBegin id=id collapsibleAreaId=contentId title=title classes=classes padded=padded menuString=menuHtml fromWidgets=false menuClass=menuClass menuId=menuId menuRole=menuRole requireMenu=requireMenu 
        forceEmptyMenu=forceEmptyMenu hasContent=hasContent autoHeaderLevel=autoHeaderLevel headerLevel=headerLevel defaultHeaderLevel=defaultHeaderLevel titleStyle=titleClass/>
        <#nested />
    <@renderScreenletEnd />
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
    <@paginate >            
                    
   * General Attributes *
   url             = Base Url to be used for pagination
   class           = css classes
   listSize        = size of the list in total
   viewIndex       = page currently displayed
   viewSize        = maximum number of items displayed
   altParam        = Use viewIndex/viewSize as parameters, instead of VIEW_INDEX / VIEW_SIZE
   forcePost       = Always use POST for non-ajax browsing (note: even if false, large requests are coerced to POST)
   paramStr        = Extra URL parameters in string format, escaped (param1=val1&amp;param2=val2)
   viewIndexFirst  = First viewIndex value number (0 or 1, only affects param values, not display)
   showCount       = If true show "Displaying..." count or string provided in countMsg; if false don't
   countMsg        = Custom message for count, optional
-->
<#macro paginate url="" class="nav-pager" viewIndex=0 listSize=0 viewSize=1 altParam=false forcePost=false paramStr="" viewIndexFirst=0 showCount=true countMsg="">
    
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
    <#else>
        <#local viewIndexString = "VIEW_INDEX">
        <#local viewSizeString = "VIEW_SIZE">
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
    <#if (url?has_content)>
        <#local commonUrl = addParamDelimToUrl(url, "&amp;")>
        <#if paramStr?has_content>
            <#local commonUrl = commonUrl + paramStr + "&amp;">
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
    </#if>
    
    <#if showCount && (!countMsg?has_content)>
       <#local messageMap = {"lowCount": lowIndex+1, "highCount": highIndex, "total": listSize}>
       <#local countMsg = Static["org.ofbiz.base.util.UtilProperties"].getMessage("CommonUiLabels", "CommonDisplaying", messageMap, locale)!"">
    </#if>
    
    <@renderNextPrev ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateStyle="nav-pager" paginateFirstStyle="nav-first" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel=uiLabelMap.CommonFirst paginatePreviousStyle="nav-previous" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel=uiLabelMap.CommonPrevious pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl commonDisplaying=showCount?string(countMsg,"") paginateNextStyle="nav-next" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel=uiLabelMap.CommonNext paginateLastStyle="nav-last" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel=uiLabelMap.CommonLast paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst />
</#macro>


<#-- 
*************
* alert box
************
    Usage example:  
    <@alert type="">
        <#nested>
    </@alert>            
                    
   * General Attributes *
    type           = (info|success|warning|secondary|alert|error)
    addClass       = additional classes for nested container
-->
<#macro alert type="" addClass="">
<div class="${styles.grid_row!}">
        <div class="${styles.grid_large!}12 ${styles.grid_cell!}">
        <div data-alert class="${styles.alert_wrap!} ${styles.alert_prefix_type!}${type}">
           <div class="${styles.grid_row!}">
              <div class="${styles.grid_large!}12 ${styles.grid_cell!}<#if addClass?has_content> ${addClass}</#if>">
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
* query result message
************
Common query result message.
Note: this is ONLY for expected, non-error messages, such as no records found in a query.
Other messages such as for missing params/record IDs are usually errors.

    Usage example:  
    <@resultMsg>${uiLabelMap.CommonNoRecordFound}.</@resultMsg>            
                    
   * General Attributes *
    addClass       = additional classes for nested container
-->
<#macro resultMsg addClass="">
  <#local class = ("result-msg " + addClass)?trim>
  <p<#if class?has_content> class="${class}"</#if>><#nested></p>
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
* pricing table
************
Since this is very foundation specific, this function may be dropped in future installations

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
    columns         = Number of columns (default 5)
    type            = (tiles|) default:empty
    
-->
<#macro grid type="" class="" columns=4>
    <#if type=="tiles" || type="freetiles">
        <#global freewallNum="${(freewallNum!0)+1}" />
        <#local id="freewall_id_${freewallNum!0}">
        <div class="${styles.tile_container!}" id="${id!}">
            <#nested>
        </div>
        <script>
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
        </script>
    <#else>
        <#local defaultClass="${styles.grid_small!}${styles.grid_block!}2 ${styles.grid_medium!}${styles.grid_block!}4 ${styles.grid_large!}${styles.grid_block!}5">
            <#if columns-2 &gt; 0>
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

<#macro elemAttribStr attribs>
  <#if attribs?is_hash_ex>
    <#t><#list attribs?keys as name> ${name}="${attribs[name]?string}"</#list>
  </#if>
</#macro>

<#-- 
*************
* Table
************
Helps define table. Required wrapper for all table sub-elem macros.

    Usage example:  
    <@table type="data" class="basic-table" id="my-table">
      <@thead>
        <@tr>
          <@tc width="15%">col 1</@tc>
          <@tc width="85%">col 2</@tc>
        </@tr>
      </@thead>
      <@tbody>
        <@tr class="my-row-class" valign="middle">
          <@tc>val 1</@tc>
          <@tc>val 2</@tc>
        </@tr>
      </@tbody>
    </@table>
                    
   * General Attributes *
    type            = [generic, data-simple, data-complex, summary, fields], default generic
                      TODO: WARN: these are WIP types, may not be enough
                      generic: generic html table (free-form, complex), no features enabled by default
                      data-simple: record-containing table, one data record per row (but row cells may be complex and may have tfoot)
                      data-complex: record-containing table, complex structure (more than one row per record, separators, etc.)
                      data: DEPRECATED: typical record-containing table
                      summary: usually table with one or a few set row of summary totals
                      fields: label-value pairs for display (note: prefer @field for input forms or @row/@cell instead?)
    class           = manual classes to add, as string, default "basic-table" for data, 
                      if specified as string replaces defaults (class="" prevents class)
    addClass        = extra classes that don't influence defaults
    id              = table id
    autoAltRows     = default false for now (temporarily false for type="data" as well, too many complex)
    firstRowAlt     = default false
    inheritAltRows  = only for nested tables: if true, all rows in nested tables will inherit alt from parent table row
    useFootAltRoots = whether use alt row logic in foot or not
    cellspacing     = cellspacing, default 0, set to "" to remove
    wrapIf          = advanced structure control, for esoteric cases
    [attribs...]    = legacy <table attributes and values
-->
<#macro table type="generic" class=true addClass="" id="" wrapIf=true cellspacing=0 scrollable=false autoAltRows="" firstRowAlt="" inheritAltRows=false useFootAltRows=false attribs...>
<#if wrapIf>
  <#-- save previous globals, for nesting -->
  <#local prevTableInfo = catoCurrentTableInfo!>
  <#local prevSectionInfo = catoCurrentTableSectionInfo!>
  <#local prevRowAlt = catoCurrentTableRowAlt!>
  <#local prevLastRowAlt = catoCurrentTableLastRowAlt!>
  <#if !autoAltRows?is_boolean>
    <#-- don't enable for all data tables by default for now, too many complex ones...
    <#local autoAltRows = (type == "data") || inheritAltRows>
    -->
    <#local autoAltRows = inheritAltRows>
  </#if>
  <#if class?is_boolean>
    <#if class>
      <#if !catoDefaultTableStyles?has_content>
        <#assign catoDefaultTableStyles = {
          "generic": styles.table_default!,
          "data": styles.table_data!,
          "summary": styles.table_summary!,
          "fields": styles.table_fields!
        }>
      </#if>
      <#local class = catoDefaultTableStyles[type]!"">
    <#else>
      <#local class = "">
    </#if>
  </#if>
  <#if addClass?is_string && addClass?has_content>
    <#local class = class + " " + addClass>
  </#if>
  <#global catoCurrentTableInfo = {"type": type, "autoAltRows": autoAltRows,
    "inheritAltRows": inheritAltRows, "parentRowAlt": prevRowAlt, "useFootAltRows": useFootAltRows}>
  <#global catoCurrentTableSectionInfo = {"type": "body", "cellElem": "td"}>
  <#if firstRowAlt?is_boolean>
    <#global catoCurrentTableRowAlt = firstRowAlt>
  <#elseif inheritAltRows>
    <#global catoCurrentTableRowAlt = prevRowAlt> 
  <#else>
    <#global catoCurrentTableRowAlt = false> 
  </#if>
  <#if prevRowAlt?is_boolean>
    <#global catoCurrentTableLastRowAlt = prevRowAlt>
  <#else>
    <#global catoCurrentTableLastRowAlt = catoCurrentTableRowAlt>
  </#if>
  <#local style = "">
  <#if scrollable>
  <#-- TODO: change this to something more foundation-like.
       this is a custom workaround to get scrolling, nothing else working. -->
  <div class="scrollable-table-container">
  </#if>
  <table<#if class?has_content> class="${class}"</#if><#if id?has_content> id="${id}"</#if><#rt>
    <#lt><#if cellspacing?has_content> cellspacing="${cellspacing}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs /></#if>>
    <#nested>
  </table>
  <#if scrollable>
  </div>
  </#if>
  <#global catoCurrentTableInfo = prevTableInfo>
  <#global catoCurrentTableSectionInfo = prevSectionInfo>
  <#global catoCurrentTableRowAlt = prevRowAlt>
  <#global catoCurrentTableLastRowAlt = prevLastRowAlt>
<#else>
<#nested>
</#if>
</#macro>

<#macro thead class="" id="" wrapIf=true attribs...>
<#if wrapIf>
  <#local prevTableSectionInfo = catoCurrentTableSectionInfo!>
  <#global catoCurrentTableSectionInfo = {"type": "head", "cellElem": "th"}>
  <thead<#if class?has_content> class="${class}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs /></#if>>
    <#nested>
  </thead>
  <#global catoCurrentTableSectionInfo = prevTableSectionInfo>
<#else>
<#nested>
</#if>
</#macro>

<#macro tbody class="" id="" wrapIf=true attribs...>
<#if wrapIf>
  <#local prevTableSectionInfo = catoCurrentTableSectionInfo!>
  <#global catoCurrentTableSectionInfo = {"type": "body", "cellElem": "td"}>
  <tbody<#if class?has_content> class="${class}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs /></#if>>
    <#nested>
  </tbody>
  <#global catoCurrentTableSectionInfo = prevTableSectionInfo>
<#else>
<#nested>
</#if>  
</#macro>

<#macro tfoot class="" id="" wrapIf=true attribs...>
<#if wrapIf>
  <#local prevTableSectionInfo = catoCurrentTableSectionInfo!>
  <#global catoCurrentTableSectionInfo = {"type": "foot", "cellElem": "td"}>
  <tfoot<#if class?has_content> class="${class}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs /></#if>>
    <#nested>
  </tfoot>
  <#global catoCurrentTableSectionInfo = prevTableSectionInfo>
<#else>
<#nested>
</#if>
</#macro>

<#-- 
*************
* Table row
************
Helps define table rows. takes care of alt row styles. must have a parent @table wrapper. 
                    
   * General Attributes *
    class           = manual classes to add
    useAlt          = boolean, if specified, can manually enable/disable whether alternate row code runs per-row
    alt             = boolean, if specified, override the automatic auto-alt styling to specific value true or false (manual mode)
    useLastAlt      = boolean, if specified, sets alt to same as last (row, or parent table row if first row)
    useParentAlt    = boolean, nested tables only, if specified, use parent table row alt
    selected        = boolean, if specified and true marked as selected
    wrapIf/openOnly/CloseOnly = advanced structure control, for esoteric cases (can omit nested)
    [attribs...]    = legacy <tr attributes and values
-->
<#macro tr class="" id="" wrapIf=true useAlt="" alt="" useLastAlt="" useParentAlt="" selected="" openOnly=false closeOnly=false attribs...>
<#if wrapIf>
  <#local sectionType = (catoCurrentTableSectionInfo.type)!"body">
  <#local isRegAltRow = ((sectionType == "body") || (sectionType == "foot" && ((catoCurrentTableInfo.useFootAltRows)!)==true))>
  <#local str = class>
  <#if !(useAlt?is_boolean && useAlt == false)>
    <#if !alt?is_boolean>
      <#if useLastAlt?is_boolean && useLastAlt == true>
        <#local alt = catoCurrentTableLastRowAlt!false>
      <#elseif useParentAlt?is_boolean && useParentAlt == true>
        <#local alt = (catoCurrentTableInfo.parentRowAlt)!false>
        <#if !alt?is_boolean><#local alt = false></#if>
      <#elseif (isRegAltRow && ((catoCurrentTableInfo.autoAltRows)!)==true)>
        <#if ((catoCurrentTableInfo.inheritAltRows)!)==true>
          <#local alt = (catoCurrentTableInfo.parentRowAlt)!false>
          <#if !alt?is_boolean><#local alt = false></#if>
        <#else>
          <#local alt = catoCurrentTableRowAlt!false>
        </#if>
      <#elseif useAlt?is_boolean && useAlt == true>
        <#-- force-enabled -->
        <#local alt = catoCurrentTableRowAlt!false>
      </#if>
    </#if>
  </#if>
<#if !closeOnly>
  <#if alt?is_boolean>
    <#local str = (str + " " + alt?string(styles.row_alt!, styles.row_reg!))?trim>
  </#if>
  <#if selected?is_boolean && selected == true>
    <#local str = (str + " " + styles.row_selected!)?trim>
  </#if>
  <tr<#if str?has_content> class="${str}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs /></#if>>
</#if>    
    <#nested>
<#if !openOnly>
  </tr>
  <#if !(useAlt?is_boolean && useAlt == false)>
    <#if alt?is_boolean && isRegAltRow> <#-- not needed:  && ((catoCurrentTableInfo.inheritAltRows)!)==false -->
      <#global catoCurrentTableLastRowAlt = alt>
      <#global catoCurrentTableRowAlt = !alt>
    </#if>
  </#if>
</#if>
<#else>
<#nested>
</#if>
</#macro>

<#-- 
*************
* Table cell
************
Helps define table cells. tc automatically knows whether th or td via @thead and @tbody, but th and td can be used too for legacy for now...
                    
   * General Attributes *
    class           = manual classes to add
    id              = cell id
    wrapIf/openOnly/CloseOnly = advanced structure control, for esoteric cases (can omit nested)
    [attribs...]    = legacy <th and <td attributes and values
-->
<#macro tc class="" id="" wrapIf=true openOnly=false closeOnly=false attribs...>
<#if wrapIf>
  <#local elem = (catoCurrentTableSectionInfo.cellElem)!"td">
  <#if !closeOnly><${elem}<#if class?has_content> class="${class}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs /></#if>></#if><#nested><#if !openOnly></${elem}></#if>
<#else>
<#nested>
</#if>
</#macro>

<#macro th class="" id="" wrapIf=true openOnly=false closeOnly=false attribs...>
<#if wrapIf>
  <#if !closeOnly><th<#if class?has_content> class="${class}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs /></#if>></#if><#nested><#if !openOnly></th></#if>
<#else>
<#nested>
</#if>
</#macro>

<#macro td class="" id="" wrapIf=true openOnly=false closeOnly=false attribs...>
<#if wrapIf>
  <#if !closeOnly><td<#if class?has_content> class="${class}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs /></#if>></#if><#nested><#if !openOnly></td></#if>
<#else>
<#nested>
</#if>
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
    class           = manual classes to add
    alt             = boolean, if true is alternate row (odd), if false regular (even)
    selected        = boolean, if true marked as selected
-->
<#macro dataRowClassStr class="" alt="" selected="">
  <#local str = class>
  <#if alt?is_boolean>
    <#local str = (str + " " + alt?string(styles.row_alt!, styles.row_reg!))?trim>
  </#if>
  <#if selected?is_boolean && selected == true>
    <#local str = (str + " " + styles.row_selected!)?trim>
  </#if>
  <#if str?has_content> class="${str}"</#if>
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
    link            = Link URL
    id              = field id
    color           = (0|1|2|3|4|5|6|7) defaul:0 (empty)   
    icon            = Set icon code (http://zurb.com/playground/foundation-icon-fonts-3)
    image           = Set a background image-url (icon won't be shown if not empty)
-->
<#macro tile type="normal" title="" class="" id="" link="" color=0 icon="" image="">
    <#local nested><#nested></#local>
    <div class="${styles.tile_wrap!} ${styles.tile_wrap!}-${type!}<#if class?has_content> ${class!}</#if> ${styles.tile_color!}${color!}"<#if id?has_content> id="${id!}"</#if> data-sizex="${calcTileSize("x",type!)}" data-sizey="${calcTileSize("y",type!)}">
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
    <#global fieldIdNum=(fieldIdNum!0)+1 />
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
        <script>
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
    showValue      = Display value inside bar
    addWrapClass   = extra classes on outer wrapper only
    progressOptions = if present, attaches progress bar to an upload form with javascript-based progress and 
                      attaches results to page using elem IDs and options in this map - 
                      see CatoUploadProgress javascript class for options; mostly same
-->
<#macro progress value=0 id="" type="" class="" showValue=false addWrapClass="" progressOptions={}>
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
    <div class="${styles.progress_container}<#if !styles.progress_wrap?has_content && class?has_content> ${class!}</#if><#if color?has_content> ${color!}</#if><#if addWrapClass?has_content> ${addWrapClass}</#if>"<#if id?has_content> id="${id}"</#if>>
      <#if styles.progress_wrap?has_content><div class="${styles.progress_wrap!}<#if class?has_content> ${class!}</#if>"<#if id?has_content> id="${id!}_meter"</#if> role="progressbar" aria-valuenow="${value!}" aria-valuemin="0" aria-valuemax="100" style="width: ${value!}%"></#if>
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
            uploadProgress = new CatoUploadProgress({
            <#list options?keys as opt>
                <#local val = options[opt]!>
                <#if opt=="successRedirectUrl">
                  <#-- shouldn't have &amp; in script tag... but code may escape and should support... -->
                  "${opt}" : "${val?replace("&amp;", "&")}",
                <#elseif val?is_number>
                  "${opt}" : ${val},
                <#elseif val?is_boolean>
                  "${opt}" : ${val?string("true", "false")},
                <#else>
                  "${opt}" : "${val}",
                </#if>
            </#list>
            });
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




<#-- UTLITY MACROS END -->

<#-- DEV MACROS -->
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
<#-- DEV MACROS END -->
</#compress>