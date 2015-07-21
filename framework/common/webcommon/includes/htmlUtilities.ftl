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
      <#assign var="${uiLabelMap[value]}" />
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
* containsStyleClass function
************
Returns true if class/style string contains given style.
                    
   * Parameters *
    classes         = classes string
    className       = name of class to find
-->
<#function containsStyleClass classes className>
  <#return classes?split(" ")?seq_contains(className)>
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
  <#if htmlwrap>
<script language="JavaScript" type="text/javascript">
<!-- //
  </#if>
    commonOfbizUrls["${uri}"] = "<@ofbizUrl>${uri}</@ofbizUrl>";
  <#if htmlwrap>
// -->
</script>
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
    onClick           = JS Event
    disabled        = field disabled
    placeholder     = field placeholder
    alert           = adds additional css alert class
    mask            = toggles jQuery mask plugin
    size            = size attribute (default: 20)
    collapse        = should the field be collapsing? (default: false)
    norows          = render without the rows-container
    norows          = render without the cells-container
    required        = required input
    addClass        = css classes in addition to default
        
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
    currentValue    = Y/N
    checked      = checked (true/false)
    
-->
<#macro field type="" label="" name="" value="" class="${style_grid_large!}12" size=20 maxlength="" id="" onClick="" 
        disabled=false placeholder="" autoCompleteUrl="" mask=false alert="false" readonly=false rows="4" 
        cols="50" dateType="date" multiple="" checked=false collapse=false tooltip="" columns="" norows=false nocells=false
        fieldFormName="" formName="" postfix=false required=false addClass="">
<#-- fieldIdNum will always increment throughout the page -->
<#global fieldIdNum=(fieldIdNum!0)+1 />

<#if !id?has_content>
    <#assign id="field_id_${renderSeqNumber!}_${fieldIdNum!0}">
</#if>
<#assign classes = class/>
<#assign columnspostfix=0/>
<#if postfix>
    <#assign columnspostfix=1/>
    <#local collapse=true/>
    <#assign classes="${style_grid_small!}${12-columnspostfix}"/>
</#if>

<#if required && (!containsStyleClass(class, "required"))>
    <#local class = class + " required">
</#if>

<#if addClass?has_content>
    <#local class = class + " " + addClass>
</#if>

<@row collapse=collapse!false norows=norows>
    <#if label?has_content>
        <#assign subclasses="${style_grid_small!}3 ${style_grid_large!}2"/>
        <#assign classes="${style_grid_small!}${9-columnspostfix} ${style_grid_large!}${10-columnspostfix}"/>
        
        <#if columns?has_content>
            <#assign subclasses="${style_grid_small!}${12-columns+1} ${style_grid_large!}${12-columns}"/>
            <#assign classes="${style_grid_small!}${columns-columnspostfix-1} ${style_grid_large!}${columns-columnspostfix}"/>
        </#if>
        
        <#if type!="radio">
        <@cell class=subclasses nocells=nocells>
                <#if type=="checkbox" || collapse==false>
                    <label class=""<#if id?has_content> for="${id}"</#if>>${label}</label>
                <#else>
                    <span class="prefix">${label}</span>
                </#if>           
        </@cell>
        </#if>
    </#if>
    <@cell class="${classes!}" nocells=nocells>
        <#switch type>
          <#case "input">
                <@renderTextField name=name 
                                  className=class 
                                  alert=alert 
                                  value=value 
                                  textSize=size 
                                  maxlength=maxlength 
                                  id=id 
                                  event="onCLick" 
                                  action=onCLick 
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
            <#if dateType == "date"><#assign shortDateInput=true/><#else><#assign shortDateInput=false/></#if>
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
            <@renderDropDownField name=name
                                    className=class 
                                    alert=alert 
                                    id=id 
                                    multiple=multiple
                                    formName=""
                                    otherFieldName="" 
                                    event="onClick" 
                                    action=onCLick  
                                    size=size
                                    firstInList="" 
                                    currentValue="" 
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
                                    tooltip=tooltip><#nested></@renderDropDownField>
            <#break>
          <#case "lookup">
            <@renderLookupField name=name formName=formName fieldFormName=fieldFormName className=class alert="false" value=value size=size?string maxlength=maxlength id=id event="onClick" action=onClick />
          <#break>
          <#case "checkbox">
                <@renderCheckBox id=id currentValue=value checked=checked name=name action=action />
            <#break>
          <#case "radio">
            <#assign items=[{"key":value, "description":label!""}]/>
                <@renderRadioField items=items className=class alert=alert currentValue=(checked?string(value,"")) noCurrentSelectedKey="" name=name event="" action="" tooltip=tooltip />
            
            <#break>
          <#case "file">
            <#-- TODO: better version of this -->
            <input type="file" name="${name}"<#if class?has_content> class="${class}"</#if><#if size?has_content> size="${size}"</#if> />
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
         <@cell class="${style_grid_small!}1">
                <span class="postfix"><input type="submit" class="${style_icon!} ${style_icon_button!}" value="${style_icon_button_value!}"/></span>
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
-->
<#macro row class="" id="" collapse=false norows=false>
    <#if !norows>
    <div class="${style_grid_row!} <#if class?has_content> ${class!}</#if><#if collapse> collapse</#if>"<#if id?has_content> id="${id}"</#if>><#rt/>
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
    Usage example:  
    <@row attr="" >
        <@cell attr="">
            cell content goes in here!
        </@cell>
    </@row>              
                    
   * General Attributes *
    class           = css classes
    columns         = expected number of columns to be rendered (default 12)
    offset          = offset in number of columns
-->
<#macro cell columns=12 offset=0 class="" id="" collapse=false nocells=false>
    <#if !nocells><div class="<#if class?has_content>${class!}<#else>${style_grid_large!}${columns!12}</#if><#if offset&gt;0>${style_grid_offset!}${offset!}</#if> ${style_grid_cell!}" <#if id?has_content> id="${id}"</#if>><#rt/></#if>
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
    class           = css classes
    id              = set id
    padded          = 
-->
<#macro section id="" title="" classes="" padded=false>
    <@renderScreenletBegin id=id title=title classes=classes padded=padded/>
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
    <div id="${id}_modal" class="${style_modal_wrap!}" data-reveal>
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
    type           = (info|success|warning|secondary|alert)
    
-->
<#macro alert type="">
<div class="${style_grid_row!}">
        <div class="${style_grid_large!}12 ${style_grid_cell!}">
        <div data-alert class="${style_alert_wrap!} ${style_alert_prefix_type!}${type}">
           <div class="${style_grid_row!}">
              <div class="${style_grid_large!}12 ${style_grid_cell!}">
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
<div class="${style_panel_wrap!} ${type}">
  <div class="${style_panel_head!}"><#if title?has_content><h5 class="${style_panel_title!}">${title!}</h5></#if></div>
  <div class="${style_panel_body!}"><p><#nested></p></div>
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
          <ul class="${style_pricing_wrap!}">
              <@pli type="title">${title!}</@pli>
              <#nested>
          </ul>
</#macro>

<#macro pli type="">
    <#switch type>
          <#case "price">
              <li class="${style_pricing_price!}"><#nested></li>
          <#break>
          <#case "description">
              <li class="${style_pricing_description!}"><#nested></li>
          <#break>
          <#case "title">
              <li class="${style_pricing_title!}"><#nested></li>
          <#break>
          <#case "button">
              <li class="${style_pricing_cta!}"><#nested></li>
          <#break>        
          <#default>
              <li class="${style_pricing_bullet!}"><#nested></li>
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
    class           = Adds classes - please use "(small|medium|large)-${style_grid_block!}#"
    columns         = Number of columns (default 5)
    type            = (tiles|) default:empty
    
-->
<#macro grid type="" class="" columns=4>
    <#if type=="tiles" || type="freetiles">
        <#global freewallNum="${(freewallNum!0)+1}" />
        <#assign id="freewall_id_${freewallNum!0}">
        <div class="${style_tile_container!}" id="${id!}">
            <#nested>
        </div>
        <script>
         $(function() {
            $('#${id}').freetile({
                selector: '.${style_tile_wrap!}'
            });
            <#--
            Alternative implementation of gridster.js
            $('#${id}').gridster({
                widget_selector: '.${style_tile_wrap!}',
                min_cols:${columns},
                autogenerate_stylesheet:false
            }).disable();
            -->
         });
        </script>
    <#else>
        <#assign defaultClass="${style_grid_small!}${style_grid_block!}2 ${style_grid_medium!}${style_grid_block!}4 ${style_grid_large!}${style_grid_block!}5">
            <#if columns-2 &gt; 0>
                <#local class="${style_grid_small!}${style_grid_block!}${columns-2} ${style_grid_medium!}${style_grid_block!}${columns-1} ${style_grid_large!}${style_grid_block!}${columns}"/>
            <#else>
                <#local class="${style_grid_large!}${style_grid_block!}${columns}"/>
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
            <ul class="${style_nav_breadcrumbs!}">
                <#nested>
            </ul>
        <#break>
        <#default>
            <ul class="${style_list_inline!} ${style_nav_subnav!}">
              <#nested>
            </ul>
        <#break>
    </#switch>
</#macro>

<#macro mli arrival="">
    <dd data-magellan-arrival="${arrival!}"><#nested></dd>
</#macro>

<#function mtarget id>
  <#assign returnValue="data-magellan-destination=\"${id}\""/>
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
    <#assign nested><#nested></#assign>
    <div class="${style_tile_wrap!} ${style_tile_wrap!}-${type!}<#if class?has_content> ${class!}</#if> ${style_tile_color!}${color!}"<#if id?has_content> id="${id!}"</#if> data-sizex="${calcTileSize("x",type!)}" data-sizey="${calcTileSize("y",type!)}">
        <#if image?has_content><div class="${style_tile_image!}" style="background-image: url(${image!})"></div></#if>
        <div class="${style_tile_content!}">
            <#if link?has_content><a href="${link!}"></#if>
            <#if icon?has_content && !icon?starts_with("AdminTileIcon") && !image?has_content><span class="${style_tile_icon!}"><i class="${icon!}"></i></span></#if>
            <#if nested?has_content><span class="${style_tile_overlay!}"><#nested></span></#if>
            <#if title?has_content><span class="${style_tile_title!}">${title!}</span></#if>
            <#if link?has_content></a></#if>
        </div>
    </div>  
</#macro>

<#function calcTileSize type="x" value="normal">
    <#assign tileSizeX={"small":0,"normal":1,"wide":2,"large":2,"big":3,"super":4}/>
    <#assign tileSizeY={"small":0,"normal":1,"wide":1,"large":2,"big":3,"super":4}/>
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
        <ul data-${type!}-id="chart_${renderSeqNumber!}_${fieldIdNum!}" class="${style_chart_legend!}">
            <#nested/>
        </ul>
        </@cell>
        <@cell columns=9><div id="chart_${renderSeqNumber!}_${fieldIdNum!}" style="height:300px;"></div></@cell>
        </@row>
    <#else>
        <#global chartId = "chart_${renderSeqNumber!}_${fieldIdNum!}"/>
        <#global chartType = type/>
        <canvas id="${chartId!}" class="${style_grid_large!}12 chart-data" height="300" style="height:300px;"></canvas>
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
    
                    
   * General Attributes *
    value          = Percentage done
    id             = custom id
    type           = (warning|info|success) default: success
    class          = Adds classes - please use "(small|medium|large)-block-grid-#"
    showValue      = Display value inside bar
    
-->
<#macro progress value=0 id="" type="" class="" showValue=false>
    <#switch type>
      <#case "alert">
        <#assign color=style_color_alert!/>
      <#break>
      <#case "info">
        <#assign color=style_color_info!/>
      <#break>
      <#case "warning">
        <#assign color=style_color_warning!/>
      <#break>
      <#default>
        <#assign color=style_color_success!/>
    </#switch>
    <div class="${style_progress_container}<#if !style_progress_wrap?has_content && class?has_content> ${class!}</#if><#if color?has_content> ${color!}</#if>"<#if id?has_content> id="${id}"</#if>>
      <#if style_progress_wrap?has_content><div class="${style_progress_wrap!}<#if class?has_content> ${class!}</#if>"<#if id?has_content> id="${id!}_meter"</#if> role="progressbar" aria-valuenow="${value!}" aria-valuemin="0" aria-valuemax="100" style="width: ${value!}%"></#if>
            <span class="${style_progress_bar!}"<#if !style_progress_wrap?has_content> style="width: ${value!}%"<#if id?has_content> id="${id!}_meter"</#if></#if>><#if showValue>${value!}</#if></span>
      <#if style_progress_wrap?has_content></div></#if>
    </div>
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
  <#assign var = value/>
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