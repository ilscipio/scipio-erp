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
******************
* UTILITY MACROS *
******************
-->
<#-- 
*************
* Field Macro
************ macro
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
        
    * input *
    autoCompleteUrl = if autocomplete function exists, specification of url will make it available

    
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
<#macro field type="" label="" name="" value="" class="large-12" size=20 maxlength="" id="" onClick="" 
        disabled=false placeholder="" autoCompleteUrl="" mask=false alert="false" readonly=false rows="4" 
        cols="50" dateType="date" multiple="" checked=false collapse=false tooltip="" columns="" norows=false
        fieldFormName="" formName="">

<#-- fieldIdNum will always increment throughout the page -->
<#global fieldIdNum="${fieldIdNum!0+1}" />

<#if !id?has_content>
    <#assign id="field_id_${fieldIdNum!0}">
</#if>
    
<#-- ToDo Remove    
<#if label?has_content>
    <#assign collapse=true/>
</#if>-->
<@row collapse=collapse!false norows=norows>
    <#assign classes = class/>
    <#if label?has_content>
        <#assign subclasses="small-3 large-2"/>
        <#assign classes="small-9 large-10"/>
        
        <#if columns?has_content>
            <#assign subclasses="small-${12-columns+1} large-${12-columns}"/>
            <#assign classes="small-${columns-1} large-${columns}"/>
        </#if>
        
        <#if type!="radio">
        <@cell class=subclasses>
                <#if type=="checkbox" || collapse==false>
                    <label class="">${label}</label>
                <#else>
                    <span class="prefix">${label}</span>
                </#if>           
        </@cell>
        </#if>
    </#if>
    <@cell class="${classes!}">
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
                <@renderCheckBox id=id checked="checked" currentValue=value name=name action=action />
            <#break>
          <#case "radio">
            <#assign items=[{"description",label!""}]/>
                <@renderRadioField items=items className=class alert=alert currentValue=value noCurrentSelectedKey="" name=name event="" action="" tooltip=tooltip />
            
            <#break>
          <#default>
            <#if value?has_content>
                <@renderField text=value/>
            <#else>
                <#nested />
            </#if>
        </#switch>
     </@cell>
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
    <div class="row <#if class?has_content> ${class!}</#if><#if collapse> collapse</#if>"<#if id?has_content> id="${id}"</#if>><#rt/>
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
<#macro cell columns=12 offset=0 class="" id="" collapse=false>
    <div class="<#if class?has_content>${class!}<#else>large-${columns!12}</#if> columns" <#if id?has_content> id="${id}"</#if>><#rt/>
        <#nested />
    </div>    
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
    id              = set id
    title           = 
-->
<#macro modal id label>
    <a href="#" data-reveal-id="${id}_modal">${label}</a>
    <div id="${id}_modal" class="reveal-modal" data-reveal>
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
-->
<#macro paginate url="" class="nav-pager" viewIndex=0 listSize=0 viewSize=1 altParam=false>
    <#local viewIndexLast = ((listSize/viewSize)?ceiling)>
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
    <#if (viewIndex > 0)>
        <#local viewIndexPrevious = (viewIndex-1)>
    <#else>
        <#local viewIndexPrevious = viewIndex>
    </#if>
    <#if (url?has_content)>
        <#if (!firstUrl?has_content)>
            <#local firstUrl=url+"?${viewSizeString}=${viewSize}&amp;${viewIndexString}=0"/>
        </#if>
        <#if (!previousUrl?has_content)>
             <#local previousUrl=url+"?${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndexPrevious}"/>
        </#if>
        <#if (!nextUrl?has_content)>
            <#local nextUrl=url+"?${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndexNext}"/>
        </#if>
        <#if (!lastUrl?has_content)>
            <#local lastUrl=url+"?${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndexLast}"/>
        </#if>
        <#if (!selectUrl?has_content)>
            <#local selectUrl=url+"?${viewSizeString}=${viewSize}&amp;${viewIndexString}="/>
        </#if>
        <#if (!selectSizeUrl?has_content)>
            <#local selectSizeUrl=url+"?${viewSizeString}='+this.value+'&amp;${viewIndexString}=0"/>
        </#if>
    </#if>
    <@renderNextPrev ajaxEnabled=false javaScriptEnabled=true paginateStyle="nav-pager" paginateFirstStyle="nav-first" viewIndex=viewIndex highIndex=0 listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel=uiLabelMap.CommonFirst paginatePreviousStyle="nav-previous" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel=uiLabelMap.CommonPrevious pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl commonDisplaying="" paginateNextStyle="nav-next" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel=uiLabelMap.CommonNext paginateLastStyle="nav-last" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel=uiLabelMap.CommonLast paginateViewSizeLabel=""/>
</#macro>
<#-- UTLITY MACROS END -->


