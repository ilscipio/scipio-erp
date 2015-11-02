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
<#include "htmlCommonMacroLibrary.ftl">
<#-- 
Cato: NOTE: since macro renderer initial context mod, macros here now have access to a few widget context objects part of the initial
context, such as request, response, etc. however it is only from the initial context,
not "current" context (too intrusive in current renderer design). still relies on macro params.
-->
<#macro renderField text>
  <#-- delegate to cato libs -->
  <@field_generic_widget text=text />
</#macro>

<#macro renderDisplayField type imageLocation idName description title class alert inPlaceEditorUrl="" inPlaceEditorParams="" imageAlt=""collapse=false fieldType="" fieldTitleBlank=false>
  <#-- delegate to cato libs -->
  <@field_display_widget type=type imageLocation=imageLocation idName=idName description=description title=title class=class alert=alert inPlaceEditorUrl=inPlaceEditorUrl inPlaceEditorParams=inPlaceEditorParams imageAlt=imageAlt fieldTitleBlank=fieldTitleBlank />
</#macro>
<#macro renderHyperlinkField></#macro>

<#macro renderTextField name className alert value textSize maxlength id event="" action="" disabled=false ajaxUrl="" ajaxEnabled=false mask=false clientAutocomplete="" placeholder="" tooltip="" collapse=false readonly=false fieldType="" fieldTitleBlank=false>
  <#-- delegate to cato libs -->
  <@field_input_widget name=name className=className alert=alert value=value textSize=textSize maxlength=maxlength id=id event=event action=action disabled=disabled ajaxUrl=ajaxUrl ajaxEnabled=ajaxEnabled mask=mask clientAutocomplete=clientAutocomplete placeholder=placeholder tooltip=tooltip collapse=collapse readonly=readonly fieldTitleBlank=fieldTitleBlank />
</#macro>

<#macro renderTextareaField name className alert cols rows id readonly value visualEditorEnable=true buttons="" language="" tooltip="" title="" fieldType="" fieldTitleBlank=false collapse=false fieldType="" fieldTitleBlank=false>
  <#-- delegate to cato libs -->
  <@field_textarea_widget name=name className=className alert=alert cols=cols rows=rows id=id readonly=readonly value=value visualEditorEnable=visualEditorEnable buttons=buttons language=language tooltip=tooltip title=title fieldTitleBlank=fieldTitleBlank collapse=collapse fieldTitleBlank=fieldTitleBlank />
</#macro>

<#macro renderDateTimeField name className title value size maxlength id dateType shortDateInput timeDropdownParamName defaultDateTimeString localizedIconTitle timeDropdown timeHourName classString hour1 hour2 timeMinutesName minutes isTwelveHour ampmName amSelected pmSelected compositeType formName alert=false mask="" event="" action="" step="" timeValues="" tooltip=""collapse=false fieldType="" fieldTitleBlank=false>
  <#-- delegate to cato libs -->
  <@field_datetime_widget name=name className=className title=title value=value size=size maxlength=maxlength id=id dateType=dateType shortDateInput=shortDateInput timeDropdownParamName=timeDropdownParamName defaultDateTimeString=defaultDateTimeString localizedIconTitle=localizedIconTitle timeDropdown=timeDropdown timeHourName=timeHourName classString=classString hour1=hour1 hour2=hour2 timeMinutesName=timeMinutesName minutes=minutes isTwelveHour=isTwelveHour ampmName=ampmName amSelected=amSelected pmSelected=pmSelected compositeType=compositeType formName=formName alert=alert mask=mask event=event action=action step=step timeValues=timeValues tooltip=tooltip fieldTitleBlank=fieldTitleBlank />
</#macro>

<#macro renderDropDownField name className alert id multiple formName otherFieldName size firstInList currentValue explicitDescription allowEmpty options fieldName otherFieldName otherValue otherFieldSize dDFCurrent noCurrentSelectedKey ajaxOptions frequency minChars choices autoSelect partialSearch partialChars ignoreCase fullSearch event="" action="" ajaxEnabled=false tooltip="" manualItems=false manualItemsOnly=false collapse=false fieldType="" fieldTitleBlank=false>
  <#-- delegate to cato libs -->
  <@field_select_widget name=name className=className alert=alert id=id multiple=multiple formName=formName otherFieldName=otherFieldName size=size firstInList=firstInList currentValue=currentValue explicitDescription=explicitDescription allowEmpty=allowEmpty options=options fieldName=fieldName otherFieldName=otherFieldName otherValue=otherValue otherFieldSize=otherFieldSize dDFCurrent=dDFCurrent noCurrentSelectedKey=noCurrentSelectedKey ajaxOptions=ajaxOptions frequency=frequency minChars=minChars choices=choices autoSelect=autoSelect partialSearch=partialSearch partialChars=partialChars ignoreCase=ignoreCase fullSearch=fullSearch event=event action=action ajaxEnabled=ajaxEnabled tooltip=tooltip manualItems=manualItems manualItemsOnly=manualItemsOnly collapse=collapse fieldTitleBlank=fieldTitleBlank />
</#macro>

<#macro renderCheckBox id="" checked=false currentValue="N" name="" action="" tooltip="" fieldType="" fieldTitleBlank=false>
  <#-- delegate to cato libs -->
  <@field_checkbox_widget id=id checked=checked currentValue=currentValue name=name action=action tooltip=tooltip fieldTitleBlank=fieldTitleBlank />
</#macro>

<#macro renderCheckField items className alert id allChecked currentValue name event action tooltip="" fieldType="" fieldTitleBlank=false>
  <#list items as item>
    <div class="switch small">
    <span <@renderClass className alert />><#rt/>
      <input type="checkbox"<#if (item_index == 0)> id="${id}"</#if><#rt/>
        <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
        <#if allChecked?has_content && allChecked> checked="checked" <#elseif allChecked?has_content && !allChecked>
          <#elseif currentValue?has_content && currentValue==item.value> checked="checked"</#if> 
          name="${name?default("")?html}" value="${item.value?default("")?html}" <#if item.event?has_content> ${item.event}="${item.action!}"<#elseif event?has_content> ${event}="${action}"</#if>/><#rt/>
          <label for="${id!}"></label>
    </span>
    </div>
  </#list>
</#macro>

<#macro renderRadioField items className alert currentValue noCurrentSelectedKey name event action tooltip="">
  <#-- delegate to cato libs -->
  <@field_radio_widget items=items className=className alert=alert currentValue=currentValue noCurrentSelectedKey=noCurrentSelectedKey name=name event=event action=action tooltip=tooltip />
</#macro>

<#macro renderSubmitField buttonType className alert formName name event action imgSrc confirmation containerId ajaxUrl title fieldType="" fieldTitleBlank=false showProgress="" href="" onClick="" inputType="" disabled=false>
  <#local progressOptions = "">
    <#if !(showProgress?is_boolean && showProgress == false) && 
       ((showProgress?is_boolean && showProgress == true) ||
        ((htmlFormRenderFormInfo.formType)! == "upload" && (htmlFormRenderFormInfo.showProgress)! == true))>
      <#local baseId = htmlFormRenderFormInfo.name!"" + "_catouplprogform">       
      <#local progressOptions = {
        "formSel" : "form[name=${htmlFormRenderFormInfo.name}]",
        "progBarId" : "${baseId}_progbar",
        "progTextBoxId" : "${baseId}_textbox",
        
        "expectedResultContainerSel" : "#main-content",
        "errorResultContainerSel" : "#main-${styles.alert_wrap!}",
        "errorResultAddWrapper" : false
      }>
      <#local action = htmlFormRenderFormInfo.progressSuccessAction!"">
      <#if action?starts_with("redirect;")>
        <#local progressOptions = concatMaps(progressOptions, { "successRedirectUrl" : action?substring("redirect;"?length) })>
      <#elseif action == "reload" || action?starts_with("reload:")>
        <#-- FIXME: js-based reload doesn't work right in too many cases (e.g. when just came back to screen from
             switching visual theme and try to upload; url is something unrelated to page) -->
        <#local progressOptions = concatMaps(progressOptions, { "successReloadWindow" : true })>
      </#if>
      
      <#if htmlFormRenderFormInfo.progressOptions?has_content>
        <#-- json is valid freemarker map -->
        <#local addOpts = ("{" + htmlFormRenderFormInfo.progressOptions + "}")?eval>
        <#if addOpts?has_content>
          <#local progressOptions = progressOptions + addOpts>  
        </#if>
      </#if>
    </#if>

  <#-- delegate to cato libs -->
  <@field_submit_widget buttonType=buttonType className=className alert=alert formName=formName name=name event=event action=action imgSrc=imgSrc confirmation=confirmation containerId=containerId ajaxUrl=ajaxUrl title=title fieldTitleBlank=fieldTitleBlank showProgress=showProgress href=href onClick=onClick inputType=inputType disabled=disabled progressOptions=progressOptions/>
</#macro>

<#macro renderResetField className alert name title="" fieldType="" fieldTitleBlank=false>
  <input type="reset" <@renderClass className alert /> name="${name}"<#if title?has_content> value="${title}"</#if>/>
</#macro>

<#macro renderHiddenField name value id event action>
  <input type="hidden" name="${name}"<#if value?has_content> value="${value}"</#if><#if id?has_content> id="${id}"</#if><#if event?has_content && action?has_content> ${event}="${action}"</#if>/>
</#macro>

<#macro renderIgnoredField></#macro>

<#macro renderFieldTitle style title id fieldHelpText="" for="">
<#if (renderFormatFieldRowTitleCellOpened!false) != true>
  <#-- <label<#if for?has_content> for="${for}"</#if><#if fieldHelpText?has_content> title="${fieldHelpText}"</#if><#if style?has_content> class="${style}"</#if><#if id?has_content> id="${id}"</#if>><#t/> -->
    ${title}<#t/>
  <#-- </label><#t/> -->
</#if>
  <#global renderFieldTitleCurrentTitle = title>
</#macro>

<#macro renderSingleFormFieldTitle></#macro>

<#macro renderFormOpen linkUrl formType targetWindow containerId containerStyle autocomplete name viewIndexField viewSizeField viewIndex viewSize useRowSubmit showProgress=false progressOptions="" progressSuccessAction="">
  <#global htmlFormRenderFormInfo = { "name" : name, "formType" : formType, "showProgress" : showProgress, "progressOptions" : StringUtil.wrapString(progressOptions), "progressSuccessAction" : StringUtil.wrapString(progressSuccessAction)}>
  <form method="post" action="${linkUrl}"<#if formType=="upload"> enctype="multipart/form-data"</#if><#if targetWindow?has_content> target="${targetWindow}"</#if><#if containerId?has_content> id="${containerId}"</#if> class=<#if containerStyle?has_content>"${containerStyle}"<#else>"basic-form"</#if> onsubmit="javascript:submitFormDisableSubmits(this)"<#if autocomplete?has_content> autocomplete="${autocomplete}"</#if> name="${name}"><#lt/>
    <#if useRowSubmit?has_content && useRowSubmit>
      <input type="hidden" name="_useRowSubmit" value="Y"/>
      <#if (linkUrl?index_of("VIEW_INDEX") <= 0) && (linkUrl?index_of(viewIndexField) <= 0)>
        <input type="hidden" name="${viewIndexField}" value="${viewIndex}"/>
      </#if>
      <#if (linkUrl?index_of("VIEW_SIZE") <= 0) && (linkUrl?index_of(viewSizeField) <= 0)>
        <input type="hidden" name="${viewSizeField}" value="${viewSize}"/>
      </#if>
    </#if>
</#macro>
<#macro renderFormClose focusFieldName formName containerId hasRequiredField>
  </form><#lt/>
  <#if focusFieldName?has_content>
    <script language="JavaScript" type="text/javascript">
      var form = document.${formName};
      form.${focusFieldName}.focus();
      <#-- enable the validation plugin for all generated forms
      only enable the validation if min one field is marked as 'required' -->
      if (jQuery(form).find(".required").size() > 0) {
        jQuery(form).validate();
      }
    </script><#lt/>
  </#if>
  <#if containerId?has_content && hasRequiredField?has_content>
    <script type="text/javascript">
      jQuery("#${containerId}").validate({
        submitHandler:
          function(form) {
            form.submit();
          }
      });
    </script>
  </#if>
</#macro>
<#macro renderMultiFormClose>
  </form><#lt/>
</#macro>

<#macro renderFormatListWrapperOpen formName style columnStyles>
  <#local styleSet = splitStyleNamesToSet(style)>
  <#local scrollable = false>
  <#if styleSet.contains("scrollable")>
    <#local scrollable = true>
    <#local style = removeStyleNames(style, "scrollable")>
  </#if>
  <#local dummy = pushRequestStack("renderFormatListWrapperStack", {"formName":formName, "style":style, "scrollable":scrollable})>
  <#if scrollable>
  <#-- TODO: change this to something more foundation-like.
       this is a custom workaround to get scrolling, nothing else working. -->
  <div class="${styles.table_responsive_wrap!}">
  </#if>
  <table cellspacing="0" class="<#if style?has_content>${style}<#else>${styles.table_default!} form-widget-table dark-grid</#if>"><#lt/>
</#macro>

<#macro renderFormatListWrapperClose formName>
  <#local stackValues = popRequestStack("renderFormatListWrapperStack")!{}>
  <#local scrollable = stackValues.scrollable>
  </table><#lt/>
  <#if scrollable>
  </div>
  </#if>
</#macro>

<#macro renderFormatHeaderRowOpen style>
<thead>
  <tr class="<#if style?has_content>${style}<#else>header-row</#if>">
</#macro>
<#macro renderFormatHeaderRowClose>
  </tr>
  </thead>
</#macro>
<#macro renderFormatHeaderRowCellOpen style positionSpan>
  <#global renderFormatHeaderRowCellOpened = true>
  <th<#if positionSpan?has_content && positionSpan gt 1> colspan="${positionSpan}"</#if><#if style?has_content> class="${style}"</#if>>
</#macro>
<#macro renderFormatHeaderRowCellClose>
  </th>
  <#global renderFormatHeaderRowCellOpened = false>
</#macro>

<#macro renderFormatHeaderRowFormCellOpen style>
  <th<#if style?has_content> class="${style}"</#if>>
</#macro>
<#macro renderFormatHeaderRowFormCellClose>
  </th>
</#macro>
<#macro renderFormatHeaderRowFormCellTitleSeparator style isLast>
  <#if style?has_content><span class="${style}"></#if> - <#if style?has_content></span></#if>
</#macro>

<#macro renderFormatItemRowOpen formName itemIndex altRowStyles evenRowStyle oddRowStyle>
  <#-- Cato: translate stock "alternate-row" odd-row-style to odd+even -->
  <#local oddRowStyleSet = splitStyleNamesToSet(oddRowStyle)>
  <#if oddRowStyleSet.contains("alternate-row")>
    <#local remOddRowStyles = removeStyleNames(oddRowStyle, "alternate-row")>
    <#local oddRowStyle = (styles.row_alt! + " " + remOddRowStyles)?trim>
    <#local evenRowStyle = (styles.row_reg! + " " + removeStyleNames(evenRowStyle, "even-row"))?trim>
  </#if>
  <tr<#if itemIndex?has_content><#if itemIndex%2==0><#if evenRowStyle?has_content> class="${evenRowStyle}<#if altRowStyles?has_content> ${altRowStyles}</#if>"<#elseif altRowStyles?has_content> class="${altRowStyles}"</#if><#else><#if oddRowStyle?has_content> class="${oddRowStyle}<#if altRowStyles?has_content> ${altRowStyles}</#if>"<#elseif altRowStyles?has_content> class="${altRowStyles}"</#if></#if></#if>>
</#macro>
<#macro renderFormatItemRowClose formName>
  </tr>
</#macro>
<#macro renderFormatItemRowCellOpen fieldName style positionSpan>
  <td<#if positionSpan?has_content && positionSpan gt 1> colspan="${positionSpan}"</#if><#if style?has_content> class="${style}"</#if>>
</#macro>
<#macro renderFormatItemRowCellClose fieldName>
  </td>
</#macro>
<#macro renderFormatItemRowFormCellOpen style="">
  <td<#if style?has_content> class="${style}"</#if>>
</#macro>
<#macro renderFormatItemRowFormCellClose>
  </td>
</#macro>

<#macro renderFormatSingleWrapperOpen formName style="">
  <#--<table cellspacing="0"<#if style?has_content> class="${style}"</#if>>-->
</#macro>
<#macro renderFormatSingleWrapperClose formName>
  <#--</table>-->
</#macro>

<#macro renderFormatFieldRowOpen collapse=false style="" positions="">
  <#global renderFormatFieldRow_gridUsed = 0>
  <div class="${styles.grid_row!} form-field-row">
    <div class="<#if style?has_content>${style}<#else>${styles.grid_large!}12</#if> ${styles.grid_cell!}">
      <div class="${styles.grid_row!}<#if collapse> ${styles.collapse!}</#if>">
</#macro>
<#macro renderFormatFieldRowClose>
      </div>
    </div>
  </div>
</#macro>

<#function isFieldTypeAction fieldType fieldTitleBlank>
    <#return (fieldType=="submit" || fieldType=="reset" || (fieldType=="hyperlink" && fieldTitleBlank))>
</#function>

<#macro renderFormatFieldRowTitleCellOpen style="" collapse=false positions="" position="" positionSpan="" nextPositionInRow="" lastPositionInRow="" fieldType="" fieldTitleBlank=false>
  <#global renderFormatFieldRowTitleCellOpened = true>
</#macro>
<#macro renderFormatFieldRowTitleCellClose collapse=false fieldType="" fieldTitleBlank=false>
  <#global renderFormatFieldRowTitleCellOpened = false>
</#macro>

<#macro renderFormatFieldRowSpacerCell></#macro>
<#macro renderFormatFieldRowWidgetCellOpen collapse=false positionSpan="" style="" positions="" position="" positionSpan="" nextPositionInRow="" lastPositionInRow="" fieldType="" fieldTitleBlank=false>
  <#local isActionField = isFieldTypeAction(fieldType, fieldTitleBlank)>
  <#-- calculate position grid usage size for this field entry (recalc positionSpan ourselves) -->
  <#--positions: ${positions!} position: ${position!} positionSpan: ${positionSpan!} nextPositionInRow: ${nextPositionInRow!} lastPositionInRow: ${lastPositionInRow!} -->
  <#local gridSize = 12>
  <#local markLast = false>
  <#local fieldEntryOffset = 0>
  <#if positions?has_content && positionSpan?has_content && position?has_content>
    <#-- note: positionSpan is one less than you'd expect -->
    <#local posSpan = positionSpan + 1>
    <#local fieldEntrySize = ((posSpan*gridSize) / positions)?floor>
    <#if !nextPositionInRow?has_content && (fieldEntrySize < gridSize)>
      <#local markLast = true>
    </#if>
    <#-- calc offset if needed -->
    <#if (position > 1)>
        <#local expectedFieldEntryOffset = (((position-1)*gridSize) / positions)?floor>
        <#local fieldEntryOffset = (expectedFieldEntryOffset - renderFormatFieldRow_gridUsed)>
    </#if>
    
    <#-- WARN: if style specified manually, isn't taken into account here -->
    <#global renderFormatFieldRow_gridUsed = renderFormatFieldRow_gridUsed + fieldEntrySize + fieldEntryOffset>
  <#else>
    <#local fieldEntrySize = gridSize>
  </#if>
  
  <#-- may be more than one title+widget in one row, so wrap each combo in another div - necessary for now... -->
 <!-- positions: ${positions!} position: ${position!} positionSpan: ${positionSpan!} nextPositionInRow: ${nextPositionInRow!} lastPositionInRow: ${lastPositionInRow!} posSpan: ${posSpan!} markLast: ${markLast!?string}
  fieldEntryOffset: ${fieldEntryOffset}
  renderFormatFieldRow_gridUsed: ${renderFormatFieldRow_gridUsed}
  fieldEntrySize: ${fieldEntrySize!} gridSize: ${gridSize!} -->
  
  <#local fieldEntryTypeClass = "field-entry-type-" + mapWidgetFieldTypeToStyleName(fieldType)>
  <div class="<#if style?has_content>${style}<#else>${styles.grid_large!}${fieldEntrySize}<#if (fieldEntryOffset > 0)> ${styles.grid_large_offset!}${fieldEntryOffset}</#if></#if> ${styles.grid_cell!}<#if markLast> ${styles.grid_end!}</#if>">
    <div class="${styles.grid_row!} form-field-entry ${fieldEntryTypeClass}">
    
  <#-- DEV NOTE: field spans were intentionally made to total to 11 instead of 12 as temporary workaround for small-vs-large-sizing-within-columns adaptation problems -->
  <#local isActionField = isFieldTypeAction(fieldType, fieldTitleBlank)>
  <#if !isActionField>
      <div class="<#if style?has_content>${style}<#else>${styles.grid_small!}3 ${styles.grid_large!}2</#if> ${styles.grid_cell!} field-entry-title ${fieldEntryTypeClass}">
        <#if collapse><span class="prefix form-field-label"><#else><label class="form-field-label" for="<#if id?has_content>${id}<#else>${name!}</#if>">${renderFieldTitleCurrentTitle!}</#if><#if collapse></span><#else></label></#if>
      </div>
  </#if>
  <#local isActionField = isFieldTypeAction(fieldType, fieldTitleBlank)>
  <#if !isActionField>
      <div class="<#if style?has_content>${style}<#else>${styles.grid_small!}8 ${styles.grid_large!}9</#if> ${styles.grid_cell!} ${styles.grid_end!} field-entry-widget ${fieldEntryTypeClass}">
  <#else>
      <div class="<#if style?has_content>${style}<#else>${styles.grid_small!}12 ${styles.grid_large!}12</#if> ${styles.grid_cell!} ${styles.grid_end!} field-entry-widget ${fieldEntryTypeClass}">
  </#if>
    
</#macro>

<#macro renderFormatFieldRowWidgetCellClose fieldType="" fieldTitleBlank=false>
  </div>
  <#local isActionField = isFieldTypeAction(fieldType, fieldTitleBlank)>
      </div>
    </div>
</#macro>


<#macro renderFormatEmptySpace>&nbsp;</#macro>

<#macro renderTextFindField name value defaultOption opEquals opBeginsWith opContains opIsEmpty opNotEqual className alert size maxlength autocomplete titleStyle hideIgnoreCase ignCase ignoreCase title="" fieldType="" fieldTitleBlank=false>
  <#-- delegate to cato libs -->
  <@field_textfind_widget name=name value=value defaultOption=defaultOption opEquals=opEquals opBeginsWith=opBeginsWith opContains=opContains opIsEmpty=opIsEmpty opNotEqual=opNotEqual className=className alert=alert size=size maxlength=maxlength autocomplete=autocomplete titleStyle=titleStyle hideIgnoreCase=hideIgnoreCase ignCase=ignCase ignoreCase=ignoreCase title=title fieldTitleBlank=fieldTitleBlank />
</#macro>

<#macro renderDateFindField className alert name localizedInputTitle value value2 size maxlength dateType formName defaultDateTimeString imgSrc localizedIconTitle titleStyle defaultOptionFrom defaultOptionThru opEquals opSameDay opGreaterThanFromDayStart opGreaterThan opGreaterThan opLessThan opUpToDay opUpThruDay opIsEmpty>
  <#-- delegate to cato libs -->
  <@field_datefind_widget className=className alert=alert name=name localizedInputTitle=localizedInputTitle value=value value2=value2 size=size maxlength=maxlength dateType=dateType formName=formName defaultDateTimeString=defaultDateTimeString imgSrc=imgSrc localizedIconTitle=localizedIconTitle titleStyle=titleStyle defaultOptionFrom=defaultOptionFrom defaultOptionThru=defaultOptionThru opEquals=opEquals opSameDay=opSameDay opGreaterThanFromDayStart=opGreaterThanFromDayStart opGreaterThan=opGreaterThan opGreaterThan=opGreaterThan opLessThan=opLessThan opUpToDay=opUpToDay opUpThruDay=opUpThruDay opIsEmpty=opIsEmpty />
</#macro>

<#macro renderRangeFindField className alert name value size maxlength autocomplete titleStyle defaultOptionFrom opEquals opGreaterThan opGreaterThanEquals opLessThan opLessThanEquals value2 defaultOptionThru>
  <#-- delegate to cato libs -->
  <@field_rangefind_widget className=className alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete titleStyle=titleStyle defaultOptionFrom=defaultOptionFrom opEquals=opEquals opGreaterThan=opGreaterThan opGreaterThanEquals=opGreaterThanEquals opLessThan=opLessThan opLessThanEquals=opLessThanEquals value2=value2 defaultOptionThru=defaultOptionThru />
</#macro>

<#--
@renderLookupField

Description: Renders a text input field as a lookup field.

Parameter: name, String, required - The name of the lookup field.
Parameter: formName, String, required - The name of the form that contains the lookup field.
Parameter: fieldFormName, String, required - Contains the lookup window form name.
Parameter: className, String, optional - The CSS class name for the lookup field.
Parameter: alert, String, optional - If "true" then the "alert" CSS class will be added to the lookup field.
Parameter: value, Object, optional - The value of the lookup field.
Parameter: size, String, optional - The size of the lookup field.
Parameter: maxlength, String or Integer, optional - The max length of the lookup field.
Parameter: id, String, optional - The ID of the lookup field.
Parameter: event, String, optional - The lookup field event that invokes "action". If the event parameter is not empty, then the action parameter must be specified as well.
Parameter: action, String, optional - The action that is invoked on "event". If action parameter is not empty, then the event parameter must be specified as well.
Parameter: readonly, boolean, optional - If true, the lookup field is made read-only.
Parameter: autocomplete, String, optional - If not empty, autocomplete is turned off for the lookup field.
Parameter: descriptionFieldName, String, optional - If not empty and the presentation parameter contains "window", specifies an alternate input field for updating.
Parameter: targetParameterIter, List, optional - Contains a list of form field names whose values will be passed to the lookup window.
Parameter: imgSrc, Not used.
Parameter: ajaxUrl, String, optional - Contains the Ajax URL, used only when the ajaxEnabled parameter contains true.
Parameter: ajaxEnabled, boolean, optional - If true, invokes the Ajax auto-completer.
Parameter: presentation, String, optional - Contains the lookup window type, either "layer" or "window".
Parameter: width, String or Integer, optional - The width of the lookup field.
Parameter: height, String or Integer, optional - The height of the lookup field.
Parameter: position, String, optional - The position style of the lookup field.
Parameter: fadeBackground, ?
Parameter: clearText, String, optional - If the readonly parameter is true, clearText contains the text to be displayed in the field, default is CommonClear label.
Parameter: showDescription, String, optional - If the showDescription parameter is true, a special span with css class "tooltip" will be created at right of the lookup button and a description will fill in (see setLookDescription in selectall.js). For now not when the lookup is read only.
Parameter: initiallyCollapsed, Not used.
Parameter: lastViewName, String, optional - If the ajaxEnabled parameter is true, the contents of lastViewName will be appended to the Ajax URL.
-->
<#macro renderLookupField name formName fieldFormName className="" alert="false" value="" size="" maxlength="" id="" event="" action="" readonly=false autocomplete="" descriptionFieldName="" targetParameterIter="" imgSrc="" ajaxUrl="" ajaxEnabled=javaScriptEnabled presentation="layer" width="" height="" position="" fadeBackground="true" clearText="" showDescription="" initiallyCollapsed="" lastViewName="main"  title="" fieldType="" fieldTitleBlank=false>
  <#-- delegate to cato libs -->
  <@field_lookup_widget name=name formName=formName fieldFormName=fieldFormName className=className alert=alert value=value size=size maxlength=maxlength id=id event=event action=action readonly=readonly autocomplete=autocomplete descriptionFieldName=descriptionFieldName targetParameterIter=targetParameterIter imgSrc=imgSrc ajaxUrl=ajaxUrl ajaxEnabled=ajaxEnabled presentation=presentation width=width height=height position=position fadeBackground=fadeBackground clearText=clearText showDescription=showDescription initiallyCollapsed=initiallyCollapsed lastViewName=lastViewName  title=title fieldTitleBlank=fieldTitleBlank />
</#macro>

<#-- Cato: new params: paginate, forcePost, viewIndexFirst, listItemsOnly, paginateToggle*
     paginate is a display hint, does not seem to mean guarantee data wasn't paginated -->
<#macro renderNextPrev paginateStyle paginateFirstStyle viewIndex highIndex listSize viewSize ajaxEnabled javaScriptEnabled ajaxFirstUrl firstUrl paginateFirstLabel paginatePreviousStyle ajaxPreviousUrl previousUrl paginatePreviousLabel pageLabel ajaxSelectUrl selectUrl ajaxSelectSizeUrl selectSizeUrl commonDisplaying paginateNextStyle ajaxNextUrl nextUrl paginateNextLabel paginateLastStyle ajaxLastUrl lastUrl paginateLastLabel paginateViewSizeLabel paginate=true forcePost=false viewIndexFirst=0 listItemsOnly=false paginateToggle=false ajaxPaginateOnUrl="" paginateOnUrl="" paginateOnStyle="" paginateOnLabel="" ajaxPaginateOffUrl="" paginateOffUrl="" paginateOffStyle="" paginateOffLabel="">
    <#-- delegate to cato libs -->
    <@paginate_core paginateStyle=paginateStyle paginateFirstStyle=paginateFirstStyle viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxEnabled=ajaxEnabled javaScriptEnabled=javaScriptEnabled ajaxFirstUrl=ajaxFirstUrl firstUrl=firstUrl paginateFirstLabel=paginateFirstLabel paginatePreviousStyle=paginatePreviousStyle ajaxPreviousUrl=ajaxPreviousUrl previousUrl=previousUrl paginatePreviousLabel=paginatePreviousLabel pageLabel=pageLabel ajaxSelectUrl=ajaxSelectUrl selectUrl=selectUrl ajaxSelectSizeUrl=ajaxSelectSizeUrl selectSizeUrl=selectSizeUrl commonDisplaying=commonDisplaying paginateNextStyle=paginateNextStyle ajaxNextUrl=ajaxNextUrl nextUrl=nextUrl 
        paginateNextLabel=paginateNextLabel paginateLastStyle=paginateLastStyle ajaxLastUrl=ajaxLastUrl lastUrl=lastUrl paginateLastLabel=paginateLastLabel paginateViewSizeLabel=paginateViewSizeLabel paginate=paginate forcePost=forcePost viewIndexFirst=viewIndexFirst listItemsOnly=listItemsOnly paginateToggle=paginateToggle ajaxPaginateOnUrl=ajaxPaginateOnUrl paginateOnUrl=paginateOnUrl paginateOnStyle=paginateOnStyle paginateOnLabel=paginateOnLabel ajaxPaginateOffUrl=ajaxPaginateOffUrl paginateOffUrl=paginateOffUrl paginateOffStyle=paginateOffStyle paginateOffLabel=paginateOffLabel />
</#macro>

<#macro renderFileField className alert name value size maxlength autocomplete id="" title="" fieldType="" fieldTitleBlank=false>
  <#-- delegate to cato libs -->
  <@field_file_widget className=className alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete id=id title=title fieldTitleBlank=fieldTitleBlank />
</#macro>
<#macro renderPasswordField className alert name value size maxlength id autocomplete title="" fieldType="" fieldTitleBlank=false>
  <#-- delegate to cato libs -->
  <@field_password_widget className=className alert=alert name=name value=value size=size maxlength=maxlength id=id autocomplete=autocomplete title=title fieldTitleBlank=fieldTitleBlank />
</#macro>
<#macro renderImageField value description alternate style event action title="" fieldType="" fieldTitleBlank=false>
  <img<#if value?has_content> src="${value}"</#if><#if description?has_content> title="${description}"</#if> alt="<#if alternate?has_content>${alternate}"</#if><#if style?has_content> class="${style}"</#if><#if event?has_content> ${event?html}="${action}" </#if>/>
</#macro>

<#macro renderBanner style leftStyle rightStyle leftText text rightText>
  <table width="100%">
    <tr><#rt/>
      <#if leftText?has_content><td align="left"><#if leftStyle?has_content><div class="${leftStyle}"></#if>${leftText}<#if leftStyle?has_content></div></#if></td><#rt/></#if>
      <#if text?has_content><td align="center"><#if style?has_content><div class="${style}"></#if>${text}<#if style?has_content></div></#if></td><#rt/></#if>
      <#if rightText?has_content><td align="right"><#if rightStyle?has_content><div class="${rightStyle}"></#if>${rightText}<#if rightStyle?has_content></div></#if></td><#rt/></#if>
    </tr>
  </table>
</#macro>

<#macro renderContainerField id className><div id="${id}" class="${className}"/></#macro>

<#macro renderFieldGroupOpen style id title collapsed collapsibleAreaId expandToolTip collapseToolTip collapsible>
    <#-- delegate to cato libs -->
    <@fieldset_core openOnly=true style=style id=id title=title collapsed=collapsed collapsibleAreaId=collapsibleAreaId expandToolTip=expandToolTip collapseToolTip=collapseToolTip collapsible=collapsible />
</#macro>

<#macro renderFieldGroupClose style id title>
    <@fieldset_core closeOnly=true style=style id=id title=title />
</#macro>

<#macro renderHyperlinkTitle name title showSelectAll="N">
  <#if title?has_content>${title}<br /></#if>
  <#if showSelectAll="Y"><input type="checkbox" name="selectAll" value="Y" onclick="javascript:toggleAll(this, '${name}');"/></#if>
</#macro>

<#macro renderSortField style title linkUrl ajaxEnabled tooltip="">
  <a<#if style?has_content> class="${style}"</#if> href="<#if ajaxEnabled?has_content && ajaxEnabled>javascript:ajaxUpdateAreas('${linkUrl}')<#else>${linkUrl}</#if>"<#if tooltip?has_content> title="${tooltip}"</#if>>${title}</a>
</#macro>

<#macro formatBoundaryComment boundaryType widgetType widgetName><!-- ${boundaryType}  ${widgetType}  ${widgetName} --></#macro>

<#macro renderTooltip tooltip tooltipStyle>
  <#if tooltip?has_content><span class="<#if tooltipStyle?has_content>${tooltipStyle}<#else>tooltip</#if>">${tooltip}</span><#rt/></#if>
</#macro>

<#macro renderClass className alert="false">
  <#if className?has_content || alert?string == "true"> class="${className!}<#if alert?string == "true"> alert</#if>" </#if>
</#macro>

<#macro renderAsterisks requiredField requiredStyle>
  <#if requiredField=="true"><#if !requiredStyle?has_content><span class="form-field-input-asterisk">*</span></#if></#if>
</#macro>

<#macro makeHiddenFormLinkForm actionUrl name parameters targetWindow>
  <form method="post" action="${actionUrl}"<#if targetWindow?has_content> target="${targetWindow}"</#if> onsubmit="javascript:submitFormDisableSubmits(this)" name="${name}">
    <#list parameters as parameter>
      <input name="${parameter.name}" value="${parameter.value}" type="hidden"/>
    </#list>
  </form>
</#macro>
<#macro makeHiddenFormLinkAnchor linkStyle hiddenFormName event action imgSrc description confirmation>
  <a<#if linkStyle?has_content> class="${linkStyle}"</#if> href="javascript:document.${hiddenFormName}.submit()"
    <#if action?has_content && event?has_content> ${event}="${action}"</#if>
    <#if confirmation?has_content> onclick="return confirm('${confirmation?js_string}')"</#if>>
      <#if imgSrc?has_content><img src="${imgSrc}" alt=""/></#if>${description}</a>
</#macro>
<#macro makeHyperlinkString linkStyle hiddenFormName event action imgSrc title alternate linkUrl targetWindow description confirmation>
    <a<#if linkStyle?has_content> class="${linkStyle}"</#if> 
      href="${linkUrl}"<#if targetWindow?has_content> target="${targetWindow}"</#if>
      <#if action?has_content && event?has_content> ${event}="${action}"</#if>
      <#if confirmation?has_content> onclick="return confirm('${confirmation?js_string}')"</#if>
      <#if imgSrc?length == 0 && title?has_content> title="${title}"</#if>>
        <#if imgSrc?has_content><img src="${imgSrc}" alt="${alternate}" title="${title}"/></#if>${description}</a>
</#macro>

<#macro renderAlternateText className text wrapperOpened headerRendered numOfColumns>
  <#-- note: numOfColumns may be zero when no header -->
  <#if wrapperOpened>
    <tr>
      <td<#if (numOfColumns > 1)> colspan="${numOfColumns}"</#if>>
        <@renderLabelCommon text=text style=className id="" />
      </td>
    </tr>
  <#else>
    <@renderLabelCommon text=text style=className id="" />
  </#if>
</#macro>

  