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
SCIPIO: NOTE: since macro renderer initial context mod, macros here now have access to a few widget context objects part of the initial
context, such as request, response, locale, and to some extent (since 2016-01-06), uiLabelMap.
WARN: no code run here or indirectly from here should assume full current context present. only use well-known generic vars.

NOTE: 2016-10-05: Widget early HTML encoding is now DISABLED for all HTML macros.
    As a result all macros here must take care to html-escape as well as js-escape values.
    Use escapeVal/escapeFullUrl for this.
-->
<#macro renderField text extraArgs...>
  <#-- delegate to scipio libs -->
  <@field_generic_widget text=text />
</#macro>

<#macro renderDisplayField type imageLocation idName description title class alert inPlaceEditorUrl="" inPlaceEditorParams="" imageAlt="" collapse=false fieldType="" fieldTitleBlank=false requiredField="" tooltip="" extraArgs...>
  <#-- delegate to scipio libs -->
  <@field_display_widget type=type imageLocation=imageLocation idName=idName description=description title=title class=class alert=alert 
    inPlaceEditorUrl=inPlaceEditorUrl inPlaceEditorParams=inPlaceEditorParams imageAlt=imageAlt fieldTitleBlank=fieldTitleBlank required=renderFieldIsRequired(requiredField) tooltip=tooltip/>
</#macro>
<#macro renderHyperlinkField extraArgs...></#macro>

<#macro renderTextField name className alert value textSize maxlength id event="" action="" disabled=false ajaxUrl="" ajaxEnabled=false mask=false clientAutocomplete="" placeholder="" tooltip="" collapse=false readonly=false fieldType="" fieldTitleBlank=false requiredField="" extraArgs...>
  <#-- delegate to scipio libs -->
  <#if event?has_content>
    <#local events = {event:action}>
  <#else>
    <#local events = {}>
  </#if>
  <@field_input_widget name=name class=className alert=alert value=value textSize=textSize maxlength=maxlength id=id events=events disabled=disabled ajaxUrl=ajaxUrl ajaxEnabled=ajaxEnabled mask=mask clientAutocomplete=clientAutocomplete placeholder=placeholder tooltip=tooltip collapse=collapse readonly=readonly fieldTitleBlank=fieldTitleBlank required=renderFieldIsRequired(requiredField)/>
</#macro>

<#macro renderTextareaField name className alert cols rows id readonly value visualEditorEnable=true language="" buttons="" tooltip="" title="" fieldType="" fieldTitleBlank=false collapse=false maxlength="" requiredField="" extraArgs...>
  <#-- delegate to scipio libs -->
  <@field_textarea_widget name=name class=className alert=alert cols=cols rows=rows id=id readonly=readonly value=value visualEditorEnable=visualEditorEnable buttons=buttons language=language tooltip=tooltip title=title fieldTitleBlank=fieldTitleBlank collapse=collapse fieldTitleBlank=fieldTitleBlank maxlength=maxlength required=renderFieldIsRequired(requiredField)/>
</#macro>

<#macro renderDateTimeField name className title value size maxlength id dateType shortDateInput timeDropdownParamName defaultDateTimeString localizedIconTitle timeDropdown timeHourName classString hour1 hour2 timeMinutesName minutes isTwelveHour ampmName amSelected pmSelected compositeType formName="" alert=false mask="" event="" action="" step="" timeValues="" tooltip="" collapse=false fieldType="" fieldTitleBlank=false requiredField="" extraArgs...>
  <#-- delegate to scipio libs -->
  <#if event?has_content>
    <#local events = {event:action}>
  <#else>
    <#local events = {}>
  </#if>
  <#local dateDisplayType = "">
  <#if shortDateInput>
    <#local dateDisplayType = "date">
  </#if>
  <@field_datetime_widget name=name class=className title=title value=value size=size maxlength=maxlength id=id dateType=dateType dateDisplayType=dateDisplayType timeDropdownParamName=timeDropdownParamName defaultDateTimeString=defaultDateTimeString localizedIconTitle=localizedIconTitle timeDropdown=timeDropdown timeHourName=timeHourName classString=classString hour1=hour1 hour2=hour2 timeMinutesName=timeMinutesName minutes=minutes isTwelveHour=isTwelveHour ampmName=ampmName amSelected=amSelected pmSelected=pmSelected compositeType=compositeType formName=formName alert=alert mask=mask events=events step=step timeValues=timeValues tooltip=tooltip fieldTitleBlank=fieldTitleBlank required=renderFieldIsRequired(requiredField)/>
</#macro>

<#macro renderDropDownField name className alert id multiple formName otherFieldName size firstInList currentValue explicitDescription allowEmpty options fieldName otherFieldName otherValue otherFieldSize dDFCurrent noCurrentSelectedKey ajaxOptions frequency minChars choices autoSelect partialSearch partialChars ignoreCase fullSearch event="" action="" ajaxEnabled=false tooltip="" manualItems=false manualItemsOnly=false collapse=false fieldType="" fieldTitleBlank=false requiredField="" extraArgs...>
  <#-- delegate to scipio libs -->
  <#if event?has_content>
    <#local events = {event:action}>
  <#else>
    <#local events = {}>
  </#if>
  <@field_select_widget name=name class=className alert=alert id=id multiple=multiple formName=formName otherFieldName=otherFieldName size=size currentFirst=firstInList currentValue=currentValue currentDescription=explicitDescription allowEmpty=allowEmpty options=options fieldName=fieldName otherFieldName=otherFieldName otherValue=otherValue otherFieldSize=otherFieldSize dDFCurrent=dDFCurrent inlineSelected=(dDFCurrent?has_content && "selected" == dDFCurrent) defaultValue=noCurrentSelectedKey ajaxOptions=ajaxOptions frequency=frequency minChars=minChars choices=choices autoSelect=autoSelect partialSearch=partialSearch partialChars=partialChars ignoreCase=ignoreCase fullSearch=fullSearch events=events ajaxEnabled=ajaxEnabled tooltip=tooltip manualItems=manualItems manualItemsOnly=manualItemsOnly collapse=collapse fieldTitleBlank=fieldTitleBlank required=renderFieldIsRequired(requiredField)/>
</#macro>

<#-- SCIPIO: NOTE: here noCurrentSelectedKey, key, and altKey may be omitted by caller (java) when null, so that way we decide the defaults ourselves -->
<#macro renderCheckField items className alert id allChecked currentValue name event action tooltip="" fieldType="" fieldTitleBlank=false requiredField="" noCurrentSelectedKey="" key="" altKey=false useHidden="" extraArgs...>
  <#-- delegate to scipio libs -->
  <#if event?has_content>
    <#local events = {event:action}>
  <#else>
    <#local events = {}>
  </#if>
  <@field_checkbox_widget items=items id=id class=className alert=alert allChecked=allChecked value=key altValue=altKey useHidden=useHidden currentValue=currentValue defaultValue=noCurrentSelectedKey name=name events=events tooltip=tooltip fieldTitleBlank=fieldTitleBlank multiMode=true required=renderFieldIsRequired(requiredField)/>
</#macro>

<#macro renderRadioField items className alert currentValue noCurrentSelectedKey name event action tooltip="" fieldType="" fieldTitleBlank=false requiredField="" extraArgs...>
  <#-- delegate to scipio libs -->
  <#if event?has_content>
    <#local events = {event:action}>
  <#else>
    <#local events = {}>
  </#if>
  <@field_radio_widget items=items class=className alert=alert currentValue=currentValue defaultValue=noCurrentSelectedKey name=name events=events tooltip=tooltip fieldTitleBlank=fieldTitleBlank multiMode=true required=renderFieldIsRequired(requiredField)/>
</#macro>

<#macro renderSubmitField buttonType className alert formName name event action imgSrc confirmation containerId ajaxUrl title fieldType="" fieldTitleBlank=false showProgress="" href="" inputType="" disabled=false id="" extraArgs...>
  <#local formInfo = readRequestStack("htmlFormRenderFormStack")!{}>
  <#local progressOptions = "">
  <#if !(showProgress?is_boolean && showProgress == false) && 
     ((showProgress?is_boolean && showProgress == true) ||
      ((formInfo.formType)! == "upload" && (formInfo.showProgress)! == true))>
    <#local baseId = rawString(formInfo.name!"") + "_scipiouplprogform">       
    <#local progressOptions = {
      "formSel" : "form[name=${rawString(formInfo.name)}]",<#-- NOTE: escaped later -->
      "progBarId" : "${rawString(baseId)}_progbar",
      "progTextBoxId" : "${rawString(baseId)}_textbox",
      
      "expectedResultContainerSel" : "#main-content",
      "errorResultContainerSel" : "#main-${styles.alert_wrap!}",
      "errorResultAddWrapper" : false
    }>
    <#if formInfo.progressSuccessAction?has_content>
      <#local action = compileProgressSuccessAction(formInfo.progressSuccessAction)>
    <#else>
      <#local action = "">
    </#if>
    <#if action?starts_with("redirect;")>
      <#local progressOptions = concatMaps(progressOptions, { "successRedirectUrl" : action?substring("redirect;"?length) })>
    <#elseif action == "reload" || action?starts_with("reload:")>
      <#-- FIXME: js-based reload doesn't work right in too many cases (e.g. when just came back to screen from
           switching visual theme and try to upload; url is something unrelated to page) -->
      <#local progressOptions = concatMaps(progressOptions, { "successReloadWindow" : true })>
    </#if>
    
    <#if formInfo.progressOptions?has_content>
      <#-- json is valid freemarker map -->
      <#local addOpts = evalToSimpleMap(formInfo.progressOptions)>
      <#if addOpts?has_content>
        <#local progressOptions = progressOptions + addOpts>  
      </#if>
    </#if>
  </#if>

  <#-- delegate to scipio libs -->
  <#if event?has_content>
    <#local events = {event:action}>
  <#else>
    <#local events = {}>
  </#if>
  <@field_submit_widget buttonType=buttonType class=className alert=alert formName=formName name=name events=events imgSrc=imgSrc confirmation=confirmation containerId=containerId ajaxUrl=ajaxUrl text=title description=title fieldTitleBlank=fieldTitleBlank showProgress=showProgress href=href inputType=inputType disabled=disabled progressOptions=progressOptions id=id/>
</#macro>

<#macro renderResetField className alert name title="" fieldType="" fieldTitleBlank=false extraArgs...>
  <#-- delegate to scipio libs -->
  <@field_reset_widget class=className alert=alert name=name text=title fieldTitleBlank=fieldTitleBlank />
</#macro>

<#macro renderHiddenField name value id event action extraArgs...>
  <#-- delegate to scipio libs -->
  <#if event?has_content>
    <#local events = {event:action}>
  <#else>
    <#local events = {}>
  </#if>
  <@field_hidden_widget name=name value=value id=id events=events />
</#macro>

<#macro renderIgnoredField extraArgs...></#macro>

<#macro renderFieldTitle style title id fieldHelpText="" for="" extraArgs...>
<#if (renderFormatFieldRowTitleCellOpened!false) != true>
  <#-- <label<#if for?has_content> for="${escapeVal(for, 'html')}"</#if><#if fieldHelpText?has_content> title="${escapeVal(fieldHelpText, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if><#if id?has_content> id="${escapeVal(id, 'html')}"</#if>><#t/> -->
    ${escapeVal(title, 'htmlmarkup')}<#t/>
  <#-- </label><#t/> -->
</#if>
  <#global renderFieldTitleCurrentTitle = title>
  <#global renderFieldTitleCurrentForId = for>
  <#global renderFieldTitleCurrentFieldHelpText = fieldHelpText>
</#macro>

<#macro renderSingleFormFieldTitle extraArgs...></#macro>

<#-- SCIPIO: formScope values: general, item; formSpread values: general, single-cell (html ok), multi-cell (may be bad html) -->
<#macro renderFormOpen linkUrl formType targetWindow containerId containerStyle autocomplete name viewIndexField viewSizeField viewIndex viewSize useRowSubmit attribs={} method="" formScope="general" formSpread="general" extraArgs...>
  <#if !method?has_content>
    <#local method = "post">
  </#if>
  <#-- showProgress=false progressOptions="" progressSuccessAction=""  -->
  <#-- extra form attribs: <@objectAsScript lang="raw" escape=false object=attribs /> -->
  <#-- SCIPIO: process extra attribs -->
  <#local showProgress = (attribs.showProgress)!false>
  <#if !showProgress?is_boolean>
    <#if showProgress?has_content>
      <#local showProgress = showProgress?boolean>
    <#else>
      <#local showProgress = false>
    </#if>
  </#if>
  <#-- SCIPIO: support fieldsType="xxx" in form widget extra attribs, equivalent to setting @fields type="xxx" -->
  <#if attribs.fieldsType?has_content>
    <@fields type=attribs.fieldsType open=true close=false />
  </#if>
  <#local progressOptions = (attribs.progressOptions)!{}><#-- NOTE: this may be a string repr of a map! -->
  <#local progressSuccessAction = (attribs.progressSuccessAction)!"">
  <#local formInfo = {"name":name, "formType":formType, "formScope":formScope, "formSpread":formSpread,
    "showProgress":showProgress, "progressOptions":progressOptions, "progressSuccessAction":progressSuccessAction, "attribs":attribs}>
  <#local dummy = pushRequestStack("htmlFormRenderFormStack", formInfo)>
<#-- SCIPIO: TODO/FIXME: prevent invalid html; (SHOULD BE) handled by renderSubmitForm
    CURRENTLY PROBLEMATIC; adding this condition prevents <form> between cells,
    BUT the form MAY be currently still needed for jQuery validate (see renderFormClose) and untold other lookups
    based on form name/id and presence of form element.
<#if !(formType == "list" && formScope == "item" && formSpread == "multi-cell")>
-->
  <form method="${escapeVal(method, 'html')}" action="${escapeFullUrl(linkUrl, 'html')}"<#if formType=="upload"> enctype="multipart/form-data"</#if><#rt/>
      <#lt/><#if targetWindow?has_content> target="${escapeVal(targetWindow, 'html')}"</#if>
      <#if containerId?has_content> id="${escapeVal(containerId, 'html')}"</#if> class=<#if containerStyle?has_content>"${escapeVal(containerStyle, 'html')}"<#else>"basic-form"</#if>
      <#lt/> onsubmit="javascript:submitFormDisableSubmits(this);"<#if autocomplete?has_content> autocomplete="${escapeVal(autocomplete, 'html')}"</#if> name="${escapeVal(name, 'html')}"><#lt/>
    <#if useRowSubmit?has_content && useRowSubmit>
      <input type="hidden" name="_useRowSubmit" value="Y"/>
    </#if>
    <#-- SCIPIO: moved this OUTSIDE the useRowSubmit check -->
    <#if (linkUrl?index_of("VIEW_INDEX") <= 0) && (linkUrl?index_of(viewIndexField) <= 0)>
      <input type="hidden" name="${escapeVal(viewIndexField, 'html')}" value="${viewIndex}"/>
    </#if>
    <#if (linkUrl?index_of("VIEW_SIZE") <= 0) && (linkUrl?index_of(viewSizeField) <= 0)>
      <input type="hidden" name="${escapeVal(viewSizeField, 'html')}" value="${viewSize}"/>
    </#if>
<#-- 
</#if>
-->
</#macro>
<#-- SCIPIO: WARN: also exists renderMultiFormClose below -->
<#macro renderFormClose focusFieldName formName containerId hasRequiredField extraArgs...>
  <#local formInfo = popRequestStack("htmlFormRenderFormStack")>
<#-- SCIPIO: TODO/FIXME: prevent invalid html; (SHOULD BE) handled by renderSubmitForm
<#if !((formInfo.formType!) == "list" && (formInfo.formScope!) == "item" && (formInfo.formSpread!) == "multi-cell")>
-->
  </form><#lt/>
  <#if (formInfo.attribs.fieldsType)?has_content>
    <@fields type=formInfo.attribs.fieldsType open=false close=true />
  </#if>
  <#if focusFieldName?has_content>
    <@script>
      var form = document['${escapeVal(formName, 'js')}'];
      form["${escapeVal(focusFieldName, 'js')}"].focus();
      <#-- enable the validation plugin for all generated forms
      only enable the validation if min one field is marked as 'required' -->
      if (jQuery(form).find(".required").length > 0) {
          <#-- SCIPIO: 2017-09-29: now delegating this
          jQuery(form).validate();-->
          <@formValidateScript formExpr="form" htmlwrap=false/>
      }
    </@script><#lt/>
  </#if>
  <#if containerId?has_content && hasRequiredField?has_content>
    <#-- SCIPIO: 2017-09-29: now delegating this
    <@script>
      jQuery("#${escapeVal(containerId, 'js')}").validate({
        submitHandler:
          function(form) {
            form.submit();
          }
      });
    </@script>-->
    <@formValidateScript id=containerId name=formName htmlwrap=true/>
  </#if>
<#--
<#else>
  <#- SCIPIO: TODO/FIXME: in this case we have to omit the <form> element, but are then missing
    the .validate call (also TODO: recheck the validate call above) ->
</#if>
-->
</#macro>
<#macro renderMultiFormClose extraArgs...>
  <#local formInfo = popRequestStack("htmlFormRenderFormStack")!{}>
  </form><#lt/>
  <#if (formInfo.attribs.fieldsType)?has_content>
    <@fields type=formInfo.attribs.fieldsType open=false close=true />
  </#if>
</#macro>

<#macro renderFormatListWrapperOpen formName style columnStyles formType="" attribs={} extraArgs...>
  <#-- SCIPIO: this may be called without a corresponding call to renderFormOpen, so may need to set form info here -->
  <#local formInfo = readRequestStack("htmlFormRenderFormStack")!{}>
  <#if !formInfo?has_content>
    <#local formInfo = { "name" : formName, "formType" : formType, "attribs":attribs, "setByListWrapper":true}>
    <#local dummy = pushRequestStack("htmlFormRenderFormStack", formInfo)>
  </#if>

  <#-- extra form attribs: <@objectAsScript lang="raw" escape=false object=attribs /> -->
  <#local styleSet = splitStyleNamesToSet(style)>
  <#-- SCIPIO: support setting and removing responsive/scrollable settings from widget table via style attribute -->
  <#local responsive = ""> <#-- SCIPIO: empty string means table type default takes effect -->
  <#if styleSet.contains("responsive")>
    <#local responsive = true>
    <#local style = removeStyleNames(style, "responsive")>
  <#elseif styleSet.contains("non-responsive")>
    <#local responsive = false>
    <#local style = removeStyleNames(style, "non-responsive")>
  </#if>
  <#local scrollable = "">
  <#if styleSet.contains("scrollable")>
    <#local scrollable = true>
    <#local style = removeStyleNames(style, "scrollable")>
  <#elseif styleSet.contains("non-scrollable")>
    <#local scrollable = false>
    <#local style = removeStyleNames(style, "non-scrollable")>
  </#if>
  <#-- SCIPIO: use @table macro to open -->
  <#if style?has_content>
    <#-- specified style will replace default class from @table (unless prefixed with "+" in widget defs) -->
    <#-- always added below <#local class = addClassArg(style, styles.table_formwidget_type!)>-->
    <#local class = style>
  <#else>
    <#-- with "+" (append only), default class will be selected by @table macro -->
    <#local class = "+${styles.table_formwidget!}">
  </#if>
  <#-- Always force add table_formwidget_type style because it's not part of table_formwidget because no one would ever remember to add the _type part on their forms and it makes sense -->
  <#local class = addClassArg(class, styles.table_formwidget_type!)>
  <#local tableType = mapOfbizFormTypeToTableType(formType)>
  <#-- table:
    type: ${escapeVal(tableType, 'html')}
    responsive: ${responsive?string} 
    scrollable: ${scrollable?string} -->
  <@table open=true close=false type=tableType class=class responsive=responsive scrollable=scrollable fixedColumnsLeft=(attribs.tableArgs.fixedColumnsLeft)!0 fixedColumnsRight=(attribs.tableArgs.fixedColumnsRight)!0 />
  <#local tableInfo = {
    "formName":formName, 
    "style":style, 
    "responsive":responsive, 
    "scrollable":scrollable,
    "tableId":getRequestVar("scipioCurrentTableInfo").id
  }>
  <#local dummy = pushRequestStack("renderFormatListWrapperStack", tableInfo)>
</#macro>

<#macro renderFormatListWrapperClose formName extraArgs...>
  <#local stackValues = popRequestStack("renderFormatListWrapperStack")!{}>
  <@table close=true open=false />
  <#-- save the table info for post-table stuff -->
  <#local dummy = setRequestVar("renderFormLastTableInfo", stackValues)>
  <#-- TABLE ID: ${escapeVal(stackValues.tableId, 'html')}, ${escapeVal(getRequestVar("scipioLastTableInfo").id, 'html')} -->
  <#-- SCIPIO: unset form info, but only if it was the list wrapper that set it -->
  <#local formInfo = readRequestStack("htmlFormRenderFormStack")!{}>
  <#if (formInfo.setByListWrapper!false) == true>
    <#local dummy = popRequestStack("htmlFormRenderFormStack")>
  </#if>
</#macro>

<#macro renderFormatHeaderRowOpen style extraArgs...>
<#-- SCIPIO: TODO: translate all thead/td/th/td/etc to @thead open/close
     I've done @thead because required by responsive tables at the moment -->
  <@thead open=true close=false />
    <tr class="<#if style?has_content>${escapeVal(style, 'html')}<#else>header-row</#if>">
</#macro>
<#macro renderFormatHeaderRowClose extraArgs...>
    </tr>
  <@thead close=true open=false />
</#macro>
<#macro renderFormatHeaderRowCellOpen style positionSpan inlineStyle="" extraArgs...>
  <#global renderFormatHeaderRowCellOpened = true>
  <th<#if positionSpan?has_content && (positionSpan > 1)> colspan="${positionSpan}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if><#if inlineStyle?has_content> style="${escapeVal(inlineStyle, 'html')}"</#if>>
</#macro>
<#macro renderFormatHeaderRowCellClose extraArgs...>
  </th>
  <#global renderFormatHeaderRowCellOpened = false>
</#macro>

<#macro renderFormatHeaderRowFormCellOpen style extraArgs...>
  <th<#if style?has_content> class="${escapeVal(style, 'html')}"</#if>>
</#macro>
<#macro renderFormatHeaderRowFormCellClose extraArgs...>
  </th>
</#macro>
<#macro renderFormatHeaderRowFormCellTitleSeparator style isLast extraArgs...>
  <#if style?has_content><span class="${escapeVal(style, 'html')}"></#if> - <#if style?has_content></span></#if>
</#macro>

<#macro renderFormatFooterRowOpen style extraArgs...>
<#-- SCIPIO: TODO: translate all tfoot/td/th/td/etc to @thead open/close -->
<tfoot>
  <tr class="<#if style?has_content>${escapeVal(style, 'html')}<#else>footer-row</#if>">
</#macro>
<#macro renderFormatFooterRowClose extraArgs...>
  </tr>
</tfoot>
</#macro>

<#macro renderFormatItemRowOpen formName itemIndex altRowStyles evenRowStyle oddRowStyle extraArgs...>
  <#-- SCIPIO: translate stock "alternate-row" odd-row-style to odd+even -->
  <#local oddRowStyleSet = splitStyleNamesToSet(oddRowStyle)>
  <#if oddRowStyleSet.contains("alternate-row")>
    <#local remOddRowStyles = removeStyleNames(oddRowStyle, "alternate-row")>
    <#local oddRowStyle = (styles.row_alt! + " " + remOddRowStyles)?trim>
    <#local evenRowStyle = (styles.row_reg! + " " + removeStyleNames(evenRowStyle, "even-row"))?trim>
  </#if>
  <tr<#if itemIndex?has_content><#if (itemIndex%2 == 0)><#if evenRowStyle?has_content> class="${escapeVal(evenRowStyle, 'html')}<#if altRowStyles?has_content> ${escapeVal(altRowStyles, 'html')}</#if>"<#elseif altRowStyles?has_content> class="${escapeVal(altRowStyles, 'html')}"</#if><#else><#if oddRowStyle?has_content> class="${escapeVal(oddRowStyle, 'html')}<#if altRowStyles?has_content> ${escapeVal(altRowStyles, 'html')}</#if>"<#elseif altRowStyles?has_content> class="${escapeVal(altRowStyles, 'html')}"</#if></#if></#if>>
</#macro>
<#macro renderFormatItemRowClose formName extraArgs...>
  </tr>
</#macro>
<#macro renderFormatItemRowCellOpen fieldName style positionSpan extraArgs...>
  <td<#if positionSpan?has_content && (positionSpan > 1)> colspan="${positionSpan}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if>>
</#macro>
<#macro renderFormatItemRowCellClose fieldName extraArgs...>
  </td>
</#macro>
<#macro renderFormatItemRowFormCellOpen style="" extraArgs...>
  <td<#if style?has_content> class="${escapeVal(style, 'html')}"</#if>>
</#macro>
<#macro renderFormatItemRowFormCellClose extraArgs...>
  </td>
</#macro>

<#macro renderFormatSingleWrapperOpen formName style="" extraArgs...>
  <#--<table cellspacing="0"<#if style?has_content> class="${escapeVal(style, 'html')}"</#if>>-->
</#macro>
<#macro renderFormatSingleWrapperClose formName extraArgs...>
  <#--</table>-->
</#macro>

<#macro renderFormatFieldRowOpen collapse=false style="" positions="" extraArgs...>
  <#global renderFormatFieldRow_gridUsed = 0>
  <@row open=true close=false class="+form-field-row" />
    <@cell open=true close=false class=style />
      <@row open=true close=false collapse=collapse />
</#macro>
<#macro renderFormatFieldRowClose extraArgs...>
      <@row close=true open=false />
    <@cell close=true open=false />
  <@row close=true open=false />
</#macro>

<#function isFieldTypeAction fieldType fieldTitleBlank>
    <#return (fieldType=="submit" || fieldType=="reset" || (fieldType=="hyperlink" && fieldTitleBlank))>
</#function>

<#macro renderFormatFieldRowTitleCellOpen style="" collapse=false positions="" position="" positionSpan="" nextPositionInRow="" lastPositionInRow="" fieldType="" fieldTitleBlank=false requiredField="" requiredStyle="" attribs={} extraArgs...>
  <#-- extra form field attribs: <@objectAsScript lang="raw" escape=false object=attribs /> -->
  <#-- SCIPIO: save common field info
      NOTE: because of the way these are organized, following macros may need to add extra info to this map. -->
  <#local htmlFormRenderFieldInfo = { "attribs":attribs }>
  <#local dummy = setRequestVar("htmlFormRenderFieldInfo", htmlFormRenderFieldInfo)> <#-- unset in renderFormatFieldRowWidgetCellClose -->
  <#global renderFormatFieldRowTitleCellOpened = true>
  <#global renderFieldTitleCurrentTitle = "">
  <#global renderFieldTitleCurrentTitleDetail = "">
  <#global renderFieldTitleCurrentForId = "">
  <#global renderFieldTitleCurrentFieldHelpText = "">
  <#global renderFieldTitleCurrentAreaStyle = style>
</#macro>
<#macro renderFormatFieldRowTitleCellClose collapse=false fieldType="" fieldTitleBlank=false extraArgs...>
  <#global renderFormatFieldRowTitleCellOpened = false>
</#macro>

<#macro renderFormatFieldRowSpacerCell extraArgs...></#macro>
<#macro renderFormatFieldRowWidgetCellOpen collapse=false positionSpan="" style="" positions="" position="" positionSpan="" nextPositionInRow="" lastPositionInRow="" fieldType="" fieldTitleBlank=false requiredField="" requiredStyle="" attribs={} extraArgs...>
  <#local isActionField = isFieldTypeAction(fieldType, fieldTitleBlank)>
  <#-- calculate position grid usage size for this field entry (recalc positionSpan ourselves) -->
  <#--positions: ${positions} position: ${position} positionSpan: ${positionSpan} nextPositionInRow: ${nextPositionInRow} lastPositionInRow: ${lastPositionInRow} -->
  <#local gridSize = 12>
  <#local markLast = false>
  <#local fieldEntryOffset = 0>
  <#local required = renderFieldIsRequired(requiredField, requiredStyle)>
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
  <#-- positions: ${positions} position: ${position} positionSpan: ${positionSpan} nextPositionInRow: ${nextPositionInRow} lastPositionInRow: ${lastPositionInRow} posSpan: ${posSpan} markLast: ${markLast?string}
       fieldEntryOffset: ${fieldEntryOffset}
       renderFormatFieldRow_gridUsed: ${renderFormatFieldRow_gridUsed}
       fieldEntrySize: ${fieldEntrySize} gridSize: ${gridSize} -->
  
  <#-- SCIPIO: widget-area-style now supports a more complex syntax similar to @heading, mainly to be able to add extra containers 
      e.g. widget-area-style="area-class;container:sub-div-class" -->
  <#local styleParts = style?split(";")>
  <#local extraContainerStyles = []>
  <#if (styleParts?size > 1)>
    <#local extraContainerStyles = styleParts[1..]>
    <#local style = styleParts[0]>
  </#if>
  <#global renderFormatFieldRowWidgetCellExtraContainerStyles = extraContainerStyles>
  
  <#local fieldEntryTypeClass = "field-entry-type-" + mapOfbizFieldTypeToStyleName(fieldType)>
  <#local outerClassDefault>${styles.grid_large!}${fieldEntrySize}<#if (fieldEntryOffset > 0)> ${styles.grid_large_offset!}${fieldEntryOffset}</#if></#local>
  <#local outerClass = ""><#-- can't specify for now -->
  <#if markLast> 
    <#local outerClass = addClassArg(outerClass, styles.grid_end!)>
  </#if>
  <#-- NOTE: using explicit version for compatibility! -->
  <#local outerClasses = compileClassArgExplicit(outerClass, outerClassDefault)>

  <#-- SCIPIO: TODO: form widgets currently only support left-position grid-like label arrangement; @field supports much more;
      not currently sure if easy way to reuse the stuff in @field here -->
  <@cell open=true close=false class=outerClasses />
    <@row open=true close=false class=("+form-field-entry " + rawString(fieldEntryTypeClass)) />
    
  <#-- SCIPIO: get estimate of the current absolute column widths (with all parent containers, as much as possible) -->
  <#local absColSizes = getAbsContainerSizeFactors()>
  <#-- Column size factors: <@objectAsScript lang="raw" escape=false object=absColSizes /> -->
  <#-- All parent/current col sizes: <@objectAsScript lang="raw" escape=false object=getAllContainerSizes()![] /> -->


  <#local defaultGridStyles = getDefaultFieldGridStyles({"labelArea":true, "labelInRow":true,
    "widgetPostfixCombined":true, "postfix":false, 
    "totalColumns": styles["fields_formwidget_totalcolumns"]!"",
    "labelColumns": styles["fields_formwidget_labelcolumns"]!""
  })>
  
  <#local isActionField = isFieldTypeAction(fieldType, fieldTitleBlank)>
  <#if !isActionField>
      <#local titleAreaClass = renderFieldTitleCurrentAreaStyle!>
      <#local titleAreaClass = addClassArg(titleAreaClass, "${styles.grid_cell!} field-entry-title ${rawString(fieldEntryTypeClass)}")>
      <#local titleAreaClassDefault = defaultGridStyles.labelArea>
      <#-- NOTE: using explicit version for compatibility! -->
      <div<@compiledClassAttribStrExplicit class=titleAreaClass defaultVal=titleAreaClassDefault />>
        <#-- TODO: currently not making use of:
          renderFieldTitleCurrentFieldHelpText
        -->
        <@field_markup_labelarea label=(renderFieldTitleCurrentTitle!"") labelDetail=(renderFieldTitleCurrentTitleDetail!"") 
          required=required 
          collapse=false fieldId=renderFieldTitleCurrentForId!""
          labelType="horizontal" labelPosition="left" 
          fieldType=mapOfbizFieldTypeToScipioFieldType(fieldType)
          origArgs={}/>
      </div>
  </#if>
  <#local innerClass = style>
  <#local innerClass = addClassArg(innerClass, "field-entry-widget ${rawString(fieldEntryTypeClass)}")>
  <#local isActionField = isFieldTypeAction(fieldType, fieldTitleBlank)>
  <#if !isActionField>
      <#local innerClassDefault = defaultGridStyles.widgetPostfixArea>
  <#else>
      <#-- SCIPIO: NOTE: This must be 12 hardcoded, NOT totalColumns -->
      <#local innerClassDefault>${styles.grid_small!}12 ${styles.grid_end!}</#local>
  </#if>
      <#-- NOTE: using explicit version for compatibility! -->
      <@cell open=true close=false class=compileClassArgExplicit(innerClass, innerClassDefault) />
        <#if extraContainerStyles?has_content>
          <#list extraContainerStyles as containerEntry>
            <#local parts = rawString(containerEntry)?trim?split(":")>
            <#local elem = parts[0]?trim>
            <#if elem?has_content>
              <#if elem == "container">
                <#local elem = "div">
              </#if>
              <${elem}<#if (parts?size > 1) && parts[1]?trim?has_content> class="${escapeVal(parts[1]?trim, 'html')}"</#if>>
            </#if>
          </#list>
        </#if>
</#macro>

<#macro renderFormatFieldRowWidgetCellClose fieldType="" fieldTitleBlank=false extraArgs...>
        <#if renderFormatFieldRowWidgetCellExtraContainerStyles?has_content>
          <#list renderFormatFieldRowWidgetCellExtraContainerStyles?reverse as containerEntry>
            <#local parts = containerEntry?trim?split(":")>
            <#local elem = parts[0]?trim>
            <#if elem?has_content>
              <#if elem == "container">
                <#local elem = "div">
              </#if>
              </${elem}>
            </#if>
          </#list>
        </#if>
      <@cell close=true open=false />
  <#local isActionField = isFieldTypeAction(fieldType, fieldTitleBlank)>
    <@row close=true open=false />
  <@cell close=true open=false />
  <#local dummy = setRequestVar("htmlFormRenderFieldInfo", {})>
</#macro>

<#-- SCIPIO: only render empty space if not running within title open section 
    2017-01-13: New role parameter (possible values: "", "field-title", ...) (DEV NOTE: acts as a surrogate to having to create a new macro for every purpose, like "renderEmptyFieldTitle") -->
<#assign rfes_roleOutMap = {"field-title-single":""}><#-- "field-title-single":"&nbsp;", "field-title-list":"&nbsp;", "field-title":"&nbsp;", ... -->
<#macro renderFormatEmptySpace role="" formType="" extraArgs...><#rt>
    <#local outStr = rfes_roleOutMap[role+"-"+formType]!rfes_roleOutMap[role]!"&nbsp;"><#t>
    <#if (renderFormatFieldRowTitleCellOpened!false) != true>${outStr}<#else><#global renderFieldTitleCurrentTitle = outStr></#if><#t>
</#macro>

<#macro renderTextFindField name value defaultOption opEquals opBeginsWith opContains opIsEmpty opNotEqual className alert size maxlength autocomplete titleStyle hideIgnoreCase ignCase ignoreCase title="" fieldType="" fieldTitleBlank=false hideOptions=false requiredField="" extraArgs...>
  <#-- delegate to scipio libs -->
  <@field_textfind_widget name=name value=value defaultOption=defaultOption opEquals=opEquals opBeginsWith=opBeginsWith opContains=opContains opIsEmpty=opIsEmpty opNotEqual=opNotEqual class=className alert=alert size=size maxlength=maxlength autocomplete=autocomplete titleClass=titleStyle hideIgnoreCase=hideIgnoreCase ignoreCase=ignCase ignoreCaseMsg=ignoreCase title=title fieldTitleBlank=fieldTitleBlank hideOptions=hideOptions required=renderFieldIsRequired(requiredField)/>
</#macro>

<#macro renderDateFindField className alert name localizedInputTitle value value2 size maxlength dateType formName defaultDateTimeString imgSrc localizedIconTitle titleStyle defaultOptionFrom defaultOptionThru opEquals opSameDay opGreaterThanFromDayStart opGreaterThan opGreaterThan opLessThan opUpToDay opUpThruDay opIsEmpty requiredField="" extraArgs...>
  <#-- delegate to scipio libs -->
  <@field_datefind_widget class=className alert=alert name=name localizedInputTitle=localizedInputTitle value=value value2=value2 size=size maxlength=maxlength dateType=dateType formName=formName defaultDateTimeString=defaultDateTimeString imgSrc=imgSrc localizedIconTitle=localizedIconTitle titleClass=titleStyle defaultOptionFrom=defaultOptionFrom defaultOptionThru=defaultOptionThru opEquals=opEquals opSameDay=opSameDay opGreaterThanFromDayStart=opGreaterThanFromDayStart opGreaterThan=opGreaterThan opGreaterThan=opGreaterThan opLessThan=opLessThan opUpToDay=opUpToDay opUpThruDay=opUpThruDay opIsEmpty=opIsEmpty required=renderFieldIsRequired(requiredField)/>
</#macro>

<#macro renderRangeFindField className alert name value size maxlength autocomplete titleStyle defaultOptionFrom opEquals opGreaterThan opGreaterThanEquals opLessThan opLessThanEquals value2 defaultOptionThru requiredField="" extraArgs...>
  <#-- delegate to scipio libs -->
  <@field_rangefind_widget class=className alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete titleClass=titleStyle defaultOptionFrom=defaultOptionFrom opEquals=opEquals opGreaterThan=opGreaterThan opGreaterThanEquals=opGreaterThanEquals opLessThan=opLessThan opLessThanEquals=opLessThanEquals value2=value2 defaultOptionThru=defaultOptionThru required=renderFieldIsRequired(requiredField)/>
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
<#macro renderLookupField name formName fieldFormName className="" alert="false" value="" size="" maxlength="" id="" event="" action="" readonly=false autocomplete="" descriptionFieldName="" targetParameterIter="" imgSrc="" ajaxUrl="" ajaxEnabled=javaScriptEnabled presentation="layer" width="" height="" position="" fadeBackground="true" clearText="" showDescription="" initiallyCollapsed="" lastViewName="main" title="" fieldType="" fieldTitleBlank=false tooltip="" requiredField="" extraArgs...>
  <#-- delegate to scipio libs -->
  <#if event?has_content>
    <#local events = {event:action}>
  <#else>
    <#local events = {}>
  </#if>
  <@field_lookup_widget name=name formName=formName fieldFormName=fieldFormName class=className alert=alert value=value 
    size=size maxlength=maxlength id=id events=events readonly=readonly autocomplete=autocomplete 
    descriptionFieldName=descriptionFieldName targetParameterIter=targetParameterIter imgSrc=imgSrc ajaxUrl=ajaxUrl 
    ajaxEnabled=ajaxEnabled presentation=presentation width=width height=height position=position fadeBackground=fadeBackground 
    clearText=clearText showDescription=showDescription initiallyCollapsed=initiallyCollapsed lastViewName=lastViewName 
    title=title fieldTitleBlank=fieldTitleBlank tooltip=tooltip required=renderFieldIsRequired(requiredField)/>
</#macro>

<#-- SCIPIO: new params: paginate, forcePost, viewIndexFirst, listItemsOnly, paginateToggle*
     paginate is a display hint, does not seem to mean guarantee data wasn't paginated -->
<#macro renderNextPrev paginateStyle paginateFirstStyle viewIndex highIndex listSize viewSize ajaxEnabled javaScriptEnabled ajaxFirstUrl firstUrl paginateFirstLabel paginatePreviousStyle ajaxPreviousUrl previousUrl paginatePreviousLabel pageLabel ajaxSelectUrl selectUrl ajaxSelectSizeUrl selectSizeUrl commonDisplaying paginateNextStyle ajaxNextUrl nextUrl paginateNextLabel paginateLastStyle ajaxLastUrl lastUrl paginateLastLabel paginateViewSizeLabel paginate=true forcePost=false viewIndexFirst=0 listItemsOnly=false paginateToggle=false paginateOn=true ajaxPaginateOnUrl="" paginateOnUrl="" paginateOnStyle="" paginateOnLabel="" ajaxPaginateOffUrl="" paginateOffUrl="" paginateOffStyle="" paginateOffLabel="" lowIndex=0 realHighIndex=-1 position="" extraArgs...>
  <#-- delegate to scipio libs -->
  <#-- don't pass commonDisplaying - let paginate markup use its default message and showCount default -->
  <#local countMsg = "">
  <@paginate_core paginateClass=paginateStyle paginateFirstClass=paginateFirstStyle viewIndex=viewIndex lowIndex=lowIndex highIndex=highIndex realHighIndex=realHighIndex listSize=listSize viewSize=viewSize ajaxEnabled=ajaxEnabled javaScriptEnabled=javaScriptEnabled ajaxFirstUrl=ajaxFirstUrl firstUrl=firstUrl paginateFirstLabel=paginateFirstLabel paginatePreviousClass=paginatePreviousStyle ajaxPreviousUrl=ajaxPreviousUrl previousUrl=previousUrl paginatePreviousLabel=paginatePreviousLabel pageLabel=pageLabel ajaxSelectUrl=ajaxSelectUrl selectUrl=selectUrl ajaxSelectSizeUrl=ajaxSelectSizeUrl selectSizeUrl=selectSizeUrl countMsg=countMsg paginateNextClass=paginateNextStyle ajaxNextUrl=ajaxNextUrl nextUrl=nextUrl 
    paginateNextLabel=paginateNextLabel paginateLastClass=paginateLastStyle ajaxLastUrl=ajaxLastUrl lastUrl=lastUrl paginateLastLabel=paginateLastLabel paginateViewSizeLabel=paginateViewSizeLabel enabled=paginate forcePost=forcePost viewIndexFirst=viewIndexFirst listItemsOnly=listItemsOnly paginateToggle=paginateToggle paginateOn=paginateOn ajaxPaginateOnUrl=ajaxPaginateOnUrl paginateOnUrl=paginateOnUrl paginateOnClass=paginateOnStyle paginateOnLabel=paginateOnLabel ajaxPaginateOffUrl=ajaxPaginateOffUrl paginateOffUrl=paginateOffUrl paginateOffClass=paginateOffStyle paginateOffLabel=paginateOffLabel position=position/>
</#macro>

<#macro renderFileField className alert name value size maxlength autocomplete id="" title="" fieldType="" fieldTitleBlank=false requiredField="" extraArgs...>
  <#-- delegate to scipio libs -->
  <@field_file_widget class=className alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete id=id title=title fieldTitleBlank=fieldTitleBlank required=renderFieldIsRequired(requiredField)/>
</#macro>
<#macro renderPasswordField className alert name value size maxlength id autocomplete title="" fieldType="" fieldTitleBlank=false requiredField="" extraArgs...>
  <#-- delegate to scipio libs -->
  <@field_password_widget class=className alert=alert name=name value=value size=size maxlength=maxlength id=id autocomplete=autocomplete title=title fieldTitleBlank=fieldTitleBlank required=renderFieldIsRequired(requiredField)/>
</#macro>
<#macro renderImageField value description alternate style event action title="" fieldType="" fieldTitleBlank=false extraArgs...>
  <img<#if value?has_content> src="${escapeFullUrl(value, 'html')}"</#if><#if description?has_content> title="${escapeVal(description, 'html')}"</#if> alt="<#if alternate?has_content>${escapeVal(alternate, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if><#if event?has_content> ${escapeVal(event, 'html')}="${escapeVal(action, 'html')}" </#if>/>
</#macro>

<#macro renderBanner style leftStyle rightStyle leftText text rightText extraArgs...>
  <table width="100%">
    <tr><#rt/>
      <#if leftText?has_content><td align="left"><#if leftStyle?has_content><div class="${escapeVal(leftStyle, 'html')}"></#if>${escapeVal(leftText, 'htmlmarkup')}<#if leftStyle?has_content></div></#if></td><#rt/></#if>
      <#if text?has_content><td align="center"><#if style?has_content><div class="${escapeVal(style, 'html')}"></#if>${escapeVal(text, 'htmlmarkup')}<#if style?has_content></div></#if></td><#rt/></#if>
      <#if rightText?has_content><td align="right"><#if rightStyle?has_content><div class="${escapeVal(rightStyle, 'html')}"></#if>${escapeVal(rightText, 'htmlmarkup')}<#if rightStyle?has_content></div></#if></td><#rt/></#if>
    </tr>
  </table>
</#macro>

<#macro renderContainerField id className extraArgs...><div id="${escapeVal(id, 'html')}" class="${escapeVal(className, 'html')}"></div></#macro>

<#macro renderFieldGroupOpen style id title collapsed collapsibleAreaId expandToolTip collapseToolTip collapsible extraArgs...>
    <#-- delegate to scipio libs -->
    <@fieldset_core open=true close=false class=style id=id title=title collapsed=collapsed collapsibleAreaId=collapsibleAreaId expandToolTip=expandToolTip collapseToolTip=collapseToolTip collapsible=collapsible />
</#macro>

<#macro renderFieldGroupClose style id title extraArgs...>
    <@fieldset_core close=true open=false class=style id=id title=title />
</#macro>

<#macro renderHyperlinkTitle name title showSelectAll="N" extraArgs...>
  <#-- SCIPIO: only render immediately if not falling within title open/close -->
  <#local titleDetail>
    <#if showSelectAll="Y"><input type="checkbox" name="selectAll" value="Y" onclick="javascript:toggleAll(this, '${escapeVal(name, 'js-html')}');"/></#if>
  </#local>
  <#if (renderFormatFieldRowTitleCellOpened!false) != true>
    <#if title?has_content>${escapeVal(title, 'htmlmarkup')}<br /></#if>
    ${titleDetail}
  <#else>
    <#--<#global renderFieldTitleCurrentTitle = content>-->
    <#global renderFieldTitleCurrentTitle = title>
    <#global renderFieldTitleCurrentTitleDetail = titleDetail>
  </#if>
</#macro>

<#macro renderSortField style title linkUrl ajaxEnabled tooltip="" extraArgs...>
  <a<#if style?has_content> class="${escapeVal(style, 'html')}"</#if> href="<#if ajaxEnabled?has_content && ajaxEnabled>javascript:ajaxUpdateAreas('${escapeFullUrl(linkUrl, 'js-html')}')<#else>${escapeFullUrl(linkUrl, 'html')}</#if>"<#if tooltip?has_content> title="${escapeVal(tooltip, 'html')}"</#if>>${escapeVal(title, 'htmlmarkup')}</a>
</#macro>

<#macro formatBoundaryComment boundaryType widgetType widgetName><!-- ${escapeVal(boundaryType, 'html')}  ${escapeVal(widgetType, 'html')}  ${escapeVal(widgetName, 'html')} --></#macro>

<#macro renderTooltip tooltip tooltipStyle extraArgs...>
  <#-- SCIPIO: DEPRECATED: this macro will be phased out in favor of tooltip= parameter. -->
  <#if tooltip?has_content><span class="<#if tooltipStyle?has_content>${escapeVal(tooltipStyle, 'html')}<#else>tooltip</#if>">${escapeVal(tooltip, 'htmlmarkup')}</span><#rt/></#if>
</#macro>

<#macro renderClass className alert="false" extraArgs...>
  <#if className?has_content || alert?string == "true"> class="${escapeVal(className, 'html')}<#if alert?string == "true"> alert</#if>" </#if>
</#macro>

<#-- SCIPIO: new macro to isolate this code -->
<#macro renderAsterisksCommon requiredField requiredStyle extraArgs...>
  <#-- SCIPIO: 2016-10-10: the asterix logic is delegated to Scipio macros, so this is counterproductive
  <#if requiredField?string == "true"><#if !requiredStyle?has_content><span class="form-field-input-asterisk">*</span></#if></#if>
  -->
  <#if requiredField?string == "true"><span class="form-field-input-asterisk">*</span></#if>
</#macro>

<#-- SCIPIO: function to isolate this if-required logic -->
<#function renderFieldIsRequired requiredField requiredStyle="">
  <#-- SCIPIO: 2016-10-10: the asterix logic is delegated to Scipio macros, so this is counterproductive
  <#return requiredField?string == "true" && !requiredStyle?has_content>-->
  <#return requiredField?string == "true">
</#function>

<#macro renderAsterisks requiredField requiredStyle extraArgs...>
  <#-- SCIPIO: don't run this here anymore; see widget cell open
  <@renderAsterisksCommon requiredField=requiredField requiredStyle=requiredStyle /> -->
</#macro>

<#macro makeHiddenFormLinkForm actionUrl name parameters targetWindow>
  <form method="post" action="${escapeFullUrl(actionUrl, 'html')}"<#if targetWindow?has_content> target="${escapeVal(targetWindow, 'html')}"</#if> onsubmit="javascript:submitFormDisableSubmits(this)" name="${escapeVal(name, 'html')}">
    <#list parameters as parameter>
      <input name="${escapeVal(parameter.name, 'html')}" value="${escapeVal(parameter.value, 'html')}" type="hidden"/>
    </#list>
  </form>
</#macro>
<#macro makeHiddenFormLinkAnchor linkStyle hiddenFormName event action imgSrc description confirmation>
  <a<#if linkStyle?has_content> class="${escapeVal(linkStyle, 'html')}"</#if> href="javascript:document['${escapeVal(hiddenFormName, 'js-html')}'].submit()"
    <#if action?has_content && event?has_content> ${escapeVal(event, 'html')}="${escapeVal(action, 'html')}"</#if>
    <#if confirmation?has_content> onclick="return confirm('${escapeVal(confirmation, 'js-html')}')"</#if>>
      <#if imgSrc?has_content><img src="${escapeFullUrl(imgSrc, 'html')}" alt=""/></#if>${escapeVal(description, 'htmlmarkup')}</a>
</#macro>
<#macro makeHyperlinkString linkStyle hiddenFormName event action imgSrc title alternate linkUrl targetWindow description confirmation>
    <a<#if linkStyle?has_content> class="${escapeVal(linkStyle, 'html')}"</#if> 
      href="${escapeFullUrl(linkUrl, 'html')}"<#if targetWindow?has_content> target="${escapeVal(targetWindow, 'html')}"</#if>
      <#if action?has_content && event?has_content> ${escapeVal(event, 'html')}="${escapeVal(action, 'html')}"</#if>
      <#if confirmation?has_content> onclick="return confirm('${escapeVal(confirmation, 'js-html')}')"</#if>
      <#if imgSrc?length == 0 && title?has_content> title="${escapeVal(title, 'html')}"</#if>>
        <#if imgSrc?has_content><img src="${escapeFullUrl(imgSrc, 'html')}" alt="${escapeVal(alternate, 'html')}" title="${escapeVal(title, 'html')}"/></#if>${escapeVal(description, 'htmlmarkup')}</a>
</#macro>

<#macro renderAlternateText className text wrapperOpened headerRendered numOfColumns extraArgs...>
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

<#-- SCIPIO: new: renders a submit form after table, for list/multi forms -->
<#macro renderSubmitForm hiddenFormName="" formName="" formType="" targetUrl="" targetWindow="" 
    params={} useRowSubmit=false useMasterSubmitField=false submitEntries=[] extraArgs...>
  <#local params = toSimpleMap(params)>
  <#-- NOTE: if (useRowSubmit==false && useMasterSubmitField==true), we can basically skip this entire macro,
    but there's no harm going through this in case we need the hook -->
  <#--<#if !(!useRowSubmit && useMasterSubmitField)>-->
  
  <#-- NOTE: escaping must be done by the macro on this one. is a glimpse of the future. -->
  <#local tableId = (getRequestVar("renderFormLastTableInfo").tableId)!"_TABLE_ID_NOT_FOUND_">
  
  <#if (useRowSubmit || useMasterSubmitField) && submitEntries?has_content>
    <@script>
        jQuery(document).ready(function() {
          <#if useRowSubmit>
            var submitForm = $('form[name="${escapeVal(hiddenFormName, 'js')}"]');
          <#else>
            var submitForm = $('form[name="${escapeVal(formName, 'js')}"]');
          </#if>
            
            if (submitForm) {
              <#list submitEntries as submitEntry>
                <#local submitFieldNameJs = escapeVal(submitEntry.submitFieldName, 'js')>
                <#local submitFieldIdJs = escapeVal(submitEntry.submitFieldId, 'js')>
               
                var submitField = $("#${submitFieldIdJs}");
                $(submitField).click(function(e) {
                    e.preventDefault();
                  <#if useRowSubmit>
                    <#local selectFieldNamePrefixJs = escapeVal(submitEntry.selectFieldNamePrefix, 'js')><#-- selectAction -->
                    var checked = false;
              
                    $("#${escapeVal(tableId, 'js')}").find("input[type=radio][name^=${selectFieldNamePrefixJs}],"+ 
                        "input[type=checkbox][name^=${selectFieldNamePrefixJs}]").each(function (j, r) {

                        if ($(r).is(":checked")) {
                            checked = true;
                            
                            <#-- makeHiddenFieldsForHiddenForm -->
                            $(this).closest("tr").find("input[type=text], input[type=hidden], input[type=radio],"+ 
                                    "input[type=checkbox], select, textarea").each(function (i, e) {
                                if ($(submitForm).find("input[name=" + $(e).attr("name") + "]").length <= 0) {
                                    var hiddenField = $("<input></input>")
                                    $(hiddenField).attr("type", "hidden");
                                    $(hiddenField).attr("name", $(e).attr("name"));
                                    $(hiddenField).attr("value", $(e).val());
                                    $(submitForm).append($(hiddenField));
                                }
                            });  
                        }
                    });
                    if (checked) {
                        submitForm.submit();
                    } else {
                        alert("${escapeVal(uiLabelMap.CommonNoRowSelected, 'js')}");
                    }
                  <#else>
                    submitForm.submit();
                  </#if>
                });
              </#list>
            } else {
                return false;
            }
        });
    </@script>
  <#elseif submitEntries?has_content>
    <@script>
        jQuery(document).ready(function() {
            var submitForm = $('form[name="${escapeVal(hiddenFormName, 'js')}"]');
            if (submitForm) {
              <#list submitEntries as submitEntry>
                <#local submitFieldNameJs = escapeVal(submitEntry.submitFieldName, 'js')>
                <#local submitFieldIdJs = escapeVal(submitEntry.submitFieldId, 'js')>
   
                var id = $("[id^=${submitFieldIdJs}]");
                $(id).click(function(e) {
                    e.preventDefault();

                    <#-- makeHiddenFieldsForHiddenForm -->
                    $(this).closest("tr").find("input[type=text], input[type=hidden], input[type=radio],"+ 
                            "input[type=checkbox], select, textarea").each(function (i, e) {
                        if ($(submitForm).find("input[name=" + $(e).attr("name") + "]").length <= 0) {
                            var hiddenField = $("<input></input>")
                            $(hiddenField).attr("type", "hidden");
                            $(hiddenField).attr("name", $(e).attr("name"));
                            $(hiddenField).attr("value", $(e).val());
                            $(submitForm).append($(hiddenField));
                        }
                    });
                        
                    submitForm.submit();
                });
              </#list>
            } else {
                return false;
            }
        });
    </@script>
  </#if>
  <#if submitEntries?has_content && (useRowSubmit || !useMasterSubmitField)>
  <form method="post" action="${escapeFullUrl(targetUrl, 'html')}"<#if targetWindow?has_content> target="${escapeVal(targetWindow, 'html')}"</#if><#rt/>
    <#lt/> onsubmit="javascript:submitFormDisableSubmits(this);" name="${escapeVal(hiddenFormName, 'html')}">
    <#list params?keys as paramName>
      <input type="hidden" name="${escapeVal(paramName, 'html')}" value="${escapeVal(params[rawString(paramName)], 'html')}" />
    </#list>
    <#if useRowSubmit>
      <input type="hidden" name="_useRowSubmit" value="Y"/>
    </#if>
  </form>
  </#if>
  <#--</#if>-->
</#macro>

<#-- SCIPIO: 2017-04-24: new -->
<#macro renderFormPageScripts pageScripts=[] extraArgs...>
  <@script>
    <#list pageScripts as pageScript>
      ${rawString(pageScript)}
    </#list>
  </@script>
</#macro>
