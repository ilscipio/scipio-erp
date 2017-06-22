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

<#macro renderField text extraArgs...><#if text??>"${text}"</#if></#macro>

<#macro renderDisplayField type imageLocation idName description class alert inPlaceEditorId="" inPlaceEditorUrl="" inPlaceEditorParams="" extraArgs...>
<@renderField description />
</#macro>
<#macro renderHyperlinkField extraArgs...></#macro>

<#macro renderTextField name className alert value textSize maxlength id event action disabled clientAutocomplete ajaxUrl ajaxEnabled tooltip="" extraArgs...><@renderField value /></#macro>

<#macro renderTextareaField name className alert cols rows id readonly value visualEditorEnable language="" buttons="" tooltip="" title="" fieldType="" fieldTitleBlank=false collapse=false maxlength="" extraArgs...><@renderField value /></#macro>

<#macro renderDateTimeField name className alert title value size maxlength id event action dateType shortDateInput timeDropdownParamName defaultDateTimeString localizedIconTitle timeDropdown timeHourName classString hour1 hour2 timeMinutesName minutes isTwelveHour ampmName amSelected pmSelected compositeType formName extraArgs...><@renderField value /></#macro>

<#macro renderDropDownField name className alert id multiple formName otherFieldName event action size firstInList currentValue explicitDescription allowEmpty options fieldName otherFieldName otherValue otherFieldSize dDFCurrent ajaxEnabled noCurrentSelectedKey ajaxOptions frequency minChars choices autoSelect partialSearch partialChars ignoreCase fullSearch extraArgs...>
<#if currentValue?has_content && firstInList?has_content>
<@renderField explicitDescription />
<#else>
<#list options as item>
<@renderField item.description />
</#list>
</#if>
</#macro>

<#macro renderTooltip tooltip tooltipStyle extraArgs...></#macro>
<#macro renderCheckField items className alert id allChecked currentValue name event action fieldType="" fieldTitleBlank=false extraArgs...></#macro>
<#macro renderRadioField items className alert currentValue noCurrentSelectedKey name event action fieldType="" fieldTitleBlank=false extraArgs...></#macro>

<#macro renderSubmitField buttonType className alert formName name event action imgSrc confirmation containerId ajaxUrl title fieldType="" fieldTitleBlank=false showProgress="" href="" onClick="" inputType="" disabled=false id="" extraArgs...></#macro>
<#macro renderResetField className alert name title extraArgs...></#macro>

<#macro renderHiddenField name value id event action extraArgs...></#macro>
<#macro renderIgnoredField extraArgs...></#macro>

<#macro renderFieldTitle style title id fieldHelpText="" for="" extraArgs...><@renderField title /></#macro>
<#macro renderSingleFormFieldTitle extraArgs...></#macro>

<#macro renderFormOpen linkUrl formType targetWindow containerId containerStyle autocomplete name useRowSubmit attribs={} method="" extraArgs...></#macro>
<#macro renderFormClose focusFieldName formName containerId hasRequiredField extraArgs...></#macro>
<#macro renderMultiFormClose extraArgs...></#macro>

<#macro renderFormatListWrapperOpen formName style columnStyles formType="" attribs={} extraArgs...></#macro>
<#macro renderFormatListWrapperClose formName extraArgs...></#macro>

<#macro renderFormatHeaderRowOpen style extraArgs...></#macro>
<#macro renderFormatHeaderRowClose extraArgs...>

</#macro>
<#macro renderFormatHeaderRowCellOpen style positionSpan extraArgs...></#macro>
<#macro renderFormatHeaderRowCellClose extraArgs...></#macro>

<#macro renderFormatHeaderRowFormCellOpen style extraArgs...> </#macro>
<#macro renderFormatHeaderRowFormCellClose extraArgs...></#macro>
<#macro renderFormatHeaderRowFormCellTitleSeparator style isLast extraArgs...></#macro>

<#macro renderFormatFooterRowOpen style extraArgs...></#macro>
<#macro renderFormatFooterRowClose extraArgs...></#macro>

<#macro renderFormatItemRowOpen formName itemIndex altRowStyles evenRowStyle oddRowStyle extraArgs...></#macro>
<#macro renderFormatItemRowClose formName extraArgs...>

</#macro>
<#macro renderFormatItemRowCellOpen fieldName style positionSpan extraArgs...></#macro>
<#macro renderFormatItemRowCellClose fieldName extraArgs...></#macro>
<#macro renderFormatItemRowFormCellOpen style extraArgs...></#macro>
<#macro renderFormatItemRowFormCellClose extraArgs...></#macro>

<#macro renderFormatSingleWrapperOpen formName style extraArgs...></#macro>
<#macro renderFormatSingleWrapperClose formName extraArgs...>
</#macro>

<#macro renderFormatFieldRowOpen positions="" extraArgs...></#macro>
<#macro renderFormatFieldRowClose extraArgs...>
</#macro>
<#macro renderFormatFieldRowTitleCellOpen style positions="" position="" positionSpan="" nextPositionInRow="" lastPositionInRow="" fieldType="" fieldTitleBlank=false requiredField="" requiredStyle="" attribs={} extraArgs...> </#macro>
<#macro renderFormatFieldRowTitleCellClose fieldType="" fieldTitleBlank=false extraArgs...></#macro>
<#macro renderFormatFieldRowSpacerCell extraArgs...></#macro>
<#macro renderFormatFieldRowWidgetCellOpen positionSpan style positions="" position="" nextPositionInRow="" lastPositionInRow="" fieldType="" fieldTitleBlank=false requiredField="" requiredStyle="" attribs={} extraArgs...></#macro>
<#macro renderFormatFieldRowWidgetCellClose fieldType="" fieldTitleBlank=false extraArgs...></#macro>

<#macro renderFormatEmptySpace extraArgs...>&nbsp;</#macro>

<#macro renderTextFindField name value defaultOption opEquals opBeginsWith opContains opIsEmpty opNotEqual className alert size maxlength autocomplete titleStyle hideIgnoreCase ignCase ignoreCase hideOptions=false extraArgs...><@renderField value /></#macro>

<#macro renderDateFindField className alert name localizedInputTitle value size maxlength dateType formName defaultDateTimeString imgSrc localizedIconTitle titleStyle defaultOptionFrom defaultOptionThru opEquals opSameDay opGreaterThanFromDayStart opGreaterThan opGreaterThan opLessThan opUpToDay opUpThruDay opIsEmpty extraArgs...><@renderField value /></#macro>

<#macro renderRangeFindField className alert name value size maxlength autocomplete titleStyle defaultOptionFrom opEquals opGreaterThan opGreaterThanEquals opLessThan opLessThanEquals value2 defaultOptionThru extraArgs...>
<@renderField value />
</#macro>

<#macro renderLookupField className alert name value size maxlength id event action readonly autocomplete descriptionFieldName formName fieldFormName targetParameterIter imgSrc ajaxUrl ajaxEnabled presentation width height position fadeBackground clearText showDescription initiallyCollapsed lastViewName="main" title="" fieldType="" fieldTitleBlank=false tooltip="" extraArgs...><@renderField value /></#macro>
<#macro renderNextPrev paginateStyle paginateFirstStyle viewIndex highIndex listSize viewSize ajaxEnabled javaScriptEnabled ajaxFirstUrl firstUrl paginateFirstLabel paginatePreviousStyle ajaxPreviousUrl previousUrl paginatePreviousLabel pageLabel ajaxSelectUrl selectUrl ajaxSelectSizeUrl selectSizeUrl commonDisplaying paginateNextStyle ajaxNextUrl nextUrl paginateNextLabel paginateLastStyle ajaxLastUrl lastUrl paginateLastLabel paginateViewSizeLabel paginate=true lowIndex=0 realHighIndex=-1 position="" extraArgs...></#macro>
<#macro renderFileField className alert name value size maxlength autocomplete extraArgs...><@renderField value /></#macro>
<#macro renderPasswordField className alert name value size maxlength id autocomplete extraArgs...></#macro>
<#macro renderImageFiel value border width height event action extraArgs...></#macro>
<#macro renderBanner style leftStyle rightStyle leftText text rightText extraArgs...></#macro>
<#macro renderFieldGroupOpen style id title collapsed collapsibleAreaId collapsible expandToolTip collapseToolTip extraArgs...></#macro>
<#macro renderFieldGroupClose style id title extraArgs...></#macro>

<#macro renderHyperlinkTitle name title showSelectAll="N" extraArgs...></#macro>
<#macro renderSortField style title linkUrl ajaxEnabled tooltip="" extraArgs...><@renderFieldTitle style title /></#macro>
<#macro formatBoundaryComment boundaryType widgetType widgetName></#macro>
<#macro makeHiddenFormLinkAnchor linkStyle hiddenFormName event action imgSrc description><@renderField description /></#macro>
<#macro makeHyperlinkString linkStyle hiddenFormName event action imgSrc alternate linkUrl targetWindow description><@renderField description /></#macro>

<#macro renderAlternateText className text wrapperOpened headerRendered numOfColumns extraArgs...></#macro>

<#-- SCIPIO: new: renders a submit form after table, for list/multi forms -->
<#macro renderSubmitForm extraArgs...></#macro>

<#-- SCIPIO: 2017-04-24: new -->
<#macro renderFormPageScripts pageScripts=[] extraArgs...></#macro>
