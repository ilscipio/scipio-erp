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
<#macro getFoStyle style>
    <#assign foStyles = {
        "listtitlestyle":"font-weight=\"bold\" text-align=\"center\" border=\"solid black\" padding=\"2pt\"",
        "tabletext":"border-left=\"solid black\" border-right=\"solid black\" padding-left=\"2pt\" padding-top=\"2pt\"",
        "tabletextright":"border-left=\"solid black\" border-right=\"solid black\" padding-left=\"2pt\" padding-top=\"2pt\" text-align=\"right\"",
        "tableheadverysmall":"column-width=\"0.3in\"",
        "tableheadsmall":"column-width=\"0.5in\"",
        "tableheadmedium":"column-width=\"1.5in\"",
        "tableheadwide":"column-width=\"3in\"",
        "tableheadhuge":"column-width=\"5in\"",
        "head1":"font-size=\"12\" font-weight=\"bold\"",
        "head2":"font-weight=\"bold\"",
        "head3":"font-weight=\"bold\" font-style=\"italic\"",
        "h1":"font-size=\"12\" font-weight=\"bold\"",
        "h2":"font-weight=\"bold\"",
        "h3":"font-weight=\"bold\" font-style=\"italic\"",
        "error":"color=\"red\""}/>
    <#list style?split(' ') as styleItem>
        <#assign foStyle = foStyles[styleItem]!""/>
        ${foStyle!""}
    </#list>
</#macro>

<#escape x as x?xml>

<#macro makeBlock style text><fo:block<#if style?has_content> <@getFoStyle style/></#if>><#if text??>${text}</#if></fo:block></#macro>

<#macro renderField text extraArgs...><#if text??>${text}</#if></#macro>

<#macro renderDisplayField type imageLocation idName description title class alert inPlaceEditorId="" inPlaceEditorUrl="" inPlaceEditorParams="" extraArgs...>
<@makeBlock class description />
</#macro>
<#macro renderHyperlinkField extraArgs...></#macro>

<#macro renderTextField name className alert value textSize maxlength id event action disabled clientAutocomplete ajaxUrl ajaxEnabled mask placeholder="" tooltip="" extraArgs...><@makeBlock className value /></#macro>

<#macro renderTextareaField name className alert cols rows id readonly value visualEditorEnable language="" buttons="" tooltip="" title="" fieldType="" fieldTitleBlank=false collapse=false maxlength="" extraArgs...><@makeBlock className value /></#macro>

<#macro renderDateTimeField name className alert title value size maxlength step timeValues id event action dateType shortDateInput timeDropdownParamName defaultDateTimeString localizedIconTitle timeDropdown timeHourName classString hour1 hour2 timeMinutesName minutes isTwelveHour ampmName amSelected pmSelected compositeType formName mask extraArgs...><@makeBlock className value /></#macro>

<#macro renderDropDownField name className alert id multiple formName otherFieldName event action size firstInList currentValue explicitDescription allowEmpty options fieldName otherFieldName otherValue otherFieldSize dDFCurrent ajaxEnabled noCurrentSelectedKey ajaxOptions frequency minChars choices autoSelect partialSearch partialChars ignoreCase fullSearch extraArgs...>
<#if currentValue?has_content && firstInList?has_content>
<@makeBlock "" explicitDescription />
<#else>
<#list options as item>
<@makeBlock "" item.description />
</#list>
</#if>
</#macro>

<#macro renderCheckField items className alert id allChecked currentValue name event action fieldType="" fieldTitleBlank=false extraArgs...><@makeBlock "" "" /></#macro>
<#macro renderRadioField items className alert currentValue noCurrentSelectedKey name event action fieldType="" fieldTitleBlank=false extraArgs...><@makeBlock "" "" /></#macro>

<#macro renderSubmitField buttonType className alert formName name event action imgSrc confirmation containerId ajaxUrl title fieldType="" fieldTitleBlank=false showProgress="" href="" onClick="" inputType="" disabled=false id="" extraArgs...><@makeBlock "" "" /></#macro>
<#macro renderResetField className alert name title extraArgs...><@makeBlock "" "" /></#macro>

<#macro renderHiddenField name value id event action extraArgs...></#macro>
<#macro renderIgnoredField extraArgs...></#macro>

<#macro renderFieldTitle style title id fieldHelpText="" for="" extraArgs...>${(title!"")?replace("&nbsp;", " ")}</#macro>
<#macro renderSingleFormFieldTitle title extraArgs...>${title!""}</#macro>
    
<#macro renderFormOpen linkUrl formType targetWindow containerId containerStyle autocomplete name viewIndexField viewSizeField viewIndex viewSize useRowSubmit attribs={} method="" extraArgs...></#macro>
<#macro renderFormClose focusFieldName formName containerId hasRequiredField extraArgs...></#macro>
<#macro renderMultiFormClose extraArgs...></#macro>

<#macro renderFormatListWrapperOpen formName style columnStyles formType="" attribs={} extraArgs...><fo:table border="solid black"><#list columnStyles as columnStyle><fo:table-column<#if columnStyle?has_content> <@getFoStyle columnStyle/></#if>/></#list></#macro>
<#macro renderFormatListWrapperClose formName extraArgs...><fo:table-row><fo:table-cell><fo:block/></fo:table-cell></fo:table-row></fo:table-body></fo:table></#macro>

<#macro renderFormatHeaderRowOpen style extraArgs...><fo:table-header><fo:table-row></#macro>
<#macro renderFormatHeaderRowClose extraArgs...></fo:table-row></fo:table-header><fo:table-body>
<#-- FIXME: this is an hack to avoid FOP rendering errors for empty lists (fo:table-body cannot be null) -->
<fo:table-row><fo:table-cell><fo:block/></fo:table-cell></fo:table-row>
</#macro>
<#macro renderFormatHeaderRowCellOpen style positionSpan extraArgs...><fo:table-cell <#if positionSpan?has_content && positionSpan gt 1 >number-columns-spanned="${positionSpan}"</#if><@getFoStyle "listtitlestyle"/>><fo:block></#macro>
<#macro renderFormatHeaderRowCellClose extraArgs...></fo:block></fo:table-cell></#macro>

<#macro renderFormatHeaderRowFormCellOpen style extraArgs...></#macro>
<#macro renderFormatHeaderRowFormCellClose extraArgs...></#macro>
<#macro renderFormatHeaderRowFormCellTitleSeparator style isLast extraArgs...></#macro>

<#macro renderFormatFooterRowOpen style extraArgs...></#macro>
<#macro renderFormatFooterRowClose extraArgs...></#macro>
    
<#macro renderFormatItemRowOpen formName itemIndex altRowStyles evenRowStyle oddRowStyle extraArgs...><fo:table-row></#macro>
<#macro renderFormatItemRowClose formName extraArgs...></fo:table-row></#macro>
<#macro renderFormatItemRowCellOpen fieldName style positionSpan extraArgs...><fo:table-cell <#if positionSpan?has_content && positionSpan gt 1 >number-columns-spanned="${positionSpan}"</#if><#if style?has_content><@getFoStyle style/><#else><@getFoStyle "tabletext"/></#if>><fo:block></#macro>
<#macro renderFormatItemRowCellClose fieldName extraArgs...></fo:block></fo:table-cell></#macro>
<#macro renderFormatItemRowFormCellOpen style extraArgs...></#macro>
<#macro renderFormatItemRowFormCellClose extraArgs...></#macro>

<#macro renderFormatSingleWrapperOpen formName style extraArgs...><fo:table><fo:table-column column-width="1.75in"/><fo:table-column column-width="1.75in"/><fo:table-column column-width="1.75in"/><fo:table-column column-width="1.75in"/><fo:table-body></#macro>
<#macro renderFormatSingleWrapperClose formName extraArgs...></fo:table-body></fo:table></#macro>

<#macro renderFormatFieldRowOpen positions="" extraArgs...><fo:table-row></#macro>
<#macro renderFormatFieldRowClose extraArgs...></fo:table-row></#macro>
<#macro renderFormatFieldRowTitleCellOpen style positions="" position="" positionSpan="" nextPositionInRow="" lastPositionInRow="" fieldType="" fieldTitleBlank=false requiredField="" requiredStyle="" attribs={} extraArgs...><fo:table-cell font-weight="bold" text-align="right" padding="3pt"><fo:block></#macro>
<#macro renderFormatFieldRowTitleCellClose fieldType="" fieldTitleBlank=false extraArgs...></fo:block></fo:table-cell></#macro>
<#macro renderFormatFieldRowSpacerCell extraArgs...></#macro>
<#macro renderFormatFieldRowWidgetCellOpen positionSpan style positions="" position="" nextPositionInRow="" lastPositionInRow="" fieldType="" fieldTitleBlank=false requiredField="" requiredStyle="" attribs={} extraArgs...><fo:table-cell text-align="left" padding="2pt" padding-left="5pt" <#if positionSpan?has_content && positionSpan gt 1 >number-columns-spanned="${positionSpan}"</#if>><fo:block></#macro>
<#macro renderFormatFieldRowWidgetCellClose fieldType="" fieldTitleBlank=false extraArgs...></fo:block></fo:table-cell></#macro>

<#macro renderFormatEmptySpace extraArgs...> <@makeBlock "" " " /><!--space--></#macro>

<#macro renderTextFindField name value defaultOption opEquals opBeginsWith opContains opIsEmpty opNotEqual className alert size maxlength autocomplete titleStyle hideIgnoreCase ignCase ignoreCase hideOptions=false extraArgs...><@makeBlock className value /></#macro>

<#macro renderDateFindField className alert name localizedInputTitle value size maxlength dateType formName defaultDateTimeString imgSrc localizedIconTitle titleStyle defaultOptionFrom defaultOptionThru opEquals opSameDay opGreaterThanFromDayStart opGreaterThan opGreaterThan opLessThan opUpToDay opUpThruDay opIsEmpty extraArgs...><@makeBlock className value /></#macro>

<#macro renderRangeFindField className alert name value size maxlength autocomplete titleStyle defaultOptionFrom opEquals opGreaterThan opGreaterThanEquals opLessThan opLessThanEquals value2 defaultOptionThru extraArgs...>
<@makeBlock className value />
</#macro>

<#macro renderLookupField className alert name value size maxlength id event action readonly autocomplete descriptionFieldName formName fieldFormName targetParameterIter imgSrc ajaxUrl ajaxEnabled presentation width height position fadeBackground clearText showDescription initiallyCollapsed lastViewName="main" title="" fieldType="" fieldTitleBlank=false tooltip="" extraArgs...></#macro>
<#macro renderNextPrev paginateStyle paginateFirstStyle viewIndex highIndex listSize viewSize ajaxEnabled javaScriptEnabled ajaxFirstUrl firstUrl paginateFirstLabel paginatePreviousStyle ajaxPreviousUrl previousUrl paginatePreviousLabel pageLabel ajaxSelectUrl selectUrl ajaxSelectSizeUrl selectSizeUrl commonDisplaying paginateNextStyle ajaxNextUrl nextUrl paginateNextLabel paginateLastStyle ajaxLastUrl lastUrl paginateLastLabel paginateViewSizeLabel paginate=true lowIndex=0 realHighIndex=-1 position="" extraArgs...></#macro>
<#macro renderFileField className alert name value size maxlength autocomplete extraArgs...><@makeBlock className value /></#macro>
<#macro renderPasswordField className alert name value size maxlength id autocomplete extraArgs...><@makeBlock className "" /></#macro>
<#macro renderImageField value description alternate border width height event action extraArgs...><@makeBlock "" "" /></#macro>
<#macro renderBanner style leftStyle rightStyle leftText text rightText extraArgs...><@makeBlock "" "" /></#macro>
<#macro renderFieldGroupOpen style id title collapsed collapsibleAreaId collapsible expandToolTip collapseToolTip extraArgs...></#macro>
<#macro renderFieldGroupClose style id title extraArgs...></#macro>

<#macro renderHyperlinkTitle name title showSelectAll="N" extraArgs...></#macro>
<#macro renderSortField style title linkUrl ajaxEnabled tooltip="" extraArgs...><@renderFieldTitle style title /></#macro>
<#macro formatBoundaryComment boundaryType widgetType widgetName></#macro>
<#macro makeHiddenFormLinkAnchor linkStyle hiddenFormName event action imgSrc description><@renderField description /></#macro>
<#macro makeHyperlinkString linkStyle hiddenFormName event action imgSrc title alternate linkUrl targetWindow description confirmation><@makeBlock linkStyle description /></#macro>
<#macro renderTooltip tooltip tooltipStyle extraArgs...></#macro>
<#macro renderAsterisks requiredField requiredStyle extraArgs...></#macro>

<#macro renderAlternateText className text wrapperOpened headerRendered numOfColumns extraArgs...>
    <#if wrapperOpened>
      <fo:table-row>
        <fo:table-cell <#if (numOfColumns > 1)>number-columns-spanned="${numOfColumns}"</#if>>
    </#if>
        <@makeBlock style=className text=text />
    <#if wrapperOpened>
        </fo:table-cell>
      </fo:table-row>
    </#if>
</#macro>
</#escape>

<#-- SCIPIO: new: renders a submit form after table, for list/multi forms -->
<#macro renderSubmitForm extraArgs...></#macro>

<#-- SCIPIO: 2017-04-24: new -->
<#macro renderFormPageScripts pageScripts=[] extraArgs...></#macro>
