<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#--
To use these macros in your template, insert the following line in
your template file:
<#include "component://common/webcommon/includes/commonMacros.ftl"/>
-->

<#assign
  dayValueList = Static["org.ofbiz.service.calendar.ExpressionUiHelper"].getDayValueList(locale)
  monthValueList = Static["org.ofbiz.service.calendar.ExpressionUiHelper"].getMonthValueList(locale)
/>

<#macro NullMacro></#macro>

<#macro DateField formName="" fieldName="" fieldValue="" fieldClass="">
  <@fields type="default-manual-widgetonly">
    <#if javaScriptEnabled>
      <@field type="datetime" required=containsStyleName(fieldClass, "required") name=fieldName event=(event!) action=(action!) class="+${fieldClass}" value=fieldValue!'' size="25" maxlength="30" id="${rawString(fieldName)}1" />
    <#else>
      <@field type="input" required=containsStyleName(fieldClass, "required") name=fieldName value=(fieldValue!) class="+${fieldClass}" maxlength="25" size="25"/>
    </#if>
  </@fields>
</#macro>

<#macro MonthField fieldName="" fieldValue=-1 fieldClass="">
  <@fields type="default-manual-widgetonly">
    <@field type="select" name=fieldName class="+${fieldClass}" required=containsStyleName(fieldClass, "required")>
      <#list monthValueList as monthValue>
        <option value="${monthValue.value}"<#if monthValue.value == fieldValue> selected="selected"</#if>>${monthValue.description}</option>
      </#list>
    </@field>
  </@fields>
</#macro>

<#macro HourOfDayField fieldName="" fieldValue=-1 fieldClass="">
  <@fields type="default-manual-widgetonly">
    <@field type="select" name=fieldName class="+${fieldClass}" required=containsStyleName(fieldClass, "required")>
      <#list 0..23 as i>
        <option value="${i}"<#if i == fieldValue> selected="selected"</#if>>${i}</option>
      </#list>
    </@field>
  </@fields>
</#macro>

<#macro MinuteField fieldName="" fieldValue=-1 fieldClass="">
  <@fields type="default-manual-widgetonly">
    <@field type="select" name=fieldName class="+${fieldClass}" required=containsStyleName(fieldClass, "required")>
      <#list 0..59 as i>
        <option value="${i}"<#if i == fieldValue> selected="selected"</#if>>${i}</option>
      </#list>
    </@field>
  </@fields>
</#macro>

<#macro DayOfWeekField fieldName="" fieldValue=-1 fieldClass="">
  <@fields type="default-manual-widgetonly">
    <@field type="select" name=fieldName class="+${fieldClass}" required=containsStyleName(fieldClass, "required")>
      <#list dayValueList as dayValue>
        <option value="${dayValue.value}"<#if dayValue.value == fieldValue> selected="selected"</#if>>${dayValue.description}</option>
      </#list>
    </@field>
  </@fields>
</#macro>

<#macro DayOfMonthField fieldName="" fieldValue=-1 fieldClass="">
  <@fields type="default-manual-widgetonly">
    <@field type="select" name=fieldName class="+${fieldClass}" required=containsStyleName(fieldClass, "required")>
      <#list 1..31 as i>
        <option value="${i}"<#if i == fieldValue> selected="selected"</#if>>${i}</option>
      </#list>
    </@field>
  </@fields>
</#macro>

<#macro fieldErrors fieldName>
  <#if errorMessageList?has_content>
    <#assign fieldMessages = Static["org.ofbiz.base.util.MessageString"].getMessagesForField(fieldName, true, errorMessageList)>
    <ul>
      <#list fieldMessages as errorMsg>
        <li class="errorMessage">${errorMsg}</li>
      </#list>
    </ul>
  </#if>
</#macro>

<#macro fieldErrorsMulti fieldName1 fieldName2 fieldName3 fieldName4>
  <#if errorMessageList?has_content>
    <#assign fieldMessages = Static["org.ofbiz.base.util.MessageString"].getMessagesForField(fieldName1, fieldName2, fieldName3, fieldName4, true, errorMessageList)>
    <ul>
      <#list fieldMessages as errorMsg>
        <li class="errorMessage">${errorMsg}</li>
      </#list>
    </ul>
  </#if>
</#macro>
