<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#if techDataCalendar?has_content>
<#assign sectionTitle>${rawLabel('ManufacturingEditCalendarExceptionWeekFor')} 
    <#if (techDataCalendar.description)?has_content>"${rawString(techDataCalendar.get("description",locale))}"</#if>
    [${rawString(techDataCalendar.calendarId!)}]</#assign>
<@section title=sectionTitle>
    ${listCalendarExceptionWeekWrapper.renderFormString(context)}
</@section>
  <#if calendarExceptionWeek?has_content>
    <@section title=uiLabelMap.PageTitleEditCalendarExceptionWeek>
        ${updateCalendarExceptionWeekWrapper.renderFormString(context)}
    </@section>
  </#if>
    <@section title=uiLabelMap.PageTitleAddCalendarExceptionWeek>
        ${addCalendarExceptionWeekWrapper.renderFormString(context)}
    </@section>
</#if>