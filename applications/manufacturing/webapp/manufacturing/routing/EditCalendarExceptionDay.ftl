<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#if techDataCalendar?has_content>
<#assign sectionTitle>${rawLabel('ManufacturingEditCalendarExceptionDayFor')} 
    <#if (techDataCalendar.description)?has_content>"${rawString(techDataCalendar.get("description",locale))}"</#if>
    [${rawString(techDataCalendar.calendarId!)}]</#assign>
<@section title=sectionTitle>
    ${listCalendarExceptionDayWrapper.renderFormString(context)}
</@section>
<#if calendarExceptionDay?has_content>
<@section title=uiLabelMap.PageTitleEditCalendarExceptionWeek>
    ${updateCalendarExceptionDayWrapper.renderFormString(context)}
</@section>
</#if>
<@section title=uiLabelMap.PageTitleAddCalendarExceptionWeek>
    ${addCalendarExceptionDayWrapper.renderFormString(context)}
</@section>
</#if>