<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if techDataCalendar?has_content>
<#assign sectionTitle>${rawLabel('ManufacturingEditCalendarExceptionWeekFor')} 
    <#if (techDataCalendar.description)?has_content>"${raw(techDataCalendar.get("description",locale))}"</#if>
    [${raw(techDataCalendar.calendarId!)}]</#assign>
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