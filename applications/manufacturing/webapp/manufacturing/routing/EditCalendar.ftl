<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if techDataCalendar?has_content>
    <@menuitem type="link" href=makeOfbizUrl("EditCalendar") text=uiLabelMap.ManufacturingNewCalendar class="+${styles.action_nav!} ${styles.action_add!}" />
  </#if>
  </@menu>
</#macro>
<@section menuContent=menuContent>

<#if techDataCalendar?has_content>
  <#assign formActionUrl><@ofbizUrl escapeAs='html'>UpdateCalendar</@ofbizUrl></#assign>
<#else>
  <#assign formActionUrl><@ofbizUrl escapeAs='html'>CreateCalendar</@ofbizUrl></#assign>
</#if>

  <form name="calendarform" method="post" action="${formActionUrl}">

  <#if techDataCalendar?has_content>
    <input type="hidden" name="calendarId" value="${techDataCalendar.calendarId}" />
  </#if>

  <#if techDataCalendar?has_content>
    <@field type="display" label=uiLabelMap.ManufacturingCalendarId tooltip="(${rawLabel('CommonNotModifRecreat')})" value=(techDataCalendar.calendarId!) />
  <#else>
    <@field type="input" required=true label=uiLabelMap.ManufacturingCalendarId size="12" name="calendarId" value=(calendarData.calendarId!) />
  </#if>
    <@field type="input" label=uiLabelMap.CommonDescription size="40" name="description" value=(calendarData.description!) />
    <@field type="select" required=true label=uiLabelMap.ManufacturingCalendarWeekId name="calendarWeekId">
      <#list calendarWeeks as calendarWeek>
        <option value="${calendarWeek.calendarWeekId}" <#if calendarData?has_content && (calendarData.calendarWeekId!"") == calendarWeek.calendarWeekId>selected="selected"</#if>>${(calendarWeek.get("description",locale))!}</option>
      </#list>
    </@field>
    <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}"/>

  </form>
</@section>