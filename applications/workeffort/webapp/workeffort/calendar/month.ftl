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
<#include 'calendarcommon.ftl'>

<#-- SCIPIO:
<#assign styleTdVal = "height: 8em; width: 10em; vertical-align: top; padding: 0.5em;">-->

<#-- SCIPIO: FTL now includes the title -->
<#macro menuContent menuArgs={}>
    <@calendarDateSwitcher period="month"/>
</#macro>
<@section title=Static['org.ofbiz.base.util.UtilDateTime'].timeStampToString(start, 'MMMM yyyy', timeZone, locale)
    menuContent=menuContent menuLayoutTitle="inline-title"> <#--${uiLabelMap.WorkEffortMonthView}: -->

<#if periods?has_content>
  <#-- Allow containing screens to specify the URL for creating a new event -->
  <#if !newCalEventUrl??>
    <#assign newCalEventUrl = parameters._LAST_VIEW_NAME_>
  </#if>

<#assign calendarIdNum = getRequestVar("monthCalendarIdNum")!0>
<#assign calendarIdNum = calendarIdNum + 1>
<#assign dummy = setRequestVar("monthCalendarIdNum", calendarIdNum)!>

<#-- not using scrollable=true, should have better resizing method-->
<div class="month-calendar-full">
<@table type="data-complex" autoAltRows=true class="+calendar"
    responsive=false><#--responsiveOptions={"ordering":false}--><#-- orig: class="basic-table calendar" --> <#-- orig: cellspacing="0" -->
  <@thead>
  <@tr class="header-row">
    <@th class="+month-header-week">&nbsp;</@th>
    <#list periods as day>
      <@th class="+${styles.text_center} month-header-day">${day.start?date?string("EEEE")?cap_first}</@th>
      <#if (day_index > 5)><#break></#if>
    </#list>
  </@tr>
  </@thead>
<#-- SCIPIO: this var is a workaround to collect the inner content and append at the end to avoid influence of table -->
<#assign periodDetailContentAll = "">  
  <#list periods as period>
    <#assign periodUniqueId = + calendarIdNum + "_" + (period_index+1)>
    <#assign currentPeriod = false/>
    <#if (nowTimestamp >= period.start) && (nowTimestamp <= period.end)><#assign currentPeriod = true/></#if>
    <#assign indexMod7 = period_index % 7>
    <#if indexMod7 == 0>
      <#-- FIXME: rearrange without open/close -->
      <@tr open=true close=false />
        <@td class="+month-entry month-entry-week">
        <div class="month-entry-wrapper">
        <div class="month-entry-abs">
        <div class="month-entry-content">
          <a href="<@ofbizUrl>${parameters._LAST_VIEW_NAME_}?period=week&amp;startTime=${period.start.time?string("#")}${urlParam!}${addlParam!}</@ofbizUrl>" class="${styles.link_nav_info_desc!}">${uiLabelMap.CommonWeek} ${period.start?date?string("w")}</a>
        </div>
        </div>
        </div>
        </@td>
    </#if>
    <#assign class><#if currentPeriod>current-period<#else><#if (period.calendarEntries?size > 0)>active-period</#if></#if></#assign>
      <#if start?date?string("MM") == period.start?date?string("MM")>
        <#assign class=addClassArg(class, 'current-month')/> 
      <#else>
        <#assign class=addClassArg(class, 'other-month')/>
      </#if>
      
      <#if period=="month">${start?date?string("MM")}</#if>
      <#assign calEntryContentAll = "">

      <#assign maxNumberOfPersons = 0/>
      <#assign maxNumberOfEvents = 0/>
      <#assign ranges = period.calendarEntriesByDateRange.keySet()/>
      <#list ranges as range>
          <#assign eventsInRange = period.calendarEntriesByDateRange.get(range)/>
          <#assign numberOfPersons = 0/>
          <#list eventsInRange as eventInRange>
              <#assign numberOfPersons = numberOfPersons + eventInRange.workEffort.reservPersons!0/>
          </#list>
          <#if (numberOfPersons > maxNumberOfPersons)>
              <#assign maxNumberOfPersons = numberOfPersons/>
          </#if>
          <#if (eventsInRange.size() > maxNumberOfEvents)>
              <#assign maxNumberOfEvents = eventsInRange.size()/>
          </#if>
      </#list>

    <#assign dayHasContent = (maxNumberOfPersons > 0) || (maxNumberOfEvents > 0) ||
        ((parameters.hideEvents!"") != "Y" && period.calendarEntries?has_content)>

    <#if dayHasContent>
      <#-- SCIPIO: dropdown tooltip-like functionality
          FIXME: macro-ify this foundation-only dropdown code somehow 
          NOTE: if you need to assign attribs to a div instead, use @commonElemAttribStr maybe or straight @elemAttribStr -->
      <#assign periodDetailTriggerAttribs = {"data-dropdown":"perioddetail_${periodUniqueId}", "aria-controls":"perioddetail_${periodUniqueId}", "aria-expanded":"false", "data-options":"is_hover:true; hover_timeout:1000"}>
    <#else>
      <#assign periodDetailTriggerAttribs = {}>
    </#if>
    <@td class=(class+" month-entry month-entry-day") attribs=periodDetailTriggerAttribs>
    <div class="month-entry-wrapper">
    <div class="month-entry-abs">
    <div class="month-entry-content">
      <span class="month-entry-content-title"><a href="<@ofbizUrl>${parameters._LAST_VIEW_NAME_}?period=day&amp;startTime=${period.start.time?string("#")}${urlParam!}${addlParam!}</@ofbizUrl>">${period.start?date?string("d")?cap_first}<#if period.start?date?string("d") == "1"> ${period.start?date?string("MMMM")}</#if></a></span>
      <span class="month-entry-content-add"><a class="add-new ${styles.link_nav_inline!} ${styles.action_add!}" href="<@ofbizUrl>${newCalEventUrl}?period=month&amp;form=edit&amp;startTime=${parameters.start!}&amp;parentTypeId=${parentTypeId!}&amp;currentStatusId=CAL_TENTATIVE&amp;estimatedStartDate=${period.start?string("yyyy-MM-dd HH:mm:ss")}&amp;estimatedCompletionDate=${period.end?string("yyyy-MM-dd HH:mm:ss")}${urlParam!}${addlParam!}</@ofbizUrl>">[+]</a><#--${uiLabelMap.CommonAddNew}--></span>
      <br/>

      <#if (parameters.hideEvents!"") != "Y">
      <#list period.calendarEntries as calEntry>
        <#if calEntry.workEffort.actualStartDate??>
            <#assign startDate = calEntry.workEffort.actualStartDate>
          <#else>
            <#assign startDate = calEntry.workEffort.estimatedStartDate!>
        </#if>

        <#if calEntry.workEffort.actualCompletionDate??>
            <#assign completionDate = calEntry.workEffort.actualCompletionDate>
          <#else>
            <#assign completionDate = calEntry.workEffort.estimatedCompletionDate!>
        </#if>

        <#if !completionDate?has_content && calEntry.workEffort.actualMilliSeconds?has_content>
            <#assign completionDate =  calEntry.workEffort.actualStartDate + calEntry.workEffort.actualMilliSeconds>
        </#if>    
        <#if !completionDate?has_content && calEntry.workEffort.estimatedMilliSeconds?has_content>
            <#assign completionDate =  calEntry.workEffort.estimatedStartDate + calEntry.workEffort.estimatedMilliSeconds>
        </#if>

      <#-- SCIPIO: capture this to include a second copy in dropdown -->
      <#assign calEntryContentStart>
        <#--<#if (calEntry_index > 0)>  
          <hr />
        </#if>-->
          
        <span class="day-event-time">
        <#if (startDate.compareTo(period.start) <= 0 && completionDate?has_content && completionDate.compareTo(period.end) >= 0)>
          ${uiLabelMap.CommonAllDay}
        <#elseif startDate.before(period.start) && completionDate?has_content>
          ${uiLabelMap.CommonUntil} ${completionDate?time?string.short}
        <#elseif !completionDate?has_content>
          ${uiLabelMap.CommonFrom} ${startDate?time?string.short} - ?
        <#elseif completionDate.after(period.end)>
          ${uiLabelMap.CommonFrom} ${startDate?time?string.short}
        <#else>
          ${startDate?time?string.short}-${completionDate?time?string.short}
        </#if>
        </span>
      </#assign>
        <div class="month-entry-content-event">  
          ${calEntryContentStart}
          <br />
          <@render resource="component://workeffort/widget/CalendarScreens.xml#calendarEventContent" 
            reqAttribs={"periodType":"month", "workEffortId":calEntry.workEffort.workEffortId,
                "calEventVerbose":false} 
            restoreValues=true asString=true/>
        </div>
      <#assign calEntryContent>
        <div class="month-entry-content-event">  
          ${calEntryContentStart}
          <br />
          <@render resource="component://workeffort/widget/CalendarScreens.xml#calendarEventContent" 
            reqAttribs={"periodType":"month", "workEffortId":calEntry.workEffort.workEffortId,
                "calEventVerbose":true} 
            restoreValues=true asString=true/>
        </div>
      </#assign>
        <#assign calEntryContentAll = calEntryContentAll + calEntryContent>
        
      </#list>
      </#if>

    
    <#if (maxNumberOfEvents > 0) || (maxNumberOfPersons > 0)>
      <#assign numEventsContent>
        <#--<hr/>--><#--<br/>-->
        <div class="day-entry-stats">
        <#if (maxNumberOfEvents > 0)>
          ${uiLabelMap.WorkEffortMaxEvents}: ${maxNumberOfEvents}<br/>
        </#if>
        <#if (maxNumberOfPersons > 0)>
          ${uiLabelMap.WorkEffortMaxPersons}: ${maxNumberOfPersons}<br/>
        </#if>
        </div>
      </#assign>
    <#else>
      <#assign numEventsContent = "">
    </#if>
     <#-- SCIPIO: Disabled for now  ${numEventsContent}
      -->
    </div>
    </div>
    </div>
    </@td>

    <#-- SCIPIO: dropdown tooltip-like functionality
        FIXME: macro-ify this foundation-only dropdown code somehow -->
    <#if calEntryContentAll?has_content || numEventsContent?has_content>
      <#assign periodDetailContent>
        <div id="perioddetail_${periodUniqueId}" data-dropdown-content class="f-dropdown content small" aria-hidden="true" tabindex="-1">
          ${calEntryContentAll}
          ${numEventsContent}
        </div>
      </#assign>
    <#else>
      <#assign periodDetailContent = "">
    </#if>
    <#assign periodDetailContentAll = periodDetailContentAll + periodDetailContent>


<#--
    <@td valign="top">
      <@table type="fields" width="100%"> <#- orig: cellspacing="0" -> <#- orig: cellpadding="0" -> <#- orig: border="0" ->
        <@tr>
          <@td nowrap="nowrap" class="monthdaynumber"><a href="<@ofbizUrl>day?startTime=${period.start.time?string("#")}<#if eventsParam?has_content>&amp;${eventsParam}</#if>${addlParam!}</@ofbizUrl>"  class="${styles.link_nav_info_number!} monthdaynumber">${period.start?date?string("d")?cap_first}</a></@td>
          <@td align="right"><a href="<@ofbizUrl>EditWorkEffort?workEffortTypeId=EVENT&amp;currentStatusId=CAL_TENTATIVE&amp;estimatedStartDate=${period.start?string("yyyy-MM-dd HH:mm:ss")}&amp;estimatedCompletionDate=${period.end?string("yyyy-MM-dd HH:mm:ss")}${addlParam!}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_add!}">${uiLabelMap.CommonAddNew}</a>&nbsp;&nbsp;</@td>
        </@tr>
      </@table>
      <#list period.calendarEntries as calEntry>
      <@table type="fields" width="100%"> <#- orig: cellspacing="0" -> <#- orig: cellpadding="0" -> <#- orig: border="0" ->
        <@tr width="100%">
          <@td class='monthcalendarentry' width="100%" valign='top'>
            <#if (calEntry.workEffort.estimatedStartDate.compareTo(period.start)  <= 0 && calEntry.workEffort.estimatedCompletionDate.compareTo(period.end) >= 0)>
              ${uiLabelMap.CommonAllDay}
            <#elseif calEntry.workEffort.estimatedStartDate.before(period.start)>
              ${uiLabelMap.CommonUntil} ${calEntry.workEffort.estimatedCompletionDate?time?string.short}
            <#elseif calEntry.workEffort.estimatedCompletionDate.after(period.end)>
              ${uiLabelMap.CommonFrom} ${calEntry.workEffort.estimatedStartDate?time?string.short}
            <#else>
              ${calEntry.workEffort.estimatedStartDate?time?string.short}-${calEntry.workEffort.estimatedCompletionDate?time?string.short}
            </#if>
            <br />
            <a href="<@ofbizUrl>WorkEffortSummary?workEffortId=${calEntry.workEffort.workEffortId}${addlParam!}</@ofbizUrl>" class="event">${calEntry.workEffort.workEffortName!"Undefined"}</a>&nbsp;
          </@td>
        </@tr>
      </@table>
      </#list>
    </@td>
-->
    <#if !period_has_next && indexMod7 != 6>
    <@td colspan=(6 - indexMod7)>&nbsp;</@td>
    </#if>
  <#if indexMod7 == 6 || !period_has_next>
  <#-- SCIPIO: FIXME: don't want open/close -->
  <@tr close=true open=false />
  </#if>
  </#list>
</@table>

  <div class="cal-detail-content">
    ${periodDetailContentAll}
  </div>
</div>

<#else>
  <@commonMsg type="result-norecord">${uiLabelMap.WorkEffortFailedCalendarEntries}</@commonMsg>
</#if>

</@section>
