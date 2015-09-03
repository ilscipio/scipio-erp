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

<@section title="${uiLabelMap.AccountingEditCustomTimePeriods}">

<#if security.hasPermission("PERIOD_MAINT", session)>
  <@section title="${uiLabelMap.AccountingShowOnlyPeriodsWithOrganization}">
     <form method="post" action="<@ofbizUrl>EditCustomTimePeriod</@ofbizUrl>" name="setOrganizationPartyIdForm">
         <input type="hidden" name="currentCustomTimePeriodId" value="${currentCustomTimePeriodId!}" />
         <span>${uiLabelMap.AccountingShowOnlyPeriodsWithOrganization}</span>
         <input type="text" size="20" name="findOrganizationPartyId" value="${findOrganizationPartyId!}" />
         <input type="submit" value='${uiLabelMap.CommonUpdate}' />
     </form>
  </@section>


  <#if currentCustomTimePeriod?has_content>
    <#assign menuHtml>
      <@menu type="section" inlineItems=true>
      <li><a href="<@ofbizUrl>EditCustomTimePeriod?findOrganizationPartyId=${findOrganizationPartyId!}</@ofbizUrl>">${uiLabelMap.CommonClearCurrent}</a></li>
      </@menu>
    </#assign>
  <#else>
    <#assign menuHtml>
      <@menu type="section" inlineItems=true>  </@menu>
</#assign>
  </#if>
  <@section title="${uiLabelMap.AccountingCurrentCustomTimePeriod}" menuHtml=menuHtml>
    <#if currentCustomTimePeriod?has_content>
        <form method="post" action="<@ofbizUrl>updateCustomTimePeriod</@ofbizUrl>" name="updateCustomTimePeriodForm">
          <input type="hidden" name="findOrganizationPartyId" value="${findOrganizationPartyId!}" />
          <input type="hidden" name="customTimePeriodId" value="${currentCustomTimePeriodId!}" />
      <@table type="data-list" class="basic-table">
       <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.CommonId}</@th>
          <@th>${uiLabelMap.CommonParent}</@th>
          <@th>${uiLabelMap.AccountingOrgPartyId}</@th>
          <@th>${uiLabelMap.AccountingPeriodType}</@th>
          <@th>${uiLabelMap.CommonNbr}</@th>
          <@th>${uiLabelMap.AccountingPeriodName}</@th>
          <@th>${uiLabelMap.CommonFromDate}</@th>
          <@th>${uiLabelMap.CommonThruDate}</@th>
          <@th>&nbsp;</@th>
        </@tr>
        </@thead>
          <@tr>
            <@td>${currentCustomTimePeriod.customTimePeriodId}</@td>
            <@td>
              <select name="parentPeriodId">
                <option value=''>&nbsp;</option>
                <#list allCustomTimePeriods as allCustomTimePeriod>
                  <#assign allPeriodType = allCustomTimePeriod.getRelatedOne("PeriodType", true)>
                  <#assign isDefault = false>
                  <#if (currentCustomTimePeriod.parentPeriodId)??>
                    <#if currentCustomTimePeriod.customTimePeriodId = allCustomTimePeriod.customTimePeriodId>
                      <#assign isDefault = true>
                    </#if>
                  </#if>
                  <option value='${allCustomTimePeriod.customTimePeriodId}'<#if isDefault> selected="selected"</#if>>
                    ${allCustomTimePeriod.organizationPartyId}
                    <#if allPeriodType??>${allPeriodType.description}:</#if>
                    ${allCustomTimePeriod.periodNum!}
                    [${allCustomTimePeriod.customTimePeriodId}]
                  </option>
                </#list>
              </select>
              <#if (currentCustomTimePeriod.parentPeriodId)??>
                <a href='<@ofbizUrl>EditCustomTimePeriod?currentCustomTimePeriodId=${currentCustomTimePeriod.parentPeriodId}&amp;findOrganizationPartyId=${findOrganizationPartyId!}</@ofbizUrl>'>
                ${uiLabelMap.CommonSetAsCurrent}</a>
              </#if>
            </@td>
            <@td><input type="text" size='12' name="currentCustomTimePeriod" value="${currentCustomTimePeriod.organizationPartyId!}" /></@td>
            <@td>
              <select name="periodTypeId">
                <#list periodTypes as periodType>
                  <#assign isDefault = false>
                  <#if (currentCustomTimePeriod.periodTypeId)??>
                    <#if currentCustomTimePeriod.periodTypeId = periodType.periodTypeId>
                      <#assign isDefault = true>
                    </#if>
                  </#if>
                  <option value='${periodType.periodTypeId}'<#if isDefault> selected="selected"</#if>>
                    ${periodType.description} [${periodType.periodTypeId}]
                  </option>
                </#list>
              </select>
            </@td>
            <@td><input type="text" size='4' name="periodNum" value="${currentCustomTimePeriod.periodNum!}" /></@td>
            <@td><input type="text" size='10' name="periodName" value="${currentCustomTimePeriod.periodName!}" /></@td>
            <@td>
              <#assign hasntStarted = false>
              <#assign compareDate = currentCustomTimePeriod.getDate("fromDate")>
              <#if compareDate?has_content>
                <#if nowTimestamp.before(compareDate)><#assign hasntStarted = true></#if>
              </#if>
              <input type="text" size='13' name="fromDate" value="${currentCustomTimePeriod.fromDate?string("yyyy-MM-dd")}"<#if hasntStarted> class="alert"</#if> />
            </@td>
            <@td>
              <#assign hasExpired = false>
              <#assign compareDate = currentCustomTimePeriod.getDate("thruDate")>
              <#if compareDate?has_content>
                <#if nowTimestamp.after(compareDate)><#assign hasExpired = true></#if>
              </#if>
              <input type="text" size='13' name="thruDate" value="${currentCustomTimePeriod.thruDate?string("yyyy-MM-dd")}"<#if hasntStarted> class="alert"</#if> />
            </@td>
            <@td class="button-col">
              <input type="submit" value='${uiLabelMap.CommonUpdate}'/>
              <a href='<@ofbizUrl>deleteCustomTimePeriod?customTimePeriodId=${currentCustomTimePeriod.customTimePeriodId}</@ofbizUrl>'>
              ${uiLabelMap.CommonDelete}</a>
            </@td>
          </@tr>
      </@table>
        </form>
    <#else>
      <@resultMsg>${uiLabelMap.AccountingNoCurrentCustomTimePeriodSelected}</@resultMsg>>
    </#if>
  </@section>
  
  <@section title="${uiLabelMap.AccountingChildPeriods}">
    <#if customTimePeriods?has_content>
      <@table type="data-list" class="basic-table">
       <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.CommonId}</@th>
          <@th>${uiLabelMap.CommonParent}</@th>
          <@th>${uiLabelMap.AccountingOrgPartyId}</@th>
          <@th>${uiLabelMap.AccountingPeriodType}</@th>
          <@th>${uiLabelMap.CommonNbr}</@th>
          <@th>${uiLabelMap.AccountingPeriodName}</@th>
          <@th>${uiLabelMap.CommonFromDate}</@th>
          <@th>${uiLabelMap.CommonThruDate}</@th>
          <@th>&nbsp;</@th>
        </@tr>
        </@thead>
        <#assign line = 0>
        <#list customTimePeriods as customTimePeriod>
          <#assign line = line + 1>
          <#assign periodType = customTimePeriod.getRelatedOne("PeriodType", true)>
          <@tr>
            <form method="post" action='<@ofbizUrl>updateCustomTimePeriod</@ofbizUrl>' name='lineForm${line}'>
              <input type="hidden" name="customTimePeriodId" value="${customTimePeriod.customTimePeriodId!}" />
            <@td>${customTimePeriod.customTimePeriodId}</@td>
            <@td>
              <select name="parentPeriodId">
                <option value=''>&nbsp;</option>
                <#list allCustomTimePeriods as allCustomTimePeriod>
                  <#assign allPeriodType = allCustomTimePeriod.getRelatedOne("PeriodType", true)>
                  <#assign isDefault = false>
                  <#if (currentCustomTimePeriod.parentPeriodId)??>
                    <#if currentCustomTimePeriod.customTimePeriodId = allCustomTimePeriod.customTimePeriodId>
                      <#assign isDefault = true>
                    </#if>
                  </#if>
                  <option value='${allCustomTimePeriod.customTimePeriodId}'<#if isDefault> selected="selected"</#if>>
                    ${allCustomTimePeriod.organizationPartyId}
                    <#if allPeriodType??> ${allPeriodType.description}: </#if>
                    ${allCustomTimePeriod.periodNum!}
                    [${allCustomTimePeriod.customTimePeriodId}]
                  </option>
                </#list>
              </select>
            </@td>
            <@td><input type="text" size='12' name="organizationPartyId" value="${customTimePeriod.organizationPartyId!}" /></@td>
            <@td>
              <select name="periodTypeId">
                <#list periodTypes as periodType>
                  <#assign isDefault = false>
                  <#if (customTimePeriod.periodTypeId)??>
                    <#if customTimePeriod.periodTypeId = periodType.periodTypeId>
                     <#assign isDefault = true>
                    </#if>
                  </#if>
                  <option value='${periodType.periodTypeId}'<#if isDefault> selected="selected"</#if>>${periodType.description} [${periodType.periodTypeId}]</option>
                </#list>
              </select>
            </@td>
            <@td><input type="text" size='4' name="periodNum" value="${customTimePeriod.periodNum!}" /></@td>
            <@td><input type="text" size='10' name="periodName" value="${customTimePeriod.periodName!}" /></@td>
            <@td>
              <#assign hasntStarted = false>
              <#assign compareDate = customTimePeriod.getDate("fromDate")>
              <#if compareDate?has_content>
                <#if nowTimestamp.before(compareDate)><#assign hasntStarted = true></#if>
              </#if>
              <input type="text" size='13' name="fromDate" value="${customTimePeriod.fromDate?string("yyyy-MM-dd")}"<#if hasntStarted> class="alert"</#if> />
            </@td>
            <@td>
              <#assign hasExpired = false>
              <#assign compareDate = customTimePeriod.getDate("thruDate")>
              <#if compareDate?has_content>
                <#if nowTimestamp.after(compareDate)><#assign hasExpired = true></#if>
              </#if>
              <input type="text" size='13' name="thruDate" value="${customTimePeriod.thruDate?string("yyyy-MM-dd")}"<#if hasExpired> class="alert"</#if> />
             </@td>
             <@td class="button-col">
              <input type="submit" value='${uiLabelMap.CommonUpdate}'/>
              <a href='<@ofbizUrl>deleteCustomTimePeriod?customTimePeriodId=${customTimePeriod.customTimePeriodId!}&amp;currentCustomTimePeriodId=${currentCustomTimePeriodId!}&amp;findOrganizationPartyId=${findOrganizationPartyId!}</@ofbizUrl>'>
              ${uiLabelMap.CommonDelete}</a>
              <a href='<@ofbizUrl>EditCustomTimePeriod?currentCustomTimePeriodId=${customTimePeriod.customTimePeriodId!}&amp;findOrganizationPartyId=${findOrganizationPartyId!}</@ofbizUrl>'>
              ${uiLabelMap.CommonSetAsCurrent}</a>
            </@td>
            </form>
          </@tr>
        </#list>
      </@table>
    <#else>
      <@resultMsg>${uiLabelMap.AccountingNoChildPeriodsFound}</@resultMsg>
    </#if>
  </@section>
  
  <@section title="${uiLabelMap.AccountingAddCustomTimePeriod}">
      <form method="post" action="<@ofbizUrl>createCustomTimePeriod</@ofbizUrl>" name="createCustomTimePeriodForm">
        <input type="hidden" name="findOrganizationPartyId" value="${findOrganizationPartyId!}" />
        <input type="hidden" name="currentCustomTimePeriodId" value="${currentCustomTimePeriodId!}" />
        <input type="hidden" name="useValues" value="true" />
        <div>
          <span>${uiLabelMap.CommonParent}</span>
          <select name="parentPeriodId">
            <option value=''>&nbsp;</option>
            <#list allCustomTimePeriods as allCustomTimePeriod>
                <#assign allPeriodType = allCustomTimePeriod.getRelatedOne("PeriodType", true)>
              <#assign isDefault = false>
              <#if currentCustomTimePeriod??>
                <#if currentCustomTimePeriod.customTimePeriodId = allCustomTimePeriod.customTimePeriodId>
                  <#assign isDefault = true>
                </#if>
              </#if>
              <option value="${allCustomTimePeriod.customTimePeriodId}"<#if isDefault> selected="selected"</#if>>
                ${allCustomTimePeriod.organizationPartyId}
                <#if (allCustomTimePeriod.parentPeriodId)??>Par:${allCustomTimePeriod.parentPeriodId}</#if>
                <#if allPeriodType??> ${allPeriodType.description}:</#if>
                ${allCustomTimePeriod.periodNum!}
                [${allCustomTimePeriod.customTimePeriodId}]
              </option>
            </#list>
          </select>
        </div>
        <div>
          <span>${uiLabelMap.AccountingOrgPartyId}</span>
          <input type="text" size='20' name='organizationPartyId' />
          <span>${uiLabelMap.AccountingPeriodType}</span>
          <select name="periodTypeId">
            <#list periodTypes as periodType>
              <#assign isDefault = false>
              <#if newPeriodTypeId??>
                <#if newPeriodTypeId = periodType.periodTypeId>
                  <#assign isDefault = true>
                </#if>
              </#if>
              <option value="${periodType.periodTypeId}" <#if isDefault>selected="selected"</#if>>${periodType.description} [${periodType.periodTypeId}]</option>
            </#list>
          </select>
          <span>${uiLabelMap.AccountingPeriodNumber}</span>
          <input type="text" size='4' name='periodNum' />
          <span>${uiLabelMap.AccountingPeriodName}</span>
          <input type="text" size='10' name='periodName' />
        </div>
        <div>
          <span>${uiLabelMap.CommonFromDate}</span>
          <input type="text" size='14' name='fromDate' />
          <span>${uiLabelMap.CommonThruDate}</span>
          <input type="text" size='14' name='thruDate' />
          <input type="submit" value="${uiLabelMap.CommonAdd}" />
        </div>
      </form>
  </@section>
<#else>
  <@alert type="error">${uiLabelMap.AccountingPermissionPeriod}.</@alert>
</#if>

</@section>
