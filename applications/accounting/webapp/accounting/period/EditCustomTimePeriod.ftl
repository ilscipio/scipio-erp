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

<@section title=uiLabelMap.AccountingEditCustomTimePeriods>

    <#if security.hasPermission("PERIOD_MAINT", session)>
        <@section title=uiLabelMap.AccountingShowOnlyPeriodsWithOrganization>
            <form method="post" action="<@ofbizUrl>EditCustomTimePeriod</@ofbizUrl>" name="setOrganizationPartyIdForm">
                <input type="hidden" name="currentCustomTimePeriodId" value="${currentCustomTimePeriodId!}" />
                <span>${uiLabelMap.AccountingShowOnlyPeriodsWithOrganization}</span>
                <input type="text" size="20" name="findOrganizationPartyId" value="${findOrganizationPartyId!}" />
                <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
            </form>
        </@section>


        <#if currentCustomTimePeriod?has_content>
            <#macro menuContent menuArgs={}>
                <@menu args=menuArgs>
                    <@menuitem type="link" href=makeOfbizUrl("EditCustomTimePeriod?findOrganizationPartyId=${findOrganizationPartyId!}") text=uiLabelMap.CommonClearCurrent class="+${styles.action_run_local!} ${styles.action_clear!}" />
                </@menu>
            </#macro>
        <#else>
            <#macro menuContent menuArgs={}>
                <@menu args=menuArgs>
                </@menu>
            </#macro>
        </#if>
        <@section title=uiLabelMap.AccountingCurrentCustomTimePeriod menuContent=menuContent>
            <#if currentCustomTimePeriod?has_content>
                <#assign alertClass="" />
                <#if hasntStarted><#assign alertClass="alert"></#if>
                <form method="post" action="<@ofbizUrl>updateCustomTimePeriod</@ofbizUrl>" name="updateCustomTimePeriodForm">
                    <input type="hidden" name="findOrganizationPartyId" value="${findOrganizationPartyId!}" />
                    <input type="hidden" name="customTimePeriodId" value="${currentCustomTimePeriodId!}" />
                    <@table type="data-list"> <#-- orig: class="basic-table" -->
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
                                <@field type="select" name="parentPeriodId">
                                        <option value="">&nbsp;</option>
                                    <#list allCustomTimePeriods as allCustomTimePeriod>
                                        <#assign allPeriodType = allCustomTimePeriod.getRelatedOne("PeriodType", true)>
                                        <#assign isDefault = false>
                                        <#if (currentCustomTimePeriod.parentPeriodId)??>
                                            <#if currentCustomTimePeriod.customTimePeriodId == allCustomTimePeriod.customTimePeriodId>
                                                <#assign isDefault = true>
                                            </#if>
                                        </#if>
                                        <option value="${allCustomTimePeriod.customTimePeriodId}"<#if isDefault> selected="selected"</#if>>
                                            ${allCustomTimePeriod.organizationPartyId}
                                            <#if allPeriodType??>${allPeriodType.description}:</#if>
                                                    ${allCustomTimePeriod.periodNum!}
                                                [${allCustomTimePeriod.customTimePeriodId}]
                                        </option>
                                    </#list>
                                </@field>
                                <#if (currentCustomTimePeriod.parentPeriodId)??>
                                    <a href="<@ofbizUrl>EditCustomTimePeriod?currentCustomTimePeriodId=${currentCustomTimePeriod.parentPeriodId}&amp;findOrganizationPartyId=${findOrganizationPartyId!}</@ofbizUrl>">
                                    ${uiLabelMap.CommonSetAsCurrent}</a>
                                </#if>
                                </@td>
                            <@td><@field type="input" size="12" name="currentCustomTimePeriod" value=(currentCustomTimePeriod.organizationPartyId!) /></@td>
                            <@td>
                                <@field type="select" name="periodTypeId">
                                    <#list periodTypes as periodType>
                                        <#assign isDefault = false>
                                        <#if (currentCustomTimePeriod.periodTypeId)??>
                                            <#if currentCustomTimePeriod.periodTypeId == periodType.periodTypeId>
                                                <#assign isDefault = true>
                                            </#if>
                                        </#if>
                                        <option value="${periodType.periodTypeId}"<#if isDefault> selected="selected"</#if>>
                                            ${periodType.description} [${periodType.periodTypeId}]
                                        </option>
                                    </#list>
                                </@field>
                            </@td>
                            <@td><@field type="input" size="4" name="periodNum" value=(currentCustomTimePeriod.periodNum!) /></@td>
                            <@td><@field type="input" size="10" name="periodName" value=(currentCustomTimePeriod.periodName!) /></@td>
                            <@td>
                                <#assign hasntStarted = false>
                                <#assign compareDate = currentCustomTimePeriod.getTimestamp("fromDate")>
                                <#if compareDate?has_content>
                                    <#if nowTimestamp.before(compareDate)><#assign hasntStarted = true></#if>
                                </#if>
                                <@field type="input" size="13" name="fromDate" value=currentCustomTimePeriod.fromDate?string('yyyy-MM-dd') class="+${alertClass}" />
                            </@td>
                            <@td>
                                <#assign hasExpired = false>
                                <#assign compareDate = currentCustomTimePeriod.getTimestamp("thruDate")>
                                <#if compareDate?has_content>
                                    <#if nowTimestamp.after(compareDate)><#assign hasExpired = true></#if>
                                </#if>
                                <@field type="input" size="13" name="thruDate" value=currentCustomTimePeriod.thruDate?string('yyyy-MM-dd') class="+${alertClass}" />
                            </@td>
                            <@td class="button-col">
                            <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
                            <a href="<@ofbizUrl>deleteCustomTimePeriod?customTimePeriodId=${currentCustomTimePeriod.customTimePeriodId}</@ofbizUrl>">
                            ${uiLabelMap.CommonDelete}</a>
                            <#-- SCIPIO: TODO (2016-06-13): INCORPORATE PATCH (REPLACE DELETE GET WITH POST, BUT JS NEEDED DUE TO NESTED FORMS
                              <form method="post" action="<@ofbizUrl>deleteCustomTimePeriod</@ofbizUrl>" name="deleteCustomTimePeriodForm">
                                <input type="hidden" name="customTimePeriodId" value="${currentCustomTimePeriod.customTimePeriodId!}" />
                                <input type="submit" value="${uiLabelMap.CommonDelete}" class="${styles.link_run_sys!} ${styles.action_remove!}"/>
                              </form>
                            -->
                            </@td>
                        </@tr>
                    </@table>
                </form>
            <#else>
                <@commonMsg type="warning">${uiLabelMap.AccountingNoCurrentCustomTimePeriodSelected}</@commonMsg>
            </#if>
        </@section>
  
        <@section title=uiLabelMap.AccountingChildPeriods>
            <#if customTimePeriods?has_content>
                <@table type="data-list"> <#-- orig: class="basic-table" -->
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
                        <#assign alertHasntStartedClass="" />
                        <#if hasntStarted?has_content && hasntStarted><#assign alertHasntStartedClass="alert"></#if>
                        <#assign alertHasExpiredClass="" />
                        <#if hasExpired?has_content && hasExpired><#assign alertHasExpiredClass="alert"></#if>
                        <#assign line = line + 1>
                        <#assign periodType = customTimePeriod.getRelatedOne("PeriodType", true)>
                        <@tr>
                            <form method="post" action="<@ofbizUrl>updateCustomTimePeriod</@ofbizUrl>" name="lineForm${line}">
                                <input type="hidden" name="customTimePeriodId" value="${customTimePeriod.customTimePeriodId!}" />
                                <@td>${customTimePeriod.customTimePeriodId}</@td>
                                <@td>
                                    <@field type="select" name="parentPeriodId">
                                        <option value="">&nbsp;</option>
                                        <#list allCustomTimePeriods as allCustomTimePeriod>
                                            <#assign allPeriodType = allCustomTimePeriod.getRelatedOne("PeriodType", true)>
                                            <#assign isDefault = false>
                                            <#if (currentCustomTimePeriod.parentPeriodId)??>
                                                <#if currentCustomTimePeriod.customTimePeriodId == allCustomTimePeriod.customTimePeriodId>
                                                    <#assign isDefault = true>
                                                </#if>
                                            </#if>
                                            <option value="${allCustomTimePeriod.customTimePeriodId}"<#if isDefault> selected="selected"</#if>>
                                                ${allCustomTimePeriod.organizationPartyId}
                                                <#if allPeriodType??> ${allPeriodType.description}: </#if>
                                                ${allCustomTimePeriod.periodNum!}
                                                [${allCustomTimePeriod.customTimePeriodId}]
                                            </option>
                                        </#list>
                                    </@field>
                                </@td>
                                <@td><@field type="input" size="12" name="organizationPartyId" value=(customTimePeriod.organizationPartyId!) /></@td>
                                <@td>
                                    <@field type="select" name="periodTypeId">
                                        <#list periodTypes as periodType>
                                            <#assign isDefault = false>
                                            <#if (customTimePeriod.periodTypeId)??>
                                                <#if customTimePeriod.periodTypeId == periodType.periodTypeId>
                                                    <#assign isDefault = true>
                                                </#if>
                                            </#if>
                                            <option value="${periodType.periodTypeId}"<#if isDefault> selected="selected"</#if>>${periodType.description} [${periodType.periodTypeId}]</option>
                                        </#list>
                                    </@field>
                                </@td>
                                <@td><@field type="input" size="4" name="periodNum" value=(customTimePeriod.periodNum!) /></@td>
                                <@td><@field type="input" size="10" name="periodName" value=(customTimePeriod.periodName!) /></@td>
                                    <@td>
                                    <#assign hasntStarted = false>
                                    <#assign compareDate = customTimePeriod.getTimestamp("fromDate")>
                                    <#if compareDate?has_content>
                                        <#if nowTimestamp.before(compareDate)><#assign hasntStarted = true></#if>
                                    </#if>
                                    <@field type="input" size="13" name="fromDate" value=customTimePeriod.fromDate?string('yyyy-MM-dd') class="+${alertHasntStartedClass}" />
                                </@td>
                                <@td>
                                    <#assign hasExpired = false>
                                    <#assign compareDate = customTimePeriod.getTimestamp("thruDate")>
                                    <#if compareDate?has_content>
                                        <#if nowTimestamp.after(compareDate)><#assign hasExpired = true></#if>
                                    </#if>
                                    <@field type="input" size="13" name="thruDate" value=customTimePeriod.thruDate?string('yyyy-MM-dd') class="+${alertHasExpiredClass}" />
                                </@td>
                                <@td class="button-col">
                                    <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                                    <a href="<@ofbizUrl>deleteCustomTimePeriod?customTimePeriodId=${customTimePeriod.customTimePeriodId!}&amp;currentCustomTimePeriodId=${currentCustomTimePeriodId!}&amp;findOrganizationPartyId=${findOrganizationPartyId!}</@ofbizUrl>">
                                    ${uiLabelMap.CommonDelete}</a>
                                    <#-- SCIPIO: TODO (2016-06-13): INCORPORATE PATCH (REPLACE DELETE GET WITH POST, BUT JS NEEDED DUE TO NESTED FORMS
                                    <form method="post" action='<@ofbizUrl>deleteCustomTimePeriod</@ofbizUrl>' name='lineForm${line}'>
                                      <input type="hidden" name="customTimePeriodId" value="${customTimePeriod.customTimePeriodId!}" />
                                      <input type="submit" value='${uiLabelMap.CommonDelete}'/>
                                    </form>
                                    -->
                                    <a href="<@ofbizUrl>EditCustomTimePeriod?currentCustomTimePeriodId=${customTimePeriod.customTimePeriodId!}&amp;findOrganizationPartyId=${findOrganizationPartyId!}</@ofbizUrl>">
                                    ${uiLabelMap.CommonSetAsCurrent}</a>
                                </@td>
                            </form>
                        </@tr>
                    </#list>
                </@table>
            <#else>
                <@commonMsg type="result-norecord">${uiLabelMap.AccountingNoChildPeriodsFound}</@commonMsg>
            </#if>
        </@section>
  
        <@section title=uiLabelMap.AccountingAddCustomTimePeriod>
            <form method="post" action="<@ofbizUrl>createCustomTimePeriod</@ofbizUrl>" name="createCustomTimePeriodForm">
                <input type="hidden" name="findOrganizationPartyId" value="${findOrganizationPartyId!}" />
                <input type="hidden" name="currentCustomTimePeriodId" value="${currentCustomTimePeriodId!}" />
                <input type="hidden" name="useValues" value="true" />
                <div>
                    <@field type="select" name="parentPeriodId" label=uiLabelMap.CommonParent>
                        <option value="">&nbsp;</option>
                        <#list allCustomTimePeriods as allCustomTimePeriod>
                            <#assign allPeriodType = allCustomTimePeriod.getRelatedOne("PeriodType", true)>
                            <#assign isDefault = false>
                            <#if currentCustomTimePeriod??>
                                <#if currentCustomTimePeriod.customTimePeriodId == allCustomTimePeriod.customTimePeriodId>
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
                    </@field>
                </div>
                <div>                      
                    <@field type="input" size="20" name="organizationPartyId" label=uiLabelMap.AccountingOrgPartyId />                      
                    <@field type="select" name="periodTypeId" label=uiLabelMap.AccountingPeriodType>
                        <#list periodTypes as periodType>
                            <#assign isDefault = false>
                            <#if newPeriodTypeId??>
                                <#if newPeriodTypeId == periodType.periodTypeId>
                                    <#assign isDefault = true>
                                </#if>
                            </#if>
                            <option value="${periodType.periodTypeId}" <#if isDefault>selected="selected"</#if>>${periodType.description} [${periodType.periodTypeId}]</option>
                        </#list>
                    </@field>                  
                    <@field type="input" size="4" name="periodNum" label=uiLabelMap.AccountingPeriodNumber />                      
                    <@field type="input" size="10" name="periodName" label=uiLabelMap.AccountingPeriodName />
                    </div>
                <div>                      
                    <@field type="input" size="14" name="fromDate" label=uiLabelMap.CommonFromDate />                      
                    <@field type="input" size="14" name="thruDate" label=uiLabelMap.CommonThruDate />
                    <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}"/>
                </div>
            </form>
        </@section>
    <#else>
      <@commonMsg type="error">${uiLabelMap.AccountingPermissionPeriod}.</@commonMsg>
    </#if>
</@section>
