<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#if security.hasPermission("PERIOD_MAINT", request)>    
    <@section>
        <form method="post" action="<@pageUrl>createCustomTimePeriod</@pageUrl>" name="createCustomTimePeriodForm">
            <input type="hidden" name="findOrganizationPartyId" value="${findOrganizationPartyId!}" />
            <#-- <input type="hidden" name="currentCustomTimePeriodId" value="${currentCustomTimePeriodId!}" /> -->
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
                <@field type="input" size="20" name="organizationPartyId" label=uiLabelMap.AccountingOrgPartyId value=findOrganizationPartyId!organizationPartyId!/>
                <@field type="select" name="periodTypeId" label=uiLabelMap.AccountingPeriodType>
                    <#list periodTypes as periodType>
                        <#assign isDefault = false>
                        <#if newPeriodTypeId??>
                            <#if newPeriodTypeId == periodType.periodTypeId>
                                <#assign isDefault = true>
                            </#if>
                        </#if>
                        <option value="${periodType.periodTypeId}"<#if isDefault> selected="selected"</#if>>${periodType.description} [${periodType.periodTypeId}]</option>
                    </#list>
                </@field>                  
                <@field type="input" size="4" name="periodNum" label=uiLabelMap.AccountingPeriodNumber />                      
                <@field type="input" size="10" name="periodName" label=uiLabelMap.AccountingPeriodName />
                </div>
            <div>                      
                <@field type="datetime" size="14" name="fromDate" label=uiLabelMap.CommonFromDate dateType="date" />                      
                <@field type="datetime" size="14" name="thruDate" label=uiLabelMap.CommonThruDate dateType="date" />
                <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}"/>
            </div>
        </form>
    </@section>
</#if>