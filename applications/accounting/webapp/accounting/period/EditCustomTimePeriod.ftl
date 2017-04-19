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

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("AddCustomTimePeriod") text=uiLabelMap.CommonAdd class="+${styles.action_run_local!} ${styles.action_add!}" />
    </@menu>
</#macro>

<@section menuContent=menuContent>
    <#if security.hasPermission("PERIOD_MAINT", session)>
        <@section title=uiLabelMap.AccountingShowOnlyPeriodsWithOrganization>
            <form method="post" action="<@ofbizUrl>EditCustomTimePeriod</@ofbizUrl>" name="setOrganizationPartyIdForm">
                <input type="hidden" name="currentCustomTimePeriodId" value="${currentCustomTimePeriodId!}" />
                <span>${uiLabelMap.AccountingShowOnlyPeriodsWithOrganization}</span>
                <input type="text" size="20" name="findOrganizationPartyId" value="${findOrganizationPartyId!}" />
                <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
            </form>
        </@section>
        
        <@section title=uiLabelMap.AccountingChildPeriods>
            <@form method="post" action=makeOfbizUrl("updateCustomTimePeriod") name="updateCustomTimePeriod">
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
                                <@th>&nbsp;</@th>                                
                            </@tr>
                        </@thead>

                        <#list customTimePeriods as customTimePeriod>
                            <#assign alertHasntStartedClass="" />
                            <#if hasntStarted?has_content && hasntStarted><#assign alertHasntStartedClass="alert"></#if>
                            <#assign alertHasExpiredClass="" />
                            <#if hasExpired?has_content && hasExpired><#assign alertHasExpiredClass="alert"></#if>
                            <#assign periodType = customTimePeriod.getRelatedOne("PeriodType", true)>
                            <input type="hidden" name="customTimePeriodId_o_${customTimePeriod_index}" value="${customTimePeriod.customTimePeriodId!}" />    
                            <input type="hidden" name="_useRowSubmit" value="Y" />    
                            <input type="hidden" name="_rowSubmit_o_${customTimePeriod_index}"/>                    
                            <@tr id="row_${customTimePeriod.customTimePeriodId!}">
                                <@td>${customTimePeriod.customTimePeriodId}</@td>
                                <@td>
                                    <@field type="select" name="parentPeriodId_o_${customTimePeriod.customTimePeriodId!}">
                                        <option value="">&nbsp;</option>
                                        <#list allCustomTimePeriods as allCustomTimePeriod>
                                            <#assign allPeriodType = allCustomTimePeriod.getRelatedOne("PeriodType", true)>                                            
                                            <option value="${allCustomTimePeriod.customTimePeriodId}"<#if customTimePeriod.parentPeriodId?has_content && customTimePeriod.parentPeriodId == allCustomTimePeriod.customTimePeriodId> selected="selected"</#if>>                                                
                                                <#if allPeriodType??> ${allPeriodType.description}: </#if>
                                                ${allCustomTimePeriod.periodNum!}
                                                [${allCustomTimePeriod.customTimePeriodId}]
                                            </option>
                                        </#list>
                                    </@field>
                                </@td>
                                <@td><@field type="input" size="12" name="organizationPartyId_o_${customTimePeriod_index}" value=(customTimePeriod.organizationPartyId!) /></@td>
                                <@td>
                                    <@field type="select" name="periodTypeId_o_${customTimePeriod_index}">
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
                                <@td><@field type="input" size="4" name="periodNum_o_${customTimePeriod_index}" value=(customTimePeriod.periodNum!) /></@td>
                                <@td><@field type="input" size="10" name="periodName_o_${customTimePeriod_index}" value=(customTimePeriod.periodName!) /></@td>
                                    <@td>
                                    <#assign hasntStarted = false>
                                    <#assign compareDate = customTimePeriod.getTimestamp("fromDate")>
                                    <#if compareDate?has_content>
                                        <#if nowTimestamp.before(compareDate)><#assign hasntStarted = true></#if>
                                    </#if>
                                    <@field type="input" size="13" name="fromDate_o_${customTimePeriod_index}" value=customTimePeriod.fromDate?string('yyyy-MM-dd') class="+${alertHasntStartedClass}" />
                                </@td>

                                <@td>
                                    <#assign hasExpired = false>
                                    <#assign callCustomTimePeriodsompareDate = customTimePeriod.getTimestamp("thruDate")>
                                    <#if compareDate?has_content>
                                        <#if nowTimestamp.after(compareDate)><#assign hasExpired = true></#if>
                                    </#if>
                                    <@field type="input" size="13" name="thruDate_o_${customTimePeriod_index}" value=customTimePeriod.thruDate?string('yyyy-MM-dd') class="+${alertHasExpiredClass}" />
                                </@td>
                                <@td>
                                    <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}" onClick="javascript:document.forms.updateCustomTimePeriod.elements['_rowSubmit_o_${customTimePeriod_index}'].value = 'Y';"/>
                                </@td>
                                <@td>
                                    <a href="javascript:document.deleteCustomTimePeriod_${customTimePeriod_index}.submit();" class="${styles.link_run_sys} ${styles.action_remove}">${uiLabelMap.CommonDelete}</a>
                                </@td>
                            </@tr>
                        </#list>
                    </@table>                   
                   
                <#else>
                    <@commonMsg type="result-norecord">${uiLabelMap.AccountingNoChildPeriodsFound}</@commonMsg>
                </#if>
            </@form>
            <#list customTimePeriods as customTimePeriod>                
                <form method="post" action="<@ofbizUrl>deleteCustomTimePeriod</@ofbizUrl>" name="deleteCustomTimePeriod_${customTimePeriod_index}">
                    <@field type="hidden" name="customTimePeriodId" value="${customTimePeriod.customTimePeriodId!}" />                      
                    <@field type="hidden" name="findOrganizationPartyId" value="${findOrganizationPartyId!}"/>                              
                </form>
            </#list>
        </@section>      
    <#else>
      <@commonMsg type="error">${uiLabelMap.AccountingPermissionPeriod}.</@commonMsg>
    </#if>
</@section>
