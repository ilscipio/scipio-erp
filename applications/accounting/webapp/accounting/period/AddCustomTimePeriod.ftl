<#if security.hasPermission("PERIOD_MAINT", session)>    
    <@section>
        <form method="post" action="<@ofbizUrl>createCustomTimePeriod</@ofbizUrl>" name="createCustomTimePeriodForm">
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
                <@field type="datetime" size="14" name="fromDate" label=uiLabelMap.CommonFromDate dateType="date" />                      
                <@field type="datetime" size="14" name="thruDate" label=uiLabelMap.CommonThruDate dateType="date" />
                <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}"/>
            </div>
        </form>
    </@section>
</#if>