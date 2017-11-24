<#-- SCIPIO: SETUP fiscal periods implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {	
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

	
	<@section title=uiLabelMap.AccountingTimePeriod>		
		<@form method="get" action=makeOfbizUrl("setupAccounting") id="setupAccounting-selectTimePeriod-form">
		    <#-- TODO: REVIEW: may make a difference later -->
		    <@defaultWizardFormFields exclude=["topGlAccountId"]/>
		    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
		    
		    <@field type="general" label=uiLabelMap.SetupAccountingSelectTimePeriod>
		       <@field type="select" name="customTimePeriodId" id="setupAccounting-selectTimePeriod-select" class="+setupAccounting-selectTimePeriod-select" inline=true style="display:inline-block;">
		            <option value="">[${uiLabelMap.SetupAccountingCreateNewTimePeriod}]</option>
		            <option value="" disabled="disabled"></option>
		              <#list timePeriods as timePeriod>
		              	<#assign selected = (rawString(timePeriod.customTimePeriodId) == rawString(params.customTimePeriod!))>
		                <option value="${timePeriod.customTimePeriodId!}"<#if selected> selected="selected"</#if>>${timePeriod.periodName!} [${timePeriod.customTimePeriodId!}]</option>
		              </#list>
		        </@field>
		    </@field>
		</@form>
	</@section>		
