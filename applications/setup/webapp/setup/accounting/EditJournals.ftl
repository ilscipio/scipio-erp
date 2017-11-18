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


<#-- CORE INCLUDE -->
<@fieldset collapsed=true collapsible=true title="Configure Journal">
	<@row>
	    <@cell medium=12 large=12>
			<@section title=uiLabelMap.AccountingJournalEntries>		
				<@form method="get" action=makeOfbizUrl("setupAccounting") id="setupAccounting-selectJournalEntry-form">
				    <#-- TODO: REVIEW: may make a difference later -->
				    <@defaultWizardFormFields exclude=["topGlAccountId"]/>
				    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
				    
				    
				</@form>
			</@section>
		</@cell>
	</@row>
</@fieldset>