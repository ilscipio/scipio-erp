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

	
<@form method="get" action=makeOfbizUrl("setupAccounting") id="setupAccounting-selectJournalEntry-form">
    <#-- TODO: REVIEW: may make a difference later -->
    <@defaultWizardFormFields exclude=["topGlAccountId"]/>
    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
	<@row>
	    <@cell medium=9 large=9>
	        <@section title=uiLabelMap.SetupAccountingJournals>	
	        	<@table>
	        		<@tr>
	        			<#list glJournals as glJournal>
		        			<@td>${glJournal.glJournalId!}</@td>
		        			<@td>${glJournal.glJournalName!}</@td>
	        			</#list>
	        		</@tr>
	        	</@table>
	        </@section>  
	    </@cell>
	    <@cell medium=3 large=3>    
	      <#-- ACTIONS MENU -->
	      <@section title=uiLabelMap.CommonActions>	        
	        <#-- MENU -->
	        <ul class="side-nav">		       	
	        	<li>
	       			<@menuitem type="link" href="javascript:void(0);" text=uiLabelMap.CommonAdd />
	       		</li>
	       	</ul>
	      </@section>
		</@cell>
	</@row>
</@form>
