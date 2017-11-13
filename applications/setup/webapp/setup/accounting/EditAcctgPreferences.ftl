<#-- SCIPIO: SETUP accounting preferences implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">

<style type="text/css">
  .setupAccounting-preferences-start-month-select, .setupAccounting-preferences-start-day-input {
    display:inline-block;
  }
  .setupAccounting-preferences-start-month-select {
    margin-right:0.5em;
  }
  .setupAccounting-preferences-start-month-select, .setupAccounting-preferences-start-day-input {
    vertical-align:top; <#-- firefox align issue hack -->
  }
</style>

<#assign defaultParams = {	
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":acctgPreferences!true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>


<#-- CORE INCLUDE -->
<@fieldset collapsed=true collapsible=true title="Preferences">
	<@row>
	    <@cell medium=12 large=12>
			<@section title=uiLabelMap.AccountingPreferences>		
				<@form method="get" action=makeOfbizUrl("setupAccounting") id="setupAccounting-preferences-form">
				    <#-- TODO: REVIEW: may make a difference later -->
				    <@defaultWizardFormFields exclude=["topGlAccountId"]/>
				    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
				    
				    <@section title=uiLabelMap.AccountingPreferencesTaxes>
				    </@section>
				   	<@section title=uiLabelMap.AccountingPreferencesFiscalPeriods>
					    <@field type="general" label=uiLabelMap.SetupAccountingSelectStartFiscalDayMonth>
						    <@field type="select" name="fiscalYearStartMonth" value=(params.fiscalYearStartMonth!) label=uiLabelMap.FormFieldTitle_fiscalYearStartMonth class="+setupAccounting-preferences-start-month-select" inline=true style="display:inline-block;">				    	
				            	<option value=""></option>  	
								<option value="1">${uiLabelMap.CommonJanuary}</option>
								<option value="2">${uiLabelMap.CommonFebruary}</option>
								<option value="3">${uiLabelMap.CommonMarch}</option>
								<option value="4">${uiLabelMap.CommonApril}</option>
								<option value="5">${uiLabelMap.CommonMay}</option>
								<option value="6">${uiLabelMap.CommonJune}</option>
								<option value="7">${uiLabelMap.CommonJuly}</option>
								<option value="8">${uiLabelMap.CommonAugust}</option>
								<option value="9">${uiLabelMap.CommonSeptember}</option>
								<option value="10">${uiLabelMap.CommonOctober}</option>
								<option value="11">${uiLabelMap.CommonNovember}</option>
								<option value="12">${uiLabelMap.CommonDecember}</option>
						    </@field>
						    <@field type="input" name="fiscalYearStartDay" value=(params.fiscalYearStartDay!) label=uiLabelMap.FormFieldTitle_fiscalYearStartDay class="+setupAccounting-preferences-start-day-input" inline=true size=3 maxlength=2 />
					    </@field>
				    </@section>
				    <@section title=uiLabelMap.AccountingPreferencesCurrencies>
				    	<@field type="input" name="baseCurrencyUomId" value=(params.baseCurrencyUomId!) label=uiLabelMap.SetupAccountingDefaultCurrency  />
				    </@section>
				    <@section title=uiLabelMap.AccountingPreferencesInvoices>
				    	<@field type="select" name="invoiceSeqCustMethId" value=(params.invoiceSeqCustMethId!) label=uiLabelMap.SetupAccountingInvoiceSeqCustMethId>
				    		<option value=""></option>
					    	<#list invoiceCustomMethods as invoiceCustomMethod>
					    		<#assign selected = (rawString(invoiceCustomMethod.customMethodId) == rawString(params.invoiceSeqCustMethId!))>
					    		<option value="${invoiceCustomMethod.customMethodId}"<#if selected> selected="selected"</#if>>${invoiceCustomMethod.customMethodName}</option>
					    	</#list>
				    	</@field>
				    	
				    	<@field type="input" name="invoiceIdPrefix" value=(params.invoiceIdPrefix!) label=uiLabelMap.SetupAccountingInvoiceIdPrefix />
				    	<@field type="input" name="lastInvoiceNumber" value=(params.lastInvoiceNumber!) label=uiLabelMap.SetupAccountingLastInvoiceNumber />
				    	<@field type="input" name="lastInvoiceRestartDate" value=(params.lastInvoiceRestartDate!) label=uiLabelMap.SetupAccountingLastInvoiceRestartDate />
				    	<@field type="input" name="useInvoiceIdForReturns" value=(params.useInvoiceIdForReturns!) label=uiLabelMap.SetupAccountingUseInvoiceIdForReturns />
				    </@section>
				    
				</@form>
			</@section>
		</@cell>
	</@row>
</@fieldset>