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
<@section title=uiLabelMap.SetupAccountingPreferences>		
	<@form method="post" action=makeOfbizUrl(acctgPreferencesActionUrl!) id="setupAccounting-preferences-form">
	    <#-- TODO: REVIEW: may make a difference later -->
	    <@defaultWizardFormFields exclude=["topGlAccountId"]/>
	    <@field type="hidden" name="partyId" value=orgPartyId! />

	    <@section title=uiLabelMap.SetupAccountingPreferencesTaxes>
	    	<#if !acctgPreferences?has_content || (acctgPreferences?has_content && acctgTransCount == 0)>
		        <@field type="select" name="taxFormId" value=(params.taxFormId!) label=uiLabelMap.FormFieldTitle_taxFormId>
		    		<option value=""></option>
			    	<#list taxForms as taxForm>
			    		<#assign selected = (rawString(taxForm.enumId) == rawString(params.taxFormId!))>
			    		<option value="${taxForm.enumId}"<#if selected> selected="selected"</#if>>${taxForm.description}</option>
			    	</#list>
		    	</@field>
		    	<@field type="select" name="cogsMethodId" value=(params.cogsMethodId!) label=uiLabelMap.FormFieldTitle_cogsMethodId>
		    		<option value=""></option>
			    	<#list cogsMethods as cogsMethod>
			    		<#assign selected = (rawString(cogsMethod.enumId) == rawString(params.cogsMethodId!))>
			    		<option value="${cogsMethod.enumId}"<#if selected> selected="selected"</#if>>${cogsMethod.description}</option>
			    	</#list>
		    	</@field>
		    <#else>
		    	<@field type="display" name="taxFormId" value=(acctgPreferences.taxFormId!) label=uiLabelMap.FormFieldTitle_taxFormId />
		    	<@field type="display" name="cogsMethodId" value=(acctgPreferences.taxFormId!) label=uiLabelMap.FormFieldTitle_cogsMethodId />
	    	</#if>
	    </@section>
	   	<@section title=uiLabelMap.SetupAccountingPreferencesFiscalPeriods>
	   		<#if !acctgPreferences?has_content || (acctgPreferences?has_content && acctgTransCount == 0)>
			    <@field type="generic" label=uiLabelMap.SetupAccountingSelectStartFiscalDayMonth>			        
			        <@script>
			        	$(document).ready(function() {
			        		$('.setupAccounting-preferences-start-month-select option:eq(${params.fiscalYearStartMonth!0})').prop("selected", true);
			        	});
			        </@script>
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
		    <#else>
		    	<@field type="display" name="fiscalYearStartMonth" value=(acctgPreferences.fiscalYearStartMonth!) label=uiLabelMap.FormFieldTitle_fiscalYearStartMonth />
		    	<@field type="display" name="fiscalYearStartDay" value=(acctgPreferences.fiscalYearStartDay!) label=uiLabelMap.FormFieldTitle_fiscalYearStartDay />
		    </#if>
	    </@section>
	    <@section title=uiLabelMap.SetupAccountingPreferencesCurrencies>
	    	<#if !acctgPreferences?has_content || (acctgPreferences?has_content && acctgTransCount == 0)>				    	
				<@field type="select" name="baseCurrencyUomId" value=(params.baseCurrencyUomId!) label=uiLabelMap.FormFieldTitle_baseCurrencyUomId>
		    		<option value=""></option>
			    	<#list currencyUoms as currencyUom>
			    		<#assign selected = (rawString(params.baseCurrencyUomId!) == rawString(currencyUom.uomId!))>
			    		<option value="${currencyUom.uomId}"<#if selected> selected="selected"</#if>>${currencyUom.description} - ${currencyUom.abbreviation}</option>
			    	</#list>
		    	</@field>
	    	<#else>
	    		<@field type="display" name="baseCurrencyUomId" value=(acctgPreferences.baseCurrencyUomId!) label=uiLabelMap.FormFieldTitle_baseCurrencyUomId />
	    	</#if>
	    </@section>
	    <@section title=uiLabelMap.AccountingInvoices>
	    	<@field type="select" name="invoiceSeqCustMethId" value=(params.invoiceSeqCustMethId!) label=uiLabelMap.FormFieldTitle_invoiceSequenceEnumId>
	    		<option value=""></option>
		    	<#list invoiceCustomMethods as invoiceCustomMethod>
		    		<#assign selected = (rawString(invoiceCustomMethod.customMethodId) == rawString(params.invoiceSeqCustMethId!))>
		    		<option value="${invoiceCustomMethod.customMethodId}"<#if selected> selected="selected"</#if>>${invoiceCustomMethod.description}</option>
		    	</#list>
	    	</@field>
	    	<#if !acctgPreferences?has_content || (acctgPreferences?has_content && invoiceCount == 0)>		    					    	
		    	<@field type="input" name="invoiceIdPrefix" value=(params.invoiceIdPrefix!) label=uiLabelMap.FormFieldTitle_invoiceIdPrefix />
		    	<@field type="input" name="lastInvoiceNumber" value=(params.lastInvoiceNumber!) label=uiLabelMap.FormFieldTitle_lastInvoiceNumber />
		    	<@field type="datetime" name="lastInvoiceRestartDate" value=(params.lastInvoiceRestartDate!) label=uiLabelMap.FormFieldTitle_lastInvoiceRestartDate />		    	
		    	<@field type="checkbox" name="useInvoiceIdForReturns" label=uiLabelMap.FormFieldTitle_useInvoiceIdForReturns value="Y" checked=("Y" == params.useInvoiceIdForReturns!) />
	    	<#else>
	    		<@field type="display" name="invoiceIdPrefix" value=(acctgPreferences.invoiceIdPrefix!) label=uiLabelMap.FormFieldTitle_invoiceIdPrefix />
		    	<@field type="display" name="lastInvoiceNumber" value=(acctgPreferences.lastInvoiceNumber!) label=uiLabelMap.FormFieldTitle_lastInvoiceNumber />
		    	<@field type="display" name="lastInvoiceRestartDate" value=(acctgPreferences.lastInvoiceRestartDate!) label=uiLabelMap.FormFieldTitle_lastInvoiceRestartDate />
		    	<@field type="display" name="useInvoiceIdForReturns" value=(acctgPreferences.useInvoiceIdForReturns!) label=uiLabelMap.FormFieldTitle_useInvoiceIdForReturns />
	    	</#if>
	    </@section>
	    <@section title=uiLabelMap.AccountingOrders>
	    	<@field type="select" name="orderSeqCustMethId" value=(params.orderSeqCustMethId!) label=uiLabelMap.FormFieldTitle_orderSequenceEnumId>
	    		<option value=""></option>
		    	<#list orderCustomMethods as orderCustomMethod>
		    		<#assign selected = (rawString(orderCustomMethod.customMethodId) == rawString(params.orderSeqCustMethId!))>
		    		<option value="${orderCustomMethod.customMethodId}"<#if selected> selected="selected"</#if>>${orderCustomMethod.description}</option>
		    	</#list>
	    	</@field>
	    	<#if !acctgPreferences?has_content || (acctgPreferences?has_content && orderCount == 0)>
		        <@field type="text" name="orderIdPrefix" value=(params.orderIdPrefix!) label=uiLabelMap.FormFieldTitle_orderIdPrefix />
		        <@field type="text" name="lastOrderNumber" value=(params.lastOrderNumber!) label=uiLabelMap.FormFieldTitle_lastOrderNumber />
	        <#else>
	        	<@field type="display" name="orderIdPrefix" value=(acctgPreferences.orderIdPrefix!) label=uiLabelMap.FormFieldTitle_orderIdPrefix />
		        <@field type="display" name="lastOrderNumber" value=(acctgPreferences.lastOrderNumber!) label=uiLabelMap.FormFieldTitle_lastOrderNumber />
	        </#if>
	    </@section>
	    <@section title=uiLabelMap.OrderOrderQuotes>
	    	<@field type="select" name="quoteSeqCustMethId" value=(params.quoteSeqCustMethId!) label=uiLabelMap.FormFieldTitle_quoteSequenceEnumId>
	    		<option value=""></option>
		    	<#list quoteCustomMethods as quoteCustomMethod>
		    		<#assign selected = (rawString(quoteCustomMethod.customMethodId) == rawString(params.quoteSeqCustMethId!))>
		    		<option value="${quoteCustomMethod.customMethodId}"<#if selected> selected="selected"</#if>>${quoteCustomMethod.description}</option>
		    	</#list>
	    	</@field>
	    	<#if !acctgPreferences?has_content || (acctgPreferences?has_content && quoteCount == 0)>
		    	<@field type="text" name="quoteIdPrefix" value=(params.quoteIdPrefix!) label=uiLabelMap.FormFieldTitle_quoteIdPrefix />
		    	<@field type="text" name="lastQuoteNumber" value=(params.lastQuoteNumber!) label=uiLabelMap.FormFieldTitle_lastQuoteNumber />
	    	<#else>
	    		<@field type="display" name="quoteIdPrefix" value=(acctgPreferences.quoteIdPrefix!) label=uiLabelMap.FormFieldTitle_quoteIdPrefix />
		    	<@field type="display" name="lastQuoteNumber" value=(acctgPreferences.lastQuoteNumber!) label=uiLabelMap.FormFieldTitle_lastQuoteNumber />
	    	</#if>        			
	    </@section>
	</@form>
</@section>		
