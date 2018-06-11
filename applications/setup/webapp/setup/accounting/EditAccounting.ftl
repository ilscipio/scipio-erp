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

<@script>
	$(document).ready(function () {		
		if (typeof Foundation !== "undefined") { 
			$('ul.tabs').on('toggled', customToggleTab);
			tabs = $('li:first-child a[data-toggle="tab"]').foundation();			
			customToggleTab(null, tabs[0]);					
		} else if (typeof  $.fn.tooltip.Constructor.VERSION!== "undefined") { 
			$('a[data-toggle="tab"]').on('shown.bs.tab', customToggleTab);			
			customToggleTab($('li:first-child a[data-toggle="tab"]').tab()[0]);
		} else {
			console.log("Bootstrap or Foundation cannot be found");			 
		}		
	});
	
	function customToggleTab (e, t) {
	  if (typeof setupControlMenu != "undefined") {
		  var tabId = null;
		  if ($(t).length) { // foundation 5.5.3		  	
		  	if ($(t.context).length) {
		  		tabId = t.context.hash.substring(1, t.context.hash.length);
		  	} else {
		  		tabId = t.hash.substring(1, t.hash.length);
		  	}	  	
		  } else if ($(e).length) { // bootstrap 4
		  	if ($(e.target).length) {
		    	tabId = e.target.hash.substring(1, e.target.hash.length);
		    } else {
		    	tabId = e.hash.substring(1, e.hash.length);
		    }		    
		  }
		  var tabIdToFormId = {
		    "preferencesTab": {
		        "formId": "setupAccounting-preferences-form"
		    },
		    "glAccountsTab": {
		    	"formId": ""
		    },
		    "fiscalPeriodsTab": {
		    	"formId": ""
		    },
		    "accountingTransactionsTab": {
		    	"formId": ""
		    },
		    "journalTab": {
		    	"formId": "setupAccounting-selectJournalEntry-form"
		    }
		  }
		  formId = tabIdToFormId[tabId].formId;
		  setupControlMenu.setSubmitFormId(formId);
		  // console.log("formId =======> " + formId);
	  } else {
	  	console.log("setupControlMenu cannot be found");
	  }
	}
</@script>

<#if !topGlAccountId?has_content>
	<#assign disabledTab>+${styles.hidden!""}</#assign>
</#if>
${Static["org.ofbiz.base.util.Debug"].log("disabledTab =====> " + disabledTab!)}

<@tabs id="accountingTabs">
	<@tab id="preferencesTab" title="Preferences">
		<@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditAcctgPreferences"/>
	</@tab>
	<@tab id="glAccountsTab" title="Configure GL Accounts" class=disabledTab!>
		<@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditGLAccountTree" />
	</@tab>
	<@tab id="fiscalPeriodsTab" title="Configure Fiscal Periods" class=disabledTab!><@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditFiscalPeriods"/></@tab>
	<@tab id="accountingTransactionsTab" title="Configure Accounting Transactions" class=disabledTab!><@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditAccountingTransactions"/></@tab>
	<@tab id="journalTab" title="Configure Journal" class=disabledTab!><@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditJournals"/></@tab>
</@tabs>     

