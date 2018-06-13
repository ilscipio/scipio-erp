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
    function setupShowFormActivatedCallback(form, ai) {
        setupControlMenu.setSubmitFormId(form.prop('id'));
    };
		
	var tabSettings = {
	    "preferencesTab": {
	        "formId": "setupAccounting-preferences-form",
	        "disabled": false
	    },
	    "glAccountsTab": {
	    	"formId": "",
	    	"disabled": ${(!topGlAccountId?has_content)?string} 
	    },
	    "fiscalPeriodsTab": {
	    	"formId": "",
	    	"disabled": ${(!topGlAccountId?has_content)?string}
	    },
	    "accountingTransactionsTab": {
	    	"formId": "",
	    	"disabled": ${(!topGlAccountId?has_content)?string}
	    },
	    "journalTab": {
	    	"formId": "setupAccounting-selectJournalEntry-form",
	    	"disabled": ${(!topGlAccountId?has_content)?string}
	    }
	  }

	$(document).ready(function () {		
		tabs = $('a[data-toggle="tab"]');
		// Check whether tabs should be enabled or disabled based on tab config			 
	  	for (i = 0; i < tabs.length; i++) {
		  	if (tabs[i].hash) {
		  	  tabId = tabs[i].hash.substring(1, tabs[i].hash.length);
			  if (tabSettings[tabId].disabled) {			  	
			  	$(tabs[i]).bind('click', function(e) {
			  		e.preventDefault();
			        e.stopImmediatePropagation();
			        e.stopPropagation();
			        $(tabs[i]).addClass("disabled");
			  	});
			  }
			}
	  	}

		if (typeof Foundation !== "undefined") { 
			$('ul.tabs').on('toggled', customToggleTab);			
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
		  var currTabId = null;
		  if ($(t).length) { // foundation 5.5.3		  	
		  	if ($(t.context).length) {
		  		currTabId = t.context.hash.substring(1, t.context.hash.length);
		  	} else {
		  		currTabId = t.hash.substring(1, t.hash.length);
		  	}	  	
		  } else if ($(e).length) { // bootstrap 4
		  	if ($(e.target).length) {
		    	currTabId = e.target.hash.substring(1, e.target.hash.length);
		    } else {
		    	currTabId = e.hash.substring(1, e.hash.length);
		    }		    
		  }		  
		  
		  // Set the proper form id to submit on tab change
		  formId = tabSettings[currTabId].formId;
		  setupControlMenu.setSubmitFormId(formId);
	  } else {
	  	console.log("setupControlMenu cannot be found");
	  }
	}
</@script>

<@tabs id="accountingTabs">
	<@tab id="preferencesTab" title="Preferences">
		<@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditAcctgPreferences"/>
	</@tab>
	<@tab id="glAccountsTab" title="Configure GL Accounts">
		<@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditGLAccountTree" />
	</@tab>
	<@tab id="fiscalPeriodsTab" title="Configure Fiscal Periods"><@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditFiscalPeriods"/></@tab>
	<#-- SCIPIO (06/13/2018): Commenting this out for now -->
	<#-- <@tab id="accountingTransactionsTab" title="Configure Accounting Transactions"><@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditAccountingTransactions"/></@tab> -->
	<@tab id="journalTab" title="Configure Journal"><@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditJournals"/></@tab>
</@tabs>     

