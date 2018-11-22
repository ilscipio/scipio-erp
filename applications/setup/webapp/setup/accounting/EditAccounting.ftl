<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
	var tabSettings = {
	    "preferencesTab": {
	        "formId": "setupAccounting-preferences-form",
	        "disabled": false,
	        "contentUrl": "default"
	    },
	    "glAccountsTab": {
	    	"formId": "",
	    	"disabled": ${(!topGlAccountId?has_content)?string},
	    	"contentUrl": "<@ofbizUrl>setupGlAccountsTab</@ofbizUrl>"	    	 
	    },
	    "fiscalPeriodsTab": {
	    	"formId": "",
	    	"disabled": ${(!topGlAccountId?has_content)?string},
	    	"contentUrl": "<@ofbizUrl>setupFiscalPeriodsTab</@ofbizUrl>"
	    },
	    "accountingTransactionsTab": {
	    	"formId": "",
	    	"disabled": ${(!topGlAccountId?has_content)?string},
	    	"contentUrl": "<@ofbizUrl>setupAccountingTransactionsTab</@ofbizUrl>"
	    },
	    "journalTab": {
	    	"formId": "setupAccounting-selectJournalEntry-form",
	    	"disabled": ${(!topGlAccountId?has_content)?string},
	    	"contentUrl": "<@ofbizUrl>setupJournalTab</@ofbizUrl>"
	    },
	    "taxAuthTab": {
	    	"formId": "",
	    	"disabled": ${(!topGlAccountId?has_content)?string},
	    	"contentUrl": "<@ofbizUrl>setupTaxAuthTab</@ofbizUrl>"
	    }
	}

	$(document).ready(function () {		
		tabs = $('a[data-toggle="tab"]');
		// Check whether tabs should be enabled or disabled based on tab config			 
		$(tabs).each(function(i, tab) {	  		
		  	if (tab.hash) {
		  	  var tabId = tab.hash.substring(1, tabs[i].hash.length);
		  	  $('a[href$="' + tabId + '"]').click(function(e) {
				  if (tabSettings[tabId].disabled) {
			  		e.preventDefault();
			        e.stopImmediatePropagation();
			        e.stopPropagation();
			        $(tabs[i]).addClass("disabled");
				  } else {
				  	contentUrl = tabSettings[tabId].contentUrl;
				  	var tabContent = $('#' + tabId).html().trim();				  	
				  	if (contentUrl !== 'default' && tabContent.length == 0) {
					  	$.ajax({
				            type: "POST",
				            url: contentUrl,
				            data: {'orgPartyId' : '${orgPartyId!}', 'topGlAccountId' : '${topGlAccountId!}'},
				            cache: false,
				            async: true,
				            success: function(data) { 
				                $('#' + tabId).html(data);
				            }
				        });
			        }
				  }
			  });
			}
		});

		if (tabs.length > 0) {
			if (typeof Foundation !== "undefined") { 
				$('ul.tabs').on('toggled', customToggleTab);			
				customToggleTab(null, tabs[0]);	
			} else if (typeof  $.fn.tooltip.Constructor.VERSION!== "undefined") { 
				$('a[data-toggle="tab"]').on('shown.bs.tab', customToggleTab);			
				customToggleTab($('li:first-child a[data-toggle="tab"]').tab()[0]);
			} else {
				console.log("Bootstrap or Foundation cannot be found");			 
			}
		}		
	
	
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
	});
</@script>

<@tabs id="accountingTabs">
	<@tab id="preferencesTab" title="Preferences">
		<@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditAcctgPreferences"/>
	</@tab>
	<@tab id="glAccountsTab" title="Configure GL Accounts">
		<#--		
		<@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditGLAccountTree" />
		-->
	</@tab>
	<@tab id="fiscalPeriodsTab" title="Configure Fiscal Periods">
		<#--
		<@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditFiscalPeriods"/>
		-->
	</@tab>
	<#-- SCIPIO (06/13/2018): Commenting this out for now -->
	<@tab id="accountingTransactionsTab" title="Configure Accounting Transactions">
		<#-- 
		<@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditAccountingTransactions"/>
		 -->
	</@tab>
	<#-- SCIPIO (06/14/2018): Commenting this out for now -->
	<@tab id="journalTab" title="Configure Journal">
		 <#--
			<@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditJournals"/>
		 -->
	</@tab>
	<@tab id="taxAuthTab" title="Configure Tax Authorities">
		<#-- 
		<@render type="screen" resource="component://setup/widget/SetupScreens.xml" name="EditTaxAuthorities"/>
		 -->
	</@tab>
</@tabs>

