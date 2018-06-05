<#-- SCIPIO: SETUP fiscal periods implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://accounting/webapp/accounting/ledger/tree/treecommon.ftl">

<@script>
    function setupShowFormActivatedCallback(form, ai) {
        setupControlMenu.setSubmitFormId(form.prop('id'));
    };
</@script>

<#assign efpCallbacks = {
    "showFormActivated": wrapRawScript("setupShowFormActivatedCallback")
}>
<#assign efpAllHideShowFormIds = [
    "acctg-newtimeperiod", "acctg-edittimeperiod"
]>
<#assign efpActionProps = {
	"default": {
        "add": {
            "type": "form",
            "mode": "show",
            "id": "acctg-newtimeperiod",
            "defaultParams": wrapRawScript("function() { return defaultGlAccountParams; }")
        }
    }, 
	"timePeriod": {   
	    "add": {
	        "type": "form",
	        "mode": "show",
	        "id": "acctg-newtimeperiod"
	    },
	    "edit": {
	    	"type": "form",
	        "mode": "show",
	        "id": "acctg-edittimeperiod"	        
	    },
	    "remove": {
	    	"type": "form",
            "mode": "submit",
            "confirmMsg": rawLabel('CommonConfirmDeleteRecordPermanent'),
            "id": "acctg-removetimeperiod-form"
	    },
	    "manage": {
            "type": "link",
            "target": "_blank",
            "url": makeOfbizInterWebappUrl({"uri":'/accounting/control/EditCustomTimePeriod', "extLoginKey":true}),
            "paramNames": {"customTimePeriodId": true }            
        }
    }
}>

<#-- RENDERS SETUP FORMS -->
<#macro efpPostTreeArea extraArgs...>
    <@render type="screen" resource=setupTimePeriodForms.location name=setupTimePeriodForms.name/>    
</#macro>

<#-- RENDERS DISPLAY OPTIONS -->
<#macro efpExtrasArea extraArgs...>
  <@section><#-- title=uiLabelMap.CommonDisplayOptions -->
    <@form action=makeOfbizUrl("setupAccounting") method="get">
      <@defaultWizardFormFields/>
    </@form>
  </@section>
</#macro>

<#-- CORE INCLUDE -->
<#include "component://accounting/webapp/accounting/period/tree/EditCustomTimePeriodCore.ftl">		
