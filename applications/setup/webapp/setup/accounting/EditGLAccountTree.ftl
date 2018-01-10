<#-- SCIPIO: SETUP interactive catalog tree implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://accounting/webapp/accounting/ledger/tree/treecommon.ftl">

<@script>
    function setupShowFormActivatedCallback(form, ai) {
        setupControlMenu.setSubmitFormId(form.prop('id'));
    };
</@script>

<#assign egltCallbacks = {
    "showFormActivated": wrapRawScript("setupShowFormActivatedCallback")
}>
<#assign egltAllHideShowFormIds = [
    "acctg-newglaccount", "acctg-editglaccount"
]>
<#assign egltActionProps = {
    "default": {
        "newglaccount": {
            "type": "form",
            "mode": "show",
            "id": "acctg-newglaccount",
            "defaultParams": wrapRawScript("function() { return defaultGlAccountParams; }")
        }
    },
    "glAccount": {
    	"add": {
            "type": "form",
            "mode": "show",
            "id": "acctg-newglaccount"          
        },
        "edit": {
            "type": "form",
            "mode": "show",
            "id": "acctg-editglaccount" <#-- NOTE: this can be ancestor container of form, both work (uses first descendent form) -->          
        },       
        "remove": {
            "type": "form",
            "mode": "submit",
            "confirmMsg": rawLabel('CommonConfirmDeleteRecordPermanent'),
            "id": "acctg-removeglaccount-form"
        },
        "manage": {
            "type": "link",
            "target": "_blank",
            "url": makeOfbizInterWebappUrl({"uri":'/accounting/control/EditGlobalGlAccount', "extLoginKey":true}),
            "paramNames": {"glAccountId": true }
            
        }
    }
   
}>

<#-- RENDERS SETUP FORMS -->
<#macro egltPostTreeArea extraArgs...>
    <@render type="screen" resource=setupGlAccountForms.location name=setupGlAccountForms.name/>
</#macro>

<#-- RENDERS DISPLAY OPTIONS -->
<#macro egltExtrasArea extraArgs...>
  <@section><#-- title=uiLabelMap.CommonDisplayOptions -->
    <@form action=makeOfbizUrl("setupAccounting") method="get">
      <@defaultWizardFormFields/>
    </@form>
  </@section>
</#macro>

<#-- CORE INCLUDE -->

<#include "component://accounting/webapp/accounting/ledger/tree/EditGLAccountTreeCore.ftl">
