<#-- SCIPIO: SETUP interactive catalog tree implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://accounting/webapp/accounting/common/treecommon.ftl">

<@alert type="warning">WARNING: WORK-IN-PROGRESS</@alert>

<@script>
    function setupShowFormActivatedCallback(form, ai) {
        <#-- special: if this is a category form (check for isCreateCategory hidden input present),
            adjust field visibility for top vs nested cat -->
        if (jQuery('input[name=isCreateCategory]', form).length) {
            refreshScfFieldVisibility(form);
        }
        
        setupControlMenu.setSubmitFormId(form.prop('id'));
    };
</@script>

<#assign egltCallbacks = {
    "showFormActivated": wrapRawScript("setupShowFormActivatedCallback")
}>
<#assign egltAllHideShowFormIds = [
    "eglt-newglaccount", "eglt-editglaccount"
]>
<#assign egltActionProps = {
    "default": {
        "newglaccount": {
            "type": "form",
            "mode": "show",
            "id": "eglt-newglaccount",
            "defaultParams": wrapRawScript("function() { return null; }")
        }
    },
    "glAccount": {
        "edit": {
            "type": "form",
            "mode": "show",
            "id": "eglt-editaccountgl" <#-- NOTE: this can be ancestor container of form, both work (uses first descendent form) -->
            <#-- paramNames* is a preprocess step for easy param renaming before going into link/form
            "paramNames": {"productStoreId": "myProductStoreIdParam" }
            "paramNamesMode": "explicit"|"default"-->
            <#-- replaces the entire default form populate script, must be a single function (ai = ActionInfo object)
            "populateForm": wrapRawScript('function(form, params, ai) { alert("preventing form populate"); return false; }')-->
            <#-- individual form field populate handlers (return false to prevent default/common behavior)
            "populateFormFields": {
                "prodCatalogId": wrapRawScript('function(elem, name, value, form, params, ai) { alert("ignoring this field"); return false; }')
            }-->
        },
        "removeassoc": {
            "type": "form",
            "mode": "submit",
            "confirmMsg": rawLabel('CommonConfirmDeleteRecordAssocPermanent'),
            "id": "ect-removecatalogassoc-form"
        },
        "remove": {
            "type": "form",
            "mode": "submit",
            "confirmMsg": rawLabel('CommonConfirmDeleteRecordPermanent'),
            "id": "ect-removecatalog-form"
        },
        "manage": {
            "type": "link",
            "target": "_blank",
            "url": makeOfbizInterWebappUrl({"uri":'/catalog/control/EditProdCatalog', "extLoginKey":true}),
            "paramNames": {"prodCatalogId": true },
            "paramNamesMode": "explicit"
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
    <@form action=makeOfbizUrl("setupCatalog") method="get">
      <@defaultWizardFormFields/>
    </@form>
  </@section>
</#macro>

<#-- CORE INCLUDE -->
<#include "component://accounting/webapp/accounting/ledger/accountsTree.ftl">
