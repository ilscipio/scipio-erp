<#include "component://setup/webapp/setup/common/common.ftl">

<@alert type="warning">WARNING: WORK-IN-PROGRESS, category forms have no effect.</@alert>

<#assign ectActionProps = {
    "catalog": {
        "edit": {
            "type": "form",
            "mode": "show",
            "id": "ect-editcatalog" <#-- NOTE: this can be ancestor container of form, both work (uses first descendent form) -->
            <#-- paramNames* is a preprocess step for easy param renaming before going into link/form
            "paramNames": {"productStoreId": "myProductStoreIdParam" }
            "paramNamesMode": "explicit"|"default"-->
            <#-- replaces the entire default form populate script, must be a single function
            "populateForm": wrapRawScript('function(form, params, actionProps, node, scth) { alert("preventing form populate"); return false; }')-->
            <#-- individual form field populate handlers (return false to prevent default/common behavior)
            "populateFormFields": {
                "prodCatalogId": wrapRawScript('function(k, v, form, params, actionProps, node, scth) { alert("ignoring this field"); return false; }')
            }-->
        },
        "remove": {
            "type": "form",
            "mode": "submit",
            "confirmMsg": rawLabel('CommonConfirmDeleteRecordPermanent'),
            "id": "ect-removecatalog-form"
        },
        "newcategory": {
            "type": "form",
            "mode": "show",
            "id": "ect-newcategory"
        },
        "manage": {
            "type": "link",
            "target": "_blank",
            "url": makeOfbizInterWebappUrl({"uri":'/catalog/control/EditProdCatalog', "extLoginKey":true})
        }
    },
    "category": {
        "edit": {
            "type": "form",
            "mode": "show",
            "id": "ect-editcategory"
        },
        "remove": {
            "type": "form",
            "mode": "submit",
            "confirmMsg": rawLabel('CommonConfirmDeleteRecordPermanent'),
            "id": "ect-removecategory-form"
        },
        "newcategory": {
            "type": "form",
            "mode": "show",
            "id": "ect-newcategory"
        },
        "manage": {
            "type": "link",
            "target": "_blank",
            "url": makeOfbizInterWebappUrl({"uri":'/catalog/control/EditCategory', "extLoginKey":true})
        }
    },
    "product": {
        "manage": {
            "type": "link",
            "target": "_blank",
            "url": makeOfbizInterWebappUrl({"uri":'/catalog/control/ViewProduct', "extLoginKey":true})
        }
    }
}>
<#assign ectAllHideShowFormIds = [
    "ect-editcatalog", "ect-newcategory", "ect-editcategory", "ect-newcategory"
]>

<#macro ectExtrasArea>
  <@section title=uiLabelMap.CommonDisplayOptions>
    <@form action=makeOfbizUrl("setupCatalog") method="get">
      <@defaultWizardFormFields/>    
      <@field type="input" name="setupEctMaxProductsPerCat" label=uiLabelMap.ProductMaxProductsPerCategory 
        value=(ectMaxProductsPerCat!) tooltip=uiLabelMap.SetupLargeStoreSlowSettingWarning/>
      <@field type="submit"/>
    </@form>
  </@section>
</#macro>

<#-- TODO: this will move later -->
<#include "EditCatalogTreeCommon.ftl">

