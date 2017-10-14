<#include "component://setup/webapp/setup/common/common.ftl">

<@alert type="warning">WARNING: WORK-IN-PROGRESS, TEMPORARILY BROKEN</@alert>

<#assign etcActionProps = {
    "catalog": {
        "edit": {
            "type": "form",
            "mode": "show",
            "id": "ect-editcatalog" <#-- NOTE: this can be ancestor container of form, both work (uses first descendent form) -->
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
            <#--"paramNames": {"productStoreId": "myProductStoreIdParam" }-->
            <#--"paramNamesMode": "explicit"|"default"-->
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
<#assign etcAllHideShowFormIds = [
    "ect-editcatalog", "ect-newcategory", "ect-editcategory", "ect-newcategory"
]>

<#macro ectExtrasArea>
  <@section title=uiLabelMap.CommonDisplayOptions>
    <@form action=makeOfbizUrl("setupCatalog") method="get">
      <@defaultWizardFormFields/>    
      <@field type="input" name="setupEctMaxProductsPerCat" label=uiLabelMap.ProductMaxProductsPerCategory value=ectMaxProductsPerCat!/>
      <@field type="submit"/>
    </@form>
  </@section>
</#macro>

<#-- TODO: this will move later -->
<#include "EditCatalogTreeCommon.ftl">

