<#-- SCIPIO: DEFAULT interactive catalog tree implementation -->

<#include "component://accounting/webapp/accounting/ledger/tree/treecommon.ftl">

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
            "defaultParams": wrapRawScript("function() { return defaultGlAccountParams; }")
        }
    },
    "glAccount": {
    	"add": {
            "type": "link",
            "target": "_top",
            "url": makeOfbizInterWebappUrl({"uri":'/accounting/control/EditGlobalGlAccount', "extLoginKey":true})            
        },
        "edit": {
            "type": "link",
            "target": "_top",
            "url": makeOfbizInterWebappUrl({"uri":'/accounting/control/EditGlobalGlAccount', "extLoginKey":true}),
            "paramNames": {"glAccountId": true }       
        },       
        "remove": {
            "type": "form",
            "mode": "submit",
            "confirmMsg": rawLabel('CommonConfirmDeleteRecordPermanent'),
            "id": "ect-removeglaccount-form"
        }
    }
   
}>

<#-- CORE INCLUDE -->

<#include "component://accounting/webapp/accounting/ledger/tree/EditGLAccountTreeCore.ftl">
