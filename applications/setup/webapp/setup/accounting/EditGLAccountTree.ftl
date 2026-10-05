<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#-- SCIPIO: SETUP interactive catalog tree implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://accounting/webapp/accounting/ledger/tree/treecommon.ftl">

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
            "url": makeServerUrl({"uri":'/accounting/control/EditGlobalGlAccount', "extLoginKey":true}),
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
    <@form action=makePageUrl("setupAccounting") method="get">
      <@defaultWizardFormFields/>
    </@form>
  </@section>
</#macro>

<#-- CORE INCLUDE -->

<#include "component://accounting/webapp/accounting/ledger/tree/EditGLAccountTreeCore.ftl">
