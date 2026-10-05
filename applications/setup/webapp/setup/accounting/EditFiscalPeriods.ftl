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
<#-- SCIPIO: SETUP fiscal periods implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://accounting/webapp/accounting/ledger/tree/treecommon.ftl">

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
            "url": makeServerUrl({"uri":'/accounting/control/EditCustomTimePeriod', "extLoginKey":true}),
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
    <@form action=makePageUrl("setupAccounting") method="get">
      <@defaultWizardFormFields/>
    </@form>
  </@section>
</#macro>

<#-- CORE INCLUDE -->
<#include "component://accounting/webapp/accounting/period/tree/EditCustomTimePeriodCore.ftl">        
