<#include "component://setup/webapp/setup/common/common.ftl">

<@section>
    <@nav type="steps" activeElem=(setupStep!"organization")>
        <#-- NOTE: the "disabled" logic is overridden and determined by SetupWorker -->
        <#assign states = setupStepStates!>
        <@step name="organization" icon="fa fa-info" href=makeOfbizUrl("setupOrganization") completed=((states.organization.completed)!false) disabled=((states.organization.disabled)!false)>${uiLabelMap.SetupOrganization}</@step>
        <@step name="user" icon="fa fa-info" href=makeOfbizUrl("setupUser") completed=((states.user.completed)!false) disabled=((states.user.disabled)!false)>${uiLabelMap.PartyParty}</@step>
        <@step name="accounting" icon="fa fa-credit-card" href=makeOfbizUrl("setupAccounting") completed=((states.accounting.completed)!false) disabled=((states.accounting.disabled)!false)>${uiLabelMap.AccountingAccounting}</@step>
        <@step name="facility" icon="fa fa-building" href=makeOfbizUrl("setupFacility") completed=((states.facility.completed)!false) disabled=((states.facility.disabled)!false)>${uiLabelMap.SetupFacility}</@step>
        <@step name="catalog" icon="fa fa-info" href=makeOfbizUrl("setupCatalog") completed=((states.catalog.completed)!false) disabled=((states.catalog.disabled)!false)>${uiLabelMap.SetupProductCatalog}</@step>
        <@step name="website" icon="fa fa-info" href=makeOfbizUrl("setupWebsite") completed=((states.website.completed)!false) disabled=((states.website.disabled)!false)>${uiLabelMap.SetupWebSite}</@step>
    </@nav>
</@section>
