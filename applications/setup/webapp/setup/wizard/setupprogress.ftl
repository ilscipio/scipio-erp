<#include "component://setup/webapp/setup/common/common.ftl">

<@section>
    <@nav type="steps" activeElem=(activeStep!"organization")>
        <@step name="organization" icon="fa fa-info" href=makeOfbizUrl("setupOrganization")>${uiLabelMap.SetupOrganization}</@step>
        <@step name="user" icon="fa fa-info" href=makeOfbizUrl("setupUser")>${uiLabelMap.PartyParty}</@step>
        <@step name="accounting" icon="fa fa-credit-card" href=makeOfbizUrl("setupAccounting")>${uiLabelMap.AccountingAccounting}</@step>
        <@step name="facility" icon="fa fa-building" href=makeOfbizUrl("setupFacility")>${uiLabelMap.SetupFacility}</@step>
        <@step name="catalog" icon="fa fa-info" href=makeOfbizUrl("setupCatalog")>${uiLabelMap.SetupProductCatalog}</@step>
        <@step name="website" icon="fa fa-info" href=makeOfbizUrl("setupWebsite")>${uiLabelMap.SetupWebSite}</@step>
    </@nav>
</@section>
