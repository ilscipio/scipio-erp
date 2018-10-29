<#-- SCIPIO: SETUP fiscal periods implementation -->

<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://accounting/webapp/accounting/ledger/tree/treecommon.ftl">

<#assign defaultParams = {    
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

<#assign eajAllHideShowFormIds = [
    "eaj-newgljournal"
]>

<#assign eajObjectProps = {
    "gljournal" : {
        "add" : {
            "type": "form",
            "mode": "show",
            "id": "eat-newgljournal",
            "formAction": makeOfbizUrl('setupCreateGlJournal'),
            "defaultParams": wrapRawScript("function() { return; }")
        }
    }
}>

    
<@form method="get" action=makeOfbizUrl("setupAccounting") id="setupAccounting-selectJournalEntry-form">
    <#-- TODO: REVIEW: may make a difference later -->
    <@defaultWizardFormFields exclude=["topGlAccountId"]/>
    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
    <@row>
        <@cell medium=9 large=9>
            <@section title=uiLabelMap.AccountingGlJournals>                
                <@table>
                    <#if glJournals?has_content>
                        <@tr>
                            <#list glJournals as glJournal>
                                <@td>${glJournal.glJournalId!}</@td>
                                <@td>${glJournal.glJournalName!}</@td>
                            </#list>
                        </@tr>
                    <#else>
                          <@tr type="meta">
                              <@td colspan="9"><@commonMsg type="result-norecord">${uiLabelMap.OrderNoOrderFound}</@commonMsg></@td>
                          </@tr>
                    </#if>
                </@table>
            </@section>  
        </@cell>
        <@cell medium=3 large=3>    
          <#-- ACTIONS MENU -->
          <@section title=uiLabelMap.CommonActions>            
            <#-- MENU -->
            <ul class="side-nav">                   
                <li>
                       <@menuitem type="link" href="javascript:void(0);" text=uiLabelMap.CommonAdd />
                   </li>
               </ul>
          </@section>
        </@cell>
    </@row>
</@form>
