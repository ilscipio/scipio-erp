<#-- SCIPIO: SETUP Gl Journal implementation -->

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
            "id": "eaj-newgljournal",
            "formAction": makePageUrl('setupCreateGlJournal'),
            "defaultParams": wrapRawScript("function() { return; }")
        }
    }
}>

<#macro setupGlJournalForm id formActionType target params>
    <@form id=id name=id action=makePageUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=[]/>
        <@field type="hidden" name="isAddGlJournal" value=(formActionType == "add")?string("Y", "N")/>
        <@field type="hidden" name="isCreateGlJournal" value=(formActionType == "new")?string("Y", "N")/>

		<@field type="hidden" name="organizationPartyId" value=orgPartyId!/>
        <#assign fieldsRequired = true>
        <#if formActionType == "edit">
            <@field type="display" label=uiLabelMap.FormFieldTitle_glJournalId><#rt/>
                <span class="acctg-managefield acctg-managefield-for-glJournal"></span><#t/>
            </@field><#lt/>
            <@field type="hidden" name="glJournalId" value=(params.glJournalId!) class="+acctg-inputfield"/>
        <#else>
            <#-- TODO: REVIEW: required=true -->
            <@field type="input" name="glJournalId" label=uiLabelMap.FormFieldTitle_glJournalId value=(params.glJournalId!) class="+acctg-inputfield"/>
        </#if>
        <@field type="input" name="glJournalName" label=uiLabelMap.FormFieldTitle_glJournalName value=(params.description!) class="+acctg-inputfield"/>
    </@form>
</#macro>

<@script>
    var actionGlJournalProps = <@objectAsScript object=(eajObjectProps!{}) lang='js'/>;
    var glJournalHideShowFormIds = <@objectAsScript object=(eajAllHideShowFormIds!{}) lang='js'/>;

    jQuery(document).ready(function() {
        jQuery('li.eaj-menu-action a').click(function(e) {
            var typeAction = this.id.split('-');
            if (typeAction && typeAction.length == 3) {
                if (typeAction[2] == "add" || typeAction[2] == "edit") {
                    props = actionGlJournalProps[typeAction[1]][typeAction[2]];
                    if (props.type == 'form' && props.mode == "show") {
                        if (glJournalHideShowFormIds) {
                            jQuery.each(glJournalHideShowFormIds, function(i, e) {
                                jQuery('#'+e).fadeOut();
                            });
                        }
                        $('#' + props.id).fadeIn();
                        setupControlMenu.setSubmitFormId(props.id + '-form');
                    }
                }
            }
        });
    });
</@script>

<@section id="mainGlJournalsSection">
    <@defaultWizardFormFields exclude=["topGlAccountId"]/>
    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
    <@row>
        <@cell medium=9 large=9>
            <@section title=uiLabelMap.AccountingGlJournals>
                <@table>
                    <#if glJournals?has_content>
                    	<@thead>
	                        <@tr>
	                            <@td>${uiLabelMap.FormFieldTitle_glJournalId}</@td>
	                            <@td>${uiLabelMap.FormFieldTitle_glJournalName!}</@td>
	                        </@tr>
                        </@thead>
                        <#list glJournals as glJournal>
                            <@tr>
                                <@td>${glJournal.glJournalId!}</@td>
                                <@td>${glJournal.glJournalName!}</@td>
                            </@tr>
                        </#list>
                    <#else>
                      <@tr type="meta">
                          <@td colspan="9"><@commonMsg type="result-norecord">${uiLabelMap.CommonNoRecordFound}</@commonMsg></@td>
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
                    <@menuitem contentId="eaj-gljournal-add" class="+eaj-menu-action" type="link" href="javascript:void(0);" text=uiLabelMap.CommonAdd />
                </li>
            </ul>
          </@section>
        </@cell>
    </@row>
</@section>

<@section title=uiLabelMap.PageTitleAddGlJournal containerId="eaj-newgljournal" containerClass="+eaj-newgljournalid acctg-recordaction acctg-newrecord" 
    containerStyle=((targetRecordAction == "newgljournal-new")?string("","display:none;"))>
  <#if targetRecordAction == "newgljournal-new">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams
    })>
  </#if>
  <@setupGlJournalForm id="eaj-newgljournal-form" formActionType="new" target="setupCreateGlJournal" params=paramMaps.values  />
</@section>
