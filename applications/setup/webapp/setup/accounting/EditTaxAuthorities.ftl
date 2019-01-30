<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {    
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

<#assign etaAllHideShowFormIds = [
    "eaj-newtaxauthority"
]>

<#assign etaObjectProps = {
    "taxauth" : {
        "add" : {
            "type": "form",
            "mode": "show",
            "id": "eta-newtaxauth",
            "formAction": makePageUrl('setupCreateTaxAuth'),
            "defaultParams": wrapRawScript("function() { return; }")
        }
    }
}>

<#macro setupTaxAuthForm id formActionType target params>
    <@form id=id name=id action=makePageUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=[]/>
        <@field type="hidden" name="isAddTaxAuthority" value=(formActionType == "add")?string("Y", "N")/>
        <@field type="hidden" name="isCreateTaxAuthority" value=(formActionType == "new")?string("Y", "N")/>

        <#assign fieldsRequired = true>
        <#if formActionType == "edit">
            <@field type="display" label=uiLabelMap.FormFieldTitle_taxAuthPartyId><#rt/>
                <span class="acctg-managefield acctg-managefield-for-taxAuthPartyId"></span><#t/>
            </@field><#lt/>
            <@field type="hidden" name="taxAuthPartyId" value=(params.taxAuthPartyId!) class="+acctg-inputfield"/>
            <@field type="display" label=uiLabelMap.FormFieldTitle_taxAuthGeoId><#rt/>
                <span class="acctg-managefield acctg-managefield-for-taxAuthGeoId"></span><#t/>
            </@field><#lt/>
            <@field type="hidden" name="taxAuthGeoId" value=(params.taxAuthGeoId!) class="+acctg-inputfield"/>
        <#else>          
        	<@field type="input" name="taxAuthPartyId" label=uiLabelMap.FormFieldTitle_taxAuthPartyId value=(params.taxAuthPartyId!) class="+acctg-inputfield"/>        	
            <@field type="input" name="groupName" label=uiLabelMap.AccountingTaxAuthority value=(params.groupName!) class="+acctg-inputfield"/>            
            <@field type="lookup" name="taxAuthGeoId" label=uiLabelMap.FormFieldTitle_taxAuthGeoId value=(params.taxAuthPartyId!) fieldFormName="LookupGeo" class="+ect-inputfield" required=true/>            
        </#if>
        <@field type="checkbox" name="requireTaxIdForExemption" label=uiLabelMap.FormFieldTitle_requireTaxIdForExemption value="Y" checked=("Y" == params.requireTaxIdForExemption!) />
        <@field type="input" name="taxIdFormatPattern" label=uiLabelMap.FormFieldTitle_taxIdFormatPattern value=(params.taxIdFormatPattern!) class="+acctg-inputfield"/>
        <@field type="checkbox" name="includeTaxInPrice" label=uiLabelMap.FormFieldTitle_includeTaxInPrice value="Y" checked=("Y" == params.includeTaxInPrice!) />
    </@form>
    
</#macro>

<@script>
    var actionTaxAuthProps = <@objectAsScript object=(etaObjectProps!{}) lang='js'/>;
    var taxAuthHideShowFormIds = <@objectAsScript object=(etaAllHideShowFormIds!{}) lang='js'/>;

    jQuery(document).ready(function() {
        jQuery('li.eta-menu-action a').click(function(e) {
            var typeAction = this.id.split('-');
            if (typeAction && typeAction.length == 3) {
                if (typeAction[2] == "add" || typeAction[2] == "edit") {
                    props = actionTaxAuthProps[typeAction[1]][typeAction[2]];
                    if (props.type == 'form' && props.mode == "show") {
                        if (taxAuthHideShowFormIds) {
                            jQuery.each(glJournalHideShowFormIds, function(i, e) {
                                jQuery('#'+e).fadeOut();
                            });
                        }
                        $('#' + props.id).fadeIn();
                        $("input[name=partyId]").val($("input[name=taxAuthPartyId]").val());
                        setupControlMenu.setSubmitFormId(props.id + '-form');
                    }
                }
            }
        });
    });
</@script>

<@section id="mainTaxAuthoritiesSection">
    <#-- TODO: REVIEW: may make a difference later -->
    <@defaultWizardFormFields exclude=["topGlAccountId"]/>
    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
    <@row>
        <@cell medium=9 large=9>
            <@section title=uiLabelMap.AccountingTaxAuthorities>    
                <@table>
                	<#if partyTaxAuthInfos?has_content>
                		<@thead>
	                        <@tr>
	                            <@td>${uiLabelMap.FormFieldTitle_taxAuthPartyId}</@td>
	                            <@td>${uiLabelMap.FormFieldTitle_taxAuthGeoId!}</@td>
	                        </@tr>
                        </@thead>
                        <#list partyTaxAuthInfos as partyTaxAuthInfo>
		                	<@tr>
		                        <@td>${partyTaxAuthInfo.taxAuthPartyId!}</@td>
		                        <@td>${partyTaxAuthInfo.taxAuthGeoId!}</@td>		                    
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
                   <@menuitem contentId="eta-taxauth-add" class="+eta-menu-action" type="link" href="javascript:void(0);" text=uiLabelMap.CommonAdd />
               </li>
            </ul>
          </@section>
        </@cell>
    </@row>
</@section>

<@section title=uiLabelMap.AccountingNewTaxAuthority containerId="eta-newtaxauth" containerClass="+eta-newtaxauth acctg-recordaction acctg-newrecord" 
    containerStyle=((targetRecordAction == "new-new")?string("","display:none;"))>
  <#if targetRecordAction == "newtaxauthid-new">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams
    })>
  </#if>
  <@setupTaxAuthForm id="eta-newtaxauth-form" formActionType="new" target="setupCreateTaxAuth" params=paramMaps.values  />
</@section>