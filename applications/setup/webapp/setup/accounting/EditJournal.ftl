<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://accounting/webapp/accounting/ledger/tree/treecommon.ftl">

<#assign defaultParams = {
    "topGlAccountId": topGlAccountId!
}>

<@script>
    <#-- DEV NOTE: currently nothing here because the JS form populate is using the default
        from ScpCatalogTreeHandler, but if need override later (action props "populateForm"),
        use a function here... 
    function setupPopulateCatalogForm(form, params, ai) {
    
    }    
    -->
    
    var defaultGlJournalParams = <@objectAsScript object=defaultParams lang='js'/>;
</@script>

<#-- SPECIAL: for this screen because there are multiple forms, we have to ignore isError
    when target was not one of our forms -->
<#assign isJournalError = isSetupEventError!false>
<#if targetRecord != "glJournal">
  <#assign isJournalError = false/>
</#if>

<#-- SPECIAL: for this screen because there are multiple forms, we have to disable useRequestParameters
    when target was not one of our forms -->
<#assign useReqParams = "">
<#-- 
<#if targetRecord != "catalog" || isDeleteCatalogSuccess>
  <#assign useReqParams = false>
</#if>
-->

<#-- DEV NOTE: WARN: avoid fixedParams here, not appropriate for this screen -->
<#assign initialParamMaps = getWizardFormFieldValueMaps({
    "record":glJournal!true,
    "defaults":defaultParams,
    "isError":isJournalError,
    "useReqParams":useReqParams
})>
<#macro setupGlJournalForm id formActionType target params treeFieldValues={}>
    <@form id=id name=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["topGlAccountId"]/>
        <@acctgCommonTreeFormFields params=params initialValues=treeFieldValues/>
       
        <@field type="hidden" name="organizationPartyId" value=(orgPartyId!)/>
        <@field type="hidden" name="isAddGlJournal" value=(formActionType == "add")?string("Y", "N")/>
        <@field type="hidden" name="isCreateGlJournal" value=(formActionType == "new")?string("Y", "N")/>
        <@field type="hidden" name="isUpdateGlJournal" value=(formActionType == "edit")?string("Y", "N")/>
        
        <#assign fieldsRequired = true>
		
	    <#if formActionType == "edit">
	        <@field type="display" label=uiLabelMap.FormFieldTitle_glJournalId><#rt/>
	            <span class="acctg-managefield acctg-managefield-for-glJournalId">
	            	<@setupExtAppLink uri="/accounting/control/EditGlJournal?organizationPartyId=${rawString(params.orgPartyId!)}&customTimePeriodId=${rawString(params.customTimePeriodId!)}" text=params.customTimePeriodId!/>
	            </span><#t/>
	        </@field><#lt/>
	        <@field type="hidden" name="glJournalId" value=(params.glJournalId!) class="+acctg-inputfield"/>
	    <#else>
	        <#-- TODO: REVIEW: required=true -->
	        <@field type="input" name="glJournalId" label=uiLabelMap.CommonId value=(params.glJournalId!) class="+acctg-inputfield"/>
	    </#if>

	    <@field type="text" name="glJournalName" value=(params.glJournalName!) label=uiLabelMap.AccountingGlJournalName class="+acctg-inputfield" />
	    <@field type="checkbox" name="isPosted" label=uiLabelMap.AccountingPosted required=false value=(params.isPosted!) altValue="N" checked=(params.isClosed?has_content && params.isClosed == "Y") />
	    <#--
		    <#if formActionType == "edit">
	        <@field type="display" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate><span class="ect-displayfield ect-displayfield-for-fromDate">${params.fromDate!}</span></@field>
	        <@field type="hidden" name="fromDate" value=(params.fromDate!) class="+ect-inputfield"/>
	      <#else>
	        <@field type="datetime" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate value=(params.fromDate!) class="+ect-inputfield"/>
	      </#if>
	      -->
	    <@field type="datetime" name="fromDate" value=(params.fromDate!"") required=false label=uiLabelMap.CommonFromDate class="+acctg-inputfield acctg-inputdate" dateType="date"/>	    	       
    </@form>
</#macro>

<@section title=uiLabelMap.AccountingCustomTimePeriod containerId="acctg-newtimeperiod" containerClass="+acctg-newtimeperiod acctg-recordaction acctg-newrecord" 
    containerStyle=((targetRecordAction == "timeperiod-new")?string("","display:none;"))>
  <#if targetRecordAction == "timeperiod-new">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams,
      "isError":isTimePeriodError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupJournalForm id="NewTimePeriod" formActionType="new" target="setupCreateTimePeriod" params=paramMaps.values
    treeFieldValues={"acctgSubmittedFormId":"NewTimePeriod"} <#-- SPECIAL: this form (only) is initially submitted outside the JS tree, so we have to pre-populate treeFieldValues -->
  />
</@section>

<@section title=uiLabelMap.AccountingEditJournal containerId="acctg-editjournal" containerClass="+acctg-editjournal acctg-recordaction acctg-editrecord" 
    containerStyle=((targetRecordAction == "journal-edit")?string("","display:none;"))>
  <#if targetRecordAction == "journal-edit">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isJournalError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupJournalForm id="EditTimePeriod" formActionType="edit" target="setupUpdateTimePeriod" params=paramMaps.values/>
</@section>

<div style="display:none;">
<#macro setupDeleteJournalForm id target isDeleteRecord>
  <@form id=id action=makeOfbizUrl(target) method="post">
      <@defaultWizardFormFields exclude=["topGlAccountId"]/>
      <@acctgCommonTreeFormFields params={}/>
      <@field type="hidden" name="setupContinue" value="N"/>
      <@field type="hidden" name="isDeleteJournal" value="Y"/><#-- for our screens -->
      <@field type="hidden" name="deleteRecordAndRelated" value=isDeleteRecord?string("true", "false")/><#-- for Versatile service -->
      <#-- <@field type="hidden" name="deleteAssocMode" value="" class="+ect-inputfield"/><#-- for Versatile service -->
      
      <@field type="hidden" name="customJournalId" value="" class="+acctg-inputfield"/>

  </@form>
</#macro>
  <@setupDeleteJournalForm id="acctg-removejournal-form" target="setupDeleteJournal" isDeleteRecord=true/>  
</div>
