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
    
    var defaultTimePeriodParams = <@objectAsScript object=defaultParams lang='js'/>;
</@script>

<#-- SPECIAL: for this screen because there are multiple forms, we have to ignore isError
    when target was not one of our forms -->
<#assign isTimePeriodError = isSetupEventError!false>
<#if targetRecord != "customTimePeriod">
  <#assign isTimePeriodError = false/>
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
    "record":customTimePeriod!true,
    "defaults":defaultParams,
    "isError":isTimePeriodError,
    "useReqParams":useReqParams
})>
<#macro setupTimePeriodForm id formActionType target params treeFieldValues={}>
    <@form id=id name=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=[]/>
        <@acctgCommonTreeFormFields params=params initialValues=treeFieldValues/>
       
        <@field type="hidden" name="isAddTimePeriod" value=(formActionType == "add")?string("Y", "N")/>
        <@field type="hidden" name="isCreateTimePeriod" value=(formActionType == "new")?string("Y", "N")/>
        <@field type="hidden" name="isUpdateTimePeriod" value=(formActionType == "edit")?string("Y", "N")/>
        
        <#assign fieldsRequired = true>
      	      	
      	<@field type="display" label=uiLabelMap.FormFieldTitle_parentPeriodId><#rt/>      		 
            <span class="acctg-managefield acctg-managefield-for-parentTimePeriodDesc">
            	<@setupExtAppLink uri="/accounting/control/EditCustomTimePeriod?organizationPartyId=${rawString(params.orgPartyId!)}&customTimePeriodId=${rawString(params.parentPeriodId!)}" text=params.parentTimePeriodDesc!"_NA_"/>
           	</span><#t/>           	
        </@field><#lt/>
        <@field type="hidden" name="parentPeriodId" value=(params.parentPeriodId!) class="+acctg-inputfield"/>
		
		
	    <#if formActionType == "edit">
	        <@field type="display" label=uiLabelMap.FormFieldTitle_customTimePeriodTimeId><#rt/>
	            <span class="acctg-managefield acctg-managefield-for-customTimePeriodTimeId">
	            	<@setupExtAppLink uri="/accounting/control/EditCustomTimePeriod?organizationPartyId=${rawString(params.orgPartyId!)}&customTimePeriodId=${rawString(params.customTimePeriodId!)}" text=params.customTimePeriodId!/>
	            </span><#t/>
	        </@field><#lt/>
	        <@field type="hidden" name="customTimePeriodId" value=(params.customTimePeriodId!) class="+acctg-inputfield"/>
	    <#else>
	        <#-- TODO: REVIEW: required=true -->
	        <@field type="input" name="customTimePeriodId" label=uiLabelMap.CommonId value=(params.customTimePeriodId!) class="+acctg-inputfield"/>
	    </#if>

	    <@field type="text" name="periodNum" value=(params.periodNum!) label=uiLabelMap.AccountingPeriodNum class="+acctg-inputfield" />
	    <@field type="text" name="periodName" value=(params.periodName!) label=uiLabelMap.AccountingPeriodName class="+acctg-inputfield" />	    
	    
	    <@field type="select" name="periodTypeId" label=uiLabelMap.CommonType class="+acctg-inputfield">
	      <option value="" disabled="disabled"></option>
	      <#list periodTypes as periodType>
	        <#assign selected = (rawString(params.periodTypeId!) == (periodType.periodTypeId!))>
	        <option value="${periodType.description!}"<#if selected> selected="selected"</#if>>${periodType.description!}</option>
	      </#list>
	    </@field>
	    
	    <@field type="date" name="fromDate" value=(params.fromDate!) required=false label=uiLabelMap.CommonFromDate class="+acctg-inputfield"/>
	    <@field type="date" name="thruDate" value=(params.thruDate!) required=false label=uiLabelMap.CommonThruDate class="+acctg-inputfield"/>
	    
	    <@field type="checkbox" name="isClosed" value=(params.thruDate!) required=false label=uiLabelMap.FormFieldTitle_isClosed class="+acctg-inputfield"/>	       
    </@form>
</#macro>

<@section title=uiLabelMap.PageTitleAddTimePeriod containerId="acctg-newtimeperiod" containerClass="+acctg-newtimeperiod acctg-recordaction acctg-newrecord" 
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
  <@setupTimePeriodForm id="NewTimePeriod" formActionType="new" target="setupCreateTimePeriod" params=paramMaps.values
    treeFieldValues={"acctgSubmittedFormId":"NewTimePeriod"} <#-- SPECIAL: this form (only) is initially submitted outside the JS tree, so we have to pre-populate treeFieldValues -->
  />
</@section>

<@section title=uiLabelMap.PageTitleEditTimePeriod containerId="acctg-edittimeperiod" containerClass="+acctg-edittimeperiod acctg-recordaction acctg-editrecord" 
    containerStyle=((targetRecordAction == "timeperiod-edit")?string("","display:none;"))>
  <#if targetRecordAction == "timeperiod-edit">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isTimePeriodError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupTimePeriodForm id="EditTimePeriod" formActionType="edit" target="setupUpdateTimePeriod" params=paramMaps.values/>
</@section>

<div style="display:none;">
<#macro setupDeleteTimePeriodForm id target isDeleteRecord>
  <@form id=id action=makeOfbizUrl(target) method="post">
      <@defaultWizardFormFields exclude=["topGlAccountId"]/>
      <@acctgCommonTreeFormFields params={}/>
      <@field type="hidden" name="setupContinue" value="N"/>
      <@field type="hidden" name="isDeleteTimePeriod" value="Y"/><#-- for our screens -->
      <@field type="hidden" name="deleteRecordAndRelated" value=isDeleteRecord?string("true", "false")/><#-- for Versatile service -->
      <#-- <@field type="hidden" name="deleteAssocMode" value="" class="+ect-inputfield"/><#-- for Versatile service -->
      
      <@field type="hidden" name="customTimePeriodId" value="" class="+acctg-inputfield"/>

  </@form>
</#macro>
  <@setupDeleteTimePeriodForm id="acctg-removetimeperiod-form" target="setupDeleteTimePeriod" isDeleteRecord=true/>  
</div>
