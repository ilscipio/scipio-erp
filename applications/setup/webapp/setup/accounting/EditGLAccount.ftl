<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://accounting/webapp/accounting/common/treecommon.ftl">

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
    
    var defaultGlAccountParams = <@objectAsScript object=defaultParams lang='js'/>;
</@script>

<#-- SPECIAL: for this screen because there are multiple forms, we have to ignore isError
    when target was not one of our forms -->
<#assign isGlAccountError = isSetupEventError!false>
<#if targetRecord != "glAccount">
  <#assign isGlAccountError = false/>
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
    "record":glAccount!true,
    "defaults":defaultParams,
    "isError":isGlAccountError,
    "useReqParams":useReqParams
})>
<#macro setupGlAccountForm id formActionType target params treeFieldValues={}>
    <@form id=id name=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=[]/>
        <@egltCommonTreeFormFields params=params initialValues=treeFieldValues/>
       
        <@field type="hidden" name="isAddGlAccount" value=(formActionType == "add")?string("Y", "N")/>
        <@field type="hidden" name="isCreateGlAccount" value=(formActionType == "new")?string("Y", "N")/>
        <@field type="hidden" name="isUpdateGlAccount" value=(formActionType == "edit")?string("Y", "N")/>
        
        <#assign fieldsRequired = true>
      	      	
      	<@field type="display" label=uiLabelMap.FormFieldTitle_parentGlAccountId><#rt/>
            <span class="eglt-managefield eglt-managefield-for-parentGlAccountDesc">            	
            	<@setupExtAppLink uri="/accounting/control/EditGlAccount?glAccountId=${rawString(params.parentGlAccountId!)}" text=params.parentGlAccountDesc!"_NA_"/>
           	</span><#t/>
        </@field><#lt/>
        <@field type="hidden" name="parentGlAccountId" value=(params.parentGlAccountId!) class="+eglt-inputfield"/>
		
		
	    <#if formActionType == "edit">
	        <@field type="display" label=uiLabelMap.FormFieldTitle_glAccountId><#rt/>
	            <span class="eglt-managefield eglt-managefield-for-glAccountId"><@setupExtAppLink uri="/accounting/control/EditGlAccount?glAccountId=${rawString(params.glAccountId!)}" text=params.glAccountId!/></span><#t/>
	        </@field><#lt/>
	        <@field type="hidden" name="glAccountId" value=(params.glAccountId!) class="+eglt-inputfield"/>
	    <#else>
	        <#-- TODO: REVIEW: required=true -->
	        <@field type="input" name="glAccountId" label=uiLabelMap.CommonId value=(params.glAccountId!) class="+eglt-inputfield"/>
	    </#if>

	    
	    <@field type="text" name="accountCode" value=(params.accountCode!) label=uiLabelMap.CommonCode class="+eglt-inputfield" />
	    <@field type="text" name="accountName" value=(params.accountName!) label=uiLabelMap.CommonName class="+eglt-inputfield" />
	    
	    <@field type="select" name="glAccountTypeId" label=uiLabelMap.CommonType class="+eglt-inputfield">
	      <option value="" disabled="disabled"></option>
	      <#list glAccountTypes as glAccountType>
	        <#assign selected = (rawString(params.glAccountTypeId!) == (glAccountType.glAccountTypeId!))>
	        <option value="${glAccountType.glAccountTypeId!}"<#if selected> selected="selected"</#if>>${glAccountType.description!}</option>
	      </#list>
	    </@field>
	    
	    <@field type="select" name="glAccountClassId" label=uiLabelMap.CommonClass class="+eglt-inputfield">
	      <option value="" disabled="disabled"></option>
	      <#list glAccountClasses as glAccountClass>
	        <#assign selected = (rawString(params.glAccountClassId!) == (glAccountClass.glAccountClassId!))>
	        <option value="${glAccountClass.glAccountClassId!}"<#if selected> selected="selected"</#if>>${glAccountClass.description!}</option>
	      </#list>
	    </@field>
	    
	    <@field type="select" name="glResourceTypeId" label=uiLabelMap.CommonResource class="+eglt-inputfield">
	      <option value="" disabled="disabled"></option>
	      <#list glResourceTypes as glResourceType>
	        <#assign selected = (rawString(params.glResourceTypeId!) == (glResourceType.glResourceTypeId!))>
	        <option value="${glResourceType.glResourceTypeId!}"<#if selected> selected="selected"</#if>>${glResourceType.description!}</option>
	      </#list>
	    </@field>
	    
	    <@field type="textarea" name="description" cols="30" rows="3" value=(params.description!) required=false label=uiLabelMap.CommonDescription class="+eglt-inputfield"/>
	       
    </@form>
</#macro>

<@section title=uiLabelMap.PageTitleAddGlAccount containerId="eglt-newglaccount" containerClass="+eglt-newglaccountid eglt-recordaction eglt-newrecord" 
    containerStyle=((targetRecordAction == "glaccount-new")?string("","display:none;"))>
  <#if targetRecordAction == "glaccount-new">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams,
      "isError":isGlAccountError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupGlAccountForm id="NewGlAccount" formActionType="new" target="setupCreateGlAccount" params=paramMaps.values
    treeFieldValues={"egltSubmittedFormId":"NewGlAccount"} <#-- SPECIAL: this form (only) is initially submitted outside the JS tree, so we have to pre-populate treeFieldValues -->
  />
</@section>

<@section title=uiLabelMap.PageTitleEditGlAccount containerId="eglt-editglaccount" containerClass="+ect-editglaccount ect-recordaction ect-editrecord" 
    containerStyle=((targetRecordAction == "glaccount-edit")?string("","display:none;"))>
  <#if targetRecordAction == "glaccount-edit">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isGlAccountError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupGlAccountForm id="EditGlAccount" formActionType="edit" target="setupUpdateGlAccount" params=paramMaps.values/>
</@section>

<div style="display:none;">
<#macro setupDeleteGlAccountForm id target isDeleteRecord>
  <@form id=id action=makeOfbizUrl(target) method="post">
      <@defaultWizardFormFields exclude=["topGlAccountId"]/>
      <@egltCommonTreeFormFields params={}/>
      <@field type="hidden" name="setupContinue" value="N"/>
      <@field type="hidden" name="isDeleteGlAccount" value="Y"/><#-- for our screens -->
      <@field type="hidden" name="deleteRecordAndRelated" value=isDeleteRecord?string("true", "false")/><#-- for Versatile service -->
      <#-- <@field type="hidden" name="deleteAssocMode" value="" class="+ect-inputfield"/><#-- for Versatile service -->
      
      <@field type="hidden" name="glAccountId" value="" class="+eglt-inputfield"/>

  </@form>
</#macro>
  <@setupDeleteGlAccountForm id="eglt-removeglaccount-form" target="setupDeleteGlAccount" isDeleteRecord=true/>  
</div>
