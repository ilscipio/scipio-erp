<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://accounting/webapp/accounting/common/treecommon.ftl">

<#assign defaultParams = {
    
}>

<@script>
    <#-- DEV NOTE: currently nothing here because the JS form populate is using the default
        from ScpCatalogTreeHandler, but if need override later (action props "populateForm"),
        use a function here... 
    function setupPopulateCatalogForm(form, params, ai) {
    
    }    
    -->
    
    var defaultCatalogParams = <@objectAsScript object=defaultParams lang='js'/>;
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
        <#-- 
        <@field type="hidden" name="partyId" value=(partyId!)/>
        <@field type="hidden" name="productStoreId" value=(productStoreId!)/>
        -->
        <@field type="hidden" name="isAddGlAccount" value=(formActionType == "add")?string("Y", "N")/>
        <@field type="hidden" name="isCreateGlAccount" value=(formActionType == "new")?string("Y", "N")/>
        <@field type="hidden" name="isUpdateGlAccount" value=(formActionType == "edit")?string("Y", "N")/>
        
    <#if formActionType == "add">
      <@field type="select" name="parentGlAccountId" label=uiLabelMap.AccountingParentGlAccountId required=true>
        <#list (parentGlAccountList![]) as parentGlAccount>
          <@field type="option" value=parentGlAccount.glAccountId 
            selected=(rawString(params.glAccountId!) == rawString(parentGlAccount.glAccountId))>${parentGlAccount.accountName!parentGlAccount.glAccountId} [${parentGlAccount.glAccountId}]</@field>
        </#list>
      </@field>
    <#else>
      <#if formActionType == "edit">
        <@field type="display" label=uiLabelMap.FormFieldTitle_prodCatalogId><#rt/>
            <span class="ect-managefield ect-managefield-for-prodCatalogId"><@setupExtAppLink uri="/catalog/control/EditProdCatalog?prodCatalogId=${rawString(params.prodCatalogId!)}" text=params.prodCatalogId!/></span><#t/>
        </@field><#lt/>
        <@field type="hidden" name="prodCatalogId" value=(params.prodCatalogId!) class="+ect-inputfield"/>
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="prodCatalogId" label=uiLabelMap.FormFieldTitle_prodCatalogId value=(params.prodCatalogId!) class="+ect-inputfield"/>
      </#if>
    </#if>

        <@field type="input" name="sequenceNum" value=(params.sequenceNum!) label=uiLabelMap.ProductSequenceNum class="+ect-inputfield"/>

      <#if formActionType == "edit">
        <@field type="display" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate><span class="ect-displayfield ect-displayfield-for-fromDate">${params.fromDate!}</span></@field>
        <@field type="hidden" name="fromDate" value=(params.fromDate!) class="+ect-inputfield"/>
      <#else>
        <@field type="datetime" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate value=(params.fromDate!) class="+ect-inputfield"/>
      </#if>
      
      <#if formActionType != "add">

      </#if>
    </@form>
</#macro>
<@section title=uiLabelMap.AccountingNewGlAccount containerId="eglt-newglaccount" containerClass="+eglt-newglaccountid eglt-recordaction eglt-newrecord" 
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
<@section title=uiLabelMap.AccountingEditGlAccount containerId="eglt-editglaccount" containerClass="+ect-editcatalog ect-recordaction ect-editrecord" 
    containerStyle=((targetRecordAction == "catalog-edit")?string("","display:none;"))>
  <#if targetRecordAction == "catalog-edit">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isCatalogError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupGlAccountForm id="EditGlAccount" formActionType="edit" target="setupUpdateGlAccount" params=paramMaps.values/>
</@section>
<#-- 
<@section title=uiLabelMap.ProductAddExistingCatalog containerId="ect-addcatalog" containerClass="+ect-addcatalog ect-recordaction ect-addrecord" 
    containerStyle=((targetRecordAction == "catalog-add")?string("","display:none;"))>
  <#if targetRecordAction == "catalog-add">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isCatalogError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupGlAccountForm id="AddCatalog" formActionType="add" target="setupAddCatalog" params=paramMaps.values/>
</@section>
 -->
<div style="display:none;">
<#macro setupDeleteGlAccountForm id target isDeleteRecord>
  <@form id=id action=makeOfbizUrl(target) method="post">
      <@defaultWizardFormFields exclude=["prodCatalogId", "productStoreId"]/>
      <@egltCommonTreeFormFields params={}/>
      <@field type="hidden" name="setupContinue" value="N"/>
      <@field type="hidden" name="isDeleteGlAccount" value="Y"/><#-- for our screens -->
      <@field type="hidden" name="deleteRecordAndRelated" value=isDeleteRecord?string("true", "false")/><#-- for Versatile service -->
      <#-- <@field type="hidden" name="deleteAssocMode" value="" class="+ect-inputfield"/><#-- for Versatile service -->
      
      <@field type="hidden" name="glAccountId" value="" class="+eglt-inputfield"/>
      <#-- 
      <@field type="hidden" name="productStoreId" value="" class="+eglt-inputfield"/>
      <@field type="hidden" name="fromDate" value="" class="+eglt-inputfield"/>
       -->
  </@form>
</#macro>
  <@setupDeleteGlAccountForm id="eglt-removeglaccount-form" target="setupDeleteGlAccount" isDeleteRecord=true/>  
</div>
