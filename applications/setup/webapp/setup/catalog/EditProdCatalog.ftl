<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://product/webapp/catalog/catalog/tree/treecommon.ftl">

<#assign defaultParams = {
    "useQuickAdd": "Y",
    "viewAllowPermReqd": "N",
    "purchaseAllowPermReqd": "N",
    "sequenceNum": defaultSequenceNum!
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
<#assign isCatalogError = isSetupEventError!false>
<#if !parameters.isCreateCatalog?has_content>
  <#assign isCatalogError = false/>
</#if>

<#-- SPECIAL: for this screen because there are multiple forms, we have to disable useRequestParameters
    when target was not one of our forms -->
<#assign useReqParams = "">
<#if targetRecord != "catalog" || isDeleteCatalogSuccess>
  <#assign useReqParams = false>
</#if>

<#-- DEV NOTE: WARN: avoid fixedParams here, not appropriate for this screen -->
<#assign initialParamMaps = getWizardFormFieldValueMaps({
    "record":prodCatalogAndStoreAssoc!true,
    "defaults":defaultParams,
    "isError":isCatalogError,
    "useReqParams":useReqParams
})>
<#macro setupCatalogForm id isCreateForm target params>
    <@form id=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["prodCatalogId", "productStoreId", "partyId"]/>
        <@ectCommonTreeFormFields params=params/>
        <@field type="hidden" name="partyId" value=(partyId!)/>
        <@field type="hidden" name="productStoreId" value=(productStoreId!)/>
        <@field type="hidden" name="isCreateCatalog" value=isCreateForm?string("Y", "N")/>
        
        <#--<field use-when="prodCatalog==null&amp;&amp;prodCatalogId==null" name="prodCatalogId" required-field="true"><text default-value="${partyId}"/>-->
      <#if !isCreateForm>
        <@field type="display" label=uiLabelMap.FormFieldTitle_prodCatalogId><#rt/>
            <span class="ect-managefield ect-managefield-for-prodCatalogId"><@setupExtAppLink uri="/catalog/control/EditProdCatalog?prodCatalogId=${rawString(params.prodCatalogId!)}" text=params.prodCatalogId!/></span><#t/>
        </@field><#lt/>
        <@field type="hidden" name="prodCatalogId" value=(params.prodCatalogId!) class="+ect-inputfield"/>
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="prodCatalogId" label=uiLabelMap.FormFieldTitle_prodCatalogId value=(params.prodCatalogId!) class="+ect-inputfield"/>
      </#if>

        <@field type="input" name="sequenceNum" value=(params.sequenceNum!) label=uiLabelMap.ProductSequenceNum class="+ect-inputfield"/>

      <#if !isCreateForm>
        <@field type="display" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate><span class="ect-displayfield ect-displayfield-for-fromDate">${params.fromDate!}</span></@field>
        <@field type="hidden" name="fromDate" value=(params.fromDate!) class="+ect-inputfield"/>
      <#else>
        <@field type="datetime" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate value=(params.fromDate!) class="+ect-inputfield"/>
      </#if>
      
        <@field type="input" name="catalogName" value=(params.catalogName!) label=uiLabelMap.FormFieldTitle_prodCatalogName required=true class="+ect-inputfield"/><#-- depends on DB: size="30" maxlength="60" -->

        <@field type="hidden" name="useQuickAdd" value=(params.useQuickAdd!)/>
        <@field type="hidden" name="styleSheet" value=(params.styleSheet!) class="+ect-inputfield"/>
        <@field type="hidden" name="headerLogo" value=(params.headerLogo!) class="+ect-inputfield"/>
        <@field type="hidden" name="contentPathPrefix" value=(params.contentPathPrefix!) class="+ect-inputfield"/>
        <@field type="hidden" name="templatePathPrefix" value=(params.templatePathPrefix!) class="+ect-inputfield"/>
        <@field type="hidden" name="viewAllowPermReqd" value=(params.viewAllowPermReqd!) class="+ect-inputfield"/>
        <@field type="hidden" name="purchaseAllowPermReqd" value=(params.purchaseAllowPermReqd!) class="+ect-inputfield"/>
    </@form>
</#macro>
<@section title=uiLabelMap.ProductNewCatalog containerId="ect-newcatalog" containerClass="+ect-newcatalog ect-recordaction ect-newrecord" 
    containerStyle=((targetRecord == "catalog" && isCreateForm)?string("","display:none;"))>
  <#if (targetRecord == "catalog" && isCreateForm)>
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams,
      "isError":isCatalogError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupCatalogForm id="NewCatalog" isCreateForm=true target="setupCreateCatalog" params=paramMaps.values/>
</@section>
<@section title=uiLabelMap.ProductEditCatalog containerId="ect-editcatalog" containerClass="+ect-editcatalog ect-recordaction ect-editrecord" 
    containerStyle=((targetRecord == "catalog" && !isCreateForm)?string("","display:none;"))>
  <#if (targetRecord == "catalog" && !isCreateForm)>
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isCatalogError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupCatalogForm id="EditCatalog" isCreateForm=false target="setupUpdateCatalog" params=paramMaps.values/>
</@section>

<div style="display:none;">
<#macro setupDeleteCatalogForm id isDeleteRecord>
  <@form id=id action=makeOfbizUrl("setupDeleteCatalog") method="post">
      <@defaultWizardFormFields exclude=["prodCatalogId", "productStoreId"]/>
      <@ectCommonTreeFormFields params={}/>
      <@field type="hidden" name="setupContinue" value="N"/>
      <@field type="hidden" name="isDeleteCatalog" value="Y"/><#-- for our screens -->
      <@field type="hidden" name="deleteProdCatalog" value=isDeleteRecord?string("true", "false")/><#-- for Versatile service -->
      <@field type="hidden" name="deleteAssocMode" value="" class="+ect-inputfield"/><#-- for Versatile service -->
      
      <@field type="hidden" name="prodCatalogId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="productStoreId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="fromDate" value="" class="+ect-inputfield"/>
  </@form>
</#macro>
  <@setupDeleteCatalogForm id="ect-removecatalog-form" isDeleteRecord=true/>
  <@setupDeleteCatalogForm id="ect-removecatalogassoc-form" isDeleteRecord=false/>
</div>
