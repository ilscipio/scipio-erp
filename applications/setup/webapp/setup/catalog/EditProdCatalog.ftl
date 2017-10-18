<#include "component://setup/webapp/setup/common/common.ftl">

<@script>
    <#-- DEV NOTE: currently nothing here because the JS form populate is using the default
        from ScpCatalogTreeHandler, but if need override later (action props "populateForm"),
        use a function here... 
    function setupPopulateCatalogForm(form, params, ai) {
    
    }    
    -->
</@script>

<#-- SPECIAL: for this screen because there are multiple forms, we have to ignore isError
    when target was not one of our forms -->
<#assign isCatalogError = isError!false>
<#if !parameters.isCreateCatalog?has_content>
  <#assign isCatalogError = false/>
</#if>

<#assign defaultParams = {
    "useQuickAdd": "Y",
    "viewAllowPermReqd": "N",
    "purchaseAllowPermReqd": "N",
    "sequenceNum": defaultSequenceNum!
}>
<#assign initialParamMaps = getWizardFormFieldValueMaps({
    "record":prodCatalogAndStoreAssoc!true,
    "defaults":defaultParams,
    "isError":isCatalogError
})>
<#macro setupCatalogForm id isCreate target params fixedParams>
    <@form id=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["prodCatalogId", "productStoreId", "partyId"]/>
        <@field type="hidden" name="partyId" value=(partyId!)/>
        <@field type="hidden" name="productStoreId" value=(productStoreId!)/>
        <@field type="hidden" name="isCreateCatalog" value=isCreate?string("Y", "N")/>
        
        <#--<field use-when="prodCatalog==null&amp;&amp;prodCatalogId==null" name="prodCatalogId" required-field="true"><text default-value="${partyId}"/>-->
      <#if !isCreate>
        <@field type="display" label=uiLabelMap.FormFieldTitle_prodCatalogId><#rt/>
            <span class="ect-managefield ect-managefield-for-prodCatalogId"><@setupExtAppLink uri="/catalog/control/EditProdCatalog?prodCatalogId=${rawString(params.prodCatalogId!)}" text=params.prodCatalogId!/></span><#t/>
        </@field><#lt/>
        <@field type="hidden" name="prodCatalogId" value=(params.prodCatalogId!) class="+ect-inputfield"/>
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="prodCatalogId" label=uiLabelMap.FormFieldTitle_prodCatalogId value=(params.prodCatalogId!) class="+ect-inputfield"/>
      </#if>

        <@field type="input" name="catalogName" value=(params.catalogName!) label=uiLabelMap.FormFieldTitle_prodCatalogName required=true size="30" maxlength="60" class="+ect-inputfield"/>
        <@field type="input" name="sequenceNum" value=(params.sequenceNum!) label=uiLabelMap.ProductSequenceNum class="+ect-inputfield"/>
        <@field type="hidden" name="fromDate" value=(params.fromDate!) class="+ect-inputfield"/>

        <@field type="hidden" name="useQuickAdd" value=(fixedParams.useQuickAdd!)/>
        <@field type="hidden" name="styleSheet" value=(params.styleSheet!) class="+ect-inputfield"/>
        <@field type="hidden" name="headerLogo" value=(params.headerLogo!) class="+ect-inputfield"/>
        <@field type="hidden" name="contentPathPrefix" value=(params.contentPathPrefix!) class="+ect-inputfield"/>
        <@field type="hidden" name="templatePathPrefix" value=(params.templatePathPrefix!) class="+ect-inputfield"/>
        <@field type="hidden" name="viewAllowPermReqd" value=(fixedParams.viewAllowPermReqd!) class="+ect-inputfield"/>
        <@field type="hidden" name="purchaseAllowPermReqd" value=(fixedParams.purchaseAllowPermReqd!) class="+ect-inputfield"/>
    </@form>
</#macro>
<@section title=uiLabelMap.ProductNewCatalog containerId="ect-newcatalog" containerClass="+ect-newcatalog ect-recordaction ect-newrecord" 
    containerStyle=((targetRecord == "catalog" && isCreate)?string("","display:none;"))>
  <#if (targetRecord == "catalog" && isCreate)>
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams,
      "isError":isCatalogError
    })>
  </#if>
  <@setupCatalogForm id="NewCatalog" isCreate=true target="setupCreateCatalog" 
    params=paramMaps.values fixedParams=paramMaps.fixedValues />
</@section>
<@section title=uiLabelMap.ProductEditCatalog containerId="ect-editcatalog" containerClass="+ect-editcatalog ect-recordaction ect-editrecord" 
    containerStyle=((targetRecord == "catalog" && !isCreate)?string("","display:none;"))>
  <#if (targetRecord == "catalog" && !isCreate)>
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isCatalogError
    })>
  </#if>
  <@setupCatalogForm id="EditCatalog" isCreate=false target="setupUpdateCatalog" 
    params=paramMaps.values fixedParams=paramMaps.fixedValues />
</@section>

<div style="display:none;">
<#macro setupDeleteCatalogForm id>
  <@form id=id action=makeOfbizUrl("setupDeleteCatalog") method="post">
      <@defaultWizardFormFields exclude=["prodCatalogId"]/>
      <@field type="hidden" name="setupContinue" value="N"/>
      <@field type="hidden" name="isDeleteCatalog" value="Y"/>
      
      <@field type="hidden" name="prodCatalogId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="productStoreId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="fromDate" value="" class="+ect-inputfield"/>
  </@form>
</#macro>
  <@setupDeleteCatalogForm id="ect-removecatalog-form"/>
</div>
