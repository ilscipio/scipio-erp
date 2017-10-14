<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {
    "useQuickAdd": "Y",
    "viewAllowPermReqd": "N",
    "purchaseAllowPermReqd": "N",
    "sequenceNum": defaultSequenceNum!
}>
<#assign initialParamMaps = getWizardFormFieldValueMaps({
    "record":prodCatalogAndStoreAssoc!true,
    "defaults":defaultParams
})>
<#macro newEditCatalogForm id isCreate target params fixedParams>
    <@form id=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["prodCatalogId", "productStoreId", "partyId"]/>
        <@field type="hidden" name="isCreateCatalog" value=isCreate?string("Y", "N")/>
        
        <#--<field use-when="prodCatalog==null&amp;&amp;prodCatalogId==null" name="prodCatalogId" required-field="true"><text default-value="${partyId}"/>-->
      <#if !isCreate>
        <@field type="display" label=uiLabelMap.FormFieldTitle_prodCatalogId><#rt/>
            <@setupExtAppLink uri="/catalog/control/EditProdCatalog?prodCatalogId=${rawString(params.prodCatalogId!)}" text=params.prodCatalogId!/><#t/>
        </@field><#lt/>
        <@field type="hidden" name="prodCatalogId" value=(params.prodCatalogId!)/>
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="prodCatalogId" label=uiLabelMap.FormFieldTitle_prodCatalogId value=(params.prodCatalogId!)/>
      </#if>

        <@field type="input" name="catalogName" value=(params.catalogName!) label=uiLabelMap.FormFieldTitle_prodCatalogName required=true size="30" maxlength="60"/>
        <@field type="input" name="sequenceNum" value=(params.sequenceNum!) label=uiLabelMap.ProductSequenceNum required=false/>
        <@field type="hidden" name="fromDate" value=(params.fromDate!)/>

        <@field type="hidden" name="partyId" value=(partyId!)/>
        <@field type="hidden" name="productStoreId" value=(productStoreId!)/>
        
        <@field type="hidden" name="useQuickAdd" value=(fixedParams.useQuickAdd!)/>
        
        <@field type="hidden" name="styleSheet" value=(params.styleSheet!)/>
        <@field type="hidden" name="headerLogo" value=(params.headerLogo!)/>
        <@field type="hidden" name="contentPathPrefix" value=(params.contentPathPrefix!)/>
        <@field type="hidden" name="templatePathPrefix" value=(params.templatePathPrefix!)/>
        
        <@field type="hidden" name="viewAllowPermReqd" value=(fixedParams.viewAllowPermReqd!)/>
        <@field type="hidden" name="purchaseAllowPermReqd" value=(fixedParams.purchaseAllowPermReqd!)/>
    </@form>
</#macro>
<@section title=uiLabelMap.ProductNewCatalog containerId="ect-newcatalog" containerClass="+ect-newcatalog ect-recordaction ect-newrecord" 
    containerStyle=((targetRecord == "catalog" && isCreate)?string("","display:none;"))>
  <#if (targetRecord == "catalog" && isCreate)>
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams
    })>
  </#if>
  <@newEditCatalogForm id="NewCatalog" isCreate=true target="setupCreateCatalog" 
    params=paramMaps.values fixedParams=paramMaps.fixedValues />
</@section>
<@section title=uiLabelMap.ProductEditCatalog containerId="ect-editcatalog" containerClass="+ect-editcatalog ect-recordaction ect-editrecord" 
    containerStyle=((targetRecord == "catalog" && !isCreate)?string("","display:none;"))>
  <#if (targetRecord == "catalog" && !isCreate)>
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams
    })>
  </#if>
  <@newEditCatalogForm id="EditCatalog" isCreate=false target="setupUpdateCatalog" 
    params=paramMaps.values fixedParams=paramMaps.fixedValues />
</@section>

<div style="display:none;">
  <@form id="ect-removecatalog-form" action=makeOfbizUrl("setupDeleteCatalog") method="post">
      <@defaultWizardFormFields exclude=["prodCatalogId"]/>
      <@field type="hidden" name="prodCatalogId" value=""/>
  </@form>
</div>
