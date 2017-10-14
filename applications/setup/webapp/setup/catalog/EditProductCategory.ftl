<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultCategoryParams = {
}>
<#assign initialCategoryParamMaps = getWizardFormFieldValueMaps({
    "record":productCategory!true,
    "defaults":defaultCategoryParams
})>
<#macro newEditCategoryForm id isCreate target params fixedParams>
    <@form id=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["productCategoryId", "prodCatalogId", "productStoreId", "partyId"]/>
        <@field type="hidden" name="isCreateCategory" value=isCreate?string("Y", "N")/>
        
      <#if !isCreate>
        <@field type="display" label=uiLabelMap.FormFieldTitle_productCategoryId><#rt/>
            <@setupExtAppLink uri="/catalog/control/EditCategory?productCategoryId=${rawString(params.productCategoryId!)}" text=params.productCategoryId!/><#t/>
        </@field><#lt/>
        <@field type="hidden" name="productCategoryId" value=(params.productCategoryId!)/>
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="productCategoryId" label=uiLabelMap.FormFieldTitle_productCategoryId value=(params.productCategoryId!)/>
      </#if>

        <@field type="input" name="categoryName" value=(params.categoryName!) label=uiLabelMap.FormFieldTitle_categoryName required=true size="30" maxlength="60"/>
        <@field type="input" name="sequenceNum" value=(params.sequenceNum!) label=uiLabelMap.ProductSequenceNum required=false/>
        <@field type="hidden" name="fromDate" value=(params.fromDate!)/>

        <@field type="hidden" name="partyId" value=(partyId!)/>
        <@field type="hidden" name="productStoreId" value=(productStoreId!)/>
        <@field type="hidden" name="prodCatalogId" value=(params.prodCatalogId!)/>
        <@field type="hidden" name="parentProductCatalogId" value=(params.parentProductCatalogId!)/>
    </@form>
</#macro>

<@section title=uiLabelMap.ProductNewCategory containerId="ect-newcategory" containerClass="+ect-newcategory ect-recordaction ect-newrecord" 
    containerStyle=((targetRecord == "category" && isCreate)?string("","display:none;"))>
  <#if (targetRecord == "category" && isCreate)>
    <#assign paramMaps = initialCategoryParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultCategoryParams
    })>
  </#if>
  <@newEditCategoryForm id="NewCategory" isCreate=true target="setupCreateCategory" 
    params=paramMaps.values fixedParams=paramMaps.fixedValues />
</@section>
<@section title=uiLabelMap.ProductEditCategory containerId="ect-editcategory" containerClass="+ect-editcategory ect-recordaction ect-editrecord" 
    containerStyle=((targetRecord == "category" && !isCreate)?string("","display:none;"))>
  <#if (targetRecord == "category" && !isCreate)>
    <#assign paramMaps = initialCategoryParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultCategoryParams
    })>
  </#if>
  <@newEditCategoryForm id="EditCategory" isCreate=false target="setupUpdateCategory" 
    params=paramMaps.values fixedParams=paramMaps.fixedValues />
</@section>

<div style="display:none;">
  <@form id="ect-removecategory-form" action=makeOfbizUrl("setupDeleteCategory") method="post">
      <@defaultWizardFormFields exclude=["productCategoryId"]/>
      <@field type="hidden" name="prodCatalogId" value=""/>
      <@field type="hidden" name="productCategoryId" value=""/>
  </@form>
</div>
