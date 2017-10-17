<#include "component://setup/webapp/setup/common/common.ftl">

<@script>
    <#-- DEV NOTE: I wanted to avoid having to write this manually but turns out can't get around it -->
    function populateSetupCategoryForm(form, params, actionProps, node, scth) {
        // TODO
    }
</@script>

<#assign defaultCategoryParams = {
}>
<#assign initialCategoryParamMaps = getWizardFormFieldValueMaps({
    "record":productCategory!true,
    "defaults":defaultCategoryParams
})>
<#macro setupCategoryForm id isCreate target params fixedParams>
    <@form id=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["productCategoryId", "prodCatalogId", "productStoreId", "partyId"]/>
        <@field type="hidden" name="isCreateCategory" value=isCreate?string("Y", "N")/>
        
      <#if !isCreate>
        <@field type="display" label=uiLabelMap.FormFieldTitle_productCategoryId><#rt/>
            <span class="ect-managefield ect-managefield-for-productCategoryId"><@setupExtAppLink uri="/catalog/control/EditCategory?productCategoryId=${rawString(params.productCategoryId!)}" text=params.productCategoryId!/></span><#t/>
        </@field><#lt/>
        <@field type="hidden" name="productCategoryId" value=(params.productCategoryId!)/>
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="productCategoryId" label=uiLabelMap.FormFieldTitle_productCategoryId value=(params.productCategoryId!)/>
      </#if>

        <@field type="hidden" name="partyId" value=(partyId!)/>
        <@field type="hidden" name="productStoreId" value=(productStoreId!)/>
        
        <@field type="display" name="productCategoryTypeId" label=uiLabelMap.FormFieldTitle_productCategoryTypeId><span class="ect-displayfield ect-displayfield-for-productCategoryTypeId">${params.productCategoryTypeId!}</span></@field>
        <@field type="hidden" name="productCategoryTypeId" value=(params.productCategoryTypeId!)/>

        <@field type="input" name="categoryName" value=(params.categoryName!) label=uiLabelMap.FormFieldTitle_categoryName/><#--  not strictly: required=true -->
        <@field type="input" name="description" value=(params.description!) label=uiLabelMap.FormFieldTitle_description/>
        <@field type="input" name="longDescription" value=(params.longDescription!) label=uiLabelMap.FormFieldTitle_longDescription/>

        <#-- TODO: LOCALIZED VERSIONS OF categoryName/description/longDescription (complex) -->

        <@field type="display" name="prodCatalogId" label=uiLabelMap.FormFieldTitle_prodCatalogId><span class="ect-displayfield ect-displayfield-for-prodCatalogId">${params.prodCatalogId!}</span></@field>
        <@field type="hidden" name="prodCatalogId" value=(params.prodCatalogId!)/>
        <#-- TODO: REVIEW: prodCatalogCategoryTypeId -->
        <@field type="display" name="prodCatalogCategoryTypeId" label=uiLabelMap.FormFieldTitle_prodCatalogCategoryTypeId><span class="ect-displayfield ect-displayfield-for-prodCatalogCategoryTypeId">${params.prodCatalogCategoryTypeId!}</span></@field>
        <@field type="hidden" name="prodCatalogCategoryTypeId" value=(params.prodCatalogCategoryTypeId!)/>
        
        <@field type="display" name="parentProductCategoryId" label=uiLabelMap.FormFieldTitle_parentCategoryId><span class="ect-displayfield ect-displayfield-for-parentProductCategoryId">${params.parentProductCategoryId!}</span></@field>
        <@field type="hidden" name="parentProductCategoryId" value=(params.parentProductCategoryId!)/>
        
        <@field type="input" name="sequenceNum" value=(params.sequenceNum!) label=uiLabelMap.ProductSequenceNum required=false/>

        <@field type="display" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate><span class="ect-displayfield ect-displayfield-for-fromDate">${params.fromDate!}</span></@field>
        <@field type="hidden" name="fromDate" value=(params.fromDate!)/>
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
  <@setupCategoryForm id="NewCategory" isCreate=true target="setupCreateCategory" 
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
  <@setupCategoryForm id="EditCategory" isCreate=false target="setupUpdateCategory" 
    params=paramMaps.values fixedParams=paramMaps.fixedValues />
</@section>

<div style="display:none;">
  <@form id="ect-removecategory-form" action=makeOfbizUrl("setupDeleteCategory") method="post">
      <@defaultWizardFormFields exclude=["productCategoryId"]/>
      <@field type="hidden" name="prodCatalogId" value=""/>
      <@field type="hidden" name="productCategoryId" value=""/>
  </@form>
</div>
