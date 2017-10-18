<#include "component://setup/webapp/setup/common/common.ftl">

<#assign setupCategoryTargetFields = [
    

]>
<@script>
    <#-- NOTE: also called from setupShowFormActivatedCallback -->
    function refreshScfFieldVisibility(form) {
        var parentProductCategoryId = jQuery('[name=parentProductCategoryId]', form).filter(':input').val();
        if (parentProductCategoryId) {
            jQuery('.epc-field-prodCatalogCategoryTypeId', form).hide();
            jQuery('.epc-field-parentProductCategoryId', form).show();
        } else {
            jQuery('.epc-field-prodCatalogCategoryTypeId', form).show();
            jQuery('.epc-field-parentProductCategoryId', form).hide();
        }
    }
        
    jQuery(document).ready(function() {
        refreshScfFieldVisibility(jQuery('#NewCategory'));
        refreshScfFieldVisibility(jQuery('#EditCategory'));
    });
    
    <#-- DEV NOTE: currently nothing here because the JS form populate is using the default
        from ScpCatalogTreeHandler, but if need override later (action props "populateForm"),
        use a function here... 
    function setupPopulateCategoryForm(form, params, ai) {
    
    }       
    -->
</@script>

<#-- SPECIAL: for this screen because there are multiple forms, we have to ignore isError
    when target was not a submit for one of our forms -->
<#assign isCategoryError = isError!false>
<#if !parameters.isCreateCategory?has_content>
  <#assign isCategoryError = false/>
</#if>

<#assign defaultCategoryParams = {
}>
<#assign initialCategoryParamMaps = getWizardFormFieldValueMaps({
    "record":productCategoryAndAssoc!true,
    "defaults":defaultCategoryParams,
    "isError":isCategoryError
})>
<#macro setupCategoryForm id isCreate target params fixedParams>
    <@form id=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["productCategoryId", "prodCatalogId", "productStoreId", "partyId"]/>
        <@field type="hidden" name="partyId" value=(partyId!)/>
        <@field type="hidden" name="productStoreId" value=(productStoreId!)/>
        <@field type="hidden" name="isCreateCategory" value=isCreate?string("Y", "N")/>
        
      <#if !isCreate>
        <@field type="display" label=uiLabelMap.FormFieldTitle_productCategoryId><#rt/>
            <span class="ect-managefield ect-managefield-for-productCategoryId"><@setupExtAppLink uri="/catalog/control/EditCategory?productCategoryId=${rawString(params.productCategoryId!)}" text=params.productCategoryId!/></span><#t/>
        </@field><#lt/>
        <@field type="hidden" name="productCategoryId" value=(params.productCategoryId!) class="+ect-inputfield"/>
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="productCategoryId" label=uiLabelMap.FormFieldTitle_productCategoryId value=(params.productCategoryId!) class="+ect-inputfield"/>
      </#if>

        <@field type="display" name="productCategoryTypeId" label=uiLabelMap.FormFieldTitle_productCategoryTypeId><span class="ect-displayfield ect-displayfield-for-productCategoryTypeId">${params.productCategoryTypeId!}</span></@field>
        <@field type="hidden" name="productCategoryTypeId" value=(params.productCategoryTypeId!) class="+ect-inputfield"/>

        <@field type="input" name="categoryName" value=(params.categoryName!) label=uiLabelMap.FormFieldTitle_categoryName class="+ect-inputfield"/><#--  not strictly: required=true -->
        <@field type="input" name="description" value=(params.description!) label=uiLabelMap.FormFieldTitle_description class="+ect-inputfield"/>
        <@field type="input" name="longDescription" value=(params.longDescription!) label=uiLabelMap.FormFieldTitle_longDescription class="+ect-inputfield"/>

        <#-- TODO: LOCALIZED VERSIONS OF categoryName/description/longDescription (complex) -->

        <@field type="display" name="prodCatalogId" label=uiLabelMap.FormFieldTitle_prodCatalogId><span class="ect-displayfield ect-displayfield-for-prodCatalogId">${params.prodCatalogId!}</span></@field>
        <@field type="hidden" name="prodCatalogId" value=(params.prodCatalogId!) class="+ect-inputfield"/>
        <#-- TODO: REVIEW: prodCatalogCategoryTypeId -->
        <@field type="display" name="prodCatalogCategoryTypeId" label=uiLabelMap.FormFieldTitle_prodCatalogCategoryTypeId containerClass="+epc-field-prodCatalogCategoryTypeId"><span class="ect-displayfield ect-displayfield-for-prodCatalogCategoryTypeId">${params.prodCatalogCategoryTypeId!}</span></@field>
        <@field type="hidden" name="prodCatalogCategoryTypeId" value=(params.prodCatalogCategoryTypeId!) class="+ect-inputfield"/>
        
        <@field type="display" name="parentProductCategoryId" label=uiLabelMap.FormFieldTitle_parentCategoryId containerClass="+epc-field-parentProductCategoryId"><span class="ect-displayfield ect-displayfield-for-parentProductCategoryId">${params.parentProductCategoryId!}</span></@field>
        <@field type="hidden" name="parentProductCategoryId" value=(params.parentProductCategoryId!) class="+ect-inputfield"/>
        
        <@field type="input" name="sequenceNum" value=(params.sequenceNum!) label=uiLabelMap.ProductSequenceNum required=false class="+ect-inputfield"/>

        <@field type="display" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate><span class="ect-displayfield ect-displayfield-for-fromDate">${params.fromDate!}</span></@field>
        <@field type="hidden" name="fromDate" value=(params.fromDate!) class="+ect-inputfield"/>
    </@form>
</#macro>

<@section title=uiLabelMap.ProductNewCategory containerId="ect-newcategory" containerClass="+ect-newcategory ect-recordaction ect-newrecord" 
    containerStyle=((targetRecord == "category" && isCreate)?string("","display:none;"))>
  <#if (targetRecord == "category" && isCreate)>
    <#assign paramMaps = initialCategoryParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultCategoryParams,
      "isError":isCategoryError
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
      "defaults":defaultCategoryParams,
      "isError":isCategoryError
    })>
  </#if>
  <@setupCategoryForm id="EditCategory" isCreate=false target="setupUpdateCategory" 
    params=paramMaps.values fixedParams=paramMaps.fixedValues />
</@section>

<div style="display:none;">
<#macro setupDeleteCategoryForm id isDeleteRecord>
  <@form id=id action=makeOfbizUrl("setupDeleteCategory") method="post">
      <@defaultWizardFormFields exclude=["productCategoryId", "prodCatalogId", "productStoreId"]/>
      <@field type="hidden" name="setupContinue" value="N"/>
      <@field type="hidden" name="isDeleteCategory" value="Y"/>
      <@field type="hidden" name="isDeleteCategoryRecord" value=isDeleteRecord?string("Y","N")/>
      
      <@field type="hidden" name="productCategoryId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="prodCatalogId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="productStoreId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="parentProductCategoryId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="fromDate" value="" class="+ect-inputfield"/>
  </@form>
</#macro>
  <@setupDeleteCategoryForm id="ect-removecategory-form" isDeleteRecord=true/>
  <@setupDeleteCategoryForm id="ect-removecategoryassoc-form" isDeleteRecord=false/>
</div>
