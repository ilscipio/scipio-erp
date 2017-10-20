<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://product/webapp/catalog/catalog/tree/treecommon.ftl">

<#assign defaultParams = {
    "prodCatalogCategoryTypeId": "PCCT_BROWSE_ROOT",
    "productCategoryTypeId": "CATALOG_CATEGORY"
}>

<@script>
    var defaultCategoryParams = <@objectAsScript object=defaultParams lang='js'/>;

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
<#assign isCategoryError = isSetupEventError!false>
<#if !parameters.isCreateCategory?has_content>
  <#assign isCategoryError = false/>
</#if>

<#-- SPECIAL: for this screen because there are multiple forms, we have to disable useRequestParameters
    when target was not one of our forms -->
<#assign useReqParams = "">
<#if targetRecord != "category" || isDeleteCategorySuccess>
  <#assign useReqParams = false>
</#if>

<#-- DEV NOTE: WARN: avoid fixedParams here, not appropriate for this screen -->
<#assign initialCategoryParamMaps = getWizardFormFieldValueMaps({
    "record":productCategoryAndAssoc!true,
    "defaults":defaultParams,
    "isError":isCategoryError,
    "useReqParams":useReqParams
})>
<#macro setupCategoryForm id isCreate target params>
    <@form id=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["productCategoryId", "prodCatalogId", "productStoreId", "partyId"]/>
        <@ectCommonTreeFormFields params=params/>
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

        <#-- DEV NOTE: some of these aren't selectable because it should be done through the js tree. -->

        <@field type="select" label=uiLabelMap.ProductProductCategoryType name="productCategoryTypeId" class="+ect-inputfield" required=true>
            <#list productCategoryTypes as productCategoryTypeData>
                <option<#if rawString(params.productCategoryTypeId!) == rawString(productCategoryTypeData.productCategoryTypeId!)> selected="selected"</#if> value="${productCategoryTypeData.productCategoryTypeId}">${productCategoryTypeData.get("description", locale)}</option>
            </#list>
        </@field>

        <@field type="display" name="prodCatalogId" label=uiLabelMap.ProductCatalog><span class="ect-displayfield ect-displayfield-for-prodCatalogId">${params.prodCatalogId!}</span></@field>
        <@field type="hidden" name="prodCatalogId" value=(params.prodCatalogId!) class="+ect-inputfield"/>

        <@field type="select" label=uiLabelMap.ProductCatalogCategoryType name="prodCatalogCategoryTypeId" class="+ect-inputfield" containerClass="+epc-field-prodCatalogCategoryTypeId" required=true>
          <#list prodCatalogCategoryTypes as prodCatalogCategoryTypeData>                       
            <option<#if rawString(params.prodCatalogCategoryTypeId!) == rawString(prodCatalogCategoryTypeData.prodCatalogCategoryTypeId!)> selected="selected"</#if> value="${prodCatalogCategoryTypeData.prodCatalogCategoryTypeId}">${prodCatalogCategoryTypeData.get("description", locale)}</option>
          </#list>
        </@field>

        <@field type="display" name="parentProductCategoryId" label=uiLabelMap.ProductParentCategory containerClass="+epc-field-parentProductCategoryId"><span class="ect-displayfield ect-displayfield-for-parentProductCategoryId">${params.parentProductCategoryId!}</span></@field>
        <@field type="hidden" name="parentProductCategoryId" value=(params.parentProductCategoryId!) class="+ect-inputfield"/>
        
        <@field type="input" name="sequenceNum" value=(params.sequenceNum!) label=uiLabelMap.ProductSequenceNum class="+ect-inputfield"/>

      <#if !isCreate>
        <@field type="display" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate><span class="ect-displayfield ect-displayfield-for-fromDate">${params.fromDate!}</span></@field>
        <@field type="hidden" name="fromDate" value=(params.fromDate!) class="+ect-inputfield"/>
      <#else>
        <@field type="datetime" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate value=(params.fromDate!) class="+ect-inputfield"/>
      </#if>

        <@field type="input" name="categoryName" value=(params.categoryName!) label=uiLabelMap.FormFieldTitle_categoryName class="+ect-inputfield"/><#--  not strictly: required=true -->
        <@field type="input" name="description" value=(params.description!) label=uiLabelMap.FormFieldTitle_description class="+ect-inputfield"/>
        <@field type="input" name="longDescription" value=(params.longDescription!) label=uiLabelMap.FormFieldTitle_longDescription class="+ect-inputfield"/>

        <#-- TODO: LOCALIZED VERSIONS OF categoryName/description/longDescription (complex) -->

    </@form>
</#macro>

<@section title=uiLabelMap.ProductNewCategory containerId="ect-newcategory" containerClass="+ect-newcategory ect-recordaction ect-newrecord" 
    containerStyle=((targetRecord == "category" && isCreate)?string("","display:none;"))>
  <#if (targetRecord == "category" && isCreate)>
    <#assign paramMaps = initialCategoryParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams,
      "isError":isCategoryError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupCategoryForm id="NewCategory" isCreate=true target="setupCreateCategory" params=paramMaps.values/>
</@section>
<@section title=uiLabelMap.ProductEditCategory containerId="ect-editcategory" containerClass="+ect-editcategory ect-recordaction ect-editrecord" 
    containerStyle=((targetRecord == "category" && !isCreate)?string("","display:none;"))>
  <#if (targetRecord == "category" && !isCreate)>
    <#assign paramMaps = initialCategoryParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isCategoryError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupCategoryForm id="EditCategory" isCreate=false target="setupUpdateCategory" params=paramMaps.values/>
</@section>

<div style="display:none;">
<#macro setupDeleteCategoryForm id isDeleteRecord>
  <@form id=id action=makeOfbizUrl("setupDeleteCategory") method="post">
      <@defaultWizardFormFields exclude=["productCategoryId", "prodCatalogId", "productStoreId"]/>
      <@ectCommonTreeFormFields params={}/>
      <@field type="hidden" name="setupContinue" value="N"/>
      <@field type="hidden" name="isDeleteCategory" value="Y"/><#-- for our screens -->
      <@field type="hidden" name="deleteProductCategory" value=isDeleteRecord?string("true", "false")/><#-- for service -->
      
      <#-- common -->
      <@field type="hidden" name="productStoreId" value="" class="+ect-inputfield"/>
      
      <#-- common PK -->
      <@field type="hidden" name="productCategoryId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="fromDate" value="" class="+ect-inputfield"/>
      
      <#-- ProdCatalogCategory PK -->
      <@field type="hidden" name="prodCatalogId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="prodCatalogCategoryTypeId" value="" class="+ect-inputfield"/>
      
      <#-- ProductCategoryRollup PK -->
      <@field type="hidden" name="parentProductCategoryId" value="" class="+ect-inputfield"/><#-- presence determines which assoc type gets targeted for delete -->
  </@form>
</#macro>
  <@setupDeleteCategoryForm id="ect-removecategory-form" isDeleteRecord=true/>
  <@setupDeleteCategoryForm id="ect-removecategoryassoc-form" isDeleteRecord=false/>
</div>
