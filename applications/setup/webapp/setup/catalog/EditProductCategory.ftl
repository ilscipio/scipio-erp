<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://product/webapp/catalog/catalog/tree/treecommon.ftl">

<#assign defaultParams = {
    "prodCatalogCategoryTypeId": "PCCT_BROWSE_ROOT",
    "productCategoryTypeId": "CATALOG_CATEGORY"
}>

<@script>
    var defaultCategoryParams = <@objectAsScript object=defaultParams lang='js'/>;

    <#-- NOTE: also called from setupShowFormActivatedCallback -->
    function refreshScfFieldVisibility(form, isCategoryForm) {
        // check make sure this is category form
        if (isCategoryForm === true || jQuery('input[name=isCategoryAction]', form).length) {
            var parentProductCategoryId = jQuery('[name=parentProductCategoryId]', form).filter(':input').val();
            if (parentProductCategoryId) {
                jQuery('.epc-field-prodCatalogCategoryTypeId', form).hide();
                jQuery('.epc-field-parentProductCategoryId', form).show();
            } else {
                jQuery('.epc-field-prodCatalogCategoryTypeId', form).show();
                jQuery('.epc-field-parentProductCategoryId', form).hide();
            }
        }
    }
        
    jQuery(document).ready(function() {
        refreshScfFieldVisibility(jQuery('#NewCategory'), true);
        refreshScfFieldVisibility(jQuery('#EditCategory'), true);
        refreshScfFieldVisibility(jQuery('#AddCategory'), true);
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
<#if targetRecord != "category">
  <#assign isCategoryError = false/>
</#if>

<#-- SPECIAL: for this screen because there are multiple forms, we have to disable useRequestParameters
    when target was not one of our forms -->
<#assign useReqParams = "">
<#if targetRecord != "category" || isDeleteCategorySuccess>
  <#assign useReqParams = false>
</#if>

<#-- DEV NOTE: WARN: avoid fixedParams here, not appropriate for this screen -->
<#assign initialParamMaps = getWizardFormFieldValueMaps({
    "record":productCategoryAndAssoc!true,
    "defaults":defaultParams,
    "isError":isCategoryError,
    "useReqParams":useReqParams
})>
<#macro setupCategoryForm id formActionType target params>
    <@form id=id name=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["productCategoryId", "prodCatalogId", "productStoreId", "partyId"]/>
        <@ectCommonTreeFormFields params=params/>
        <@field type="hidden" name="partyId" value=(partyId!)/>
        <@field type="hidden" name="productStoreId" value=(productStoreId!)/>
        
        <@field type="hidden" name="isCategoryAction" value="Y"/><#-- currently only used by JS above, find better solutions later... -->
        <@field type="hidden" name="isAddCategory" value=(formActionType == "add")?string("Y", "N")/><#-- used by screens -->
        <@field type="hidden" name="isCreateCategory" value=(formActionType == "new")?string("Y", "N")/><#-- used by screens -->
        <@field type="hidden" name="isUpdateCategory" value=(formActionType == "edit")?string("Y", "N")/><#-- used by screens -->
        
    <#if formActionType == "add">
      <@field type="lookup" name="productCategoryId" label=uiLabelMap.FormFieldTitle_productCategoryId value=(params.productCategoryId!) fieldFormName="LookupProductCategory" class="+ect-inputfield" required=true/>
    <#else>
      <#if formActionType == "edit">
        <@field type="display" label=uiLabelMap.FormFieldTitle_productCategoryId><#rt/>
            <span class="ect-managefield ect-managefield-for-productCategoryId"><@setupExtAppLink uri="/catalog/control/EditCategory?productCategoryId=${rawString(params.productCategoryId!)}" text=params.productCategoryId!/></span><#t/>
        </@field><#lt/>
        <@field type="hidden" name="productCategoryId" value=(params.productCategoryId!) class="+ect-inputfield"/>
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="productCategoryId" label=uiLabelMap.FormFieldTitle_productCategoryId value=(params.productCategoryId!) class="+ect-inputfield"/>
      </#if>
    </#if>

      <#if formActionType != "add">
        <@field type="select" name="productCategoryTypeId" label=uiLabelMap.ProductProductCategoryType class="+ect-inputfield" required=true>
            <#list productCategoryTypes as productCategoryTypeData>
                <option<#if rawString(params.productCategoryTypeId!) == rawString(productCategoryTypeData.productCategoryTypeId!)> selected="selected"</#if> value="${productCategoryTypeData.productCategoryTypeId}">${productCategoryTypeData.get("description", locale)}</option>
            </#list>
        </@field>
      </#if>

        <#-- DEV NOTE: some of these aren't selectable because it should be done through the js tree. -->

        <@field type="display" name="prodCatalogId" label=uiLabelMap.ProductCatalog><span class="ect-displayfield ect-displayfield-for-prodCatalogId">${params.prodCatalogId!}</span></@field>
        <@field type="hidden" name="prodCatalogId" value=(params.prodCatalogId!) class="+ect-inputfield"/>

      <#if formActionType == "edit">
        <#-- NOTE: unfortunately this cannot be edited after create, because it is part of the PK... 
            FIXME?: could do a big workaround by service to make this editable by re-creating the assoc... -->
        <@field type="select" disabled=true label=uiLabelMap.ProductCatalogCategoryType class="+ect-inputfield ect-inputfield-for-prodCatalogCategoryTypeId" containerClass="+epc-field-prodCatalogCategoryTypeId" required=true
            tooltip=uiLabelMap.ProductRecreateAssociation>
          <#list prodCatalogCategoryTypes as prodCatalogCategoryTypeData>                       
            <option<#if rawString(params.prodCatalogCategoryTypeId!) == rawString(prodCatalogCategoryTypeData.prodCatalogCategoryTypeId!)> selected="selected"</#if> value="${prodCatalogCategoryTypeData.prodCatalogCategoryTypeId}">${prodCatalogCategoryTypeData.get("description", locale)}</option>
          </#list>
        </@field>
        <@field type="hidden" name="prodCatalogCategoryTypeId" value=(params.prodCatalogCategoryTypeId!) class="+ect-inputfield"/>
      <#else>
        <@field type="select" label=uiLabelMap.ProductCatalogCategoryType name="prodCatalogCategoryTypeId" class="+ect-inputfield" containerClass="+epc-field-prodCatalogCategoryTypeId" required=true>
          <#list prodCatalogCategoryTypes as prodCatalogCategoryTypeData>                       
            <option<#if rawString(params.prodCatalogCategoryTypeId!) == rawString(prodCatalogCategoryTypeData.prodCatalogCategoryTypeId!)> selected="selected"</#if> value="${prodCatalogCategoryTypeData.prodCatalogCategoryTypeId}">${prodCatalogCategoryTypeData.get("description", locale)}</option>
          </#list>
        </@field>
      </#if>

        <@field type="display" name="parentProductCategoryId" label=uiLabelMap.ProductParentCategory containerClass="+epc-field-parentProductCategoryId"><span class="ect-displayfield ect-displayfield-for-parentProductCategoryId">${params.parentProductCategoryId!}</span></@field>
        <@field type="hidden" name="parentProductCategoryId" value=(params.parentProductCategoryId!) class="+ect-inputfield"/>
        
        <@field type="input" name="sequenceNum" value=(params.sequenceNum!) label=uiLabelMap.ProductSequenceNum class="+ect-inputfield"/>

      <#if formActionType == "edit">
        <@field type="display" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate><span class="ect-displayfield ect-displayfield-for-fromDate">${params.fromDate!}</span></@field>
        <@field type="hidden" name="fromDate" value=(params.fromDate!) class="+ect-inputfield"/>
      <#else>
        <@field type="datetime" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate value=(params.fromDate!) class="+ect-inputfield"/>
      </#if>

      <#if formActionType != "add">
        <@field type="input" name="categoryName" value=(params.categoryName!) label=uiLabelMap.FormFieldTitle_categoryName class="+ect-inputfield" tooltip=uiLabelMap.ProductNonLocalizedContentFieldInfo/><#--  not strictly: required=true -->
        <@field type="input" name="description" value=(params.description!) label=uiLabelMap.FormFieldTitle_description class="+ect-inputfield" tooltip=uiLabelMap.ProductNonLocalizedContentFieldInfo/>
        <@field type="input" name="longDescription" value=(params.longDescription!) label=uiLabelMap.FormFieldTitle_longDescription class="+ect-inputfield" tooltip=uiLabelMap.ProductNonLocalizedContentFieldInfo/>

        <@fieldset title=uiLabelMap.CommonLocalizedFields collapsed=true>
            <#-- TODO: LOCALIZED VERSIONS OF categoryName/description/longDescription (complex) -->
        </@fieldset>
      </#if>
    </@form>
</#macro>

<@section title=uiLabelMap.ProductNewCategory containerId="ect-newcategory" containerClass="+ect-newcategory ect-recordaction ect-newrecord" 
    containerStyle=((targetRecordAction == "category-new")?string("","display:none;"))>
  <#if targetRecordAction == "category-new">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams,
      "isError":isCategoryError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupCategoryForm id="NewCategory" formActionType="new" target="setupCreateCategory" params=paramMaps.values/>
</@section>
<@section title=uiLabelMap.ProductEditCategory containerId="ect-editcategory" containerClass="+ect-editcategory ect-recordaction ect-editrecord" 
    containerStyle=((targetRecordAction == "category-edit")?string("","display:none;"))>
  <#if targetRecordAction == "category-edit">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isCategoryError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupCategoryForm id="EditCategory" formActionType="edit" target="setupUpdateCategory" params=paramMaps.values/>
</@section>
<@section title=uiLabelMap.ProductAddExistingCategory containerId="ect-addcategory" containerClass="+ect-addcategory ect-recordaction ect-editrecord" 
    containerStyle=((targetRecordAction == "category-add")?string("","display:none;"))>
  <#if targetRecordAction == "category-add">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isCategoryError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupCategoryForm id="AddCategory" formActionType="add" target="setupAddCategory" params=paramMaps.values/>
</@section>

<div style="display:none;">
<#macro setupDeleteCategoryForm id target isDeleteRecord>
  <@form id=id action=makeOfbizUrl(target) method="post">
      <@defaultWizardFormFields exclude=["productCategoryId", "prodCatalogId", "productStoreId"]/>
      <@ectCommonTreeFormFields params={}/>
      <@field type="hidden" name="setupContinue" value="N"/>
      <@field type="hidden" name="isDeleteCategory" value="Y"/><#-- for our screens -->
      <@field type="hidden" name="deleteCategoryRecordAndRelated" value=isDeleteRecord?string("true", "false")/><#-- for Versatile service -->
      <@field type="hidden" name="deleteAssocMode" value="" class="+ect-inputfield"/><#-- for Versatile service -->
      
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
  <@setupDeleteCategoryForm id="ect-removecategory-form" target="setupDeleteCategory" isDeleteRecord=true/>
  <@setupDeleteCategoryForm id="ect-removecategoryassoc-form" target="setupDeleteCategory" isDeleteRecord=false/>
  
<#macro setupCopyMoveAssocCategoryForm id target formActionType>
  <@form id=id action=makeOfbizUrl(target) method="post">
      <@defaultWizardFormFields exclude=["productCategoryId", "prodCatalogId", "productStoreId"]/>
      <@ectCommonTreeFormFields params={}/>
      <@field type="hidden" name="setupContinue" value="N"/>
    <#if formActionType == "move">
      <@field type="hidden" name="isCopyCategory" value="Y"/><#-- for our screens -->
    <#elseif formActionType == "copy">
      <@field type="hidden" name="isMoveCategory" value="Y"/><#-- for our screens -->
      <@field type="hidden" name="deleteAssocMode" value="" class="+ect-inputfield"/><#-- for Versatile service -->
    </#if>
      <@field type="hidden" name="returnAssocFields" value="" class="+ect-inputfield"/><#-- for Versatile service -->

      <#-- common -->
      <@field type="hidden" name="productStoreId" value="" class="+ect-inputfield"/>
      
      <#-- SOURCE -->
      <#-- common PK -->
      <@field type="hidden" name="productCategoryId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="fromDate" value="" class="+ect-inputfield"/>
      <#-- ProdCatalogCategory PK -->
      <@field type="hidden" name="prodCatalogId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="prodCatalogCategoryTypeId" value="" class="+ect-inputfield"/>
      <#-- ProductCategoryRollup PK -->
      <@field type="hidden" name="parentProductCategoryId" value="" class="+ect-inputfield"/><#-- presence determines which assoc type gets targeted for delete -->
      
      <#-- TARGET -->
      <#-- common PK -->
      <#--<@field type="hidden" name="to_productCategoryId" value="" class="+ect-inputfield"/> not an input-->
      <@field type="hidden" name="to_fromDate" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="to_sequenceNum" value="" class="+ect-inputfield"/>
      <#-- ProdCatalogCategory PK -->
      <@field type="hidden" name="to_prodCatalogId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="to_prodCatalogCategoryTypeId" value="" class="+ect-inputfield"/>
      <#-- ProductCategoryRollup PK -->
      <@field type="hidden" name="to_parentProductCategoryId" value="" class="+ect-inputfield"/><#-- presence determines which assoc type gets targeted for delete -->
  </@form>
</#macro>
  <@setupCopyMoveAssocCategoryForm id="ect-copycategoryassoc-form" target="setupCopyCategory" formActionType="copy"/>
  <@setupCopyMoveAssocCategoryForm id="ect-movecategoryassoc-form" target="setupMoveCategory" formActionType="move"/>
</div>
