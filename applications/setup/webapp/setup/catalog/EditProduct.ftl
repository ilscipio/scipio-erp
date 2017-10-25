<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://product/webapp/catalog/catalog/tree/treecommon.ftl">

<#assign defaultParams = {
    "productTypeId": "FINISHED_GOOD"
}>
<@script>
    var defaultProductParams = <@objectAsScript object=defaultParams lang='js'/>;
</@script>

<#-- SPECIAL: for this screen because there are multiple forms, we have to ignore isError
    when target was not a submit for one of our forms -->
<#assign isProductError = isSetupEventError!false>
<#if targetRecord != "product">
  <#assign isProductError = false/>
</#if>

<#-- SPECIAL: for this screen because there are multiple forms, we have to disable useRequestParameters
    when target was not one of our forms -->
<#assign useReqParams = "">
<#if targetRecord != "product" || isDeleteProductSuccess>
  <#assign useReqParams = false>
</#if>

<#-- DEV NOTE: WARN: avoid fixedParams here, not appropriate for this screen -->
<#assign initialParamMaps = getWizardFormFieldValueMaps({
    "record":productAndAssoc!true,
    "defaults":defaultParams,
    "isError":isProductError,
    "useReqParams":useReqParams
})>
<#macro setupProductForm id formActionType target params>
    <@form id=id name=id action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["productId", "productCategoryId", "prodCatalogId", "productStoreId", "partyId"]/>
        <@ectCommonTreeFormFields params=params/>
        <@field type="hidden" name="partyId" value=(partyId!)/>
        <@field type="hidden" name="productStoreId" value=(productStoreId!)/>
        <@field type="hidden" name="prodCatalogId" value=(params.prodCatalogId!) class="+ect-inputfield"/>
        
        <@field type="hidden" name="isAddProduct" value=(formActionType == "add")?string("Y", "N")/>
        <@field type="hidden" name="isCreateProduct" value=(formActionType == "new")?string("Y", "N")/>
        <@field type="hidden" name="isUpdateProduct" value=(formActionType == "edit")?string("Y", "N")/>
        
    <#if formActionType == "add">
        <@field type="lookup" name="productId" label=uiLabelMap.FormFieldTitle_productId value=(params.productId!) fieldFormName="LookupProduct" class="+ect-inputfield" required=true/>
    <#else>
      <#if formActionType == "edit">
        <@field type="display" label=uiLabelMap.FormFieldTitle_productId><#rt/>
            <span class="ect-managefield ect-managefield-for-productId"><@setupExtAppLink uri="/catalog/control/EditProduct?productId=${rawString(params.productId!)}" text=params.productId!/></span><#t/>
        </@field><#lt/>
        <@field type="hidden" name="productId" value=(params.productId!) class="+ect-inputfield"/>
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="productId" label=uiLabelMap.FormFieldTitle_productId value=(params.productId!) class="+ect-inputfield"/>
      </#if>
    </#if>

      <#if formActionType != "add">
        <@field type="select" label=uiLabelMap.ProductProductType name="productTypeId" class="+ect-inputfield" required=true>
            <#list productTypes as productTypeData>
                <option<#if rawString(params.productTypeId!) == rawString(productTypeData.productTypeId!)> selected="selected"</#if> value="${productTypeData.productTypeId}">${productTypeData.get("description", locale)}</option>
            </#list>
        </@field>
        
        <@field type="input" name="internalName" value=(params.internalName!) label=uiLabelMap.ProductInternalName class="+ect-inputfield" required=true/>
      </#if>

        <#-- DEV NOTE: some of these aren't selectable because it should be done through the js tree. -->
        
        <@field type="display" name="productCategoryId" label=uiLabelMap.ProductCategory><span class="ect-displayfield ect-displayfield-for-productCategoryId">${params.productCategoryId!}</span></@field>
        <@field type="hidden" name="productCategoryId" value=(params.productCategoryId!) class="+ect-inputfield"/>

        <@field type="input" name="sequenceNum" value=(params.sequenceNum!) label=uiLabelMap.ProductSequenceNum class="+ect-inputfield"/>

      <#if formActionType == "edit">
        <@field type="display" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate><span class="ect-displayfield ect-displayfield-for-fromDate">${params.fromDate!}</span></@field>
        <@field type="hidden" name="fromDate" value=(params.fromDate!) class="+ect-inputfield"/>
      <#else>
        <@field type="datetime" name="fromDate" label=uiLabelMap.FormFieldTitle_fromDate value=(params.fromDate!) class="+ect-inputfield"/>
      </#if>

      <#if formActionType != "add">
        <@field type="input" name="productName" value=(params.productName!) label=uiLabelMap.ProductProductName class="+ect-inputfield" tooltip=uiLabelMap.ProductNonLocalizedContentFieldInfo/><#--  not strictly: required=true -->
        <@field type="input" name="description" value=(params.description!) label=uiLabelMap.FormFieldTitle_description class="+ect-inputfield" tooltip=uiLabelMap.ProductNonLocalizedContentFieldInfo/>
        <@field type="input" name="longDescription" value=(params.longDescription!) label=uiLabelMap.FormFieldTitle_longDescription class="+ect-inputfield" tooltip=uiLabelMap.ProductNonLocalizedContentFieldInfo/>

        <@fieldset title=uiLabelMap.CommonLocalizedFields collapsed=true>
            <#-- TODO: LOCALIZED VERSIONS OF productName/description/longDescription (complex) -->
            <@field type="display">(${uiLabelMap.CommonNone})</@field>
        </@fieldset>
      </#if>
    </@form>
</#macro>

<@section title=uiLabelMap.ProductNewProduct containerId="ect-newproduct" containerClass="+ect-newproduct ect-recordaction ect-newrecord" 
    containerStyle=((targetRecordAction == "product-new")?string("","display:none;"))>
  <#if targetRecordAction == "product-new">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":true,
      "defaults":defaultParams,
      "isError":isProductError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupProductForm id="NewProduct" formActionType="new" target="setupCreateProduct" params=paramMaps.values/>
</@section>
<@section title=uiLabelMap.ProductEditProduct containerId="ect-editproduct" containerClass="+ect-editproduct ect-recordaction ect-editrecord" 
    containerStyle=((targetRecordAction == "product-edit")?string("","display:none;"))>
  <#if targetRecordAction == "product-edit">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isProductError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupProductForm id="EditProduct" formActionType="edit" target="setupUpdateProduct" params=paramMaps.values/>
</@section>
<@section title=uiLabelMap.ProductAddExistingProduct containerId="ect-addproduct" containerClass="+ect-addproduct ect-recordaction ect-editrecord" 
    containerStyle=((targetRecordAction == "product-add")?string("","display:none;"))>
  <#if targetRecordAction == "product-add">
    <#assign paramMaps = initialParamMaps>
  <#else>
    <#assign paramMaps = getWizardFormFieldValueMaps({
      "record":{},
      "defaults":defaultParams,
      "isError":isProductError,
      "useReqParams":useReqParams
    })>
  </#if>
  <@setupProductForm id="AddProduct" formActionType="add" target="setupAddProduct" params=paramMaps.values/>
</@section>

<div style="display:none;">
<#macro setupDeleteProductForm id target isDeleteRecord>
  <@form id=id action=makeOfbizUrl(target) method="post">
      <@defaultWizardFormFields exclude=["productId", "productCategoryId", "prodCatalogId", "productStoreId"]/>
      <@ectCommonTreeFormFields params={}/>
      <@field type="hidden" name="setupContinue" value="N"/>
      <@field type="hidden" name="isDeleteProduct" value="Y"/><#-- for our screens -->
      <@field type="hidden" name="deleteRecordAndRelated" value=isDeleteRecord?string("true", "false")/><#-- for Versatile service -->
      <@field type="hidden" name="deleteAssocMode" value="" class="+ect-inputfield"/><#-- for Versatile service -->
      
      <@field type="hidden" name="productStoreId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="prodCatalogId" value="" class="+ect-inputfield"/>
      
      <#-- ProductCategoryMember -->
      <@field type="hidden" name="productId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="productCategoryId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="fromDate" value="" class="+ect-inputfield"/>
  </@form>
</#macro>
  <@setupDeleteProductForm id="ect-removeproduct-form" target="setupDeleteProduct" isDeleteRecord=true/>
  <@setupDeleteProductForm id="ect-removeproductassoc-form" target="setupDeleteProduct" isDeleteRecord=false/>

<#macro setupCopyMoveAssocProductForm id target formActionType>
  <@form id=id action=makeOfbizUrl(target) method="post">
      <@defaultWizardFormFields exclude=["productId", "productCategoryId", "prodCatalogId", "productStoreId"]/>
      <@ectCommonTreeFormFields params={}/>
      <@field type="hidden" name="setupContinue" value="N"/>
      
      <@field type="hidden" name="isCopyProduct" value=(formActionType == "copy")?string("Y","N")/><#-- for our screens -->
      <@field type="hidden" name="isMoveProduct" value=(formActionType == "move")?string("Y","N")/><#-- for our screens -->
      
      <@field type="hidden" name="returnAssocFields" value="" class="+ect-inputfield"/><#-- for Versatile service -->
    <#if formActionType == "move">
      <@field type="hidden" name="deleteAssocMode" value="" class="+ect-inputfield"/><#-- for Versatile service -->
    </#if>
      
      <@field type="hidden" name="productStoreId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="prodCatalogId" value="" class="+ect-inputfield"/>
      
      <#-- SOURCE -->
      <@field type="hidden" name="productId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="productCategoryId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="fromDate" value="" class="+ect-inputfield"/>
      
      <#-- TARGET -->
      <#--<@field type="hidden" name="to_productId" value="" class="+ect-inputfield"/> not an input-->
      <@field type="hidden" name="to_productCategoryId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="to_fromDate" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="to_sequenceNum" value="" class="+ect-inputfield"/>

  </@form>
</#macro>
  <@setupCopyMoveAssocProductForm id="ect-copyproductassoc-form" target="setupCopyProduct" formActionType="copy"/>
  <@setupCopyMoveAssocProductForm id="ect-moveproductassoc-form" target="setupMoveProduct" formActionType="move"/>
</div>
