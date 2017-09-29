<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {
    "useQuickAdd": "Y",
    "viewAllowPermReqd": "N",
    "purchaseAllowPermReqd": "N",
    "sequenceNum": defaultSequenceNum!
}>
<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":prodCatalogAndStoreAssoc!true,<#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

    <@form id=submitFormId action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["prodCatalogId", "productStoreId", "partyId"]/>
        <@field type="hidden" name="isCreateCatalog" value=(prodCatalog??)?string("N","Y")/>
        
        <#--<field use-when="prodCatalog==null&amp;&amp;prodCatalogId==null" name="prodCatalogId" required-field="true"><text default-value="${partyId}"/>-->
      <#if prodCatalog??>
        <@field type="display" label=uiLabelMap.FormFieldTitle_prodCatalogId><#rt/>
            <@setupExtAppLink uri="/catalog/control/EditProdCatalog?prodCatalogId=${rawString(params.prodCatalogId!)}" text=params.prodCatalogId!/><#t/>
        </@field><#lt/>
        <@field type="hidden" name="prodCatalogId" value=(params.prodCatalogId!)/> 
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="prodCatalogId" label=uiLabelMap.FormFieldTitle_prodCatalogId value=(params.prodCatalogId!)/>
      </#if>

        <@field type="input" name="catalogName" value=(params.catalogName!) label=uiLabelMap.FormFieldTitle_prodCatalogName required=true  size="30" maxlength="60"/>
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

