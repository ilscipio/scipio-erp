<#include "component://common/webcommon/includes/listLocalesMacros.ftl">
<#include "component://setup/webapp/setup/common/common.ftl">


    <@form id=submitFormId action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=[
            "productStoreId", <#-- ProductStore excludes -->
            "partyId", "webSiteId"  <#-- WebSite excludes -->
            ]/>

<#-- 
  ProductStore parameters
-->

<#assign defaultParams = {
    "companyName": (partyGroup.groupName)!"",
    "payToPartyId": partyId!"",
    <#--"partyId": partyId!"",-->
    <#-- "inventoryFacilityId": facilityId!, // not this way - instead we set parameters.inventoryFacilityId in SetupStore.groovy -->
    "visualThemeId": defaultVisualThemeId!,
    "manualAuthIsCapture": "N",
    "prorateShipping": "Y",
    "prorateTaxes": "Y",
    "viewCartOnAdd": "N",
    "autoSaveCart": "N",
    "autoApproveReviews": "N",
    "autoInvoiceDigitalItems": "Y",
    "reqShipAddrForDigItems": "Y",
    "isDemoStore": "Y",
    "isImmediatelyFulfilled": "N",
    "checkInventory": "Y",
    "requireInventory": "N",
    "reserveInventory": "Y",
    "reserveOrderEnumId": "INVRO_FIFO_REC",
    "balanceResOnOrderCreation": "Y",
    "oneInventoryFacility": "Y", <#-- NOTE: currently forcing one facility required per store, as was in ofbizsetup (TODO: REVIEW);
                                      this is also forced by store step depends on facilityId in SetupWorker -->
    "defaultSalesChannelEnumId": "WEB_SALES_CHANNEL",
    "allowPassword": "Y",
    "retryFailedAuths": "Y",
    "headerApprovedStatus": "ORDER_APPROVED",
    "itemApprovedStatus": "ITEM_APPROVED",
    "digitalItemApprovedStatus": "ITEM_APPROVED",
    "headerDeclinedStatus": "ORDER_REJECTED",
    "itemDeclinedStatus": "ITEM_REJECTED",
    "headerCancelStatus": "ORDER_CANCELLED",
    "itemCancelStatus": "ITEM_CANCELLED",
    "storeCreditAccountEnumId": "FIN_ACCOUNT",
    "explodeOrderItems": "N",
    "checkGcBalance": "N",
    "usePrimaryEmailUsername": "N",
    "requireCustomerRole": "N",
    "showCheckoutGiftOptions": "Y",
    "selectPaymentTypePerItem": "N",
    "showPricesWithVatTax": "N",
    "showTaxIsExempt": "Y",
    "prodSearchExcludeVariants": "Y",
    "enableDigProdUpload": "N",
    "autoOrderCcTryExp": "Y",
    "autoOrderCcTryOtherCards": "Y",
    "autoOrderCcTryLaterNsf": "Y",
    "autoApproveInvoice": "Y",
    "autoApproveOrder": "Y",
    "shipIfCaptureFails": "Y",
    "reqReturnInventoryReceive": "N",

    "orderNumberPrefix": "WS",
    "defaultLocaleString": defaultDefaultLocaleString!,
    "showOutOfStockProducts": "Y",
    "authDeclinedMessage": "There has been a problem with your method of payment. Please try a different method or call customer service.",
    "authFraudMessage": "Your order has been rejected and your account has been disabled due to fraud.",
    "authErrorMessage": "Problem connecting to payment processor; we will continue to retry and notify you by email.",
    
    <#--"paymentList": paymentList![]-->
    
    <#-- SCIPIO: newly added defaults -->
    "defaultCurrencyUomId": defaultDefaultCurrencyUomId!
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":productStore!true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>
<#assign storeParams = params>
<#assign storeFixedParams = fixedParams>

        <@field type="hidden" name="isCreateStore" value=(productStore??)?string("N","Y")/>
        
      <#if productStore??>
        <@field type="display" label=uiLabelMap.FormFieldTitle_productStoreId tooltip=uiLabelMap.ProductNotModificationRecreatingProductStore><#rt/>
            <@setupExtAppLink uri="/catalog/control/EditProductStore?productStoreId=${rawString(params.productStoreId!)}" text=(params.productStoreId!)/><#t/>
        </@field><#lt/>
        <@field type="hidden" name="productStoreId" value=(params.productStoreId!)/> 
      <#else>
        <@field type="input" name="productStoreId" label=uiLabelMap.FormFieldTitle_productStoreId value=(params.productStoreId!) placeholder="ScipioShop"/>
      </#if>
        <@field type="input" name="storeName" label=uiLabelMap.ProductStoreName required=true size="30" maxlength="60" value=(params.storeName!)/>
        
        <#macro facilityOption facility selected=false>
            <option value="${facility.facilityId}"<#if selected> selected="selected"</#if>><#rt/>
              <#if facility.facilityName?has_content>
                ${facility.facilityName} [${facility.facilityId}]<#t/>
              <#else>
                ${facility.facilityId}<#t/>
              </#if>
            </option><#lt/>
        </#macro>
        <@field type="generic" label=uiLabelMap.ProductFacility>
          <#-- DEV NOTE: all the checks below were done before I decided to make Store step fully
              depend on facility step (inventoryFacilityId cannot be null anymore), but these
              will help against bad configurations -->
          <#-- NOTE: here fixedParams.inventoryFacilityId returns the 
              productStore.inventoryFacilityId field if productStore != null,
              otherwise it gives the parameter facilityId -->
          <@field type="select" name="inventoryFacilityId" label=uiLabelMap.ProductFacility>
            <#if productStoreFacilityMissing>
              <option value="">(${uiLabelMap.CommonNone})</option>
            <#elseif !storeInventoryFacilityOk>
              <#-- special case to show weird value without squashing it silently -->
              <@facilityOption facility=inventoryFacility selected=true/>
            </#if>
            <#if facilities?has_content>
              <#list (facilities![]) as currFacility>
                <#assign selected = rawString(currFacility.facilityId) == rawString(params.inventoryFacilityId!)>
                <@facilityOption facility=currFacility selected=selected/>
              </#list>
            </#if>
          </@field>
          <#if productStoreFacilityMissing>
            <@alert type="error">${uiLabelMap.SetupStoreMissingFacility}</@alert>
          <#elseif !storeInventoryFacilityOk>
            <@alert type="warning">${uiLabelMap.SetupInvalidFacilityForStore} (${inventoryFacility.facilityId})</@alert>
          </#if>
        </@field>
        
        <#-- BEST-EFFORT ATTEMPT to support drop-down expanded locale string (not supported OOTB ofbiz)
        <@field type="input" name="defaultLocaleString" label=uiLabelMap.FormFieldTitle_defaultLocaleString value=(params.defaultLocaleString!) placeholder="en_US"/>-->
        <@field type="select" name="defaultLocaleString" label=uiLabelMap.FormFieldTitle_defaultLocaleString>
            <@availableLocalesOptions expandCountries=true requireCountries=false currentLocale=(params.defaultLocaleString!) allowExtra=true allowEmpty=true/>
        </@field>
        
        <@field type="select" name="defaultCurrencyUomId" label=uiLabelMap.FormFieldTitle_defaultCurrencyUomId>
          <@field type="option" value=""></@field>
          <#list (currencyUomList!) as currencyUom>
            <@field type="option" value=currencyUom.uomId
                selected=(rawString(params.defaultCurrencyUomId!)==rawString(currencyUom.uomId))
                >${currencyUom.get("description", locale)!} (${currencyUom.abbreviation!})</@field>
          </#list>
        </@field>

        <@field type="select" name="visualThemeId" label=uiLabelMap.FormFieldTitle_visualThemeId tooltip=uiLabelMap.SetupProductStoreVisualThemeIdInfo>
          <@field type="option" value=""></@field>
          <#list (visualThemeList!) as visualTheme>
            <@field type="option" value=visualTheme.visualThemeId
                selected=(rawString(params.visualThemeId!)==rawString(visualTheme.visualThemeId))
                >${visualTheme.get("description", locale)!} [${visualTheme.visualThemeId!}]</@field>
          </#list>
        </@field>

        <#-- NOTE: some of these use fixedParams, others use params, it is based on whether original
           form widget specified a hidden value="..." (fixedParams) or not (params) -->
        <@field type="hidden" name="companyName" value=(fixedParams.companyName!)/>
        <@field type="hidden" name="primaryStoreGroupId" value=(params.primaryStoreGroupId!)/>
        <@field type="hidden" name="title" value=(params.title!)/>
        <@field type="hidden" name="subtitle" value=(params.subtitle!)/>
        <@field type="hidden" name="payToPartyId" value=(fixedParams.payToPartyId!)/>
        <#-- SCIPIO: need drop-down in case org has multiple facility
        <@field type="hidden" name="inventoryFacilityId" value=(fixedParams.inventoryFacilityId!)/>-->
        <#--<@field type="hidden" name="visualThemeId" value=(fixedParams.visualThemeId!)/> configurable -->
        <@field type="hidden" name="manualAuthIsCapture" value=(fixedParams.manualAuthIsCapture!)/>
        <@field type="hidden" name="prorateShipping" value=(fixedParams.prorateShipping!)/>
        <@field type="hidden" name="prorateTaxes" value=(fixedParams.prorateTaxes!)/>
        <@field type="hidden" name="viewCartOnAdd" value=(fixedParams.viewCartOnAdd!)/>
        <@field type="hidden" name="autoSaveCart" value=(fixedParams.autoSaveCart!)/>
        <@field type="hidden" name="autoApproveReviews" value=(fixedParams.autoApproveReviews!)/>
        <@field type="hidden" name="autoInvoiceDigitalItems" value=(fixedParams.autoInvoiceDigitalItems!)/>
        <@field type="hidden" name="reqShipAddrForDigItems" value=(fixedParams.reqShipAddrForDigItems!)/>
        <@field type="hidden" name="isDemoStore" value=(fixedParams.isDemoStore!)/>
        <@field type="hidden" name="isImmediatelyFulfilled" value=(fixedParams.isImmediatelyFulfilled!)/>
        <@field type="hidden" name="checkInventory" value=(fixedParams.checkInventory!)/>
        <@field type="hidden" name="requireInventory" value=(fixedParams.requireInventory!)/>
        <@field type="hidden" name="reserveInventory" value=(fixedParams.reserveInventory!)/>
        <@field type="hidden" name="reserveOrderEnumId" value=(fixedParams.reserveOrderEnumId!)/>
        <@field type="hidden" name="balanceResOnOrderCreation" value=(fixedParams.balanceResOnOrderCreation!)/>
        <@field type="hidden" name="oneInventoryFacility" value=(fixedParams.oneInventoryFacility!)/>
        <@field type="hidden" name="requirementMethodEnumId" value=(params.requirementMethodEnumId!)/>
        <#--<@field type="hidden" name="defaultCurrencyUomId" value=(params.defaultCurrencyUomId!)/> configurable -->
        <@field type="hidden" name="defaultSalesChannelEnumId" value=(fixedParams.defaultSalesChannelEnumId!)/>
        <@field type="hidden" name="allowPassword" value=(fixedParams.allowPassword!)/>
        <@field type="hidden" name="retryFailedAuths" value=(fixedParams.retryFailedAuths!)/>
        <@field type="hidden" name="headerApprovedStatus" value=(fixedParams.headerApprovedStatus!)/>
        <@field type="hidden" name="itemApprovedStatus" value=(fixedParams.itemApprovedStatus!)/>
        <@field type="hidden" name="digitalItemApprovedStatus" value=(fixedParams.digitalItemApprovedStatus!)/>
        <@field type="hidden" name="headerDeclinedStatus" value=(fixedParams.headerDeclinedStatus!)/>
        <@field type="hidden" name="itemDeclinedStatus" value=(fixedParams.itemDeclinedStatus!)/>
        <@field type="hidden" name="headerCancelStatus" value=(fixedParams.headerCancelStatus!)/>
        <@field type="hidden" name="itemCancelStatus" value=(fixedParams.itemCancelStatus!)/>
        <@field type="hidden" name="storeCreditAccountEnumId" value=(fixedParams.storeCreditAccountEnumId!)/>
        <@field type="hidden" name="explodeOrderItems" value=(fixedParams.explodeOrderItems!)/>
        <@field type="hidden" name="checkGcBalance" value=(fixedParams.checkGcBalance!)/>
        <@field type="hidden" name="usePrimaryEmailUsername" value=(fixedParams.usePrimaryEmailUsername!)/>
        <@field type="hidden" name="requireCustomerRole" value=(fixedParams.requireCustomerRole!)/>
        <@field type="hidden" name="showCheckoutGiftOptions" value=(fixedParams.showCheckoutGiftOptions!)/>
        <@field type="hidden" name="selectPaymentTypePerItem" value=(fixedParams.selectPaymentTypePerItem!)/>
        <@field type="hidden" name="showPricesWithVatTax" value=(fixedParams.showPricesWithVatTax!)/>
        <@field type="hidden" name="showTaxIsExempt" value=(fixedParams.showTaxIsExempt!)/>
        <@field type="hidden" name="vatTaxAuthGeoId" value=(params.vatTaxAuthGeoId!)/>
        <@field type="hidden" name="vatTaxAuthPartyId" value=(params.vatTaxAuthPartyId!)/>
        <@field type="hidden" name="prodSearchExcludeVariants" value=(fixedParams.prodSearchExcludeVariants!)/>
        <@field type="hidden" name="enableDigProdUpload" value=(fixedParams.enableDigProdUpload!)/>
        <@field type="hidden" name="digProdUploadCategoryId" value=(params.digProdUploadCategoryId!)/>
        <@field type="hidden" name="autoOrderCcTryExp" value=(fixedParams.autoOrderCcTryExp!)/>
        <@field type="hidden" name="autoOrderCcTryOtherCards" value=(fixedParams.autoOrderCcTryOtherCards!)/>
        <@field type="hidden" name="autoOrderCcTryLaterNsf" value=(fixedParams.autoOrderCcTryLaterNsf!)/>
        <@field type="hidden" name="autoApproveInvoice" value=(fixedParams.autoApproveInvoice!)/>
        <@field type="hidden" name="autoApproveOrder" value=(fixedParams.autoApproveOrder!)/>
        <@field type="hidden" name="shipIfCaptureFails" value=(fixedParams.shipIfCaptureFails!)/>
        <@field type="hidden" name="setOwnerUponIssuance" value=(params.setOwnerUponIssuance!)/>
        <@field type="hidden" name="reqReturnInventoryReceive" value=(fixedParams.reqReturnInventoryReceive!)/>
        <@field type="hidden" name="addToCartReplaceUpsell" value=(params.addToCartReplaceUpsell!)/>
        <@field type="hidden" name="addToCartRemoveIncompat" value=(params.addToCartRemoveIncompat!)/>
        <@field type="hidden" name="splitPayPrefPerShpGrp" value=(params.splitPayPrefPerShpGrp!)/>
        <@field type="hidden" name="autoOrderCcTryLaterMax" value=(params.autoOrderCcTryLaterMax!)/>
        <@field type="hidden" name="orderNumberPrefix" value=(fixedParams.orderNumberPrefix!)/>
        <#--<@field type="hidden" name="defaultLocaleString" value=(fixedParams.defaultLocaleString!)/> configurable -->
        <@field type="hidden" name="enableAutoSuggestionList" value=(params.enableAutoSuggestionList!)/>
        <@field type="hidden" name="showOutOfStockProducts" value=(fixedParams.showOutOfStockProducts!)/>
        <@field type="hidden" name="authDeclinedMessage" value=(fixedParams.authDeclinedMessage!)/>
        <@field type="hidden" name="authFraudMessage" value=(fixedParams.authFraudMessage!)/>
        <@field type="hidden" name="authErrorMessage" value=(fixedParams.authErrorMessage!)/>
        <@field type="hidden" name="defaultPassword" value=(params.defaultPassword!)/>
        
        <#-- TODO: REVIEW: non-ProductStore fields (some do nothing?) -->
        <@field type="hidden" name="partyId" value=(partyId!)/>
        <@field type="hidden" name="inventoryFacilityAction" value=(inventoryFacilityAction!)/>
        <@field type="hidden" name="paymentList" value=(paymentList!)/><#-- SPECIAL: not a ProductStore field -->

      <#-- TODO?
      <@fieldset title=uiLabelMap.CommonAdvanced collapsed=true>
    
      </@fieldset>
      -->
       
<#if (useWebsiteFields!true) == true>

<#-- 
  WebSite parameters
-->

<#assign defaultParams = {
    "visualThemeSetId": defaultVisualThemeSetId!,
    "visualThemeSelectorScript": defaultVisualThemeSelectorScript!,
    "webSiteId": defaultWebSiteId!
}>
<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":webSite!true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>
<#assign websiteParams = params>
<#assign websiteFixedParams = fixedParams>

    <@script>
        jQuery(document).ready(function() {
            <#assign submitFormIdJs = escapeVal(submitFormId, 'js')>
            
            var storeNameVirgin = <#if storeParams.storeName?has_content>false<#else>true</#if>;
            var siteNameVirgin = <#if websiteParams.siteName?has_content>false<#else>true</#if>;
            
            var storeNameUpdate = function() {
                if (siteNameVirgin) {
                    var storeNameElem = jQuery('#${submitFormIdJs} input[name=storeName]');
                    var siteNameElem = jQuery('#${submitFormIdJs} input[name=siteName]');
                    siteNameElem.val(storeNameElem.val());
                }
            };
            var siteNameUpdate = function() {
                if (storeNameVirgin) {
                    var storeNameElem = jQuery('#${submitFormIdJs} input[name=storeName]');
                    var siteNameElem = jQuery('#${submitFormIdJs} input[name=siteName]');
                    storeNameElem.val(siteNameElem.val());
                }
            };
            
            var storeNameElem = jQuery('#${submitFormIdJs} input[name=storeName]');
            var siteNameElem = jQuery('#${submitFormIdJs} input[name=siteName]');
            
            storeNameElem.on('input', function() {
                storeNameUpdate();
                storeNameVirgin = false;
            });
            siteNameElem.on('input', function() {
                siteNameUpdate();
                siteNameVirgin = false;
            });
          
        });
    </@script>

        <@heading>${uiLabelMap[setupStepTitlePropMap['website']]}</@heading>

        <#if webSiteCount?? && (webSiteCount >= 2)>
          <@alert type="warning">${uiLabelMap.SetupMultipleWebSitesForProductStore}</@alert>
        </#if>

        <@field type="hidden" name="isCreateWebsite" value=(webSite??)?string("N", "Y")/>

        <#--
        <actions><set field="webSiteId" from-field="webSite.webSiteId"/></actions>
        <field use-when="webSite==null&amp;&amp;webSiteId==null" name="webSiteId" required-field="true"><text default-value="ScipioWebStore"/></field>
        -->
      <#if webSite??>
        <@field type="display" label=uiLabelMap.FormFieldTitle_webSiteId><#rt/>
            <@setupExtAppLink uri="/catalog/control/EditWebSite?webSiteId=${rawString(params.webSiteId!)}&productStoreId=${rawString(params.productStoreId!)}" text=(params.webSiteId!)/><#t/>
        </@field><#lt/>
        <@field type="hidden" name="webSiteId" value=(params.webSiteId!)/> 
      <#else>
        <@field type="input" name="webSiteId" label=uiLabelMap.FormFieldTitle_webSiteId value=(params.webSiteId!) placeholder=(defaultInitialWebSiteId!)/>
      </#if>
        
        <@field type="input" name="siteName" label=uiLabelMap.FormFieldTitle_siteName value=(params.siteName!) required=true size="30" maxlength="60"/>
        <@field type="hidden" name="visualThemeSetId" value=(fixedParams.visualThemeSetId!)/>

        <#--<@field type="hidden" name="partyId" value=(partyId!)/> // already set above -->
                
        <#--
        <@field type="hidden" name="httpHost" value=(params.httpHost!)/>
        <@field type="hidden" name="httpPort" value=(params.httpPort!)/>
        <@field type="hidden" name="httpsHost" value=(params.httpsHost!)/>
        <@field type="hidden" name="httpsPort" value=(params.httpsPort!)/>
        <@field type="hidden" name="enableHttps" value=(params.enableHttps!)/>
        <@field type="hidden" name="standardContentPrefix" value=(params.standardContentPrefix!)/>
        <@field type="hidden" name="secureContentPrefix" value=(params.secureContentPrefix!)/>
        <@field type="hidden" name="cookieDomain" value=(params.cookieDomain!)/>
        -->
      <@fieldset title=uiLabelMap.CommonAdvanced collapsed=true>
        <@field type="input" name="visualThemeSelectorScript" label=uiLabelMap.FormFieldTitle_visualThemeSelectorScript value=(params.visualThemeSelectorScript!) placeholder=(defaultVisualThemeSelectorScript!)/>

        <@field type="input" name="httpHost" label=uiLabelMap.FormFieldTitle_httpHost value=(params.httpHost!)/>
        <@field type="input" name="httpPort" label=uiLabelMap.FormFieldTitle_httpPort value=(params.httpPort!)/>
        <@field type="input" name="httpsHost" label=uiLabelMap.FormFieldTitle_httpsHost value=(params.httpsHost!)/>
        <@field type="input" name="httpsPort" label=uiLabelMap.FormFieldTitle_httpsPort value=(params.httpsPort!)/>
        <@field type="select" name="enableHttps" label=uiLabelMap.FormFieldTitle_enableHttps>
            <@field type="option" value=""></@field>
            <@field type="option" value="Y" selected=("Y" == params.enableHttps!)>Y</@field>
            <@field type="option" value="N" selected=("N" == params.enableHttps!)>N</@field>
        </@field>
        <@field type="input" name="standardContentPrefix" label=uiLabelMap.FormFieldTitle_standardContentPrefix value=(params.standardContentPrefix!)/>
        <@field type="input" name="secureContentPrefix" label=uiLabelMap.FormFieldTitle_secureContentPrefix value=(params.secureContentPrefix!)/>
        <@field type="input" name="cookieDomain" label=uiLabelMap.FormFieldTitle_cookieDomain value=(params.cookieDomain!)/>
      </@fieldset>
        
        <#--<@field type="hidden" name="productStoreId" value=(params.productStoreId!)/> // already set above -->
        <@field type="hidden" name="allowProductStoreChange" value=(params.allowProductStoreChange!)/>

        <#-- NOT doing this one for now, setting on ProductStore instead... 
            FIXME: this has a name problem too, so the below would not work as-is...
        <@field type="hidden" name="webSite_visualThemeId" value=(params.webSite_visualThemeId!)/> -->

</#if>

    </@form>



