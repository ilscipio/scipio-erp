<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {
        "companyName": (partyGroup.groupName)!"",
        "payToPartyId": partyId!"",
        <#--"partyId": partyId!"",-->
        "inventoryFacilityId": parameters.facilityId!, <#-- FIXME -->
        "visualThemeId": "EC_DEFAULT",
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
        "oneInventoryFacility": "Y",
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
        "defaultLocaleString": "en_US",
        "showOutOfStockProducts": "Y",
        "authDeclinedMessage": "There has been a problem with your method of payment. Please try a different method or call customer service.",
        "authFraudMessage": "Your order has been rejected and your account has been disabled due to fraud.",
        "authErrorMessage": "Problem connecting to payment processor; we will continue to retry and notify you by email."
        
        <#--"paymentList": paymentList![]-->
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":productStore!true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

    <@form id="EditProductStore" action=makeOfbizUrl(target) method="post">
        <@defaultWizardFormFields/>

      <#if productStore?has_content>
        <@field type="display" label=uiLabelMap.CommonId tooltip=uiLabelMap.ProductNotModificationRecreatingProductStore value=(params.productStoreId!)/>
        <@field type="hidden" name="productStoreId" value=(params.productStoreId!)/>
      <#else>
        <@field type="input" name="productStoreId" label=uiLabelMap.CommonId value=(params.productStoreId!)/>
      </#if>
        <@field type="input" name="storeName" label=uiLabelMap.ProductStoreName required=true size="30" maxlength="60" value=(params.storeName!)/>
        
        <#-- NOTE: some of these use fixedParams, others use params, it is based on whether original
           form widget specified a hidden value="..." (fixedParams) or not (params) -->
        <@field type="hidden" name="companyName" value=(fixedParams.companyName!)/>
        <@field type="hidden" name="primaryStoreGroupId" value=(params.primaryStoreGroupId!)/>
        <@field type="hidden" name="title" value=(params.title!)/>
        <@field type="hidden" name="subtitle" value=(params.subtitle!)/>
        <@field type="hidden" name="payToPartyId" value=(fixedParams.payToPartyId!)/>
        <@field type="hidden" name="inventoryFacilityId" value=(fixedParams.inventoryFacilityId!)/>
        <@field type="hidden" name="visualThemeId" value=(fixedParams.visualThemeId!)/>
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
        <@field type="hidden" name="defaultCurrencyUomId" value=(params.defaultCurrencyUomId!)/>
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
        <@field type="hidden" name="defaultLocaleString" value=(fixedParams.defaultLocaleString!)/>
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
        
        <@field type="submit" title=uiLabelMap.CommonUpdate class="+${styles.link_run_sys} ${styles.action_update}"/>
    </@form>
    
