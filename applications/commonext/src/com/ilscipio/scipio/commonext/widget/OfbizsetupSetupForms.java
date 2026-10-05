/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.commonext.widget;

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OfbizsetupSetupForms {

    @Form(
        name = "NewOrganization",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        target = "${target}${previousParams}",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "USE_ADDRESS", hidden = @HiddenField(value = "${USE_ADDRESS}")),
            @FormField(name = "require_email", hidden = @HiddenField(value = "${require_email}")),
            @FormField(name = "partyId", text = @TextField),
            @FormField(name = "groupName", title = "${uiLabelMap.SetupOrganizationName}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "ShippingAddressTitle", title = "${uiLabelMap.PartyAddressMailingShipping}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "USER_ADDRESS1", title = "${uiLabelMap.CommonAddress1}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "USER_ADDRESS2", title = "${uiLabelMap.CommonAddress2}", text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "USER_CITY", title = "${uiLabelMap.CommonCity}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "USER_STATE", title = "${uiLabelMap.CommonState}", requiredField = true, dropDown = @DropDownField),
            @FormField(name = "USER_POSTAL_CODE", title = "${uiLabelMap.CommonZipPostalCode}", requiredField = true, text = @TextField(size = 10, maxlength = 30)),
            @FormField(name = "USER_COUNTRY", title = "${uiLabelMap.CommonCountry}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Geo", description = "${geoId}: ${geoName}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "COUNTRY")}, orderBy = {@EntityOrderBy(fieldName = "geoId")}))),
            @FormField(name = "USER_ADDRESS_ALLOW_SOL", hidden = @HiddenField(value = "Y")),
            @FormField(name = "WorkPhoneTitle", title = "${uiLabelMap.PartyContactWorkPhoneNumber}", titleAreaStyle = "group-label", widgetStyle = "tooltip", display = @DisplayField(description = "${uiLabelMap.PartyPhoneNumberRequired}", alsoHidden = false)),
            @FormField(name = "USER_WORK_COUNTRY", title = "${uiLabelMap.CommonCountryCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_WORK_AREA", title = "${uiLabelMap.PartyAreaCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_WORK_CONTACT", title = "${uiLabelMap.PartyPhoneNumber}", text = @TextField(size = 15, maxlength = 15)),
            @FormField(name = "USER_WORK_EXT", title = "${uiLabelMap.PartyContactExt}", text = @TextField(size = 6, maxlength = 10)),
            @FormField(name = "USER_WORK_ALLOW_SOL", hidden = @HiddenField(value = "Y")),
            @FormField(name = "FaxPhoneTitle", title = "${uiLabelMap.PartyContactFaxPhoneNumber}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "USER_FAX_COUNTRY", title = "${uiLabelMap.CommonCountryCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_FAX_AREA", title = "${uiLabelMap.PartyAreaCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_FAX_CONTACT", title = "${uiLabelMap.PartyPhoneNumber}", text = @TextField(size = 15, maxlength = 15)),
            @FormField(name = "USER_FAX_EXT", title = "${uiLabelMap.PartyContactExt}", text = @TextField(size = 6, maxlength = 10)),
            @FormField(name = "USER_FAX_ALLOW_SOL", hidden = @HiddenField(value = "Y")),
            @FormField(name = "EmailAddressTitle", title = "${uiLabelMap.PartyEmailAddress}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "USER_EMAIL", title = "${uiLabelMap.CommonEmail}", useWhen = "require_email!=null", requiredField = true, text = @TextField(size = 60)),
            @FormField(name = "USER_EMAIL", title = "${uiLabelMap.CommonEmail}", useWhen = "require_email==null", requiredField = true, text = @TextField(size = 60)),
            @FormField(name = "USER_EMAIL_ALLOW_SOL", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface NewOrganization {}

    @Form(
        name = "NewCustomer",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        extendsForm = "NewUser",
        extendsResource = "component://party/widget/partymgr/PartyForms.xml",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "customerPartyId", hidden = @HiddenField(value = "CUST${partyId}")),
            @FormField(name = "USERNAME", title = "${uiLabelMap.CommonUsername}", useWhen = "displayPassword!=null", text = @TextField(size = 30)),
            @FormField(name = "PASSWORD", title = "${uiLabelMap.CommonPassword}", useWhen = "displayPassword!=null", password = @PasswordField(size = 15)),
            @FormField(name = "CONFIRM_PASSWORD", title = "${uiLabelMap.CommonPassword}", tooltip = "* ${uiLabelMap.CommonConfirm}", useWhen = "displayPassword!=null", password = @PasswordField(size = 15)),
            @FormField(name = "USERNAME", title = "${uiLabelMap.CommonUsername}", tooltip = "* ${uiLabelMap.PartyTemporaryPassword}", text = @TextField(size = 30)),
            @FormField(name = "USER_ADDRESS_ALLOW_SOL", hidden = @HiddenField(value = "Y")),
            @FormField(name = "USER_HOME_ALLOW_SOL", hidden = @HiddenField(value = "Y")),
            @FormField(name = "USER_WORK_ALLOW_SOL", hidden = @HiddenField(value = "Y")),
            @FormField(name = "USER_FAX_ALLOW_SOL", hidden = @HiddenField(value = "Y")),
            @FormField(name = "USER_MOBILE_ALLOW_SOL", hidden = @HiddenField(value = "Y")),
            @FormField(name = "USER_EMAIL_ALLOW_SOL", hidden = @HiddenField(value = "Y"))
        }
    )
    public interface NewCustomer {}

    @Form(
        name = "ViewOrganization",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        defaultMapName = "lookupGroup",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.SetupOrganizationPartyId}", display = @DisplayField),
            @FormField(name = "groupName", title = "${uiLabelMap.SetupOrganizationName}", display = @DisplayField),
            @FormField(name = "preferredCurrencyUomId", title = "${uiLabelMap.CommonCurrency}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${description}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}"))
        }
    )
    public interface ViewOrganization {}

    @Form(
        name = "EditFacility",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        target = "UpdateFacility",
        defaultMapName = "facility",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "facilityId", tooltip = "${uiLabelMap.ProductNotModificationRecrationFacility}", useWhen = "facility!=null", display = @DisplayField),
            @FormField(name = "facilityId", useWhen = "facility==null", requiredField = true, text = @TextField(defaultValue = "${partyId}")),
            @FormField(name = "facilityName", title = "${uiLabelMap.ProductName}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "description", title = "${uiLabelMap.SetupFacilityDescription}", text = @TextField(size = 60)),
            @FormField(name = "defaultDaysToShip", title = "${uiLabelMap.ProductDefaultDaysToShip}", text = @TextField(size = 10, maxlength = 20)),
            @FormField(name = "facilityTypeId", hidden = @HiddenField(value = "WAREHOUSE")),
            @FormField(name = "ownerPartyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "defaultInventoryItemTypeId", hidden = @HiddenField(value = "NON_SERIAL_INV_ITEM")),
            @FormField(name = "defaultWeightUomId", hidden = @HiddenField(value = "WT_lb")),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "facility!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", useWhen = "facility==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "facility==null", target = "CreateFacility")
        }
    )
    public interface EditFacility {}

    @Form(
        name = "EditProductStore",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        target = "updateProductStore",
        defaultMapName = "productStore",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productStoreId", tooltip = "${uiLabelMap.ProductNotModificationRecreatingProductStore}", useWhen = "productStore!=null", display = @DisplayField),
            @FormField(name = "productStoreId", useWhen = "productStore==null&&productStoreId==null", requiredField = true, text = @TextField(defaultValue = "${partyId}")),
            @FormField(name = "productStoreId", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${productStoreId}]", useWhen = "productStore==null&&productStoreId!=null", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "storeName", title = "${uiLabelMap.ProductStoreName}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "companyName", hidden = @HiddenField(value = "${partyGroup.groupName}")),
            @FormField(name = "primaryStoreGroupId", hidden = @HiddenField),
            @FormField(name = "title", hidden = @HiddenField),
            @FormField(name = "subtitle", hidden = @HiddenField),
            @FormField(name = "payToPartyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "inventoryFacilityId", hidden = @HiddenField(value = "${parameters.facilityId}")),
            @FormField(name = "visualThemeId", hidden = @HiddenField(value = "EC_DEFAULT")),
            @FormField(name = "manualAuthIsCapture", hidden = @HiddenField(value = "N")),
            @FormField(name = "prorateShipping", hidden = @HiddenField(value = "Y")),
            @FormField(name = "prorateTaxes", hidden = @HiddenField(value = "Y")),
            @FormField(name = "viewCartOnAdd", hidden = @HiddenField(value = "N")),
            @FormField(name = "autoSaveCart", hidden = @HiddenField(value = "N")),
            @FormField(name = "autoApproveReviews", hidden = @HiddenField(value = "N")),
            @FormField(name = "autoInvoiceDigitalItems", hidden = @HiddenField(value = "Y")),
            @FormField(name = "reqShipAddrForDigItems", hidden = @HiddenField(value = "Y")),
            @FormField(name = "isDemoStore", hidden = @HiddenField(value = "Y")),
            @FormField(name = "isImmediatelyFulfilled", hidden = @HiddenField(value = "N")),
            @FormField(name = "checkInventory", hidden = @HiddenField(value = "Y")),
            @FormField(name = "requireInventory", hidden = @HiddenField(value = "N")),
            @FormField(name = "reserveInventory", hidden = @HiddenField(value = "Y")),
            @FormField(name = "reserveOrderEnumId", hidden = @HiddenField(value = "INVRO_FIFO_REC")),
            @FormField(name = "balanceResOnOrderCreation", hidden = @HiddenField(value = "Y")),
            @FormField(name = "oneInventoryFacility", hidden = @HiddenField(value = "Y")),
            @FormField(name = "requirementMethodEnumId", hidden = @HiddenField),
            @FormField(name = "defaultCurrencyUomId", hidden = @HiddenField),
            @FormField(name = "defaultSalesChannelEnumId", hidden = @HiddenField(value = "WEB_SALES_CHANNEL")),
            @FormField(name = "allowPassword", hidden = @HiddenField(value = "Y")),
            @FormField(name = "retryFailedAuths", hidden = @HiddenField(value = "Y")),
            @FormField(name = "headerApprovedStatus", hidden = @HiddenField(value = "ORDER_APPROVED")),
            @FormField(name = "itemApprovedStatus", hidden = @HiddenField(value = "ITEM_APPROVED")),
            @FormField(name = "digitalItemApprovedStatus", hidden = @HiddenField(value = "ITEM_APPROVED")),
            @FormField(name = "headerDeclinedStatus", hidden = @HiddenField(value = "ORDER_REJECTED")),
            @FormField(name = "itemDeclinedStatus", hidden = @HiddenField(value = "ITEM_REJECTED")),
            @FormField(name = "headerCancelStatus", hidden = @HiddenField(value = "ORDER_CANCELLED")),
            @FormField(name = "itemCancelStatus", hidden = @HiddenField(value = "ITEM_CANCELLED")),
            @FormField(name = "storeCreditAccountEnumId", hidden = @HiddenField(value = "FIN_ACCOUNT")),
            @FormField(name = "explodeOrderItems", hidden = @HiddenField(value = "N")),
            @FormField(name = "checkGcBalance", hidden = @HiddenField(value = "N")),
            @FormField(name = "usePrimaryEmailUsername", hidden = @HiddenField(value = "N")),
            @FormField(name = "requireCustomerRole", hidden = @HiddenField(value = "N")),
            @FormField(name = "showCheckoutGiftOptions", hidden = @HiddenField(value = "Y")),
            @FormField(name = "selectPaymentTypePerItem", hidden = @HiddenField(value = "N")),
            @FormField(name = "showPricesWithVatTax", hidden = @HiddenField(value = "N")),
            @FormField(name = "showTaxIsExempt", hidden = @HiddenField(value = "Y")),
            @FormField(name = "vatTaxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "vatTaxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "prodSearchExcludeVariants", hidden = @HiddenField(value = "Y")),
            @FormField(name = "enableDigProdUpload", hidden = @HiddenField(value = "N")),
            @FormField(name = "digProdUploadCategoryId", hidden = @HiddenField),
            @FormField(name = "autoOrderCcTryExp", hidden = @HiddenField(value = "Y")),
            @FormField(name = "autoOrderCcTryOtherCards", hidden = @HiddenField(value = "Y")),
            @FormField(name = "autoOrderCcTryLaterNsf", hidden = @HiddenField(value = "Y")),
            @FormField(name = "autoApproveInvoice", hidden = @HiddenField(value = "Y")),
            @FormField(name = "autoApproveOrder", hidden = @HiddenField(value = "Y")),
            @FormField(name = "shipIfCaptureFails", hidden = @HiddenField(value = "Y")),
            @FormField(name = "setOwnerUponIssuance", hidden = @HiddenField),
            @FormField(name = "reqReturnInventoryReceive", hidden = @HiddenField(value = "N")),
            @FormField(name = "addToCartReplaceUpsell", hidden = @HiddenField),
            @FormField(name = "addToCartRemoveIncompat", hidden = @HiddenField),
            @FormField(name = "splitPayPrefPerShpGrp", hidden = @HiddenField),
            @FormField(name = "autoOrderCcTryLaterMax", hidden = @HiddenField),
            @FormField(name = "orderNumberPrefix", hidden = @HiddenField(value = "WS")),
            @FormField(name = "defaultLocaleString", hidden = @HiddenField(value = "en_US")),
            @FormField(name = "enableAutoSuggestionList", hidden = @HiddenField),
            @FormField(name = "showOutOfStockProducts", hidden = @HiddenField(value = "Y")),
            @FormField(name = "authDeclinedMessage", hidden = @HiddenField),
            @FormField(name = "authFraudMessage", hidden = @HiddenField),
            @FormField(name = "authErrorMessage", hidden = @HiddenField),
            @FormField(name = "defaultPassword", hidden = @HiddenField),
            @FormField(name = "inventoryFacilityAction", hidden = @HiddenField),
            @FormField(name = "paymentList", hidden = @HiddenField(value = "${paymentList}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productStore==null", target = "createProductStore")
        }
    )
    public interface EditProductStore {}

    @Form(
        name = "EditWebSite",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        extendsForm = "EditWebSite",
        extendsResource = "component://content/widget/website/WebSiteForms.xml",
        fields = {
            @FormField(name = "webSiteId", useWhen = "webSite==null&&webSiteId==null", requiredField = true, text = @TextField(defaultValue = "ScipioWebStore")),
            @FormField(name = "siteName", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "visualThemeSetId", hidden = @HiddenField(value = "ECOMMERCE")),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "httpHost", hidden = @HiddenField),
            @FormField(name = "httpPort", hidden = @HiddenField),
            @FormField(name = "httpsHost", hidden = @HiddenField),
            @FormField(name = "httpsPort", hidden = @HiddenField),
            @FormField(name = "enableHttps", hidden = @HiddenField),
            @FormField(name = "standardContentPrefix", hidden = @HiddenField),
            @FormField(name = "secureContentPrefix", hidden = @HiddenField),
            @FormField(name = "cookieDomain", hidden = @HiddenField),
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "allowProductStoreChange", hidden = @HiddenField)
        },
        actions = @FormActions(set = {@SetAction(field = "webSiteId", fromField = "webSite.webSiteId")})
    )
    public interface EditWebSite {}

    @Form(
        name = "EditProdCatalog",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        extendsForm = "EditProdCatalog",
        extendsResource = "component://product/widget/catalog/ProdCatalogForms.xml",
        fields = {
            @FormField(name = "prodCatalogId", useWhen = "prodCatalog==null&&prodCatalogId==null", requiredField = true, text = @TextField(defaultValue = "${partyId}")),
            @FormField(name = "catalogName", title = "${uiLabelMap.FormFieldTitle_prodCatalogName}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "productStoreId", hidden = @HiddenField(value = "${productStoreId}")),
            @FormField(name = "useQuickAdd", hidden = @HiddenField(value = "Y")),
            @FormField(name = "styleSheet", hidden = @HiddenField),
            @FormField(name = "headerLogo", hidden = @HiddenField),
            @FormField(name = "contentPathPrefix", hidden = @HiddenField),
            @FormField(name = "templatePathPrefix", hidden = @HiddenField),
            @FormField(name = "viewAllowPermReqd", hidden = @HiddenField(value = "N")),
            @FormField(name = "purchaseAllowPermReqd", hidden = @HiddenField(value = "N"))
        }
    )
    public interface EditProdCatalog {}

    @Form(
        name = "EditProductCategory",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        target = "updateProductCategory",
        defaultMapName = "productCategory",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productCategoryId", title = "${uiLabelMap.ProductProductCategoryId}", tooltip = "${uiLabelMap.ProductNotModificationRecrationCategory}.", useWhen = "productCategory!=null", display = @DisplayField),
            @FormField(name = "productCategoryId", useWhen = "productCategory==null&&productCategoryId==null", requiredField = true, text = @TextField(defaultValue = "${partyId}")),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "prodCatalogId", hidden = @HiddenField(value = "${prodCatalogId}")),
            @FormField(name = "productCategoryTypeId", hidden = @HiddenField(value = "CATALOG_CATEGORY")),
            @FormField(name = "categoryName", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "description", title = "${uiLabelMap.ProductCategoryDescription}", textarea = @TextareaField(rows = 2)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productCategory==null", target = "createProductCategory")
        }
    )
    public interface EditProductCategory {}

    @Form(
        name = "EditProduct",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        target = "createUpdateProduct",
        defaultMapName = "product",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "isCreate", useWhen = "product==null", hidden = @HiddenField(value = "true")),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", tooltip = "${uiLabelMap.ProductNotModificationRecreatingProduct}", useWhen = "product!=null", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", tooltip = "${uiLabelMap.ProductNotFindProductId} [${productId}]", useWhen = "product==null&&productId!=null", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "productId", useWhen = "product==null&&productId==null", requiredField = true, text = @TextField(defaultValue = "${partyId}")),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "productCategoryId", hidden = @HiddenField(value = "${productCategoryId}")),
            @FormField(name = "promoCat", hidden = @HiddenField(value = "${promoCat}")),
            @FormField(name = "productTypeId", hidden = @HiddenField(value = "FINISHED_GOOD")),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductInternalName}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "productName", title = "${uiLabelMap.ProductProductName}", text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "description", title = "${uiLabelMap.ProductShortDescription}", text = @TextField(size = 60)),
            @FormField(name = "defaultPrice", title = "${uiLabelMap.ProductDefaultPrice}", text = @TextField(size = 8, defaultValue = "${defaultPrice}")),
            @FormField(name = "averageCost", title = "${uiLabelMap.ProductAverageCost}", text = @TextField(size = 8, defaultValue = "${averageCost}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.ProductUpdateProduct}", useWhen = "product!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.ProductCreateProduct}", useWhen = "product==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditProduct {}

    @Form(
        name = "ListProduct",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        defaultMapName = "product",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "internalName", display = @DisplayField),
            @FormField(name = "productName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField)
        }
    )
    public interface ListProduct {}

    @Form(
        name = "ListOrganizations",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        type = FormType.LIST,
        title = "Internal Organizations",
        listName = "parties",
        oddRowStyle = "alternate-row",
        viewSize = 10,
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.AccountingCompanies}", useWhen = "partyAcctgPreference==null", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "viewprofile", description = "${partyGroup.groupName} [${partyId}]", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "partyId", title = "${uiLabelMap.AccountingCompanies}", useWhen = "partyAcctgPreference!=null", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "showMessage", description = "${partyGroup.groupName} [${partyId}]", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "setToCompleteAction", title = " ", useWhen = "partyAcctgPreference==null", widgetStyle = "${styles.link_run_sys} ${styles.action_complete}", hyperlink = @HyperlinkField(target = "OrganizationToComplete", description = "${uiLabelMap.SetupSetToComplete}", parameters = {@ParameterDef(paramName = "partyId")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "PartyAcctgPreference", valueField = "partyAcctgPreference"), @EntityOneAction(entityName = "PartyGroup", valueField = "partyGroup")})
    )
    public interface ListOrganizations {}

    @Form(
        name = "EditCustomer",
        location = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        extendsForm = "EditPerson",
        extendsResource = "component://party/widget/partymgr/PartyForms.xml",
        fields = {
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId", fromField = "parameters.partyId")}))
        }
    )
    public interface EditCustomer {}

}
