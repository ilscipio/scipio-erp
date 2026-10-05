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
package com.ilscipio.scipio.setup.service;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * SCIPIO: 4.0.0: Headless setup services, ported from the setup component minilang
 * (SetupEvents.xml, commonext SetupEvents.xml) so the Setup Wizard steps can be run
 * without the web screens (used by the setup MCP tools).
 */
public class SetupServices {

    private SetupServices() {}

    /**
     * Ported from commonext SetupEvents.xml#createOrganization.
     */
    public static Map<String, Object> setupCreateOrganization(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        try {
            Map<String, Object> partyGroupCtx = new LinkedHashMap<>();
            partyGroupCtx.put("groupName", context.get("groupName"));
            if (context.get("partyId") != null) {
                partyGroupCtx.put("partyId", context.get("partyId"));
            }
            if (context.get("taxIdNumber") != null) {
                partyGroupCtx.put("ein", context.get("taxIdNumber"));
            }
            partyGroupCtx.put("userLogin", userLogin);
            Map<String, Object> groupResult = dispatcher.runSync("createPartyGroup", partyGroupCtx);
            if (ServiceUtil.isError(groupResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(groupResult));
            }
            String partyId = (String) groupResult.get("partyId");

            Map<String, Object> orgRoleResult = dispatcher.runSync("createPartyRole",
                    UtilMisc.toMap("partyId", partyId, "roleTypeId", "INTERNAL_ORGANIZATIO", "userLogin", userLogin));
            if (ServiceUtil.isError(orgRoleResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(orgRoleResult));
            }
            // SCIPIO: matches minilang: organizations are also given the CARRIER role
            dispatcher.runSync("createPartyRole", UtilMisc.toMap("partyId", partyId, "roleTypeId", "CARRIER", "userLogin", userLogin));

            List<String> contactMechIds = new ArrayList<>();

            if (UtilValidate.isNotEmpty((String) context.get("address1"))) {
                Map<String, Object> addrCtx = new LinkedHashMap<>();
                addrCtx.put("partyId", partyId);
                addrCtx.put("address1", context.get("address1"));
                addrCtx.put("address2", context.get("address2"));
                addrCtx.put("city", context.get("city"));
                addrCtx.put("stateProvinceGeoId", context.get("stateProvinceGeoId"));
                addrCtx.put("postalCode", context.get("postalCode"));
                addrCtx.put("countryGeoId", context.get("countryGeoId"));
                addrCtx.put("userLogin", userLogin);
                Map<String, Object> addrResult = dispatcher.runSync("createPartyPostalAddress", addrCtx);
                if (ServiceUtil.isError(addrResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(addrResult));
                }
                String contactMechId = (String) addrResult.get("contactMechId");
                contactMechIds.add(contactMechId);
                for (String purpose : new String[] {"PAYMENT_LOCATION", "GENERAL_LOCATION", "BILLING_LOCATION"}) {
                    dispatcher.runSync("createPartyContactMechPurpose", UtilMisc.toMap(
                            "partyId", partyId, "contactMechId", contactMechId, "contactMechPurposeTypeId", purpose, "userLogin", userLogin));
                }
            }

            if (UtilValidate.isNotEmpty((String) context.get("contactNumber"))) {
                Map<String, Object> telCtx = new LinkedHashMap<>();
                telCtx.put("partyId", partyId);
                telCtx.put("countryCode", context.get("countryCode"));
                telCtx.put("contactNumber", context.get("contactNumber"));
                telCtx.put("userLogin", userLogin);
                Map<String, Object> telResult = dispatcher.runSync("createPartyTelecomNumber", telCtx);
                if (ServiceUtil.isError(telResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(telResult));
                }
                String contactMechId = (String) telResult.get("contactMechId");
                contactMechIds.add(contactMechId);
                dispatcher.runSync("createPartyContactMechPurpose", UtilMisc.toMap(
                        "partyId", partyId, "contactMechId", contactMechId, "contactMechPurposeTypeId", "PHONE_WORK", "userLogin", userLogin));
            }

            if (UtilValidate.isNotEmpty((String) context.get("emailAddress"))) {
                Map<String, Object> emailResult = dispatcher.runSync("createPartyEmailAddress", UtilMisc.toMap(
                        "partyId", partyId, "emailAddress", context.get("emailAddress"), "userLogin", userLogin));
                if (ServiceUtil.isError(emailResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(emailResult));
                }
                String contactMechId = (String) emailResult.get("contactMechId");
                contactMechIds.add(contactMechId);
                dispatcher.runSync("createPartyContactMechPurpose", UtilMisc.toMap(
                        "partyId", partyId, "contactMechId", contactMechId, "contactMechPurposeTypeId", "PRIMARY_EMAIL", "userLogin", userLogin));
            }

            // SCIPIO: setupUpdatePartyAcctgPreference is the setup-component variant of updatePartyAcctgPreference;
            // it accepts creating the PartyAcctgPreference record for a brand-new organization.
            if (context.get("currencyUomId") != null || context.get("fiscalYearStartMonth") != null || context.get("fiscalYearStartDay") != null) {
                Map<String, Object> acctgPrefCtx = new LinkedHashMap<>();
                acctgPrefCtx.put("partyId", partyId);
                acctgPrefCtx.put("baseCurrencyUomId", context.get("currencyUomId"));
                acctgPrefCtx.put("fiscalYearStartMonth", context.get("fiscalYearStartMonth"));
                acctgPrefCtx.put("fiscalYearStartDay", context.get("fiscalYearStartDay"));
                acctgPrefCtx.put("userLogin", userLogin);
                Map<String, Object> acctgPrefResult = dispatcher.runSync("setupUpdatePartyAcctgPreference", acctgPrefCtx);
                if (ServiceUtil.isError(acctgPrefResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(acctgPrefResult));
                }
            }

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("partyId", partyId);
            result.put("contactMechIds", contactMechIds);
            return result;
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError("Error creating organization: " + e.getMessage());
        }
    }

    /**
     * Ported from commonext SetupEvents.xml#createFacilityAndContactMech.
     */
    public static Map<String, Object> setupCreateFacility(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        try {
            Map<String, Object> facilityCtx = new LinkedHashMap<>();
            facilityCtx.put("facilityName", context.get("facilityName"));
            facilityCtx.put("ownerPartyId", context.get("ownerPartyId"));
            facilityCtx.put("facilityTypeId", UtilValidate.isNotEmpty((String) context.get("facilityTypeId"))
                    ? context.get("facilityTypeId") : "WAREHOUSE");
            facilityCtx.put("defaultInventoryItemTypeId", UtilValidate.isNotEmpty((String) context.get("defaultInventoryItemTypeId"))
                    ? context.get("defaultInventoryItemTypeId") : "NON_SERIAL_INV_ITEM");
            facilityCtx.put("userLogin", userLogin);
            Map<String, Object> facilityResult = dispatcher.runSync("createFacility", facilityCtx);
            if (ServiceUtil.isError(facilityResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(facilityResult));
            }
            String facilityId = (String) facilityResult.get("facilityId");

            if (UtilValidate.isNotEmpty((String) context.get("address1"))) {
                Map<String, Object> addrCtx = new LinkedHashMap<>();
                addrCtx.put("facilityId", facilityId);
                addrCtx.put("address1", context.get("address1"));
                addrCtx.put("address2", context.get("address2"));
                addrCtx.put("city", context.get("city"));
                addrCtx.put("stateProvinceGeoId", context.get("stateProvinceGeoId"));
                addrCtx.put("postalCode", context.get("postalCode"));
                addrCtx.put("countryGeoId", context.get("countryGeoId"));
                addrCtx.put("userLogin", userLogin);
                Map<String, Object> addrResult = dispatcher.runSync("createFacilityPostalAddress", addrCtx);
                if (ServiceUtil.isError(addrResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(addrResult));
                }
                String contactMechId = (String) addrResult.get("contactMechId");
                for (String purpose : new String[] {"SHIPPING_LOCATION", "SHIP_ORIG_LOCATION"}) {
                    dispatcher.runSync("createFacilityContactMechPurpose", UtilMisc.toMap(
                            "facilityId", facilityId, "contactMechId", contactMechId, "contactMechPurposeTypeId", purpose, "userLogin", userLogin));
                }
            }

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("facilityId", facilityId);

            // SCIPIO: create a default FacilityLocation so inventory can be received right away
            Map<String, Object> locResult = dispatcher.runSync("createFacilityLocation",
                    UtilMisc.toMap("facilityId", facilityId, "userLogin", userLogin));
            if (!ServiceUtil.isError(locResult)) {
                result.put("locationSeqId", locResult.get("locationSeqId"));
            }
            return result;
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError("Error creating facility: " + e.getMessage());
        }
    }

    /**
     * Ported from setup SetupEvents.xml#createProductStoreAndWebSite / commonext#createProductStoreWithDefaultSetting.
     */
    public static Map<String, Object> setupCreateStore(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        try {
            Map<String, Object> storeCtx = new LinkedHashMap<>();
            storeCtx.put("storeName", context.get("storeName"));
            storeCtx.put("payToPartyId", context.get("ownerPartyId"));
            storeCtx.put("inventoryFacilityId", context.get("inventoryFacilityId"));
            storeCtx.put("defaultCurrencyUomId", context.get("defaultCurrencyUomId"));
            storeCtx.put("defaultLocaleString", context.get("defaultLocaleString"));
            storeCtx.put("userLogin", userLogin);
            Map<String, Object> storeResult = dispatcher.runSync("createProductStore", storeCtx);
            if (ServiceUtil.isError(storeResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(storeResult));
            }
            String productStoreId = (String) storeResult.get("productStoreId");

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("productStoreId", productStoreId);

            String webSiteName = (String) context.get("webSiteName");
            String hostname = UtilValidate.isNotEmpty((String) context.get("hostname")) ? (String) context.get("hostname")
                    : (String) context.get("siteName");
            if (UtilValidate.isNotEmpty(webSiteName) || UtilValidate.isNotEmpty(hostname)) {
                Map<String, Object> webSiteCtx = new LinkedHashMap<>();
                webSiteCtx.put("siteName", UtilValidate.isNotEmpty(webSiteName) ? webSiteName : context.get("storeName"));
                webSiteCtx.put("productStoreId", productStoreId);
                if (UtilValidate.isNotEmpty(hostname)) {
                    webSiteCtx.put("httpHost", hostname);
                    webSiteCtx.put("httpsHost", hostname);
                }
                if (context.get("visualThemeSetId") != null) {
                    webSiteCtx.put("visualThemeSetId", context.get("visualThemeSetId"));
                }
                webSiteCtx.put("userLogin", userLogin);
                Map<String, Object> webSiteResult = dispatcher.runSync("createWebSite", webSiteCtx);
                if (ServiceUtil.isError(webSiteResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(webSiteResult));
                }
                result.put("webSiteId", webSiteResult.get("webSiteId"));
            }

            if (context.get("ownerPartyId") != null) {
                dispatcher.runSync("ensureProductStoreRole", UtilMisc.toMap(
                        "partyId", context.get("ownerPartyId"), "productStoreId", productStoreId, "roleTypeId", "OWNER", "userLogin", userLogin));
            }

            return result;
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError("Error creating store: " + e.getMessage());
        }
    }

    /**
     * SCIPIO: New (not in minilang, needed for the setup wizard catalog step).
     */
    public static Map<String, Object> setupCreateCatalog(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        try {
            Map<String, Object> catalogCtx = new LinkedHashMap<>();
            catalogCtx.put("catalogName", context.get("catalogName"));
            catalogCtx.put("userLogin", userLogin);
            Map<String, Object> catalogResult = dispatcher.runSync("createProdCatalog", catalogCtx);
            if (ServiceUtil.isError(catalogResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(catalogResult));
            }
            String prodCatalogId = (String) catalogResult.get("prodCatalogId");

            Timestamp now = UtilDateTime.nowTimestamp();
            Map<String, Object> storeCatalogResult = dispatcher.runSync("createProductStoreCatalog", UtilMisc.toMap(
                    "productStoreId", context.get("productStoreId"), "prodCatalogId", prodCatalogId, "fromDate", now, "userLogin", userLogin));
            if (ServiceUtil.isError(storeCatalogResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(storeCatalogResult));
            }

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("prodCatalogId", prodCatalogId);

            String rootCategoryName = (String) context.get("rootCategoryName");
            if (UtilValidate.isNotEmpty(rootCategoryName)) {
                Map<String, Object> categoryResult = dispatcher.runSync("createProductCategory", UtilMisc.toMap(
                        "productCategoryTypeId", "CATALOG_CATEGORY", "categoryName", rootCategoryName, "userLogin", userLogin));
                if (ServiceUtil.isError(categoryResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(categoryResult));
                }
                String productCategoryId = (String) categoryResult.get("productCategoryId");
                Map<String, Object> addResult = dispatcher.runSync("addProductCategoryToProdCatalog", UtilMisc.toMap(
                        "prodCatalogId", prodCatalogId, "productCategoryId", productCategoryId,
                        "prodCatalogCategoryTypeId", "PCCT_BROWSE_ROOT", "fromDate", now, "userLogin", userLogin));
                if (ServiceUtil.isError(addResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(addResult));
                }
                result.put("productCategoryId", productCategoryId);
            }
            return result;
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError("Error creating catalog: " + e.getMessage());
        }
    }

    /**
     * Ported from setup SetupEvents.xml#createUser (calls party's createUser chain via createPersonAndUserLogin).
     */
    public static Map<String, Object> setupCreateUser(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        try {
            Map<String, Object> personCtx = new LinkedHashMap<>();
            personCtx.put("userLoginId", context.get("userLoginId"));
            personCtx.put("currentPassword", context.get("password"));
            personCtx.put("currentPasswordVerify", context.get("password"));
            personCtx.put("firstName", context.get("firstName"));
            personCtx.put("lastName", context.get("lastName"));
            personCtx.put("userLogin", userLogin);
            Map<String, Object> personResult = dispatcher.runSync("createPersonAndUserLogin", personCtx);
            if (ServiceUtil.isError(personResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(personResult));
            }
            String newPartyId = (String) personResult.get("partyId");

            if (UtilValidate.isNotEmpty((String) context.get("emailAddress"))) {
                Map<String, Object> emailResult = dispatcher.runSync("createPartyEmailAddress", UtilMisc.toMap(
                        "partyId", newPartyId, "emailAddress", context.get("emailAddress"), "userLogin", userLogin));
                if (!ServiceUtil.isError(emailResult)) {
                    dispatcher.runSync("createPartyContactMechPurpose", UtilMisc.toMap(
                            "partyId", newPartyId, "contactMechId", emailResult.get("contactMechId"),
                            "contactMechPurposeTypeId", "PRIMARY_EMAIL", "userLogin", userLogin));
                }
            }

            String securityGroupId = UtilValidate.isNotEmpty((String) context.get("securityGroupId"))
                    ? (String) context.get("securityGroupId") : "FULLADMIN";
            dispatcher.runSync("addUserLoginToSecurityGroup", UtilMisc.toMap(
                    "userLoginId", context.get("userLoginId"), "groupId", securityGroupId, "userLogin", userLogin));

            if (context.get("partyId") != null) {
                String roleTypeId = UtilValidate.isNotEmpty((String) context.get("roleTypeId"))
                        ? (String) context.get("roleTypeId") : "EMPLOYEE";
                String partyRelationshipTypeId = UtilValidate.isNotEmpty((String) context.get("partyRelationshipTypeId"))
                        ? (String) context.get("partyRelationshipTypeId") : "EMPLOYMENT";
                Map<String, Object> relCtx = new LinkedHashMap<>();
                relCtx.put("partyIdFrom", context.get("partyId"));
                relCtx.put("roleTypeIdFrom", "INTERNAL_ORGANIZATIO");
                relCtx.put("roleTypeIdTo", roleTypeId);
                relCtx.put("partyRelationshipTypeId", partyRelationshipTypeId);
                relCtx.put("partyIdTo", newPartyId);
                relCtx.put("userLogin", userLogin);
                dispatcher.runSync("createPartyRelationshipAndRole", relCtx);
            }

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("partyId", newPartyId);
            result.put("userLoginId", context.get("userLoginId"));
            return result;
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError("Error creating user: " + e.getMessage());
        }
    }

    /**
     * Ported from commonext SetupEvents.xml#setupDefaultGeneralLedger (importDefaultGL/importGlAccounts in the
     * setup component's own SetupEvents.xml are unimplemented stubs; this is the actual working GL import logic).
     */
    public static Map<String, Object> setupLoadAccounting(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String organizationPartyId = (String) context.get("organizationPartyId");
        String countryGeoId = (String) context.get("countryGeoId");
        try {
            String note = "The general chart of accounts was loaded.";
            if ("DEU".equals(countryGeoId) || "AUT".equals(countryGeoId) || "ESP".equals(countryGeoId)) {
                note = "Chart of accounts for " + countryGeoId + " ships as an enterprise addon; the general chart was loaded.";
            }

            long existingGlAccounts = EntityQuery.use(delegator).from("GlAccount").queryCount();
            if (existingGlAccounts == 0) {
                Map<String, Object> importChartResult = dispatcher.runSync("entityImport", UtilMisc.toMap(
                        "filename", System.getProperty("ofbiz.home") + "/applications/accounting/data/DemoGeneralChartOfAccounts.xml",
                        "userLogin", userLogin));
                if (ServiceUtil.isError(importChartResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(importChartResult));
                }
            }

            Timestamp now = UtilDateTime.nowTimestamp();
            Map<String, Object> placeholderValues = new LinkedHashMap<>();
            placeholderValues.put("orgPartyId", organizationPartyId);
            placeholderValues.put("fromDate", now.toString());
            Map<String, Object> importOrgResult = dispatcher.runSync("entityImport", UtilMisc.toMap(
                    "filename", System.getProperty("ofbiz.home") + "/applications/commonext/data/GlAccountData.xml",
                    "placeholderValues", placeholderValues, "userLogin", userLogin));
            if (ServiceUtil.isError(importOrgResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(importOrgResult));
            }

            if (context.get("baseCurrencyUomId") != null || context.get("fiscalYearStartMonth") != null
                    || context.get("fiscalYearStartDay") != null) {
                Map<String, Object> acctgPrefCtx = new LinkedHashMap<>();
                acctgPrefCtx.put("partyId", organizationPartyId);
                acctgPrefCtx.put("baseCurrencyUomId", context.get("baseCurrencyUomId"));
                acctgPrefCtx.put("fiscalYearStartMonth", context.get("fiscalYearStartMonth"));
                acctgPrefCtx.put("fiscalYearStartDay", context.get("fiscalYearStartDay"));
                acctgPrefCtx.put("userLogin", userLogin);
                dispatcher.runSync("setupUpdatePartyAcctgPreference", acctgPrefCtx);
            }

            long glAccountCount = EntityQuery.use(delegator).from("GlAccountOrganization")
                    .where("organizationPartyId", organizationPartyId).queryCount();

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("glAccountCount", glAccountCount);
            result.put("taxAuthorityCreated", Boolean.FALSE);
            result.put("note", note);
            return result;
        } catch (GenericServiceException | org.ofbiz.entity.GenericEntityException e) {
            return ServiceUtil.returnError("Error loading accounting data: " + e.getMessage());
        }
    }

    /**
     * Ported from setup SetupEvents.xml#createTaxAuthority.
     */
    public static Map<String, Object> setupCreateTaxAuthority(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        try {
            Map<String, Object> taxAuthCtx = new LinkedHashMap<>();
            taxAuthCtx.put("groupName", context.get("groupName"));
            taxAuthCtx.put("taxAuthGeoId", context.get("taxAuthGeoId"));
            taxAuthCtx.put("taxAuthPartyId", context.get("taxAuthPartyId"));
            taxAuthCtx.put("requireTaxIdForExemption", context.get("requireTaxIdForExemption"));
            taxAuthCtx.put("includeTaxInPrice", context.get("includeTaxInPrice"));
            taxAuthCtx.put("partyTypeId", "PARTY_GROUP");
            taxAuthCtx.put("roleTypeId", "TAX_AUTHORITY");
            taxAuthCtx.put("partyId", context.get("taxAuthPartyId"));
            taxAuthCtx.put("userLogin", userLogin);
            Map<String, Object> taxAuthResult = dispatcher.runSync("createSetupTaxAuthority", taxAuthCtx);
            if (ServiceUtil.isError(taxAuthResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(taxAuthResult));
            }

            if (context.get("orgPartyId") != null) {
                Map<String, Object> taxInfoCtx = new LinkedHashMap<>();
                taxInfoCtx.put("partyId", context.get("orgPartyId"));
                taxInfoCtx.put("taxAuthGeoId", context.get("taxAuthGeoId"));
                taxInfoCtx.put("taxAuthPartyId", context.get("taxAuthPartyId"));
                taxInfoCtx.put("fromDate", UtilDateTime.nowTimestamp());
                if (context.get("taxIdNumber") != null) {
                    taxInfoCtx.put("partyTaxId", context.get("taxIdNumber"));
                }
                taxInfoCtx.put("userLogin", userLogin);
                Map<String, Object> taxInfoResult = dispatcher.runSync("createPartyTaxAuthInfo", taxInfoCtx);
                if (ServiceUtil.isError(taxInfoResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(taxInfoResult));
                }
            }

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("taxAuthGeoId", context.get("taxAuthGeoId"));
            result.put("taxAuthPartyId", context.get("taxAuthPartyId"));
            return result;
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError("Error creating tax authority: " + e.getMessage());
        }
    }
}
