package com.ilscipio.scipio.setup;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.setup.ContactMechPurposeInfo.FacilityContactMechPurposeInfo;
import com.ilscipio.scipio.setup.ContactMechPurposeInfo.PartyContactMechPurposeInfo;

/**
 * Raw setup step data check logic. 
 * USE {@link SetupWorker} TO INVOKE THESE DURING REAL SETUP. 
 * This is for general reuse and to keep the core logic clear/separate.
 */
public abstract class SetupDataUtil {

    public static final String module = SetupDataUtil.class.getName();

    public static final Set<String> ORGANIZATION_MAINADDR_PURPOSES = UtilMisc.unmodifiableLinkedHashSet(
            "GENERAL_LOCATION", "PAYMENT_LOCATION", "BILLING_LOCATION"
    );
        
    public static final Set<String> FACILITY_MAINADDR_PURPOSES = UtilMisc.unmodifiableLinkedHashSet(
            "SHIP_ORIG_LOCATION", "SHIPPING_LOCATION"
    );
    
    public static final Set<String> USER_MAINADDR_PURPOSES = UtilMisc.unmodifiableLinkedHashSet(
            "GENERAL_LOCATION", "BILLING_LOCATION"
    );
    
    protected SetupDataUtil() {
    }

    /*
     * ******************************************* 
     * Setup step elemental data state queries 
     * *******************************************
     */

    /*
     * These serve as partial substitutes for the screen groovy scripts.
     * 
     * These methods return 2-3 different things at the same time (TODO: maybe separate in future; combined for performance reasons):
     * * the "completed" state boolean (always)
     * * generic queries needed for all screens
     * * the requested object (facility/store/etc) if explicit ID requested OR a default if none requested
     *   -> if the ID requested is invalid or inapplicable (new/create), the method must NOT return
     *      any main record (facility/store/etc), but it must still evaluate the "completed" boolean.
     *      it may return extra records if needed (e.g. defaultFacility)
     *      
     * WARN: params map may contain unvalidated user input - others in the map may be already validated.
     * The caller (SetupWorker.CommonStepState subclasses) handles the implicit deps and decides which params must be pre-validated.
     * DO NOT call these methods from screen - all must go through SetupWorker.
     * 
     * TODO: these could return error message via ServiceUtil.returnError + let caller log, but not much point yet.
     */

    public static Map<String, Object> getOrganizationStepData(Delegator delegator, LocalDispatcher dispatcher, Map<String, Object> params, boolean useCache)
            throws GeneralException {
        // NOTE: coreCompleted is required for cases where need to recognized half-completed
        // it means just the core entities
        Map<String, Object> result = UtilMisc.toMap("completed", false, "coreCompleted", false);

        String orgPartyId = (String) params.get("orgPartyId");

        boolean isNewOrFailedCreate = isUnspecificRecordRequest(params, "Organization");
        
        if (UtilValidate.isNotEmpty(orgPartyId) && !isNewOrFailedCreate) {
            GenericValue party = delegator.findOne("Party", UtilMisc.toMap("partyId", orgPartyId), useCache);
            if (party != null) {
                GenericValue partyRole = delegator.findOne("PartyRole", 
                        UtilMisc.toMap("partyId", orgPartyId, "roleTypeId", "INTERNAL_ORGANIZATIO"), useCache);
                if (partyRole != null) {
                    GenericValue partyGroup = delegator.findOne("PartyGroup", UtilMisc.toMap("partyId", orgPartyId), useCache);
                    if (partyGroup != null) {
                        result.put("orgPartyId", orgPartyId);
                        result.put("party", party);
                        result.put("partyGroup", partyGroup);
                        result.put("coreCompleted", true);
                        
                        // TODO?: check for CARRIER roleTypeId??
                        
                        PartyContactMechPurposeInfo contactMechInfo = PartyContactMechPurposeInfo.forParty(delegator, dispatcher, orgPartyId, useCache, "Setup: Organization: ");
                        contactMechInfo.resultsToMap(result);
                        
                        Set<String> generalAddressContactMechPurposes = null;
                        GenericValue generalAddressContactMech = contactMechInfo.getPartyContactMechForPurpose(delegator, "GENERAL_LOCATION", useCache);
                        if (generalAddressContactMech == null) {
                            if (UtilMisc.booleanValueVersatile(params.get("useClosestAddress"), true)) {
                                Debug.logInfo("Setup: Organization: party '" + orgPartyId + "' has no GENERAL_LOCATION address; trying closest-matching for address purposes " + ORGANIZATION_MAINADDR_PURPOSES, module);
                                generalAddressContactMech = contactMechInfo.getClosestPartyContactMechForPurposes(delegator, ORGANIZATION_MAINADDR_PURPOSES, useCache);
                            }
                        }
                        if (generalAddressContactMech != null) {
                            generalAddressContactMechPurposes = contactMechInfo.getContactMechPurposes(generalAddressContactMech.getString("contactMechId"));
                        }
                        result.put("generalAddressContactMech", generalAddressContactMech);
                        result.put("generalAddressContactMechPurposes", generalAddressContactMechPurposes);
                        boolean generalAddressStandaloneCompleted = (generalAddressContactMech != null) && setContainsAll(generalAddressContactMechPurposes, ORGANIZATION_MAINADDR_PURPOSES);
                        result.put("generalAddressStandaloneCompleted", generalAddressStandaloneCompleted);
                        
                        result.put("locationPurposes", ORGANIZATION_MAINADDR_PURPOSES);
                        Map<String, GenericValue> locationContactMechs = contactMechInfo.getPartyContactMechForPurposeMap(delegator, ORGANIZATION_MAINADDR_PURPOSES, useCache);
                        boolean locationAddressesCompleted = (locationContactMechs.size() >= ORGANIZATION_MAINADDR_PURPOSES.size());
                        result.put("locationAddressesCompleted", locationAddressesCompleted);
                        
                        GenericValue workPhoneContactMech = contactMechInfo.getPartyContactMechForPurpose(delegator, "PHONE_WORK", useCache);
                        GenericValue faxPhoneContactMech = contactMechInfo.getPartyContactMechForPurpose(delegator, "FAX_NUMBER", useCache);
                        GenericValue primaryEmailContactMech = contactMechInfo.getPartyContactMechForPurpose(delegator, "PRIMARY_EMAIL", useCache);

                        result.put("workPhoneContactMech", workPhoneContactMech);
                        result.put("faxPhoneContactMech", faxPhoneContactMech);
                        result.put("primaryEmailContactMech", primaryEmailContactMech);
                        // not required anymore
//                        boolean simpleContactMechsCompleted = (workPhoneContactMech != null) && 
//                                (faxPhoneContactMech != null) &&
//                                (primaryEmailContactMech != null);
//                        result.put("simpleContactMechsCompleted", simpleContactMechsCompleted);
                        
                        boolean contactMechsCompleted = locationAddressesCompleted; // && simpleContactMechsCompleted
                        result.put("contactMechsCompleted", contactMechsCompleted);
                        
                        if (contactMechsCompleted) {
                            result.put("completed", true);
                        }
                    } else {
                        Debug.logError("Setup: Organization: Party '" + orgPartyId + "' does not have a PartyGroup record (invalid organization)", module);
                    }
                } else {
                    Debug.logError("Setup: Organization: Party '" + orgPartyId + "' does not have INTERNAL_ORGANIZATIO role (invalid organization)", module);
                }
            } else {
                Debug.logError("Setup: Organization: Party '" + orgPartyId + "' does not exist (invalid organization)", module);
            }
        }
        return result;
    }
    
    public static Map<String, Object> getUserStepData(Delegator delegator, LocalDispatcher dispatcher, Map<String, Object> params, boolean useCache)
            throws GeneralException {
        Map<String, Object> result = UtilMisc.toMap("completed", false, "coreCompleted", false);

        String orgPartyId = (String) params.get("orgPartyId");
        String userPartyId = (String) params.get("userPartyId");

        boolean isNewOrFailedCreate = isUnspecificRecordRequest(params, "User");

        GenericValue party = null;
        if (UtilValidate.isNotEmpty(orgPartyId) && !isNewOrFailedCreate) {
            if (UtilValidate.isNotEmpty(userPartyId)) {
                party = delegator.findOne("Party", UtilMisc.toMap("partyId", userPartyId), useCache);
                if (party != null) {
                    List<GenericValue> partyRelationshipList = party.getRelated("ToPartyRelationship",
                            UtilMisc.toMap("partyIdFrom", orgPartyId, "roleTypeIdFrom", "INTERNAL_ORGANIZATIO"), UtilMisc.toList("fromDate DESC"), false);
                    if (UtilValidate.isNotEmpty(partyRelationshipList)) {
                        if (partyRelationshipList.size() > 1) {
                            Debug.logWarning("Setup: User: party " + userPartyId + "' got multiple owner relationships for organization '" + orgPartyId + "'",
                                    module);
                        }
                    } else {
                        Debug.logError("Setup: User: party '" + userPartyId + "'" + " is not an owner of organization '" + orgPartyId + "'; ignoring", module);
                        party = null;
                    }
                } else {
                    Debug.logError("Setup: User: party '" + userPartyId + "' not found; ignoring", module);
                }
            } else {
                GenericValue orgParty = delegator.findOne("Party", UtilMisc.toMap("partyId", orgPartyId), useCache);
                if (orgParty != null) {
                    List<GenericValue> partyRelationshipList = orgParty.getRelated("FromPartyRelationship",
                            UtilMisc.toMap("partyIdFrom", orgPartyId, "roleTypeIdFrom", "INTERNAL_ORGANIZATIO"), UtilMisc.toList("fromDate DESC"), false);
                    if (UtilValidate.isNotEmpty(partyRelationshipList)) {
                        if (partyRelationshipList.size() > 1) {
                            Debug.logWarning("Setup: User " + userPartyId + "' got multiple owner relationships for organization '" + orgPartyId + "'", module);
                        }
                        GenericValue partyRelationshipOwner = EntityUtil.getFirst(partyRelationshipList);
                        party = partyRelationshipOwner.getRelatedOne("ToParty", false);
                    }
                }
            }
        }
        if (party != null) {
            userPartyId = party.getString("partyId");
            GenericValue userUserLogin = EntityUtil.getFirst(party.getRelated("UserLogin", UtilMisc.toMap("partyId", userPartyId), null, false));
            GenericValue userPerson = party.getRelatedOne("Person", false);
            
            result.put("userUserLogin", userUserLogin);
            result.put("userPerson", userPerson);
            result.put("userPartyId", userPartyId);
            result.put("userParty", party);
            result.put("coreCompleted", true);

            PartyContactMechPurposeInfo contactMechInfo = PartyContactMechPurposeInfo.forParty(delegator, dispatcher, orgPartyId, useCache, "Setup: User: ");
            contactMechInfo.resultsToMap(result);
            Set<String> generalAddressContactMechPurposes = null;
            GenericValue generalAddressContactMech = contactMechInfo.getPartyContactMechForPurpose(delegator, "GENERAL_LOCATION", useCache);
            if (generalAddressContactMech == null) {
                if (UtilMisc.booleanValueVersatile(params.get("useClosestAddress"), true)) {
                    Debug.logInfo("Setup: User: party '" + userPartyId + "' has no GENERAL_LOCATION address; trying closest-matching for address purposes "
                            + USER_MAINADDR_PURPOSES, module);
                    generalAddressContactMech = contactMechInfo.getClosestPartyContactMechForPurposes(delegator, USER_MAINADDR_PURPOSES, useCache);
                }
            }
            if (generalAddressContactMech != null) {
                generalAddressContactMechPurposes = contactMechInfo.getContactMechPurposes(generalAddressContactMech.getString("contactMechId"));
            }
            result.put("generalAddressContactMech", generalAddressContactMech);
            result.put("generalAddressContactMechPurposes", generalAddressContactMechPurposes);
            boolean generalAddressStandaloneCompleted = (generalAddressContactMech != null)
                    && setContainsAll(generalAddressContactMechPurposes, USER_MAINADDR_PURPOSES);
            result.put("generalAddressStandaloneCompleted", generalAddressStandaloneCompleted);

            result.put("locationPurposes", USER_MAINADDR_PURPOSES);
            Map<String, GenericValue> locationContactMechs = contactMechInfo.getPartyContactMechForPurposeMap(delegator, USER_MAINADDR_PURPOSES, useCache);
            boolean locationAddressesCompleted = (locationContactMechs.size() >= USER_MAINADDR_PURPOSES.size());
            result.put("locationAddressesCompleted", locationAddressesCompleted);

            GenericValue workPhoneContactMech = contactMechInfo.getPartyContactMechForPurpose(delegator, "PHONE_WORK", useCache);
            GenericValue faxPhoneContactMech = contactMechInfo.getPartyContactMechForPurpose(delegator, "FAX_NUMBER", useCache);
            GenericValue primaryEmailContactMech = contactMechInfo.getPartyContactMechForPurpose(delegator, "PRIMARY_EMAIL", useCache);

            result.put("workPhoneContactMech", workPhoneContactMech);
            result.put("faxPhoneContactMech", faxPhoneContactMech);
            result.put("primaryEmailContactMech", primaryEmailContactMech);

            boolean contactMechsCompleted = locationAddressesCompleted;
            result.put("contactMechsCompleted", contactMechsCompleted);

            if (contactMechsCompleted) {
                result.put("completed", true);
            }
        }
        return result;
    }

    public static Map<String, Object> getAccountingStepData(Delegator delegator, LocalDispatcher dispatcher, Map<String, Object> params, boolean useCache)
            throws GeneralException {
        Map<String, Object> result = UtilMisc.toMap("completed", false);

        // TODO

        return result;
    }

    public static Map<String, Object> getFacilityStepData(Delegator delegator, LocalDispatcher dispatcher, Map<String, Object> params, boolean useCache)
            throws GeneralException {
        Map<String, Object> result = UtilMisc.toMap("completed", false, "coreCompleted", false);

        String facilityId = (String) params.get("facilityId");
        String orgPartyId = (String) params.get("orgPartyId");
        String productStoreId = (String) params.get("productStoreId");

        // if new or failed create, we must still determine if overall "completed", but we cannot return
        // a specific facility ID - see the result map assignments below
        boolean isNewOrFailedCreate = isUnspecificRecordRequest(params, "Facility");
        
        GenericValue facility = null;
        if (UtilValidate.isNotEmpty(orgPartyId)) {
            if (UtilValidate.isNotEmpty(facilityId) && !isNewOrFailedCreate) { // ignore ID if new or failed create
                // filter by owner to prevent editing other companies's facilities
                facility = delegator.findOne("Facility", UtilMisc.toMap("facilityId", facilityId), useCache);
                if (facility != null) {
                    if (orgPartyId.equals(facility.getString("ownerPartyId"))) {
                        ; // ok
                    } else {
                        Debug.logError("Setup: Facility '" + facilityId + "' does not belong to organization '" + orgPartyId + "'; ignoring", module);
                        facility = null;
                    }
                } else {
                    Debug.logError("Setup: Facility '" + facilityId + "' not found; ignoring", module);
                }
            } else {
                if (UtilValidate.isNotEmpty(productStoreId)) {
                    // this case selects the best facility for the passed store
                    // TODO: REVIEW: this is not reusing the getStoreStepStateData for now because
                    // facility step now comes first and will create endless loop 
                    GenericValue productStore = delegator.findOne("ProductStore", UtilMisc.toMap("productStoreId", productStoreId), useCache);
                    if (productStore != null) {
                        if (orgPartyId.equals(productStore.getString("payToPartyId"))) {
                            facilityId = productStore.getString("inventoryFacilityId");
                            if (UtilValidate.isNotEmpty(facilityId)) {
                                facility = delegator.findOne("Facility", UtilMisc.toMap("facilityId", facilityId), useCache);
                                if (facility != null) {
                                    if (orgPartyId.equals(facility.getString("ownerPartyId"))) {
                                        ; // ok
                                    } else {
                                        Debug.logError("Setup: Warehouse '" + facilityId + "'" 
                                                + " does not belong to organization '" 
                                                + orgPartyId + "'; ignoring", module);
                                        facility = null;
                                    }
                                } else {
                                    Debug.logError("Setup: Warehouse '" + facilityId + "' not found; ignoring", module);
                                }
                            } else {
                                // TODO: REVIEW: there are multiple reasons for this;
                                // * does not support ProductStoreFacility-only or multi-facility for now;
                                // * product store was created without a facility
                                Debug.logWarning("Setup: Cannot get warehouse for store '" 
                                        + productStoreId + "'" + " because ProductStore.inventoryFacilityId is not set", module);
                            }
                        } else {
                            Debug.logError("Setup: ProductStore '" + productStoreId + "' does not appear to belong to"
                                    + " organization '" + orgPartyId + "'; ignoring", module);
                            productStore = null;
                        }
                    } else {
                        Debug.logError("Setup: ProductStore '" + productStoreId + "' not found; ignoring", module);
                    }
                } else {
                    List<GenericValue> facilities = delegator.findByAnd("Facility", UtilMisc.toMap("ownerPartyId", orgPartyId), null, useCache);
                    facility = EntityUtil.getFirst(facilities);
                    if (facilities.size() >= 2) {
                        Debug.logInfo("Setup: Multiple warehouses found for organization '" + orgPartyId 
                                + "'; selecting first ('" + facility.getString("facilityId") + "')", module);
                    }
                }
            }
        }
        if (facility != null) {
            facilityId = facility.getString("facilityId");
            if (!isNewOrFailedCreate) { // if new or failed create, do not return specific info
                result.put("facilityId", facilityId);
                result.put("facility", facility);
            }
            result.put("coreCompleted", true);

            // supporting one address only for now
//            Map<String, Object> fields = UtilMisc.toMap("facilityId", facilityId);
//            List<GenericValue> contactMechPurposes = EntityUtil.filterByDate(delegator.findByAnd("FacilityContactMechPurpose", 
//                    fields, UtilMisc.toList("fromDate DESC"), useCache));
//            if (!isNewOrFailedCreate) { // if new or failed create, do not return specific info
//                result.put("facilityContactMechPurposeList", contactMechPurposes);
//            }
            
            FacilityContactMechPurposeInfo contactMechInfo = FacilityContactMechPurposeInfo.forFacility(delegator, dispatcher, facilityId, useCache, "Setup: Facility: ");
            if (!isNewOrFailedCreate) {
                contactMechInfo.resultsToMap(result);
            }
            
            Set<String> shipAddressContactMechPurposes = null;
            GenericValue shipAddressContactMech = contactMechInfo.getFacilityContactMechForPurpose(delegator, "SHIP_ORIG_LOCATION", useCache);
            if (shipAddressContactMech == null) {
                if (UtilMisc.booleanValueVersatile(params.get("useClosestAddress"), true)) {
                    Debug.logInfo("Setup: Facility: facility '" + facilityId + "' has no SHIP_ORIG_LOCATION address; trying closest-matching for address purposes " + FACILITY_MAINADDR_PURPOSES, module);
                    shipAddressContactMech = contactMechInfo.getClosestFacilityContactMechForPurposes(delegator, FACILITY_MAINADDR_PURPOSES, useCache);
                }
            }
            if (shipAddressContactMech != null) {
                shipAddressContactMechPurposes = contactMechInfo.getContactMechPurposes(shipAddressContactMech.getString("contactMechId"));
            }
            if (!isNewOrFailedCreate) {
                result.put("shipAddressContactMech", shipAddressContactMech);
                result.put("shipAddressContactMechPurposes", shipAddressContactMechPurposes);
            }
            boolean shipAddressStandaloneCompleted = (shipAddressContactMech != null) && setContainsAll(shipAddressContactMechPurposes, FACILITY_MAINADDR_PURPOSES);
            result.put("shipAddressStandaloneCompleted", shipAddressStandaloneCompleted);

            if (!isNewOrFailedCreate) {
                result.put("locationPurposes", FACILITY_MAINADDR_PURPOSES);
            }
            Map<String, GenericValue> locationContactMechs = contactMechInfo.getFacilityContactMechForPurposeMap(delegator, FACILITY_MAINADDR_PURPOSES, useCache);
            boolean locationAddressesCompleted = (locationContactMechs.size() >= FACILITY_MAINADDR_PURPOSES.size());
            result.put("locationAddressesCompleted", locationAddressesCompleted);
            
            boolean completed = locationAddressesCompleted;
            result.put("completed", completed);
        }
        return result;
    }

    public static Map<String, Object> getCatalogStepStateData(Delegator delegator, LocalDispatcher dispatcher, Map<String, Object> params, boolean useCache)
            throws GeneralException {
        Map<String, Object> result = UtilMisc.toMap("completed", false);

        String productStoreId = (String) params.get("productStoreId");
        String prodCatalogId = (String) params.get("prodCatalogId");

        boolean isNewOrFailedCreate = isUnspecificRecordRequest(params, "Catalog");
        
        List<GenericValue> productStoreCatalogList = EntityUtil.filterByDate(delegator.findByAnd("ProductStoreCatalog", 
                UtilMisc.toMap("productStoreId", productStoreId), UtilMisc.toList("sequenceNum ASC"), useCache));
        result.put("productStoreCatalogList", productStoreCatalogList);
        
        GenericValue productStoreCatalog = null;
        if (UtilValidate.isNotEmpty(prodCatalogId) && !isNewOrFailedCreate) { // ignore ID if new or failed create
            List<GenericValue> filteredList = EntityUtil.filterByAnd(productStoreCatalogList, UtilMisc.toMap("prodCatalogId", prodCatalogId));
            productStoreCatalog = EntityUtil.getFirst(filteredList);
            if (productStoreCatalog == null) {
                Debug.logError("Setup: Could not find catalog '" + prodCatalogId + "' for store '" + productStoreId + "'", module);
            }
        } else {
            productStoreCatalog = EntityUtil.getFirst(productStoreCatalogList);
            if (productStoreCatalogList.size() >= 2) {
                Debug.logInfo("Setup: Store '" + productStoreId + "' has multiple active catalogs, selecting first ('" 
                        + productStoreCatalog.getString("prodCatalogId") + "') as default for setup"
                        + " (catalogs: " + getEntityStringFieldValues(productStoreCatalogList, "prodCatalogId", new ArrayList<String>(productStoreCatalogList.size())) + ")",
                        productStoreCatalog.getString("prodCatalogId"));
            } else if (productStoreCatalogList.size() == 0 && UtilValidate.isNotEmpty(prodCatalogId)) {
                Debug.logInfo("Setup: Store '" + productStoreId + "' has no active catalog", module);
            }
        }
        
        if (productStoreCatalog != null) {
            GenericValue prodCatalog = productStoreCatalog.getRelatedOne("ProdCatalog", useCache);
            if (!isNewOrFailedCreate) { // if new or failed create, do not return specific info
                result.put("productStoreCatalog", productStoreCatalog);
                result.put("prodCatalog", prodCatalog);
            }
            result.put("completed", true);
        }
        
        return result;
    }

    public static Map<String, Object> getStoreStepStateData(Delegator delegator, LocalDispatcher dispatcher, Map<String, Object> params, boolean includeWebsite, boolean useCache)
            throws GeneralException {
        Map<String, Object> result = UtilMisc.toMap("completed", false, "coreCompleted", false);

        String productStoreId = (String) params.get("productStoreId");
        String orgPartyId = (String) params.get("orgPartyId");
        
        boolean isNewOrFailedCreate = isUnspecificRecordRequest(params, "Store");
        
        GenericValue productStore = null;
        if (UtilValidate.isNotEmpty(productStoreId) && !isNewOrFailedCreate) { // ignore ID if new or failed create
            if (UtilValidate.isNotEmpty(orgPartyId)) {
                Map<String, Object> fields = UtilMisc.toMap("productStoreId", productStoreId, "payToPartyId", orgPartyId);
                List<GenericValue> productStores = delegator.findByAnd("ProductStore", fields, null, useCache);
                if (UtilValidate.isNotEmpty(productStores)) {
                    productStore = productStores.get(0);
                }
            } else {
                // we'll require a non-null orgPartyId here to simplify, so both parameters should be passed around
            }
        } else {
            // Unless asked to create a new store, read the first store by default;
            // in majority cases clients will create one store per company, so this saves some reloading.
            if (UtilValidate.isNotEmpty(orgPartyId)) {
                Map<String, Object> fields = UtilMisc.toMap("payToPartyId", orgPartyId);
                List<GenericValue> productStores = delegator.findByAnd("ProductStore", fields, null, useCache);
                if (UtilValidate.isNotEmpty(productStores)) {
                    productStore = productStores.get(0);
                    if (productStores.size() >= 2) {
                        Debug.logInfo("Setup: Organization '" + orgPartyId 
                            + "' has multiple ProductStores (" + productStores.size()
                            + "); assume first as default for the setup process (productStoreId: "
                            + productStore.getString("productStoreId") + ")", module);
                    }
                }
            }
        }

        if (productStore != null) {
            result.put("coreCompleted", true);
            productStoreId = productStore.getString("productStoreId");
            if (!isNewOrFailedCreate) { // if new or failed create, do not return specific info
                result.put("productStoreId", productStoreId);
                result.put("productStore", productStore);
            }

            boolean productStoreCompleted = false;
            String facilityId = productStore.getString("inventoryFacilityId");
            if (UtilValidate.isNotEmpty(facilityId)) {
                Map<String, Object> fields = UtilMisc.toMap("productStoreId", productStoreId, "facilityId", facilityId);
                List<GenericValue> productFacilityList = EntityUtil.filterByDate(delegator.findByAnd("ProductStoreFacility", 
                        fields, UtilMisc.toList("sequenceNum ASC"), useCache));
                if (UtilValidate.isNotEmpty(productFacilityList)) {
                    productStoreCompleted = true;
                } else {
                    Debug.logWarning("Setup: ProductStore '" + productStoreId + "' has no ProductStoreFacility relation for warehouse '" + facilityId
                            + "'; treating store as incomplete" + " (NOTE: may require manually fixing the schema)", module);
                }
            } else {
                Debug.logWarning("Setup: ProductStore '" + productStoreId + "' has no inventoryFacilityId field; treating store as incomplete", module);
            }
            if (!isNewOrFailedCreate) { // if new or failed create, do not return specific info
                result.put("facilityId", facilityId);
            }
            result.put("productStoreCompleted", productStoreCompleted);
            
            if (includeWebsite) {
                Map<String, Object> websiteParams = new HashMap<>(params);
                websiteParams.put("productStoreId", productStoreId);
                // TODO: REVIEW: not sure if will be needed
                //websiteParams.put("unspecReqWebsite", isNewOrFailedCreate);
                Map<String, Object> websiteResult = getWebsiteStepStateData(delegator, dispatcher, websiteParams, useCache);
                result.putAll(websiteResult); // this magically works for now
                
                boolean websiteCompleted = Boolean.TRUE.equals(websiteResult.get("completed"));
                result.put("websiteCompleted", websiteCompleted);
                
                result.put("completed", productStoreCompleted && websiteCompleted);
            } else {
                result.put("completed", productStoreCompleted);
            }
        }
        return result;
    }
    public static Map<String, Object> getStoreStepStateData(Delegator delegator, LocalDispatcher dispatcher, Map<String, Object> params, boolean useCache) throws GeneralException {
        return getStoreStepStateData(delegator, dispatcher, params, true, useCache);
    }
    
    public static Map<String, Object> getWebsiteStepStateData(Delegator delegator, LocalDispatcher dispatcher, Map<String, Object> params, boolean useCache)
            throws GeneralException {
        Map<String, Object> result = UtilMisc.toMap("completed", false);

        String productStoreId = (String) params.get("productStoreId");
        
        boolean isNewOrFailedCreate = isUnspecificRecordRequest(params, "Website");
        
        GenericValue webSite = null;
        Map<String, Object> fields = UtilMisc.toMap("productStoreId", productStoreId);
        List<GenericValue> webSiteList = delegator.findByAnd("WebSite", fields, null, useCache);
        webSite = getFirstMaxOneExpected(webSiteList, fields);
        if (!isNewOrFailedCreate) {
            result.put("webSiteList", webSiteList);
        }
        result.put("webSiteCount", webSiteList.size());
        
        if (webSite != null) {
            if (!isNewOrFailedCreate) { // if new or failed create, do not return specific info
                result.put("webSiteId", webSite.getString("webSiteId"));
                result.put("webSite", webSite);
            }
            result.put("completed", true);
        }
        return result;
    }

    /*
     * ******************************************* 
     * Generic helpers
     * *******************************************
     */

    private static GenericValue getFirstMaxOneExpected(List<GenericValue> values, Object query) {
        GenericValue value = EntityUtil.getFirst(values);
        if (values != null && values.size() >= 2) {
            // essential for debugging
            Debug.logWarning("Setup: Expected one " + value.getEntityName() 
                + " record at most, but found " + values.size() + " records matching for query: "
                + query + "; using first only (" + value.getPkShortValueString() + ")", module);
        }
        return value;
    }
    
    // TODO: REVIEW: unclear which code should order fromDate by ASC or DESC, so in the meantime,
    // use this to centralize any fix needed (for setup code only!)
    private static final List<String> defaultContactOrderBy = UtilMisc.unmodifiableArrayList("fromDate DESC");
    public static List<String> getDefaultContactOrderBy() {
        return defaultContactOrderBy;
    }

    private static <T extends Collection<String>> T getEntityStringFieldValues(List<GenericValue> values, String fieldName, T out) {
        for (GenericValue value : values) {
            String str = value.getString(fieldName);
            if (str != null)
                out.add(str);
        }
        return out;
    }
    
    private static boolean isEventError(Map<String, Object> params) {
        return SetupEvents.isPreviousEventSavedError(params);
    }
    
    // Exact request states
    
    static boolean isNewRecordRequest(Map<String, Object> params, String stepNameCamel) {
        return UtilMisc.booleanValueVersatile(params.get("new" + stepNameCamel), false);
    }
    
    static boolean isCreateRecordRequest(Map<String, Object> params, String stepNameCamel) {
        return UtilMisc.booleanValueVersatile(params.get("isCreate" + stepNameCamel), false);
    }
    
    static boolean isFailedCreateRecordRequest(Map<String, Object> params, String stepNameCamel) {
        return isCreateRecordRequest(params, stepNameCamel) && isEventError(params);
    }
    
    static boolean isDeleteRecordRequest(Map<String, Object> params, String stepNameCamel) {
        return UtilMisc.booleanValueVersatile(params.get("isDelete" + stepNameCamel), false);
    }
    
    static boolean isSuccessDeleteRecordRequest(Map<String, Object> params, String stepNameCamel) {
        return isDeleteRecordRequest(params, stepNameCamel) && !isEventError(params);
    }
    
    // Aggregate/high-level states
    
    /**
     * Returns true if "new" form was requested 
     * OR if a form was submitted as create and creation failed
     * OR it's a delete request.
     * <p>
     * For second case, if create form was submitted with a specific ID requested but error because the ID already exists, 
     * we don't want the page to look up existing, because it discards the user input.
     * <p>
     * newXxx: passed when loading the page/form
     * isCreateXxx: passed with form submit
     * isDeleteXxx: passed with delete form submit
     */
    static boolean isUnspecificRecordRequest(Map<String, Object> params, String stepNameCamel) {
        //stepName = stepName.substring(0, 1).toUpperCase() + stepName.substring(1);
        
        // SPECIAL: this can be used internally to override
        Boolean unspecific = UtilMisc.booleanValue(params.get("unspecReq" + stepNameCamel));
        if (unspecific != null) return unspecific;
        
        return isNewRecordRequest(params, stepNameCamel) || 
                isFailedCreateRecordRequest(params, stepNameCamel) || 
                isSuccessDeleteRecordRequest(params, stepNameCamel);
    }
    
    static boolean isEffectiveNewRecordRequest(Map<String, Object> params, String stepNameCamel) {
        return isNewRecordRequest(params, stepNameCamel) || 
                isFailedCreateRecordRequest(params, stepNameCamel);
    }
    
    
    private static <T> boolean setContainsAll(Set<T> set, Iterable<T> values) {
        if (set == null) return false;
        for(T value : values) {
            if (!set.contains(value)) return false;
        }
        return true;
    }

}
