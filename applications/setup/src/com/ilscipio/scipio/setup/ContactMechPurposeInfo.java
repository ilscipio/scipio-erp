package com.ilscipio.scipio.setup;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;

/**
 * Helper class for {@link SetupDataUtil}.
 * <p>
 * TODO: REVIEW: it could simpler in screens in future to just show more than one address with their purposes than
 * to spend this effort trying to find the "closest" single address to show... but implications for UI.
 * <p>
 * NOT thread-safe due to logPrefix.
 */
abstract class ContactMechPurposeInfo {
    public static final String module = ContactMechPurposeInfo.class.getName();
    
    //private final List<GenericValue> contactMechAndPurposeList;
    protected final Map<String, Set<String>> contactMechPurposes;
    protected final Map<String, Set<String>> purposeContactMechs;
    protected final String logPrefix;

    protected ContactMechPurposeInfo(Map<String, Set<String>> contactMechPurposes, Map<String, Set<String>> purposeContactMechs, String logPrefix) {
        //this.contactMechAndPurposeList = contactMechAndPurposeList;
        this.contactMechPurposes = contactMechPurposes;
        this.purposeContactMechs = purposeContactMechs;
        this.logPrefix = logPrefix != null ? logPrefix : "";
    }
    
//    public List<GenericValue> getContactMechAndPurposeList() {
//        return contactMechAndPurposeList;
//    }
    
    public Map<String, Set<String>> getContactMechPurposes() {
        return contactMechPurposes;
    }
    
    public Map<String, Set<String>> getPurposeContactMechs() {
        return purposeContactMechs;
    }
    
    protected abstract String getOwnerLogPrefix();
    
    public Set<String> getContactMechPurposes(String contactMechId) {
        Set<String> purposes = contactMechPurposes.get(contactMechId);
        return purposes != null ? purposes : Collections.<String>emptySet();
    }
    
    public Set<String> getPurposeContactMechs(String purpose) {
        Set<String> contactMechIds = purposeContactMechs.get(purpose);
        return contactMechIds != null ? contactMechIds : Collections.<String>emptySet();
    }
    
    protected static void populateIdMaps(List<? extends Map<String, Object>> contactMechAndPurposeList, String contactMechIdField, String purposeTypeIdField, 
            Map<String, Set<String>> contactMechPurposes, Map<String, Set<String>> purposeContactMechs) {
        for(Map<String, Object> contactMechAndPurpose : contactMechAndPurposeList) {
            String contactMechId = (String) contactMechAndPurpose.get(contactMechIdField);
            String purpose = (String) contactMechAndPurpose.get(purposeTypeIdField);
            
            Set<String> purposes = contactMechPurposes.get(contactMechId);
            if (purposes == null) {
                purposes = new LinkedHashSet<>();
                contactMechPurposes.put(contactMechId, purposes);
            }
            purposes.add(purpose);
            
            Set<String> contactMechIds = purposeContactMechs.get(purpose);
            if (contactMechIds == null) {
                contactMechIds = new LinkedHashSet<>();
                purposeContactMechs.put(purpose, contactMechIds);
            }
            contactMechIds.add(contactMechId);
        }
    }
    
    public void resultsToMap(Map<String, Object> map) {
        //map.put("ownerId", ownerId); // not for us
        //map.put("contactMechAndPurposeList", contactMechAndPurposeList);
        map.put("contactMechPurposes", contactMechPurposes);
        map.put("purposeContactMechs", purposeContactMechs);
    }
    
    /*
     * ******************************************* 
     * Purpose queries
     * *******************************************
     */
    
    public String getContactMechIdForPurpose(String purpose) {
        Set<String> contactMechIds = purposeContactMechs.get(purpose);
        if (contactMechIds != null && contactMechIds.size() > 0) {
            String id = contactMechIds.iterator().next();
            if (contactMechIds.size() >= 2) {
                Debug.logWarning(getOwnerLogPrefix() + " has multiple active contact mechs for purpose '" + purpose 
                        + "'; using first only (contactMechId '" + id + "')", module);
            } else {
                Debug.logInfo(getOwnerLogPrefix() + ": contactMechId '" + id + "' found matching purpose '" + purpose 
                        + "'", module);
            }
            return id;
        } else {
            Debug.logInfo(getOwnerLogPrefix() + " has no contact mech for purpose '" + purpose + "'", module);
        }
        return null;
    }
    
    public GenericValue getContactMechForPurpose(Delegator delegator, String purpose, boolean useCache) throws GenericEntityException {
        String contactMechId = getContactMechIdForPurpose(purpose);
        return getContactMechById(delegator, contactMechId, useCache);
    }

    

    
    /**
     * BEST-EFFORT! Returns closest match only. May be incomplete.
     */
    public String getClosestContactMechIdForPurposes(Set<String> purposes) {
        Map<String, Integer> matchCounts = getContactMechIdMatchCountsForPurposes(purposes);
        Set<String> contactMechIds = getClosestValuesByCount(matchCounts, purposes.size());
        if (contactMechIds != null && contactMechIds.size() > 0) {
            String id = contactMechIds.iterator().next();
            if (contactMechIds.size() >= 2) {
                Debug.logWarning(getOwnerLogPrefix() + " has multiple active candidate contact mechs for purposes " + purposes 
                        + " (contactMechIds: " + contactMechIds + "); using first only (contactMechId '" + id + "')", module);
            } 
            int count = matchCounts.get(id);
            if (count >= purposes.size()) {
                Debug.logInfo(getOwnerLogPrefix() + ": contactMechId '" + id + "' found matching purposes " + purposes, module);
            } else {
                Debug.logInfo(getOwnerLogPrefix() + ": contactMechId '" + id + "' only PARTIALLY matches purposes " + purposes, module);
            }
            return id;
        } else {
            Debug.logInfo(getOwnerLogPrefix() + " has no contact mech matching purposes " + purposes, module);
        }
        return null;
    }
    
    public GenericValue getClosestContactMechForPurposes(Delegator delegator, Set<String> purposes, boolean useCache) throws GenericEntityException {
        String contactMechId = getClosestContactMechIdForPurposes(purposes);
        return getContactMechById(delegator, contactMechId, useCache);
    }
    
    private GenericValue getContactMechById(Delegator delegator, String contactMechId, boolean useCache) throws GenericEntityException {
        if (UtilValidate.isNotEmpty(contactMechId)) {
            return delegator.findOne("ContactMech", 
                    UtilMisc.toMap("contactMechId", contactMechId), useCache);
        }
        return null;
    }
    
    Map<String, Integer> getContactMechIdMatchCountsForPurposes(Set<String> purposes) {
        return makeValueCountMap(purposeContactMechs, purposes);
    }
    
    /*
     * ******************************************* 
     * Count queries (purpose matching)
     * *******************************************
     */
    
    /**
     * Makes a map of the number of the number of occurrences for each value
     * in found in the sets of the map, considered only the passed keys.
     */
    Map<String, Integer> makeValueCountMap(Map<String, Set<String>> map, Set<String> keys) {
        if (keys == null) keys = map.keySet();
        Map<String, Integer> counts = new HashMap<>();
        for(String key : keys) {
            Set<String> values = map.get(key);
            if (values != null) {
                for(String contactMechId : values) {
                    Integer count = counts.get(contactMechId);
                    if (count == null) count = 0;
                    count++;
                    counts.put(contactMechId, count);
                }
            }
        }
        return counts;
    }
    
    /**
     * Arranges values in a map by their counts.
     */
    static Map<Integer, Set<String>> makeCountValueMap(Map<String, Integer> idCountMap) {
        Map<Integer, Set<String>> countMechs = new HashMap<>();
        for(Map.Entry<String, Integer> entry : idCountMap.entrySet()) {
            Set<String> set = countMechs.get(entry.getValue());
            if (set == null) {
                set = new LinkedHashSet<>();
                countMechs.put(entry.getValue(), set);
            }
            set.add(entry.getKey());
        }
        return countMechs;
    }

    /**
     * Gets the values that have occurrences closest to given target count.
     * Above the count is always preferred to below the count, but exact is preferred to above.
     * TODO: VERIFY this works.
     */
    static Set<String> getClosestValuesByCount(Map<String, Integer> counts, int targetCount) {
        Map<Integer, Set<String>> countMechs = makeCountValueMap(counts);
        if (countMechs.isEmpty()) return Collections.emptySet();
        List<Integer> highToLowCounts = new ArrayList<>(countMechs.keySet());
        Collections.sort(highToLowCounts, Collections.reverseOrder());
        
        ListIterator<Integer> it = highToLowCounts.listIterator();
        int count = it.next();
        while(it.hasNext() && (count = it.next()) >= targetCount) {
            ;
        }
        if (count >= targetCount) return countMechs.get(count);
        else {
            it.previous(); // discard to go back one
            if (it.hasPrevious()) return countMechs.get(it.previous());
            else return countMechs.get(count);
        }
    }
    
    /*
     * ******************************************* 
     * Entity-specific
     * *******************************************
     */
    
    public static class PartyContactMechPurposeInfo extends ContactMechPurposeInfo {
        protected final String partyId;
        protected final List<GenericValue> partyContactMechPurposeList;
        
        protected PartyContactMechPurposeInfo(String partyId, Map<String, Set<String>> contactMechPurposes,
                Map<String, Set<String>> purposeContactMechs, List<GenericValue> partyContactMechPurposeList, String logPrefix) {
            super(contactMechPurposes, purposeContactMechs, logPrefix);
            this.partyId = partyId;
            this.partyContactMechPurposeList = partyContactMechPurposeList;
        }
        
        public static PartyContactMechPurposeInfo forParty(Delegator delegator, LocalDispatcher dispatcher, String partyId, boolean useCache, String logPrefix) throws GenericEntityException {
            List<EntityCondition> condList = new ArrayList<>();
            condList.add(EntityCondition.makeCondition("partyId", partyId));
            condList.add(EntityUtil.getFilterByDateExpr());
            List<GenericValue> partyContactMechPurposeList = delegator.findList("PartyContactMechPurpose", 
                    EntityCondition.makeCondition(condList, EntityOperator.AND), null, null, null, useCache);
            return fromPartyContactMechPurposeList(partyId, partyContactMechPurposeList, logPrefix);
        }
        
        public static PartyContactMechPurposeInfo fromPartyContactMechPurposeList(String partyId, List<GenericValue> partyContactMechPurposeList, String logPrefix) {
            Map<String, Set<String>> contactMechPurposes = new HashMap<>();
            Map<String, Set<String>> purposeContactMechs = new HashMap<>();
            populateIdMaps(partyContactMechPurposeList, "contactMechId", "contactMechPurposeTypeId", contactMechPurposes, purposeContactMechs);
            return new PartyContactMechPurposeInfo(partyId,
                    Collections.unmodifiableMap(contactMechPurposes), 
                    Collections.unmodifiableMap(purposeContactMechs),
                    Collections.unmodifiableList(partyContactMechPurposeList),
                    logPrefix);
        }

        @Override
        public void resultsToMap(Map<String, Object> map) {
            super.resultsToMap(map);
            map.put("partyContactMechPurposeList", partyContactMechPurposeList);
        }

        public GenericValue getPartyContactMechForPurpose(Delegator delegator, String purpose, boolean useCache) throws GenericEntityException {
            String contactMechId = getContactMechIdForPurpose(purpose);
            return getPartyContactMechById(delegator, contactMechId, useCache);
        }
        
        public Map<String, GenericValue> getPartyContactMechForPurposeMap(Delegator delegator, Set<String> purposes, boolean useCache) throws GenericEntityException {
            Map<String, GenericValue> map = new HashMap<>();
            for(String purpose : purposes) {
                GenericValue contactMech = getPartyContactMechForPurpose(delegator, purpose, useCache);
                if (contactMech != null) {
                    map.put(purpose, contactMech);
                }
            }
            return map;
        }
        
        public GenericValue getClosestPartyContactMechForPurposes(Delegator delegator, Set<String> purposes, boolean useCache) throws GenericEntityException {
            String contactMechId = getClosestContactMechIdForPurposes(purposes);
            return getPartyContactMechById(delegator, contactMechId, useCache);
        }

        private GenericValue getPartyContactMechById(Delegator delegator, String contactMechId, boolean useCache) throws GenericEntityException {
            if (UtilValidate.isNotEmpty(contactMechId)) {
                List<EntityCondition> condList = new ArrayList<>();
                condList.add(EntityCondition.makeCondition("contactMechId", contactMechId));
                condList.add(EntityCondition.makeCondition("partyId", partyId));
                condList.add(EntityUtil.getFilterByDateExpr());
                List<GenericValue> pcmList = delegator.findList("PartyContactMech", EntityCondition.makeCondition(condList, EntityOperator.AND),
                        null, SetupDataUtil.getDefaultContactOrderBy(), null, useCache);
                if (pcmList.size() > 0) {
                    GenericValue result = pcmList.get(0);
                    if (pcmList.size() > 2) {
                        Debug.logWarning("Setup: Multiple active PartyContactMech records found for contactMechId '" 
                            + contactMechId + "' and partyId '" + partyId + "'; using first only (fromDate '" + result.get("fromDate") + ")'", module);
                    }
                    return result;
                }
            }
            return null;
        }

        @Override
        protected String getOwnerLogPrefix() {
            return logPrefix + "party '" + partyId + "'";
        }
        
    }
    
    public static class FacilityContactMechPurposeInfo extends ContactMechPurposeInfo {
        protected final String facilityId;
        protected final List<GenericValue> facilityContactMechPurposeList;
        
        protected FacilityContactMechPurposeInfo(String facilityId, Map<String, Set<String>> contactMechPurposes,
                Map<String, Set<String>> purposeContactMechs, List<GenericValue> facilityContactMechPurposeList, String logPrefix) {
            super(contactMechPurposes, purposeContactMechs, logPrefix);
            this.facilityId = facilityId;
            this.facilityContactMechPurposeList = facilityContactMechPurposeList;
        }
        
        public static FacilityContactMechPurposeInfo forFacility(Delegator delegator, LocalDispatcher dispatcher, String facilityId, boolean useCache, String logPrefix) throws GenericEntityException {
            List<EntityCondition> condList = new ArrayList<>();
            condList.add(EntityCondition.makeCondition("facilityId", facilityId));
            condList.add(EntityUtil.getFilterByDateExpr());
            List<GenericValue> facilityContactMechPurposeList = delegator.findList("FacilityContactMechPurpose", 
                    EntityCondition.makeCondition(condList, EntityOperator.AND), null, null, null, useCache);
            return fromFacilityContactMechPurposeList(facilityId, facilityContactMechPurposeList, logPrefix);
        }
        
        public static FacilityContactMechPurposeInfo fromFacilityContactMechPurposeList(String facilityId, List<GenericValue> facilityContactMechPurposeList, String logPrefix) {
            Map<String, Set<String>> contactMechPurposes = new HashMap<>();
            Map<String, Set<String>> purposeContactMechs = new HashMap<>();
            populateIdMaps(facilityContactMechPurposeList, "contactMechId", "contactMechPurposeTypeId", contactMechPurposes, purposeContactMechs);
            return new FacilityContactMechPurposeInfo(facilityId,
                    Collections.unmodifiableMap(contactMechPurposes), 
                    Collections.unmodifiableMap(purposeContactMechs),
                    Collections.unmodifiableList(facilityContactMechPurposeList),
                    logPrefix);
        }
        
        @Override
        public void resultsToMap(Map<String, Object> map) {
            super.resultsToMap(map);
            map.put("facilityContactMechPurposeList", facilityContactMechPurposeList);
        }
        
        public GenericValue getFacilityContactMechForPurpose(Delegator delegator, String purpose, boolean useCache) throws GenericEntityException {
            String contactMechId = getContactMechIdForPurpose(purpose);
            return getFacilityContactMechById(delegator, contactMechId, useCache);
        }
        
        public Map<String, GenericValue> getFacilityContactMechForPurposeMap(Delegator delegator, Set<String> purposes, boolean useCache) throws GenericEntityException {
            Map<String, GenericValue> map = new HashMap<>();
            for(String purpose : purposes) {
                GenericValue contactMech = getFacilityContactMechForPurpose(delegator, purpose, useCache);
                if (contactMech != null) {
                    map.put(purpose, contactMech);
                }
            }
            return map;
        }
        
        public GenericValue getClosestFacilityContactMechForPurposes(Delegator delegator, Set<String> purposes, boolean useCache) throws GenericEntityException {
            String contactMechId = getClosestContactMechIdForPurposes(purposes);
            return getFacilityContactMechById(delegator, contactMechId, useCache);
        }
        
        private GenericValue getFacilityContactMechById(Delegator delegator, String contactMechId, boolean useCache) throws GenericEntityException {
            if (UtilValidate.isNotEmpty(contactMechId)) {
                List<EntityCondition> condList = new ArrayList<>();
                condList.add(EntityCondition.makeCondition("contactMechId", contactMechId));
                condList.add(EntityCondition.makeCondition("facilityId", facilityId));
                condList.add(EntityUtil.getFilterByDateExpr());
                List<GenericValue> pcmList = delegator.findList("FacilityContactMech", EntityCondition.makeCondition(condList, EntityOperator.AND),
                        null, SetupDataUtil.getDefaultContactOrderBy(), null, useCache);
                if (pcmList.size() > 0) {
                    GenericValue result = pcmList.get(0);
                    if (pcmList.size() > 2) {
                        Debug.logWarning("Setup: Multiple active FacilityContactMech records found for contactMechId '" 
                            + contactMechId + "' and facility '" + facilityId + "'; using first only (fromDate: " + result.get("fromDate") + ")", module);
                    }
                    return result;
                }
            }
            return null;
        }

        @Override
        protected String getOwnerLogPrefix() {
            return logPrefix + "facility '" + facilityId + "'";
        }
    }
    
}