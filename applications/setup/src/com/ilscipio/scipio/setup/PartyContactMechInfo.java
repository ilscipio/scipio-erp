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
 * NOT thread-safe due to logPrefix.
 */
class PartyContactMechInfo {
    public static final String module = PartyContactMechInfo.class.getName();
    
    private final String partyId;
    private final List<GenericValue> contactMechAndPurposeList;
    private final Map<String, Set<String>> contactMechPurposes;
    private final Map<String, Set<String>> purposeContactMechs;
    private final String logPrefix;

    protected PartyContactMechInfo(String partyId, List<GenericValue> contactMechAndPurposeList,
            Map<String, Set<String>> contactMechPurposes, Map<String, Set<String>> purposeContactMechs, String logPrefix) {
        this.partyId = partyId;
        this.contactMechAndPurposeList = contactMechAndPurposeList;
        this.contactMechPurposes = contactMechPurposes;
        this.purposeContactMechs = purposeContactMechs;
        this.logPrefix = logPrefix != null ? logPrefix : "";
    }
    
    public List<GenericValue> getContactMechAndPurposeList() {
        return contactMechAndPurposeList;
    }
    
    public Map<String, Set<String>> getContactMechPurposes() {
        return contactMechPurposes;
    }
    
    public Map<String, Set<String>> getPurposeContactMechs() {
        return purposeContactMechs;
    }
    
    public Set<String> getContactMechPurposes(String contactMechId) {
        Set<String> purposes = contactMechPurposes.get(contactMechId);
        return purposes != null ? purposes : Collections.<String>emptySet();
    }
    
    public Set<String> getPurposeContactMechs(String purpose) {
        Set<String> contactMechIds = purposeContactMechs.get(purpose);
        return contactMechIds != null ? contactMechIds : Collections.<String>emptySet();
    }

    public static PartyContactMechInfo forParty(Delegator delegator, LocalDispatcher dispatcher, String partyId, boolean useCache, String logPrefix) throws GenericEntityException {
        List<EntityCondition> condList = new ArrayList<>();
        condList.add(EntityCondition.makeCondition("partyId", partyId));
        condList.add(EntityUtil.getFilterByDateExpr("contactFromDate", "contactThruDate"));
        condList.add(EntityUtil.getFilterByDateExpr("purposeFromDate", "purposeThruDate"));
        List<GenericValue> contactMechAndPurposeList = delegator.findList("PartyContactWithPurpose", 
                EntityCondition.makeCondition(condList, EntityOperator.AND), null, null, null, useCache);
        return fromContactMechAndPurposeList(partyId, contactMechAndPurposeList, logPrefix);
    }
    
    public static PartyContactMechInfo fromContactMechAndPurposeList(String partyId, List<GenericValue> contactMechAndPurposeList, String logPrefix) {
        Map<String, Set<String>> contactMechPurposes = new HashMap<>();
        Map<String, Set<String>> purposeContactMechs = new HashMap<>();
        
        for(GenericValue contactMechAndPurpose : contactMechAndPurposeList) {
            String contactMechId = contactMechAndPurpose.getString("contactMechId");
            String purpose = contactMechAndPurpose.getString("contactMechPurposeTypeId");
            
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
        
        return new PartyContactMechInfo(partyId,
                Collections.unmodifiableList(contactMechAndPurposeList), 
                Collections.unmodifiableMap(contactMechPurposes), 
                Collections.unmodifiableMap(purposeContactMechs),
                logPrefix);
    }
    
    public void resultsToMap(Map<String, Object> map) {
        //map.put("partyId", partyId); // not for us
        map.put("contactMechAndPurposeList", contactMechAndPurposeList);
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
                Debug.logWarning(logPrefix+"party '" + partyId + "' has multiple active contact mechs for purpose '" + purpose 
                        + "'; using first (contactMechId: '" + id + ") only", module);
            } else {
                Debug.logInfo(logPrefix+"party '" + partyId + "': contactMechId '" + id + "' found matching purpose '" + purpose 
                        + "'", module);
            }
            return id;
        } else {
            Debug.logInfo(logPrefix+"party '" + partyId + "' has no contact mech for purpose '" + purpose + "'", module);
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
                Debug.logWarning(logPrefix+"party '" + partyId + "' has multiple active candidate contact mechs for purposes " + purposes 
                        + " (contactMechIds: " + contactMechIds + "); using first (contactMechId: '" + id + ") only", module);
            } 
            int count = matchCounts.get(id);
            if (count >= purposes.size()) {
                Debug.logInfo(logPrefix+"party '" + partyId + "': contactMechId '" + id + "' found matching purposes " + purposes, module);
            } else {
                Debug.logInfo(logPrefix+"party '" + partyId + "': contactMechId '" + id + "' only PARTIALLY matches purposes " + purposes, module);
            }
            return id;
        } else {
            Debug.logInfo(logPrefix+"party '" + partyId + "' has no contact mech matching purposes " + purposes, module);
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
}