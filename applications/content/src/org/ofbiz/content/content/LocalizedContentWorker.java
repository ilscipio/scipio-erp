package org.ofbiz.content.content;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * SCIPIO: Helpers to help manage localized simple text content, largely ALTERNATE_LOCALE records,
 * help manage the parameters for the services that must update them.
 * <p>
 * May help implement services that implement/use:
 * replaceEntityContentLocalizedSimpleTextsInterface
 * replaceContentLocalizedSimpleTexts
 * <p>
 * NOTE: part of this code may rely on the existence of these views:
 * ProductContentAndElectronicText, ProductCategoryContentAndElectronicText
 * <p>
 * Added 2017-12-06.
 */
public abstract class LocalizedContentWorker {

    public static final String module = LocalizedContentWorker.class.getName();
    
    protected LocalizedContentWorker() {
    }

    /**
     * SCIPIO: rearranges a viewsByType map into viewsByTypeAndLocale map.
     * Logs warnings if multiple records for same locales.
     * Added 2017-10-27.
     */
    public static Map<String, Map<String, GenericValue>> splitContentLocalizedSimpleTextContentAssocViewsByLocale(Map<String, List<GenericValue>> viewsByType) {
        Map<String, Map<String, GenericValue>> viewsByTypeAndLocale = new HashMap<>();
        
        for(Map.Entry<String, List<GenericValue>> entry : viewsByType.entrySet()) {
            Map<String, GenericValue> viewsByLocale = new HashMap<>();
            for(GenericValue view : entry.getValue()) {
                String localeString = view.getString("localeString");
                if (viewsByLocale.containsKey(localeString)) {
                    Debug.logWarning("splitContentLocalizedSimpleTextContentAssocViewsByLocale: multiple eligible records found"
                            + " for localeString '" + localeString + "'; using first found only (this may cause unexpected texts to appear)."
                            + " Offending value: " + view.toString(), module);
                    continue;
                }
                viewsByLocale.put(localeString, view);
            }
            viewsByTypeAndLocale.put(entry.getKey(), viewsByLocale);
        }
        
        return viewsByTypeAndLocale;
    }
    
    /**
     * SCIPIO: rearranges a viewsByType map into textByTypeAndLocale map.
     * Logs warnings if multiple records for same locales.
     * Added 2017-10-27.
     */
    public static Map<String, Map<String, String>> extractContentLocalizedSimpleTextDataByLocale(Map<String, List<GenericValue>> viewsByType) {
        Map<String, Map<String, String>> textDataByTypeAndLocale = new HashMap<>();
        
        for(Map.Entry<String, List<GenericValue>> entry : viewsByType.entrySet()) {
            Map<String, String> textDataByLocale = new HashMap<>();
            for(GenericValue view : entry.getValue()) {
                String localeString = view.getString("localeString");
                if (textDataByLocale.containsKey(localeString)) {
                    Debug.logWarning("splitContentLocalizedSimpleTextContentAssocViewsByLocale: multiple eligible records found"
                            + " for localeString '" + localeString + "'; using first found only (this may cause unexpected texts to appear)."
                            + " Offending value: " + view.toString(), module);
                    continue;
                }
                textDataByLocale.put(localeString, view.getString("textData"));
            }
            textDataByTypeAndLocale.put(entry.getKey(), textDataByLocale);
        }
        
        return textDataByTypeAndLocale;
    }
    
    /**
     * SCIPIO: Makes a request parameter name prefix for special simple-text content field handling services.
     * (used by catalog tree)
     */
    public static String makeLocalizedSimpleTextContentFieldStringParamPrefix(String basePrefix, String typeId, int index) {
        return basePrefix + typeId + "." + index + ".";
    }
    
    /**
     * SCIPIO: Parses request parameters whose names follow {@link #makeLocalizedSimpleTextContentFieldStringParamPrefix},
     * notably for use with services that implement replaceEntityContentLocalizedSimpleTextsInterface service.
     * <p>
     * If basePrefix null the entries are assumed to have been extracted such that they no longer have the basePrefix.
     * If allowPreparsed true any non-param entries are found their lists crush the params with same type keys.
     * <p>
     * TODO: the algorithm could be optimized (non-trivial) - hard to parse all the parameters involved.
     * 
     * @return map of type IDs to lists of maps
     */
    public static Map<String, List<Map<String, Object>>> parseLocalizedSimpleTextContentFieldParams(Map<String, Object> entries, String basePrefix, boolean allowPreparsed) {
        Map<String, List<Map<String, Object>>> typeMap = new HashMap<>();
        if (entries != null) {
            Map<String, List<Map<String, Object>>> preparsedTypeMap = new HashMap<>();
            // indexes will be out of order, so we have to use ugly map of integer and sort after
            Map<String, Map<Integer, Map<String, Object>>> typeIndexMap = new HashMap<>();
            
            if (basePrefix != null && basePrefix.isEmpty()) basePrefix = null;
            
            for(Map.Entry<String, Object> entry : entries.entrySet()) {
                String name = entry.getKey();
                
                if (basePrefix != null) {
                    if (name.startsWith(basePrefix)) name = name.substring(basePrefix.length());
                    else continue;
                }
                
                Object value = entry.getValue();
                if (name.contains(".")) {
                    String[] parts = StringUtils.split(name, ".", 3);
                    if (parts.length < 3 || parts[0].isEmpty() || parts[2].isEmpty()) throw new IllegalArgumentException("invalid composed content field key: " + name);
                    
                    String typeId = parts[0];
                    int index = Integer.parseInt(parts[1]);
                    String mapKey = parts[2];
                    
                    Map<Integer, Map<String, Object>> indexMap = typeIndexMap.get(typeId);
                    if (indexMap == null) {
                        indexMap = new HashMap<>();
                        Map<String, Object> entryData = new HashMap<>();
                        entryData.put(mapKey, value);
                        indexMap.put(index, entryData);
                        typeIndexMap.put(typeId, indexMap);
                    } else {
                        Map<String, Object> entryData = indexMap.get(index);
                        if (entryData == null) {
                            entryData = new HashMap<>();
                            indexMap.put(index, entryData);
                        } 
                        entryData.put(mapKey, value);
                    }
                } else if (allowPreparsed) {
                    preparsedTypeMap.put(name, UtilGenerics.<Map<String, Object>>checkList(value));
                }
            }
            
            // get as sorted lists
            // TODO: optimize better
            for(Map.Entry<String, Map<Integer, Map<String, Object>>> entry : typeIndexMap.entrySet()) {
                List<Integer> indexes = new ArrayList<>(entry.getValue().keySet());
                Collections.sort(indexes);
                
                List<Map<String, Object>> entryDataList = new ArrayList<>(indexes.size());
                for(int index : indexes) {
                    entryDataList.add(entry.getValue().get(index));
                }
                
                typeMap.put(entry.getKey(), entryDataList);
            }
            
            typeMap.putAll(preparsedTypeMap);
        }
        return typeMap;
    }
    
    @SuppressWarnings("serial")
    public static class LocalizedSimpleTextInfo implements Serializable {
        
        protected final List<Map<String, Object>> entries;
        protected final boolean hasTextData;
        protected final Map<String, Object> mainEntry;
        protected final Map<String, Map<String, Object>> localeEntryMap;

        public LocalizedSimpleTextInfo() {
            this.hasTextData = false;
            this.entries = Collections.emptyList();
            this.mainEntry = null;
            this.localeEntryMap = Collections.emptyMap();
        }
        
        private LocalizedSimpleTextInfo(List<Map<String, Object>> entries, boolean hasTextData,
                Map<String, Object> mainEntry, Map<String, Map<String, Object>> localeEntryMap) {
            this.entries = entries;
            this.hasTextData = hasTextData;
            this.mainEntry = mainEntry;
            this.localeEntryMap = localeEntryMap;
        }

        public List<Map<String, Object>> getEntries() {
            return entries;
        }

        public boolean isHasTextData() {
            return hasTextData;
        }
        
        public Map<String, Object> getMainEntry() {
            return mainEntry;
        }
        
        public String getMainLocaleString() {
            return (mainEntry != null) ? (String) mainEntry.get("localeString") : null;
        }
        
        public String getMainTextData() {
            return (mainEntry != null) ? (String) mainEntry.get("textData") : null;
        }
        
        /**
         * Maps localeString to record entry, excluding the main record (first records in entries).
         */
        public Map<String, Map<String, Object>> getLocaleEntryMap() {
            return localeEntryMap;
        }

        /**
         * Factory method.
         * Checks for duplicate locales (exception), text presence, etc.
         */
        public static LocalizedSimpleTextInfo fromEntries(List<Map<String, Object>> entries) throws GeneralException {
            if (UtilValidate.isEmpty(entries)) return new LocalizedSimpleTextInfo();

            boolean hasTextData = false;
            Map<String, Object> mainEntry = entries.get(0);
            if (entryHasTextData(mainEntry)) hasTextData = true;
            String mainLocaleString = (String) mainEntry.get("localeString");

            Map<String, Map<String, Object>> localeEntryMap = new LinkedHashMap<>();
            
            Iterator<Map<String, Object>> entryIt = entries.iterator();
            entryIt.next(); // skip first
            while(entryIt.hasNext()) {
                Map<String, Object> entry = entryIt.next();
                String localeString = (String) entry.get("localeString");
                boolean entryHasTextData = entryHasTextData(entry);
                // if no textData, will simply omit from the map, and it will be auto-deleted
                // NOTE: if textData empty, we will forgive duplicate locales for now (maybe revisit later if need)
                if (entryHasTextData) {
                    // only the first record is allowed to have empty localeString and non-empty textData
                    if (UtilValidate.isEmpty(localeString)) {
                        throw new GeneralException(PropertyMessage.make("ContentErrorUiLabels", "contentservices.localized_field_entry_missing_locale"));
                    }
                    
                    hasTextData = true;
                    if (localeEntryMap.containsKey(localeString) || localeString.equals(mainLocaleString)) {
                        throw new GeneralException(PropertyMessage.make("ContentErrorUiLabels", "contentservices.localized_field_entry_duplicate_locale", null, ": " + localeString));
                    }
                    localeEntryMap.put(localeString, entry);
                }
            }
            return new LocalizedSimpleTextInfo(entries, hasTextData, mainEntry, localeEntryMap);
        }
        
        public static boolean entryHasTextData(Map<String, ?> entry) {
            String textData = (String) entry.get("textData");
            if (textData == null) return false;
            else return textData.trim().length() > 0;
        }

        /**
         * Gets the textData key from a localeEntryMap entry, or if it's a localeString-to-textData map,
         * simply returns the string (supports two map formats).
         */
        @SuppressWarnings("unchecked")
        public static String getLocaleMapTextData(Object localeEntryMapEntry) {
            if (localeEntryMapEntry instanceof Map) {
                Map<String, ?> entryMap = (Map<String, ?>) localeEntryMapEntry;
                return (String) entryMap.get("textData");
            } else if (localeEntryMapEntry instanceof String) return (String) localeEntryMapEntry;
            else if (localeEntryMapEntry == null) return null;
            else throw new IllegalArgumentException("internal error: replaceLocalizedContent: localeEntryMap has invalid entry: " + localeEntryMapEntry);
        }
    }
    
    public static Map<String, String> getSimpleTextsByLocaleString(Delegator delegator, LocalDispatcher dispatcher,
            GenericValue mainContent, Timestamp moment, boolean useCache) throws GenericEntityException {
        Map<String, String> localeTextMap = new HashMap<>();
        
        List<GenericValue> assocViewList = EntityQuery.use(delegator).from("ContentAssocToElectronicText")
                .where("contentIdStart", mainContent.getString("contentId"), 
                        "contentAssocTypeId", "ALTERNATE_LOCALE").filterByDate(moment).cache(useCache).queryList();
        for(GenericValue assocView : assocViewList) {
            String assocLocaleString = assocView.getString("localeString");
            if (UtilValidate.isNotEmpty(assocLocaleString)) {
                localeTextMap.put(assocLocaleString, assocView.getString("textData"));
            }
        }
        
        String mainLocaleString = mainContent.getString("localeString");
        if (UtilValidate.isNotEmpty(mainLocaleString)) {
            GenericValue elecText = getSimpleTextContentElectronicText(delegator, dispatcher, mainContent);
            String mainTextData = elecText.getString("textData");
            if (UtilValidate.isNotEmpty(mainTextData)) {
                localeTextMap.put(mainLocaleString, mainTextData);
            }
        }
        
        return localeTextMap;
    }
    
    public static GenericValue createSimpleTextContent(Delegator delegator, LocalDispatcher dispatcher, String localeString, String textData, 
            Map<String, Object> contentFields, Map<String, Object> dataResourceFields) throws GenericEntityException {
        // create dataResource
        GenericValue dataResource = delegator.makeValue("DataResource");
        dataResource.put("dataResourceTypeId", "ELECTRONIC_TEXT");
        dataResource.setNonPKFields(dataResourceFields);
        dataResource = delegator.createSetNextSeqId(dataResource);
        String dataResourceId = dataResource.getString("dataResourceId");

        // create electronicText
        GenericValue electronicText = delegator.makeValue("ElectronicText");
        electronicText.put("dataResourceId", dataResourceId);
        electronicText.put("textData", textData);
        electronicText = delegator.create(electronicText);

        // create content
        GenericValue content = delegator.makeValue("Content");
        if (UtilValidate.isNotEmpty(localeString)) content.put("localeString", localeString);
        content.put("dataResourceId", dataResourceId);
        content.put("contentTypeId", "DOCUMENT");
        content.setNonPKFields(contentFields);
        content = delegator.createSetNextSeqId(content);
        
        return content;
    }
    
    
    public static void updateSimpleTextContent(Delegator delegator, LocalDispatcher dispatcher, GenericValue content, String localeString, String textData) throws GenericEntityException {
        updateSimpleTextContent(delegator, dispatcher, content, textData);
        content.put("localeString", localeString);
        content.store();
    }
    
    public static void updateSimpleTextContent(Delegator delegator, LocalDispatcher dispatcher, GenericValue content, String textData) throws GenericEntityException {
        GenericValue elecText = getSimpleTextContentElectronicText(delegator, dispatcher, content);
        elecText.put("textData", textData);
        elecText.store();
    }
    
    public static GenericValue getSimpleTextContentElectronicText(Delegator delegator, LocalDispatcher dispatcher, GenericValue content) throws GenericEntityException {
        GenericValue elecText = EntityQuery.use(delegator).from("ElectronicText")
                .where("dataResourceId", content.getString("dataResourceId"))
                .cache(false).queryOne();
        if (elecText == null) {
            throw new GenericEntityException("Simple text content '" + content.getString("contentId") + "' has no ElectronicText");
        }
        return elecText;
    }

    public static void removeAllAlternateLocaleRecords(Delegator delegator, LocalDispatcher dispatcher, Map<String, ?> context, String contentId) throws GeneralException {
        List<GenericValue> contentAssocList = EntityQuery.use(delegator).from("ContentAssoc")
                .where("contentId", contentId, "contentAssocTypeId", "ALTERNATE_LOCALE")
                .filterByDate().cache(false).queryList();
        for(GenericValue contentAssoc : contentAssocList) {
            removeContentAndRelated(delegator, dispatcher, context, contentAssoc.getString("contentIdTo"));
        }
    }
    
    public static void removeContentAndRelated(Delegator delegator, LocalDispatcher dispatcher, Map<String, ?> context, GenericValue content) throws GeneralException {
        removeContentAndRelated(delegator, dispatcher, context, content.getString("contentId"));
    }
    
    public static void removeContentAndRelated(Delegator delegator, LocalDispatcher dispatcher, Map<String, ?> context, String contentId) throws GeneralException {
        Map<String, Object> servCtx = new HashMap<>();
        servCtx.put("userLogin", context.get("userLogin"));
        servCtx.put("locale", context.get("locale"));
        servCtx.put("contentId", contentId);
        Map<String, Object> servResult = dispatcher.runSync("removeContentAndRelated", servCtx);
        if (ServiceUtil.isError(servResult)) {
            throw new GeneralException("Cannot remove ALTERNATE_LOCALE record contentId '" 
                    + contentId + "': " + ServiceUtil.getErrorMessage(servResult));
        }
    }
    
    /**
     * Replaces the textData of the Content and its associated ALTERNATE_LOCALE Contents.
     * NOTE: The first entry 
     * <p>
     * TODO?: move elsewhere, may need to reuse...
     * @param localeEntryMap can be either a map of localeString to textData strings,
     *                       or map of localeString to entry maps where a key is "textData"
     */
    public static GenericValue replaceLocalizedContent(Delegator delegator, LocalDispatcher dispatcher, Map<String, ?> context,
            GenericValue mainContent, String mainLocaleString, String mainTextData, Map<String, ?> localeEntryMap,
            boolean removeOldLocales, Timestamp moment, Map<String, Object> newRecordContentFields, Map<String, Object> newRecordDataResourceFields) throws Exception {
        // DEV NOTE: to simplify, I am setting removeDupLocales to same as removeOldLocales...
        // as long as removal is understood, there is really no reason to keep duplicates because
        // the prior code was setting them all to the exact same value, so in other words,
        // having removeDupLocales false was completely pointless...
        final boolean removeDupLocales = removeOldLocales;
        
        Set<String> remainingLocales = new HashSet<>(localeEntryMap.keySet());

        // update main content
        if (mainContent == null) {
            mainContent = createSimpleTextContent(delegator, dispatcher, mainLocaleString, mainTextData, newRecordContentFields, newRecordDataResourceFields);
        } else {
            LocalizedContentWorker.updateSimpleTextContent(delegator, dispatcher, mainContent, mainLocaleString, mainTextData);
        }
        
        // SPECIAL: 2017-11-28: we must remove the mainLocaleString from remainingLocales because
        // 1) when creating brand new records, don't want the second loop to create new ALTERNATE_LOCALEs
        //    for locale already covered in the main record, and
        // 2) if we remove mainLocaleString from remainingLocales BEFORE the next "update in-place or remove" loop,
        //    it will allow the loop to correct past mistakes and remove duplicates that were previously created
        if (UtilValidate.isNotEmpty(mainLocaleString)) {
            remainingLocales.remove(mainLocaleString);
        }
        
        List<GenericValue> contentAssocList = EntityQuery.use(delegator).from("ContentAssoc")
                .where("contentId", mainContent.getString("contentId"), "contentAssocTypeId", "ALTERNATE_LOCALE")
                .filterByDate(moment).cache(false).queryList();
        
        // update in-place or remove existing assoc records
        for(GenericValue contentAssoc : contentAssocList) {
            GenericValue content = contentAssoc.getRelatedOne("ToContent", false);
            String localeString = content.getString("localeString");

            // Removal logic:
            // * The localeString was "old" if it's not in localeUrlMap at all.
            //   * We also "treat" localeString also as "old" if it's in localeUrlMap but empty value.
            // * The localeString was a "duplicate" if it's in localeUrlMap but was removed from remainingLocales.
            
            String textData = LocalizedSimpleTextInfo.getLocaleMapTextData(localeEntryMap.get(localeString));
            if (UtilValidate.isNotEmpty(textData)) {
                if (!removeDupLocales || remainingLocales.contains(localeString)) {
                    LocalizedContentWorker.updateSimpleTextContent(delegator, dispatcher, content, textData);
                    remainingLocales.remove(localeString);
                } else {
                    removeContentAndRelated(delegator, dispatcher, context, content);
                }
            } else {
                if (removeOldLocales) {
                    removeContentAndRelated(delegator, dispatcher, context, content);
                }
            }
        }
        
        // see above comment - could have done this code here, but think it works better if earlier
        //if (UtilValidate.isNotEmpty(mainLocaleString)) {
        //    remainingLocales.remove(mainLocaleString);
        //}
        
        // create new assoc records
        for(String localeString : remainingLocales) {
            String textData = LocalizedSimpleTextInfo.getLocaleMapTextData(localeEntryMap.get(localeString));
            if (UtilValidate.isNotEmpty(textData)) {
                GenericValue content = createSimpleTextContent(delegator, dispatcher, localeString, textData, newRecordContentFields, newRecordDataResourceFields);
                GenericValue contentAssoc = delegator.makeValue("ContentAssoc");
                contentAssoc.put("contentId", mainContent.getString("contentId"));
                contentAssoc.put("contentIdTo", content.getString("contentId"));
                contentAssoc.put("fromDate", moment);
                contentAssoc.put("contentAssocTypeId", "ALTERNATE_LOCALE");
                contentAssoc = delegator.create(contentAssoc);
            }
        }

        return mainContent;
    }

}
