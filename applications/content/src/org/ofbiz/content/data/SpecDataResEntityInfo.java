package org.ofbiz.content.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

/**
 * SCIPIO: Represents metadata about ImageDataResource, VideoDataResource, etc. entities (very repetition operations).
 * TODO?: use config files in future
 */
public class SpecDataResEntityInfo {
    
    public static List<SpecDataResEntityInfo> populateEntityInfoList(List<SpecDataResEntityInfo> list) {
        list.add(new SpecDataResEntityInfo("IMAGE_OBJECT", "ImageDataResource", "imageData"));
        list.add(new SpecDataResEntityInfo("VIDEO_OBJECT", "VideoDataResource", "videoData"));
        list.add(new SpecDataResEntityInfo("AUDIO_OBJECT", "AudioDataResource", "audioData"));
        list.add(new SpecDataResEntityInfo("DOCUMENT_OBJECT", "DocumentDataResource", "documentData"));
        list.add(new SpecDataResEntityInfo("OTHER_OBJECT", "OtherDataResource", "dataResourceContent"));
        return list;
    }
    
    private static final List<SpecDataResEntityInfo> entityInfoList = Collections.unmodifiableList(populateEntityInfoList(new ArrayList<SpecDataResEntityInfo>()));
    private static final Map<String, SpecDataResEntityInfo> dataResourceTypeIdMap = Collections.unmodifiableMap(populateEntityInfoMap(new HashMap<String, SpecDataResEntityInfo>(), "dataResourceTypeId", entityInfoList));
    private static final Map<String, SpecDataResEntityInfo> entityNameMap = Collections.unmodifiableMap(populateEntityInfoMap(new HashMap<String, SpecDataResEntityInfo>(), "entityName", entityInfoList));
    private static final Map<String, SpecDataResEntityInfo> dataFieldNameMap = Collections.unmodifiableMap(populateEntityInfoMap(new HashMap<String, SpecDataResEntityInfo>(), "dataFieldName", entityInfoList));

    private final String dataResourceTypeId;
    private final String entityName;
    private final String dataFieldName;
    private final Map<String, Object> mapRepr;
    
    protected SpecDataResEntityInfo(String dataResourceTypeId, String entityName, String dataFieldName, Map<String, Object> mapRepr) {
        this.dataResourceTypeId = dataResourceTypeId;
        this.entityName = entityName;
        this.dataFieldName = dataFieldName;
        this.mapRepr = mapRepr;
    }
    
    public SpecDataResEntityInfo(String dataResourceTypeId, String entityName, String dataFieldName) {
        this.dataResourceTypeId = dataResourceTypeId;
        this.entityName = entityName;
        this.dataFieldName = dataFieldName;
        this.mapRepr = Collections.unmodifiableMap(toMap());
    }
    
    public static SpecDataResEntityInfo fromDataResource(GenericValue dataResource) {
        return getDataResourceTypeIdMap().get(dataResource.getString("dataResourceTypeId"));
    }
    
    public static List<SpecDataResEntityInfo> getEntityInfoList() {
        return entityInfoList;
    }

    public static Map<String, SpecDataResEntityInfo> getDataResourceTypeIdMap() {
        return dataResourceTypeIdMap;
    }

    public static Map<String, SpecDataResEntityInfo> getEntityNameMap() {
        return entityNameMap;
    }

    public static Map<String, SpecDataResEntityInfo> getDataFieldNameMap() {
        return dataFieldNameMap;
    }

    public String getDataResourceTypeId() { return dataResourceTypeId; }
    public String getEntityName() { return entityName; }
    public String getDataFieldName() { return dataFieldName; }
    public Map<String, Object> asMap() { return mapRepr; }
    
    public Map<String, ? super String> toMap(Map<String, ? super String> map) {
        map.put("dataResourceTypeId", getDataResourceTypeId());
        map.put("entityName", getEntityName());
        map.put("dataFieldName", getDataFieldName());
        return map;
    }
    
    public Map<String, Object> toMap() { 
        Map<String, Object> map = new HashMap<>();
        toMap(map);
        return map;
    }
    
    public GenericValue getSpecValuefromDataResource(GenericValue dataResource, boolean useCache) throws GenericEntityException {
        return dataResource.getRelatedOne(getEntityName(), useCache);
    }
    
    public static Map<String, SpecDataResEntityInfo> populateEntityInfoMap(Map<String, SpecDataResEntityInfo> map, String key, List<SpecDataResEntityInfo> list) {
        if (list != null) {
            for(SpecDataResEntityInfo entityInfo : list) {
                map.put((String) entityInfo.asMap().get(key), entityInfo);
            }
        }
        return map;
    }
}