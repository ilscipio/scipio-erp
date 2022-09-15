package com.ilscipio.scipio.content.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Essentially a wrapper for the new ProductCategoryContentType fields (SCIPIO).
 */
public abstract class ContentImageViewType implements Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final GenericValue contentType;
    protected final String contentTypeId;
    protected final String contentTypeEntityName;
    protected final String contentTypeIdFieldName;

    protected ContentImageViewType(GenericValue contentType, String contentTypeEntityName, String contentTypeIdFieldName) throws IllegalArgumentException {
        this.contentType = contentType;
        this.contentTypeId = contentType.getString(contentTypeIdFieldName);
        this.contentTypeEntityName = contentTypeEntityName;
        this.contentTypeIdFieldName = contentTypeIdFieldName;
    }


    protected Delegator getDelegator() {
        return contentType.getDelegator();
    }

    public GenericValue getContentType() {
        return contentType;
    }

    public String getContentTypeId() {
        return contentTypeId;
    }

    /**
     * Returns the parent type ID, supposed to be IMAGE_URL_FULL, ORIGINAL_IMAGE_URL, ADDITIONAL_IMAGE_x or a custom "original" viewSize type.
     */
    public String getParentTypeId() {
        return contentType.getString("parentTypeId");
    }

    /**
     * Returns viewType (main, additional, ...).
     */
    public String getViewType() {
        return contentType.getString("viewType");
    }

    /**
     * Returns viewNumber (0 for original/main, 1-4 or higher for additional images).
     */
    public String getViewNumber() {
        return contentType.getString("viewNumber");
    }

    public int getViewNumberInt() {
        return Integer.parseInt(getViewNumber());
    }

    /**
     * Returns viewSize (original, large, 320x240, ...), also knows as sizeType.
     */
    public String getViewSize() {
        return contentType.getString("viewSize");
    }

    public boolean isMain() {
        return "main".equals(getViewType());
    }

    public boolean isAdditional() {
        return "additional".equals(getViewType());
    }

    public boolean isZero() {
        return "0".equals(getViewNumber());
    }

    public boolean isOriginal() {
        return "original".equals(getViewSize());
    }

    @Override
    public String toString() {
        return "{" + contentTypeIdFieldName + "='" + contentTypeId + "'}'";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ContentImageViewType that = (ContentImageViewType) o;
        return getContentTypeId().equals(that.getContentTypeId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getContentTypeId());
    }

    public abstract ContentImageViewType getOriginal(boolean useCache) throws GeneralException;

    public String getOriginalContentTypeId(boolean useCache) throws GeneralException {
        if (isOriginal()) {
            return getContentTypeId();
        } else {
            return getParentTypeId();
        }
    }

    public List<GenericValue> getContentTypes(boolean includeOriginal, boolean useCache) throws GeneralException {
        List<EntityCondition> orCondList = new ArrayList<>();

        if (isOriginal()) {
            if (includeOriginal) {
                orCondList.add(EntityCondition.makeCondition(contentTypeIdFieldName, getContentTypeId()));
            }
            orCondList.add(EntityCondition.makeCondition("parentTypeId", getContentTypeId()));
        } else if (getParentTypeId() != null && !"IMAGE_URL_FULL".equals(getParentTypeId())) {
            if (includeOriginal) {
                orCondList.add(EntityCondition.makeCondition(contentTypeIdFieldName, getParentTypeId()));
            }
            orCondList.add(EntityCondition.makeCondition("parentTypeId", getParentTypeId()));
        }

        List<GenericValue> ctList = getDelegator().from(contentTypeEntityName).where(orCondList, EntityOperator.OR).queryList();
        return ctList != null ? ctList : Collections.emptyList();
    }

    public Map<String, GenericValue> getContentTypesById(boolean includeOriginal, boolean useCache) throws GeneralException {
        List<GenericValue> ctList = getContentTypes(includeOriginal, useCache);
        if (ctList.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String, GenericValue> idMap = new LinkedHashMap<>();
        for (GenericValue pct : ctList) {
            idMap.put(pct.getString(contentTypeEntityName), pct);
        }
        return idMap;
    }

    public abstract Map<ContentImageViewType, GenericValue> getContentTypesByViewType(boolean includeOriginal, boolean useCache) throws GeneralException;

    public List<String> getContentTypeIds(boolean includeOriginal, boolean useCache) throws GeneralException {
        List<GenericValue> pcctList = getContentTypes(includeOriginal, useCache);
        return pcctList.isEmpty() ? Collections.emptyList() : pcctList.stream().map(c -> c.getString(contentTypeIdFieldName)).collect(Collectors.toList());
    }

    public Map<String, GenericValue> getContentTypesByViewSize(boolean includeOriginal, boolean useCache) throws GeneralException {
        Map<String, GenericValue> ctMap;
        List<GenericValue> ctList = getContentTypes(includeOriginal, useCache);
        if (UtilValidate.isNotEmpty(ctList)) {
            ctMap = new LinkedHashMap<>();
            for (GenericValue ct : ctList) {
                ctMap.put(ct.getString("viewSize"), ct);
            }
        } else {
            ctMap = Collections.emptyMap();
        }
        return ctMap;
    }



}
