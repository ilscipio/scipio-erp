package com.ilscipio.scipio.cms.media;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.content.image.ContentImageWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityFindOptions;
import org.ofbiz.entity.util.EntityListIterator;

public abstract class CmsMediaWorker {

    public static final String module = CmsMediaWorker.class.getName();
    
    /**
     * NOTE: currently (2017-08-01) it is recommended to not use this file and to use 
     * /applications/content/config/ImageProperties.xml instead, so images everywhere are the same
     * (there may currently be issues if they differ... TODO: REVIEW).
     */
    public static final String CMS_IMAGEPROP_FILEPATH = "/applications/cms/config/ImageProperties.xml";
    
    public static final Map<String, FlexibleStringExpander> RESIZEIMG_CONTENT_FIELDEXPR = ContentImageWorker.RESIZEIMG_CONTENT_FIELDEXPR;
    public static final Map<String, FlexibleStringExpander> RESIZEIMG_DATARESOURCE_FIELDEXPR = ContentImageWorker.RESIZEIMG_DATARESOURCE_FIELDEXPR;
    
    public static final Set<String> VALID_DATA_RESOURCE_TYPE_LIST = Collections.unmodifiableSet(UtilMisc.toHashSet("AUDIO_OBJECT", "VIDEO_OBJECT", "IMAGE_OBJECT", "DOCUMENT_OBJECT"));

    private static final ImageVariantConfig defaultCmsImgVariantCfg;
    static {
        ImageVariantConfig cfg = null;
        try {
            cfg = ImageVariantConfig.fromImagePropertiesXml(getCmsImagePropertiesPath());
        } catch(Exception e) {
            Debug.logError(e, "Cms: Media: Could not read ImageProperties.xml: " + e.getMessage(), module);
        }
        defaultCmsImgVariantCfg = cfg;
    }

    
    protected CmsMediaWorker() {
    }
    
    public static ImageVariantConfig getDefaultCmsImageVariantConfig() {
        return defaultCmsImgVariantCfg;
    }

    public static GenericValue getContentForMedia(Delegator delegator, String contentId, String dataResourceId) throws GenericEntityException, IllegalArgumentException, IllegalStateException {
        GenericValue content;
        if (UtilValidate.isNotEmpty(contentId)) {
            content = delegator.findOne("Content", UtilMisc.toMap("contentId", contentId), false);
            if (UtilValidate.isEmpty(content)) {
                throw new IllegalArgumentException("Media file not found for contentId '" + contentId + "'");
            }
            //dataResourceId = content.getString("dataResourceId");
        } else {
            List<GenericValue> contentList = delegator.findByAnd("Content", UtilMisc.toMap("dataResourceId", dataResourceId), null, false);
            if (UtilValidate.isEmpty(contentList)) {
                // DEV NOTE: I was going to make this auto-create one for backward compat but not worth it, cms not released yet
                throw new IllegalArgumentException("Invalid media file - dataResourceId '" + dataResourceId + "' has no Content record"
                        + " - either invalid media file ID or schema error - please contact your administrator");
            } else if (contentList.size() > 1){
                throw new IllegalStateException("Media file DataResource is associated to multiple Content records - cannot safely modify -"
                        + " db corruption could occur if we tried to update one - please contact your administrator");
            }
            content = contentList.get(0);
            //contentId = content.getString("contentId");
        }
        return content;
    }
    
    /**
     * Returns as ContentDataResourceRequiredView values (NOTE: the DataResource fields have "dr" prefix).
     * @throws GenericEntityException 
     */
    public static EntityListIterator getAllMediaContentDataResourceRequired(Delegator delegator, String dataResourceTypeId, List<String> orderBy) throws GenericEntityException {
        List<EntityCondition> condList = new ArrayList<>();
        condList.add(EntityCondition.makeCondition("contentTypeId", "SCP_MEDIA"));
        if (dataResourceTypeId != null) condList.add(EntityCondition.makeCondition("drDataResourceTypeId", dataResourceTypeId));
        return delegator.find("ContentDataResourceRequiredView", EntityCondition.makeCondition(condList, EntityOperator.AND), null, null, orderBy, null);
    }
    
    public static EntityListIterator getMediaContentDataResourceRequiredByContentId(Delegator delegator, String dataResourceTypeId, Collection<String> contentIdList, List<String> orderBy) throws GenericEntityException {
        List<EntityCondition> condList = new ArrayList<>();
        condList.add(EntityCondition.makeCondition("contentTypeId", "SCP_MEDIA"));
        if (dataResourceTypeId != null) condList.add(EntityCondition.makeCondition("drDataResourceTypeId", dataResourceTypeId));
        List<EntityCondition> contentIdCondList = new ArrayList<>();
        for(String contentId : contentIdList) {
            contentIdCondList.add(EntityCondition.makeCondition("contentId", contentId));
        }
        condList.add(EntityCondition.makeCondition(contentIdCondList, EntityOperator.OR));
        return delegator.find("ContentDataResourceRequiredView", 
                EntityCondition.makeCondition(condList, EntityOperator.AND), null, null, null, null);
    }

    
    /**
     * SCIPIO: Returns the full path to the ImageProperties.xml file to use for cms image size definitions.
     * Uses the one from cms component if available; otherwise falls back on the generic one under content component.
     * WARN: Currently recommend NOT to use the CMS-specific one until better tested.
     * Added 2017-08-01.
     */
    public static String getCmsImagePropertiesFullPath() throws IOException {
        String path = ImageVariantConfig.getImagePropertiesFullPath(CMS_IMAGEPROP_FILEPATH);
        if (new java.io.File(path).exists()) {
            return path;
        } else {
            return ContentImageWorker.getContentImagePropertiesFullPath();
        }
    }
    
    public static String getCmsImagePropertiesPath() throws IOException {
        String path = ImageVariantConfig.getImagePropertiesFullPath(CMS_IMAGEPROP_FILEPATH);
        if (new java.io.File(path).exists()) {
            return CMS_IMAGEPROP_FILEPATH;
        } else {
            return ContentImageWorker.getContentImagePropertiesPath();
        }
    }
    
    // TODO: REVIEW: for now we are intentionally ignoring the thruDate on ContentAssoc to simplify.
    // I don't see the point in keeping old records...
    
    public static List<GenericValue> getVariantContentAssocTo(Delegator delegator, String contentId) throws GenericEntityException {
        EntityCondition cond = EntityCondition.makeCondition(
                EntityCondition.makeCondition("contentIdStart", contentId),
                EntityOperator.AND,
                EntityCondition.makeCondition("contentTypeId", "SCP_MEDIA_VARIANT")); // alternative: EntityCondition.makeCondition("caContentAssocTypeId", EntityOperator.LIKE, "IMGSZ_%")
        return delegator.findList("ContentAssocViewTo", cond, null, null, null, false);
    }
    
    public static Set<String> getVariantContentAssocContentIdTo(Delegator delegator, String contentId) throws GenericEntityException {
        List<GenericValue> assocList = getVariantContentAssocTo(delegator, contentId);
        Set<String> res = new LinkedHashSet<>();
        if (assocList != null) {
            for(GenericValue assoc : assocList) {
                res.add(assoc.getString("contentId"));
            }
        }
        return res;
    }
    
    public static List<String> getVariantContentMapKeys(Delegator delegator, String contentId) throws GenericEntityException {
        List<GenericValue> assocList = getVariantContentAssocTo(delegator, contentId);
        List<String> res = new ArrayList<>();
        if (assocList != null) {
            for(GenericValue assoc : assocList) {
                res.add(assoc.getString("caMapKey"));
            }
        }
        return res;
    }
    
    public static EntityListIterator findVariantContentAssocTypes(Delegator delegator) throws GenericEntityException {
        return delegator.find("ContentAssocType", 
                EntityCondition.makeCondition("contentAssocTypeId", EntityOperator.LIKE, "IMGSZ_%"),
                null, null, null, null);
    }
    
    // TODO: optimize
    public static boolean hasVariantContent(Delegator delegator, String contentId) throws GenericEntityException {
        EntityCondition cond = EntityCondition.makeCondition(
                EntityCondition.makeCondition("contentIdStart", contentId),
                EntityOperator.AND,
                EntityCondition.makeCondition("contentTypeId", "SCP_MEDIA_VARIANT")); // alternative: EntityCondition.makeCondition("caContentAssocTypeId", EntityOperator.LIKE, "IMGSZ_%")
        return delegator.findCountByCondition("ContentAssocViewTo", cond, null, null) > 0;
    }

}
