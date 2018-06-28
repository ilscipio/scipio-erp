package com.ilscipio.scipio.cms.template;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.data.DataResourceWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsInputException;
import com.ilscipio.scipio.cms.data.CmsDataObject;

/**
 * Basic template and template common code for all template and template-like classes - 
 * used as base for both Version classes and the template sub-classes that don't support versions.
 * <p>
 * NOTE: "Template" refers to the most generic definition of the word possible; template
 * for any kind of code. It does NOT imply it's a renderable template (such as Freemarker); 
 * see {@link CmsRenderTemplate} interface for those.
 * <p>
 * DEV NOTE: This is different from the old class that was named CmsTemplate; that one
 * is now CmsComplexTemplate.
 */
public abstract class CmsTemplate extends CmsDataObject {

    private static final long serialVersionUID = -9152090660349076392L;

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    /**
     * Fields recognized by service but not physically present in entity.
     */
    public static final Set<String> virtualFields = Collections.unmodifiableSet(new HashSet<>(Arrays.asList(new String[] {
            "templateLocation", "templateBody", "templateSource"
    })));
    

    /**
     * Cached template body as either stored body or location,
     * main used to delay set operations to store() methods and read back during editing.
     * NOTE/WARN: For actual rendering, this is set to the Unsupported instance to save
     * memory. See preloadContent method.
     */
    protected TemplateBodySource tmplBodySrc = null;

    protected CmsTemplate(GenericValue entity) {
        super(entity);
    }

    public CmsTemplate(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
        this.setTemplateBodySource(fields);
    }
    
    /**
     * Copy constructor.
     * <p>
     * TODO?: misses an option to copy the body into a new stored object, but
     * it's mostly an efficiency thing.
     */
    protected CmsTemplate(CmsTemplate other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
        this.setTemplateBodySource(getTemplateBodySourceCopy(other, copyArgs));
    }
    
    @Override
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
        this.setTemplateBodySource(fields);
    }
    
    /**
     * Subclasses may need to override this - versioned templates should do nothing here.
     */
    protected TemplateBodySource getTemplateBodySourceCopy(CmsTemplate other, Map<String, Object> copyArgs) {
        return TemplateBodySource.fromTemplateBodySource(other.getTemplateBodySource());
    }
    
    /**
     * 2016: Loads ALL this object's content into the current instance.
     * <p>
     * WARN: IMPORTANT: AFTER THIS CALL, 
     * NO FURTHER CALLS ARE ALLOWED TO MODIFY THE INSTANCE IN MEMORY.
     * Essential for thread safety!!!
     */
    @Override
    public void preload(PreloadWorker preloadWorker) {
        super.preload(preloadWorker);
        this.getTemplateBodySource();
    }
    
    public abstract String getName();
    
    /**
     * Returns the template body source, including effective template body.
     * <p>
     * Subclasses may override to provide alternate source.
     */
    public TemplateBodySource getTemplateBodySource() {
        TemplateBodySource tmplBodySrc = this.tmplBodySrc;
        if (tmplBodySrc == null) {
            //return entity.getString("templateBody");
            try {
                tmplBodySrc = getTemplateBodySourceFromContent(getDelegator(), getTemplateContentId(), false);
            } catch (CmsException e) {
                throw new CmsException("Could not get template body for template " + getId() + " of " + getEntityName(), e);
            }
            this.tmplBodySrc = tmplBodySrc;
        }
        return tmplBodySrc;
    }
    
    /**
     * Returns the template body.
     * 
     * @return freemarker template
     */
    public String getTemplateBody() {
        return getTemplateBodySource().getEffectiveBody();
    }
    
    /**
     * Returns the template location, if any.
     * 
     * @return freemarker template
     */
    public String getTemplateLocation() {
        return getTemplateBodySource().getLocation();
    }

    public String getTemplateContentId() {
        return entity.getString("contentId"); 
    }
    
    protected void setTemplateContentId(String contentId) {
        entity.setString("contentId", contentId); 
    }
    
    @Override
    public void store() throws CmsException {
        storeTemplateBodySource();
        super.store();
    }
    
    @Override
    public int remove() throws CmsException {
        // WARNING: This does NOT attempt to remove the related Content entities
        // subclass must override this and implement
        return super.remove();
    }

    @Override
    public DataObjectWorker<?> getWorkerInst() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Explicitly Stores only the template record. For special circumstances.
     */
    protected void storeSelfOnly() throws CmsException {
        super.store();
    }
    
    /**
     * NOTE: subclasses may override this to extend or prevent.
     */
    protected void storeTemplateBodySource() {
        GenericValue content = replaceTemplateContent(getDelegator(), getTemplateContentId(), tmplBodySrc, null, null);
        if (content != null) {
            setTemplateContentId(content.getString("contentId"));
        }
    }
    
    public static int removeTemplateBodySourceCommon(Delegator delegator, String contentId) {
        int rowsAffected = 0;
        if (UtilValidate.isNotEmpty(contentId)) {
            rowsAffected += CmsTemplate.removeTemplateContent(delegator, contentId);
        }
        return rowsAffected;
    }

    /**
     * 2016: checks and sets body or location as appropriate - helper.
     */
    public void setTemplateBodySource(Map<String, ?> fields) {
        TemplateBodySource newTmplBodySrc = TemplateBodySource.fromFields(fields);
        if (newTmplBodySrc.isDefined()) {
            this.tmplBodySrc = newTmplBodySrc;
        }
    }
    
    /**
     * 2016: checks and sets body or location as appropriate - helper.
     */
    protected void setTemplateBodySource(TemplateBodySource tmplBodySrc) {
        this.tmplBodySrc = tmplBodySrc;
    }
    
    /**
     * Master content create/update method - replaces template body or location - only if required, creating a new Content record, 
     * with optional extra Content fields.
     * <p>
     * NOTE: this method is complicated because of the effort to update in-place, which is mostly optimization and easier to debug IDs.
     */
    public static GenericValue replaceTemplateContent(Delegator delegator, String contentId, TemplateBodySource tmplBodySrc, 
            Map<String, ?> contentFields, Map<String, ?> dataResourceFields) throws CmsException {
        if (tmplBodySrc == null) {
            tmplBodySrc = TemplateBodySource.getUndefined();
        }
        // first, validate the file location if any passed
        if (UtilValidate.isNotEmpty(tmplBodySrc.getLocation())) {
            try {
                URL locUrl = FlexibleLocation.resolveLocation(tmplBodySrc.getLocation());
                if (locUrl == null) {
                    throw new CmsInputException("Invalid template file location: " + tmplBodySrc.getLocation());
                }
                InputStream is = locUrl.openStream();
                is.close();
            } catch (Exception e) {
                throw new CmsInputException("Invalid template file location: " + tmplBodySrc.getLocation());
            }
        }
        
        if (UtilValidate.isNotEmpty(contentId)) {
            try {
                GenericValue content = delegator.findOne("Content", UtilMisc.toMap("contentId", contentId), false);
                if (content == null) {
                    throw new CmsException("Could not get Content entity for template with contentId " + contentId);
                }
                GenericValue dataResource = content.getRelatedOne("DataResource", false);
                if (dataResource == null) {
                    throw new CmsException("Could not get DataResource entity for template with contentId " + contentId);
                }
              
                boolean createNew;
                if (tmplBodySrc.isDefined()) {
                    // need prev body for checks
                    String prevEffectiveBody = DataResourceWorker.getDataResourceText(dataResource, 
                            "text/plain", Locale.getDefault(), new HashMap<>(), delegator, false);
                    
                    String dataResourceTypeId = dataResource.getString("dataResourceTypeId");
                    if ("ELECTRONIC_TEXT".equals(dataResourceTypeId)) {
                        GenericValue electronicText = delegator.findOne("ElectronicText", UtilMisc.toMap("dataResourceId", dataResource.getString("dataResourceId")), false);
                        if (electronicText == null) {
                            throw new CmsException("Could not get ElectronicText entity for template (schema error - ELECTRONIC_TYPE dataResourceTypeId but no ElectronicText value)");
                        }  
                        
                        boolean isSame;
                        boolean isBodyUpdate;
                        if (UtilValidate.isNotEmpty(tmplBodySrc.getLocation())) {
                            isSame = false;
                            // only consider location if non-empty
                            if (UtilValidate.isEmpty(tmplBodySrc.getStoredBody())) {
                                // if location non-empty and body was passed empty, it means an intention
                                // to switch to location
                                isBodyUpdate = false;
                            } else {
                                // if both are non-empty, check against the effective body.
                                // if body changed we use that, otherwise we use location
                                isBodyUpdate = (!StringUtils.equals(tmplBodySrc.getStoredBody(), prevEffectiveBody));
                            }
                        } else {
                            isSame = StringUtils.equals(tmplBodySrc.getStoredBody(), prevEffectiveBody);
                            isBodyUpdate = true;
                        }
                        
                        if (!isSame) {
                            if (isBodyUpdate) {
                                electronicText.set("textData", tmplBodySrc.getStoredBody());
                                electronicText.store();
                            } else {
                                dataResource.set("objectInfo", tmplBodySrc.getLocation());
                                dataResource.set("dataResourceTypeId", "URL_RESOURCE");
                                electronicText.remove();
                            }
                        }
                        
                        createNew = false;
                    } else if ("URL_RESOURCE".equals(dataResourceTypeId)) {
                        boolean isSame;
                        boolean isLocationUpdate;
                        if (UtilValidate.isNotEmpty(tmplBodySrc.getStoredBody())) {
                            // only consider body if non-empty
                            if (UtilValidate.isEmpty(tmplBodySrc.getLocation())) {
                                // if body non-empty and location was passed empty, it means an intention
                                // to switch to body
                                isLocationUpdate = false;
                                isSame = false;
                            } else {
                                // if both are non-empty, check against the effective body.
                                // if body is different from us, use body (has priority), otherwise check if location changed (may end up the same)
                                if (!StringUtils.equals(tmplBodySrc.getStoredBody(), prevEffectiveBody)) {
                                    isLocationUpdate = false;
                                    isSame = false;
                                } else {
                                    isSame = StringUtils.equals(tmplBodySrc.getLocation(), dataResource.getString("objectInfo"));
                                    isLocationUpdate = true;
                                }
                            }
                        } else {
                            isSame = StringUtils.equals(tmplBodySrc.getLocation(), dataResource.getString("objectInfo"));
                            isLocationUpdate = true;
                        }
                        
                        if (!isSame) {
                            if (isLocationUpdate) {
                                dataResource.set("objectInfo", tmplBodySrc.getLocation());
                            } else {
                                GenericValue electronicText = delegator.makeValue("ElectronicText", 
                                        UtilMisc.toMap("dataResourceId", dataResource.getString("dataResourceId"), 
                                                "textData", tmplBodySrc.getStoredBody()));
                                electronicText = delegator.create(electronicText);
                                dataResource.set("dataResourceTypeId", "ELECTRONIC_TEXT");
                            }
                        }
                        
                        createNew = false;
                    } else {
                        Debug.logWarning("CMS: DataResource " + dataResource.getString("dataResourceId") + " had"
                                + " unsupported dataResourceTypeId (" + dataResourceTypeId + ") and will be replaced with fresh records", module);
                        createNew = true;
                    }
                } else {
                    // here we can update the extra fields in-place
                    createNew = false;
                }
                
                if (!createNew) {
                    if (UtilValidate.isNotEmpty(contentFields)) {
                        content.setNonPKFields(contentFields);
                    }
                    if (UtilValidate.isNotEmpty(dataResource != null)) {
                        dataResource.setNonPKFields(dataResourceFields);
                    }
                    dataResource.store();
                    content.store();
                    return content;
                }
            } catch (GenericEntityException e) {
                throw new CmsException("Could not get template content", e);
            } catch (IOException e) {
                throw new CmsException("Could not get template content", e);
            } catch (GeneralException e) {
                throw new CmsException("Could not get template content", e);
            } 
        }
        
        // if above checks failed, we enter remove and create mode...
        
        if (UtilValidate.isNotEmpty(contentId)) {
            // NOTE: this should not really happen unless data was defined in weird way
            Debug.logInfo("CMS: removing previous template Content record because unable to update in-place, contentId: " + contentId, module);
            removeTemplateContent(delegator, contentId);
        }
        
        if (UtilValidate.isNotEmpty(tmplBodySrc.getStoredBody())) { // NOTE: stored body gets priority
            return createContentWithTemplateBody(delegator, tmplBodySrc.getStoredBody(), contentFields, dataResourceFields);
        } else if (UtilValidate.isNotEmpty(tmplBodySrc.getLocation())) {
            return createContentWithTemplateLocation(delegator, tmplBodySrc.getLocation(), contentFields, dataResourceFields);
        } else {
            // here we'll create an empty template body (again stored body gets priority)
            return createContentWithTemplateBody(delegator, "", contentFields, dataResourceFields);
        }
    }

    /**
     * Returns template body source including the effective body.
     */
    public static TemplateBodySource getTemplateBodySourceFromContent(Delegator delegator, String contentId, boolean useCache) throws CmsException {
        if (UtilValidate.isEmpty(contentId)) {
            throw new CmsException("Could not get template content");
        }
        try {
            GenericValue content = delegator.findOne("Content", UtilMisc.toMap("contentId", contentId), useCache);
            if (content == null) {
                throw new CmsException("Could not get Content entity for template");
            }
            GenericValue dataResource = content.getRelatedOne("DataResource", useCache);
            if (dataResource == null) {
                throw new CmsException("Could not get DataResource entity for template");
            }
            
            // force this for now: dataResource.getString("mimeTypeId")
            String effectiveBody = DataResourceWorker.getDataResourceText(dataResource, 
                    "text/plain", Locale.getDefault(), new HashMap<>(), delegator, useCache);
            
            String storedBody = null;
            String location = null;
            // NOTE: we make some assumptions here that might not hold in the future, but that's ok
            // I added extra checks for read support, but we write back only ELECTRONIC_TEXT and URL_RESOURCE for now
            String dataResourceTypeId = dataResource.getString("dataResourceTypeId");
            if ("ELECTRONIC_TEXT".equals(dataResourceTypeId)) {
                storedBody = effectiveBody;
            } else if ("SHORT_TEXT".equals(dataResourceTypeId)) {
                storedBody = effectiveBody;
            } else if ("URL_RESOURCE".equals(dataResourceTypeId)) {
                location = dataResource.getString("objectInfo");
            } else if ("LOCAL_FILE".equals(dataResourceTypeId)) {
                location = dataResource.getString("objectInfo");
                if (!location.startsWith("component://")) {
                    // we'll write this back as URL_RESOURCE for now, so only support this form
                    throw new CmsException("Unsupported template content dataResourceTypeId LOCAL_FILE - does not begin with 'component://': " + dataResourceTypeId);
                }
            } else {
                // NOTE: reject OFBIZ_FILE and LOCAL_FILE for now because interface 
                throw new CmsException("Unsupported template content dataResourceTypeId: " + dataResourceTypeId);
            }
            return new TemplateBodySource(dataResourceTypeId, effectiveBody, storedBody, location);
        } catch (GenericEntityException e) {
            throw new CmsException("Could not get template content", e);
        } catch (IOException e) {
            throw new CmsException("Could not get template content", e);
        } catch (GeneralException e) {
            throw new CmsException("Could not get template content", e);
        }
    }
    
    /**
     * Returns the effective template body no matter storage mechanism.
     * NOTE: already done by getTemplateBodySourceFromContent
     */
    public static String getTemplateBodyFromContentAsText(Delegator delegator, String contentId, boolean useCache) throws CmsException {
        if (UtilValidate.isEmpty(contentId)) {
            Debug.logError("CMS: template body contentId null", module);
            return "";
        }
        try {
            GenericValue content = delegator.findOne("Content", UtilMisc.toMap("contentId", contentId), useCache);
            if (content == null) {
                throw new CmsException("Could not get Content entity for template");
            }
            GenericValue dataResource = content.getRelatedOne("DataResource", useCache);
            if (dataResource == null) {
                throw new CmsException("Could not get DataResource entity for template");
            }
            // force this for now: dataResource.getString("mimeTypeId")
            String templateBody = DataResourceWorker.getDataResourceText(dataResource, 
                    "text/plain", Locale.getDefault(), new HashMap<>(), delegator, useCache);
            return templateBody;
        } catch (GenericEntityException e) {
            throw new CmsException("Could not get template content", e);
        } catch (IOException e) {
            throw new CmsException("Could not get template content", e);
        } catch (GeneralException e) {
            throw new CmsException("Could not get template content", e);
        }
    }
    
    public static GenericValue getContent(Delegator delegator, String contentId, boolean useCache) throws CmsException {
        if (UtilValidate.isEmpty(contentId)) {
            throw new CmsException("Could not get template content");
        }
        try {
            GenericValue content = delegator.findOne("Content", UtilMisc.toMap("contentId", contentId), useCache);
            if (content == null) {
                throw new CmsException("Could not get Content entity for template");
            }
            return content;
        } catch (GenericEntityException e) {
            throw new CmsException("Could not get template content", e);
        }
    }
    
    public static void updateContent(Delegator delegator, String contentId, Map<String, ?> contentFields) {
        if (UtilValidate.isEmpty(contentId)) {
            throw new CmsException("Could not get template content");
        }
        try {
            GenericValue content = delegator.findOne("Content", UtilMisc.toMap("contentId", contentId), false);
            if (content == null) {
                throw new CmsException("Could not get Content entity for template");
            }
            content.setNonPKFields(contentFields);
            content.store();
        } catch (GenericEntityException e) {
            throw new CmsException("Could not get template content", e);
        }
    }
    
    /**
     * Creates an ElectronicText DataResource and Content and returns the new Content for it.
     * optional extra manual fields.
     */
    private static GenericValue createContentWithTemplateBody(Delegator delegator, String templateBody, 
            Map<String, ?> contentFields, Map<String, ?> dataResourceFields) throws CmsException {
        try {
            GenericValue dataResource = delegator.makeValue("DataResource", 
                    UtilMisc.toMap("dataResourceTypeId", "ELECTRONIC_TEXT", "mimeTypeId", "text/plain", 
                            "dataTemplateTypeId", "NONE"));
            if (dataResourceFields != null) {
                dataResource.setNonPKFields(dataResourceFields);
            }
            dataResource = delegator.createSetNextSeqId(dataResource);
           
            GenericValue electronicText = delegator.makeValue("ElectronicText", 
                    UtilMisc.toMap("dataResourceId", dataResource.getString("dataResourceId"), 
                            "textData", templateBody));
            electronicText = delegator.create(electronicText);
           
            GenericValue content = delegator.makeValue("Content", UtilMisc.toMap("dataResourceId", dataResource.getString("dataResourceId"), 
                    "contentTypeId", "DOCUMENT"));
            if (contentFields != null) {
                content.setNonPKFields(contentFields);
            }
            content = delegator.createSetNextSeqId(content);
            return content;
        } catch (GenericEntityException e) {
            throw new CmsException("Could not create template content", e);
        }
    }
        
    /**
     * Creates a new Content and DataResource as URL_RESOURCE (component://) and returns the new Content.
     * optional extra content fields.
     */
    private static GenericValue createContentWithTemplateLocation(Delegator delegator, String templateLocation, 
            Map<String, ?> contentFields, Map<String, ?> dataResourceFields) throws CmsException {
        try {
            // NOTE: dataResourceTypeId could be either URL_RESOURCE or LOCAL_FILE, both
            // will work with component:// prefix
            // I think URL_RESOURCE is more powerful(?) so use that
            GenericValue dataResource = delegator.makeValue("DataResource", 
                    UtilMisc.toMap("dataResourceTypeId", "URL_RESOURCE", "mimeTypeId", "text/plain", 
                            "objectInfo", templateLocation,
                            "dataTemplateTypeId", "NONE"));
            if (dataResourceFields != null) {
                dataResource.setNonPKFields(dataResourceFields);
            }
            dataResource = delegator.createSetNextSeqId(dataResource);
           
            GenericValue content = delegator.makeValue("Content", UtilMisc.toMap("dataResourceId", dataResource.getString("dataResourceId"), 
                    "contentTypeId", "DOCUMENT"));
            if (contentFields != null) {
                content.setNonPKFields(contentFields);
            }
            content = delegator.createSetNextSeqId(content);
            return content;
        } catch (GenericEntityException e) {
            throw new CmsException("Could not create template content", e);
        }
    }
    
    /**
     * Deletes a Content and associated records.
     */
    public static int removeTemplateContent(Delegator delegator, String contentId) throws CmsException {
        int removed = 0;
        if (UtilValidate.isEmpty(contentId)) {
            Debug.logError("CMS: template contentId null", module);
            return removed;
        }
        try {
            GenericValue content = delegator.findOne("Content", UtilMisc.toMap("contentId", contentId), false);
            if (content == null) {
                throw new CmsException(
                        "Could not get Content entity for template");
            }
            GenericValue dataResource = content.getRelatedOne("DataResource", false);
            if (dataResource == null) {
                throw new CmsException(
                        "Could not get DataResource entity for template");
            }
            if ("ELECTRONIC_TEXT".equals(dataResource.getString("dataResourceTypeId"))) {
                GenericValue electronicText = delegator.findOne("ElectronicText", UtilMisc.toMap("dataResourceId", dataResource.getString("dataResourceId")), false);
                if (electronicText == null) {
                    throw new CmsException(
                            "Could not get ElectronicText entity for template (schema error - ELECTRONIC_TYPE dataResourceTypeId but no ElectronicText value)");
                }
                electronicText.remove();
                removed += 1;
            }
            content.remove();
            removed += 1;
            dataResource.remove();
            removed += 1;
        } catch (GenericEntityException e) {
            throw new CmsException("Could not get template content", e);
        }
        return removed;
    }
    

    /**
     * Holds the body, location, any possible source of body for the template,
     * before storage (in storage there is only one used at given time).
     * <p>
     * NOTE: dataResourceTypeId and effectiveBody may be null in between update and store calls (not perfect abstraction).
     */
    @SuppressWarnings("serial")
    public static class TemplateBodySource implements Serializable {
        private static final TemplateBodySource UNDEFINED = new TemplateBodySource();
        private static final TemplateBodySource EMPTY_STRINGS = new TemplateBodySource(null, "", "", "");
        private static final TemplateBodySource UNSUPPORTED = new UnsupportedTemplateBodySource();

        private final String dataResourceTypeId; // used transiently
        
        // NOTE: currently if storedBody is non-null, then when effective body is non-null,
        // they should always be the same reference; but code should NOT assume this
        private final String effectiveBody;
        
        private final String storedBody;
        private final String location;
        
        private TemplateBodySource(String dataResourceTypeId, String effectiveBody, String storedBody, String location) {
            this.dataResourceTypeId = dataResourceTypeId;
            this.effectiveBody = effectiveBody;
            this.storedBody = storedBody;
            this.location = location;
        }
        
        private TemplateBodySource() {
            this.dataResourceTypeId = null;
            this.effectiveBody = null;
            this.storedBody = null;
            this.location = null;
        }
        
        public static TemplateBodySource fromFields(String dataResourceTypeId, String effectiveBody, String storedBody, String location) {
            return new TemplateBodySource(dataResourceTypeId, effectiveBody, storedBody, location);
        }
        
        /**
         * Builds instance using the fields supported by the service interfaces.
         * templateBody - the EFFECTIVE body (treated as incoming stored)
         * templateLocation - the source location
         * <p>
         * NOTE: this sets the templateBody as the "stored" body because it's incoming.
         */
        public static TemplateBodySource fromFields(String templateBody, String location) {
            return new TemplateBodySource(null, null, templateBody, location);
        }
        
        public static TemplateBodySource fromLocation(String location) {
            return new TemplateBodySource(null, null, null, location);
        }
        
        public static TemplateBodySource fromBody(String templateBody) {
            return new TemplateBodySource(null, null, templateBody, null);
        }
        
        public static TemplateBodySource fromTemplateBodySource(TemplateBodySource other) {
            return new TemplateBodySource(null, null, other.storedBody, other.location);
        }
        
        /**
         * Builds instance using the fields supported by the service interfaces.
         * templateBody - the EFFECTIVE body (treated as incoming stored)
         * templateLocation - the source location
         * templateSource - OPTIONAL switch that forces "Body" or "Location"; if omitted, tries to find out automatically
         * <p>
         * NOTE: this sets the templateBody as the "stored" body because it's incoming.
         */
        public static TemplateBodySource fromFields(Map<String, ?> fields) {
            if (fields.containsKey("templateBody") || fields.containsKey("templateLocation") || fields.containsKey("templateSource")) {
                String source = (String) fields.get("templateSource");
                
                String storedBody = null;
                String location = null;
                if ("Body".equals(source)) {
                    storedBody = (String) fields.get("templateBody");
                    if (storedBody == null) {
                        storedBody = "";
                    }
                } else if ("Location".equals(source)) {
                    location = (String) fields.get("templateLocation");
                    if (location == null) {
                        location = "";
                    }
                    // 2016-12-23: I fail to see any case where we want to allow setting an empty
                    // template location... nothing good can come of this
                    if (UtilValidate.isEmpty(location.trim())) {
                        throw new CmsInputException("Template location is empty");
                    }
                } else {
                    // AUTO mode
                    storedBody = (String) fields.get("templateBody");
                    // mark presence with empty string
                    if (storedBody == null && fields.containsKey("templateBody")) {
                        storedBody = "";
                    }
                    location = (String) fields.get("templateLocation");
                    // mark presence with empty string
                    if (location == null && fields.containsKey("templateLocation")) {
                        location = "";
                    }
                }
                if (location != null) {
                    location = location.trim();
                }
                return new TemplateBodySource(null, null, storedBody, location);
            } else {
                return UNDEFINED;
            }
        }

        /**
         * Populates fields map using the fields supported by the service interfaces.
         * templateBody - the EFFECTIVE body
         * templateLocation - the source location
         */
        public void toFields(Map<String, ? super String> fields) {
            fields.put("templateBody", effectiveBody);
            fields.put("templateLocation", location);
        }
        
        public static TemplateBodySource getUndefined() {
            return UNDEFINED;
        }
        
        public static TemplateBodySource getEmptyStrings() {
            return EMPTY_STRINGS;
        }
        
        public static TemplateBodySource getUnsupported() {
            return UNSUPPORTED;
        }
        
        public boolean isDefined() {
            return storedBody != null || location != null;
        }
        
        public String getStoredBody() {
            return storedBody;
        }
        
        public String getLocation() {
            return location;
        }
        
        /**
         * NOTE: this will return null if getting between set and store operations.
         */
        public String getEffectiveBody() {
            return effectiveBody;
        }
        
        public String getDataResourceTypeId() {
            return dataResourceTypeId;
        }
        
        public String getAvailableBody() {
            return effectiveBody != null ? effectiveBody : storedBody;
        }
        
        public static class UnsupportedTemplateBodySource extends TemplateBodySource {
            @Override
            public boolean isDefined() {
                return false;
            }

            @Override
            public String getStoredBody() {
                throw new UnsupportedOperationException();
            }

            @Override
            public String getLocation() {
                throw new UnsupportedOperationException();
            }

            @Override
            public String getEffectiveBody() {
                throw new UnsupportedOperationException();
            }

            @Override
            public String getDataResourceTypeId() {
                throw new UnsupportedOperationException();
            }
        }
    }

}
