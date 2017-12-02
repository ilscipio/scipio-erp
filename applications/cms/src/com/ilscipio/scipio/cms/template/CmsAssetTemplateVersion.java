package com.ilscipio.scipio.cms.template;

import java.util.Map;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.CmsException;

/**
 * 2016: Dedicated version instance for asset templates.
 */
public class CmsAssetTemplateVersion extends CmsTemplateVersion {

    private static final long serialVersionUID = -2831896712431295094L;

    public static final String module = CmsAssetTemplateVersion.class.getName();
    
    private CmsAssetTemplate assetTemplate; // Mutable - set on first get if necessary.
    
    // Constructors

    protected CmsAssetTemplateVersion(GenericValue entity) {
        this(entity, null);
    }
    
    CmsAssetTemplateVersion(GenericValue entity, CmsAssetTemplate assetTemplate) {
        super(entity);
        this.assetTemplate = assetTemplate; // Ability to specify assetTemplate here is almost merely an internal optimization
    }

    
    /**
     * Creates a new template version with the given fields (see CmsAssetTemplateVersion entity for available fields)
     * for the given asset. If asset template ID is passed in fields, must match given asset template instance's.
     * 
     * @param fields
     * @param assetTemplate
     */
    public CmsAssetTemplateVersion(Delegator delegator, Map<String, ?> fields, CmsAssetTemplate assetTemplate) {
        super(delegator, checkAssetTemplateId(delegator, fields, assetTemplate, false));
        this.assetTemplate = assetTemplate;
        this.setAssetTemplateId(assetTemplate.getId());
    }
    
    protected CmsAssetTemplateVersion(CmsAssetTemplateVersion other, Map<String, Object> copyArgs, CmsAssetTemplate assetTemplate) {
        super(other, copyArgs);
        if (assetTemplate != null) { // if null, we must be keeping the same parent template
            this.assetTemplate = assetTemplate;
            this.setAssetTemplateId(assetTemplate.getId());
        } else if (other.assetTemplate != null) {
            this.assetTemplate = other.assetTemplate;
            this.setAssetTemplateId(other.assetTemplate.getId());
        }
    }
    
    private static Map<String, ?> checkAssetTemplateId(Delegator delegator, Map<String, ?> fields, CmsAssetTemplate assetTemplate,
            boolean useCache) {
        String fieldsTmpId = getAssetTemplateId(fields);
        if (UtilValidate.isNotEmpty(fieldsTmpId)) {
            if (!fieldsTmpId.equals(assetTemplate.getId())) {
                throw new CmsException("Trying to create an asset template version instance " +
                    "with a template ID in fields (" + fieldsTmpId + ") that is different from " +
                    "asset template instance ID (" + assetTemplate.getId() + ")");
            }
        }
        return fields;
    }
    
    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
    }
    
    @Override
    public CmsAssetTemplateVersion copy(Map<String, Object> copyArgs) throws CmsException {
        return new CmsAssetTemplateVersion(this, copyArgs, null);
    }
    
    @Override
    public CmsAssetTemplateVersion copy(Map<String, Object> copyArgs, CmsVersionedComplexTemplate<?, ?> template) throws CmsException {
        return new CmsAssetTemplateVersion(this, copyArgs, (CmsAssetTemplate) template);
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
        // NOTE: parent handles this.getTemplate()
    }
    
    // Getters and operational methods    
    public CmsAssetTemplate getAssetTemplate() {
        if (assetTemplate != null) {
            return assetTemplate;
        } else {
            assetTemplate = CmsAssetTemplate.getWorker().findByIdAlways(getDelegator(), getAssetTemplateId(), false);
            return assetTemplate;
        }
    }
    
    public String getAssetTemplateId() {
        return entity.getString("assetTemplateId");
    }
    
    public static String getAssetTemplateId(Map<String, ?> fields) {
        return (String) fields.get("assetTemplateId");
    }
    
    private void setAssetTemplateId(String assetTemplateId) {
        entity.set("assetTemplateId", assetTemplateId);
    }

    @Override
    public int remove() {
        Delegator delegator = getDelegator();
        String contentId = getTemplateContentId();
        return super.remove() + removeTemplateBodySourceCommon(delegator, contentId);
    }
    
    // Helpers

    @Override
    public String getTemplateId() {
        return getAssetTemplateId();
    }

    @Override
    public CmsAssetTemplate getTemplate() {
        return getAssetTemplate();
    }
    
    @Override
    protected void setTemplate(CmsVersionedComplexTemplate<?, ?> template) {
        if (template == null) {
            setAssetTemplateId(null);
            this.assetTemplate = null;
        } else {
            if (!(template instanceof CmsAssetTemplate)) throw new CmsException("tried to assign a non-CmsAssetTemplate to CmsAssetTemplateVersion: " + template.getClass().getName());
            setAssetTemplateId(template.getId());
            this.assetTemplate = (CmsAssetTemplate) template;
        }
    }
    
    @Override
    protected void setTemplateId(String templateId) {
        setAssetTemplateId(templateId);
    }

    @Override
    public AssetTemplateVersionWorker getWorkerInst() {
        return AssetTemplateVersionWorker.worker;
    }
    
    public static AssetTemplateVersionWorker getWorker() {
        return AssetTemplateVersionWorker.worker;
    }

    public static class AssetTemplateVersionWorker extends DataObjectWorker<CmsAssetTemplateVersion> {
        private static final AssetTemplateVersionWorker worker = new AssetTemplateVersionWorker();
        
        protected AssetTemplateVersionWorker() {
            super(CmsAssetTemplateVersion.class);
        }

        @Override
        public CmsAssetTemplateVersion makeFromValue(GenericValue value) throws CmsException {
            return new CmsAssetTemplateVersion(value);
        }

        @Override
        public CmsAssetTemplateVersion makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsAssetTemplateVersion(delegator, fields, null);
        }
    }
    
    @Override
    protected AssetTemplateActiveVersionWorker getActiveVersionWorkerInst() {
        return AssetTemplateActiveVersionWorker.activeVersionWorker;
    }
    
    protected static AssetTemplateActiveVersionWorker getActiveVersionWorker() {
        return AssetTemplateActiveVersionWorker.activeVersionWorker;
    }
    
    public static class AssetTemplateActiveVersionWorker extends CmsTemplateVersion.ActiveVersionWorker<CmsAssetTemplate, CmsAssetTemplateVersion> {
        protected static final AssetTemplateActiveVersionWorker activeVersionWorker = new AssetTemplateActiveVersionWorker();
        
        @Override
        protected String getStateEntityName() {
            return "CmsAssetTemplateVersionState";
        }

        @Override
        protected String getRecordIdFieldName() {
            return "assetTemplateId";
        }
    }
}
