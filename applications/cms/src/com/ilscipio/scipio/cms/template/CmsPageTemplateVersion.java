package com.ilscipio.scipio.cms.template;

import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.CmsException;

/**
 * CmsPageTemplateVersion - Represents a version of a page template.
 * <p>
 * Not thread-safe (keep local).
 */
public class CmsPageTemplateVersion extends CmsTemplateVersion {

    private static final long serialVersionUID = 8929881699136390307L;
    
    public static final String module = CmsPageTemplateVersion.class.getName();

    private CmsPageTemplate pageTemplate; // Mutable - set on first get if necessary.    
    private String versionComment = null; // Content.description
    
    // Constructors

    protected CmsPageTemplateVersion(GenericValue entity) {
        this(entity, null);
    }
    
    CmsPageTemplateVersion(GenericValue entity, CmsPageTemplate pageTemplate) {
        super(entity);
        this.pageTemplate = pageTemplate; // Ability to specify pageTemplate here is almost merely an internal optimization
    }

    /**
     * Creates a new template version with the given fields (see CmsPageTemplateVersion entity for available fields)
     * for the given page. If page template ID is passed in fields, must match given page template instance's.
     * 
     * @param fields
     * @param pageTemplate
     */
    public CmsPageTemplateVersion(Delegator delegator, Map<String, ?> fields, CmsPageTemplate pageTemplate) {
        super(delegator, checkPageTemplateId(delegator, fields, pageTemplate, false));
        this.pageTemplate = pageTemplate;
        this.setPageTemplateId(pageTemplate.getId());
        setVersionCommentFromFields(fields, true);
    }
    
    protected CmsPageTemplateVersion(CmsPageTemplateVersion other, Map<String, Object> copyArgs, CmsPageTemplate pageTemplate) {
        super(other, copyArgs);
        if (pageTemplate != null) { // if null, we must be keeping the same parent template
            this.pageTemplate = pageTemplate;
            this.setPageTemplateId(pageTemplate.getId());
        } else if (other.pageTemplate != null) {
            this.pageTemplate = other.pageTemplate;
            this.setPageTemplateId(other.pageTemplate.getId());
        }
    }
    
    private static Map<String, ?> checkPageTemplateId(Delegator delegator, Map<String, ?> fields, CmsPageTemplate pageTemplate,
            boolean useCache) {
        String fieldsTmpId = getPageTemplateId(fields);
        if (UtilValidate.isNotEmpty(fieldsTmpId)) {
            if (!fieldsTmpId.equals(pageTemplate.getId())) {
                throw new CmsException("Trying to create a page template version instance " +
                    "with a template ID in fields (" + fieldsTmpId + ") that is different from " +
                    "page template instance ID (" + pageTemplate.getId() + ")");
            }
        }
        return fields;
    }
    
    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
        setVersionCommentFromFields(fields, setIfEmpty);
    }
    
    @Override
    public CmsPageTemplateVersion copy(Map<String, Object> copyArgs) throws CmsException {
        return new CmsPageTemplateVersion(this, copyArgs, null);
    }
    
    @Override
    public CmsPageTemplateVersion copy(Map<String, Object> copyArgs, CmsVersionedComplexTemplate<?, ?> template) throws CmsException {
        return new CmsPageTemplateVersion(this, copyArgs, (CmsPageTemplate) template);
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
        //this.getPageTemplate(); // NOTE: parent covers this.getTemplate()
        //this.getVersionComment(); // no need when live
    }
 
    // Getters and operational methods    
    public CmsPageTemplate getPageTemplate() {
        if (pageTemplate != null) {
            return pageTemplate;
        } else {
            pageTemplate = CmsPageTemplate.getWorker().findByIdAlways(getDelegator(), getPageTemplateId(), false);
            return pageTemplate;
        }
    }

    public String getPageTemplateId() {
        return entity.getString("pageTemplateId");
    }
    
    public static String getPageTemplateId(Map<String, ?> fields) {
        return (String) fields.get("pageTemplateId");
    }
    
    void setPageTemplateId(String pageTemplateId) {
        entity.set("pageTemplateId", pageTemplateId);
    }
    
    public String getVersionComment() {
        preventIfImmutable();
        
        String versionComment = this.versionComment;
        if (versionComment == null) {
            //versionComment = entity.getString("versionComment");
            // 2016: stored as Content.description
            String contentId = getTemplateContentId();
            if (UtilValidate.isNotEmpty(contentId)) {
                try {
                    versionComment = CmsComplexTemplate.getContent(getDelegator(), contentId, false).getString("description");
                    if (versionComment == null) versionComment = "";
                } catch (CmsException e) {
                    throw new CmsException("Could not get version comment for page template version " + getId() + " of " + getEntityName(), e);
                }
            } else {
                versionComment = "";
            }
            this.versionComment = versionComment;
        }
        return versionComment;
    }
    
    /**
     * Sets version comment.
     * <p>
     * Note: Is only public because it may be desirable to be able to edit past comments (i.e., like SVN).
     * 
     * @param versionComment
     */
    public void setVersionComment(String versionComment) {
        //entity.set("versionComment", versionComment);
        this.versionComment = versionComment; // defer to store()
    }
    
    protected void setVersionCommentFromFields(Map<String, ?> fields, boolean setIfEmpty) {
        if (setIfEmpty) {
            if (fields.containsKey("versionComment")) {
                this.versionComment = (String) fields.get("versionComment");
            }
        } else {
            if (UtilValidate.isNotEmpty((String) fields.get("versionComment"))) {
                this.versionComment = (String) fields.get("versionComment");
            }
        }
    }
    
    @Override
    protected void storeTemplateBodySource() {
        // SPECIAL for page template version ONLY: 
        // override this to store versionComment as description
        // TODO: REVIEW: possible issues with the empty string check?
        Map<String, Object> contentFields = new HashMap<>();
        String versionComment = this.versionComment;
        if (versionComment != null) {
            contentFields.put("description", versionComment);
        }
        GenericValue content = replaceTemplateContent(getDelegator(), getTemplateContentId(), tmplBodySrc, 
                contentFields, null);
        if (content != null) {
            setTemplateContentId(content.getString("contentId"));
        }
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
        return getPageTemplateId();
    }

    @Override
    public CmsVersionedComplexTemplate<?, ?> getTemplate() {
        return getPageTemplate();
    }
    
    @Override
    protected void setTemplate(CmsVersionedComplexTemplate<?, ?> template) {
        if (template == null) {
            setPageTemplateId(null);
            this.pageTemplate = null;
        } else {
            if (!(template instanceof CmsPageTemplate)) throw new CmsException("tried to assign a non-CmsPageTemplate to CmsPageTemplateVersion: " + template.getClass().getName());
            setPageTemplateId(template.getId());
            this.pageTemplate = (CmsPageTemplate) template;
        }
    }

    @Override
    protected void setTemplateId(String templateId) {
        setPageTemplateId(templateId);
    }

    @Override
    public PageTemplateVersionWorker getWorkerInst() {
        return PageTemplateVersionWorker.worker;
    }
    
    public static PageTemplateVersionWorker getWorker() {
        return PageTemplateVersionWorker.worker;
    }

    public static class PageTemplateVersionWorker extends DataObjectWorker<CmsPageTemplateVersion> {
        private static final PageTemplateVersionWorker worker = new PageTemplateVersionWorker();
        
        protected PageTemplateVersionWorker() {
            super(CmsPageTemplateVersion.class);
        }

        @Override
        public CmsPageTemplateVersion makeFromValue(GenericValue value) throws CmsException {
            return new CmsPageTemplateVersion(value);
        }

        @Override
        public CmsPageTemplateVersion makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsPageTemplateVersion(delegator, fields, null);
        }
    }
    
    @Override
    protected CmsPageTemplateActiveVersionWorker getActiveVersionWorkerInst() {
        return CmsPageTemplateActiveVersionWorker.activeVersionWorker;
    }
    
    protected static CmsPageTemplateActiveVersionWorker getActiveVersionWorker() {
        return CmsPageTemplateActiveVersionWorker.activeVersionWorker;
    }  

    public static class CmsPageTemplateActiveVersionWorker extends CmsTemplateVersion.ActiveVersionWorker<CmsPageTemplate, CmsPageTemplateVersion> {
        protected static final CmsPageTemplateActiveVersionWorker activeVersionWorker = new CmsPageTemplateActiveVersionWorker();
        
        @Override
        protected String getStateEntityName() {
            return "CmsPageTemplateVersionState";
        }

        @Override
        protected String getRecordIdFieldName() {
            return "pageTemplateId";
        }
    }
}
