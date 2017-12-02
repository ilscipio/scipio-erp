package com.ilscipio.scipio.cms.content;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityFindOptions;

import com.ilscipio.scipio.ce.util.Optional;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.template.CmsComplexTemplate;
import com.ilscipio.scipio.cms.template.CmsTemplate;
import com.ilscipio.scipio.cms.template.CmsTemplate.TemplateBodySource;
import com.ilscipio.scipio.cms.template.CmsTemplateVersion;

public class CmsPageVersion extends CmsDataObject {
    
    private static final long serialVersionUID = -714031134721469544L;
    
    public static final String module = CmsPageVersion.class.getName();
    
    protected static final CmsPageActiveVersionWorker activeVersionWorker = new CmsPageActiveVersionWorker();
    
    private CmsPage page;
    private Map<String, ?> content = null;
    protected Optional<String> contentString = null; // NOTE: this is left null in live render; for create/update only
    private String versionComment = null; // Content.description

    protected CmsPageVersion(GenericValue entity) {
        super(entity);
    }

    protected CmsPageVersion(Delegator delegator, Map<String, ?> fields, CmsPage page) {
        super(delegator, fields);
        this.page = page;
        if (!fields.containsKey("pageId")) {
            this.entity.put("pageId", page.getId());
        }
        setContentFromFields(fields, true);
        setVersionCommentFromFields(fields, true);
    }
    
    protected CmsPageVersion(CmsPageVersion other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
    }
    
    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
        setContentFromFields(fields, setIfEmpty);
        setVersionCommentFromFields(fields, setIfEmpty);
    }
    
    @Override
    public CmsPageVersion copy(Map<String, Object> copyArgs) throws CmsException {
        return new CmsPageVersion(this, copyArgs);
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
        this.content = preloadWorker.transformContainer(this.getContent());
        //this.getVersionComment(); // no need when live
    }
    
    public String getContentId() {
        return entity.getString("contentId");
    }
    
    public void setContentId(String contentId) {
        entity.setString("contentId", contentId);
    }
    
    protected void setContentFromFields(Map<String, ?> fields, boolean setIfEmpty) {
        if (setIfEmpty) {
            if (fields.containsKey("content")) {
                contentString = Optional.ofNullable((String) fields.get("content"));
            }
        } else {
            if (UtilValidate.isNotEmpty((String) fields.get("content"))) {
                contentString = Optional.ofNullable((String) fields.get("content"));
            }
        }
    }
    
//    public void setContent(String jsonContent) {
//        entity.setString("content", jsonContent);
//    }
//
//    public void setContent(Map<String, ?> content) {
//        try {
//            setContent(JSON.from(content).toString());
//        } catch (IOException e) {
//            Debug.logError(e, module);
//        }
//    }

    protected String getContentBody() {
        String contentId = getContentId();
        if (contentId == null) return null;
        TemplateBodySource tmplBodySrc = CmsTemplate.getTemplateBodySourceFromContent(getDelegator(), contentId, false);
        return tmplBodySrc.getEffectiveBody();
    }
    
    @SuppressWarnings("unchecked")
    public Map<String, ?> getContent() {
        Map<String, ?> content = this.content;
        if (content == null) {
            String entityContent = getContentBody();
            if (entityContent != null) {
                try {
                    content = (Map<String, ?>) JSON.from(entityContent).toObject(Map.class);
                } catch (IOException e) {
                    Debug.logError(e, module);
                }
            }
            content = content != null ? content : new HashMap<String, Object>();
            this.content = content;
        }
        return content;
    }

    public String getVersionComment() {
        preventIfImmutable();
        
        String versionComment = this.versionComment;
        if (versionComment == null) {
            //versionComment = entity.getString("versionComment");
            // 2016: stored as Content.description
            String contentId = getContentId();
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
    
    @Deprecated
    public String getComment() {
        return getVersionComment();
    }

    public void setVersionComment(String versionComment) {
        //entity.setString("versionComment", comment);
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
    
    public String getCreatedBy() {
        return entity.getString("createdBy");
    }
    
    public String getCreatedByName(){
        try {
            return CmsUtil.getPersonDisplayName(getDelegator(), entity.getString("createdBy"));
        } catch (GenericEntityException e) {
            throw new CmsException(
                "Could not retrieve user for pageVersion " + entity.getString("versionId"), e);
        }  
    }

    public void setCreatedBy(String createdBy) {
        entity.setString("createdBy", createdBy);
    }

    public CmsPage getPage() {
        return page;
    }

    public void setPage(CmsPage page) {
        this.page = page;
        this.entity.put("pageId", page.getId());
    }

    @Override
    public void store() throws CmsException {
        Map<String, Object> contentFields = new HashMap<>();
        String versionComment = this.versionComment;
        if (versionComment != null) {
            contentFields.put("description", versionComment);
        }
        TemplateBodySource tmplBodySrc = TemplateBodySource.fromBody(this.contentString != null ? this.contentString.orElse("") : "");
        GenericValue content = CmsTemplate.replaceTemplateContent(getDelegator(), getContentId(), tmplBodySrc, 
                contentFields, null);
        if (content != null) {
            setContentId(content.getString("contentId"));
        }
        super.store();
    }
    
    @Override
    public int remove() throws CmsException {
        Delegator delegator = getDelegator();
        String contentId = getContentId();
        return super.remove() + CmsTemplate.removeTemplateBodySourceCommon(delegator, contentId);
    }
    
    
    // Helpers

    @Override
    public PageVersionWorker getWorkerInst() {
        return PageVersionWorker.worker;
    }
    
    public static PageVersionWorker getWorker() {
        return PageVersionWorker.worker;
    }

    public static class CmsPageActiveVersionWorker extends CmsTemplateVersion.ActiveVersionWorker<CmsPage, CmsPageVersion> {
        @Override
        protected String getStateEntityName() {
            return "CmsPageVersionState";
        }

        @Override
        protected String getRecordIdFieldName() {
            return "pageId";
        }
    }
    
    public static class PageVersionWorker extends DataObjectWorker<CmsPageVersion> {
        private static final PageVersionWorker worker = new PageVersionWorker();
        
        protected PageVersionWorker() {
            super(CmsPageVersion.class);
        }

        @Override
        public CmsPageVersion makeFromValue(GenericValue value) throws CmsException {
            return new CmsPageVersion(value);
        }

        @Override
        public CmsPageVersion makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsPageVersion(delegator, fields, null);
        }

        /**
         * Returns the most current page version.
         * 
         * @param pageId
         * @return
         */
        public CmsPageVersion findLast(Delegator delegator, String pageId, boolean useCache) {
            CmsPageVersion pv = null;
            List<CmsPageVersion> list = findAll(delegator, pageId, useCache);
            if (list.size() > 0) {
                pv = list.get(0);
            }
            return pv;
        }

        /**
         * Returns all versions of a page starting with the most recent.
         * 
         * @param pageId
         * @return
         */
        public List<CmsPageVersion> findAll(Delegator delegator, String pageId, boolean useCache) {
            List<CmsPageVersion> versions = new ArrayList<>();
            try {
                EntityFindOptions efo = new EntityFindOptions();
                efo.setFetchSize(1);
                EntityCondition ec = EntityCondition.makeCondition("pageId",
                        EntityOperator.EQUALS, pageId);
                List<GenericValue> values = (List<GenericValue>) delegator
                        .findList("CmsPageVersion", ec, null,
                                UtilMisc.toList("createdStamp DESC"), efo, useCache);

                for (GenericValue v : values) {
                    versions.add(new CmsPageVersion(v));
                }
            } catch (GenericEntityException e) {
                throw new CmsException(
                        "Could not retrieve page version. Page Id: " + pageId, e);
            }

            return versions;
        }

        /**
         * Returns a specific page version.
         * 
         * @param pageId
         * @param versionId
         * @return
         */
        public CmsPageVersion find(Delegator delegator, String pageId, String versionId, boolean useCache) {
            return findFirst(delegator,
                    UtilMisc.toMap("pageId", pageId, "versionId", versionId),
                    useCache);
        }
    }

}
