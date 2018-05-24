package com.ilscipio.scipio.content.content;

import java.sql.Timestamp;
import java.util.List;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.content.data.DataResourceWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.content.content.ContentEntityVisitor.AbstractContentEntityVisitor;
import com.ilscipio.scipio.content.content.ContentTraversalException.StopContentTraversalException;

/**
 * SCIPIO: Helper class to go through the Content and related entities.
 * Derived from enhanced stock <code>removeContentAndRelated</code> service behavior,
 * best-effort.
 * <p>
 * FIXME: includeParentContentAssoc should be true by default, but currently it can't work
 * properly when includeChildContentRecursive is true, which is also a wanted default generally.
 * we need a special "seen" set for Content and/or ContentAssoc.
 * If this warning is ignored it leads to either infinite loops or data that exports but that does
 * not load.
 */
public class ContentTraverser extends AbstractContentEntityVisitor {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    protected final ContentEntityVisitor visitor;
    protected final Delegator delegator;
    protected final LocalDispatcher dispatcher;
    protected final ContentTraverserConfig travConfig;
    
    public ContentTraverser(ContentEntityVisitor visitor, Delegator delegator, LocalDispatcher dispatcher, ContentTraverserConfig travConfig) {
        this.visitor = (visitor != null) ? visitor : this;
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.travConfig = (travConfig != null) ? travConfig : newTravConfig();
    }
    
    public ContentTraverser(Delegator delegator, LocalDispatcher dispatcher, ContentTraverserConfig travConfig) {
        this(null, delegator, dispatcher, travConfig);
    }

    public static class ContentTraverserConfig {
        private boolean useCache = false;
        private boolean filterByDate;
        private Timestamp moment;
        
        private boolean includeCore = true;
        private boolean includeRole = true;
        private boolean includeAttr = true;
        private boolean includeChildContentAssoc = true;
        private boolean includeChildContentRecursive = true;
        // FIXME: includeParentContentAssoc should be true by default, but currently it can't work
        private boolean includeParentContentAssoc = false;
        private boolean includeParentContentRecursive = false; // WARN: dangerous
        private boolean includeMediaData = true;
        private boolean includeKeyword = true;
        
        public boolean isUseCache() {
            return useCache;
        }
        
        public ContentTraverserConfig setUseCache(boolean useCache) {
            this.useCache = useCache;
            return this;
        }
        
        public boolean isFilterByDate() {
            return filterByDate;
        }
        
        public ContentTraverserConfig setFilterByDate(boolean filterByDate) {
            this.filterByDate = filterByDate;
            return this;
        }
        
        public Timestamp getMoment() {
            return moment;
        }
        
        public ContentTraverserConfig setMoment(Timestamp moment) {
            this.moment = moment;
            return this;
        }
        
        public boolean isIncludeCore() {
            return includeCore;
        }
        
        public ContentTraverserConfig setIncludeCore(boolean includeCore) {
            this.includeCore = includeCore;
            return this;
        }
        
        public boolean isIncludeRole() {
            return includeRole;
        }
        
        public ContentTraverserConfig setIncludeRole(boolean includeRole) {
            this.includeRole = includeRole;
            return this;
        }
        
        public boolean isIncludeAttr() {
            return includeAttr;
        }
        
        public ContentTraverserConfig setIncludeAttr(boolean includeAttr) {
            this.includeAttr = includeAttr;
            return this;
        }
        
        public boolean isIncludeChildContentAssoc() {
            return includeChildContentAssoc;
        }
        
        public ContentTraverserConfig setIncludeChildContentAssoc(boolean includeChildContentAssoc) {
            this.includeChildContentAssoc = includeChildContentAssoc;
            return this;
        }
        
        public boolean isIncludeChildContentRecursive() {
            return includeChildContentRecursive;
        }
        
        public ContentTraverserConfig setIncludeChildContentRecursive(boolean includeChildContentRecursive) {
            this.includeChildContentRecursive = includeChildContentRecursive;
            return this;
        }
        
        public boolean isIncludeParentContentAssoc() {
            return includeParentContentAssoc;
        }
        
        public ContentTraverserConfig setIncludeParentContentAssoc(boolean includeParentContentAssoc) {
            this.includeParentContentAssoc = includeParentContentAssoc;
            return this;
        }
        
        public boolean isIncludeParentContentRecursive() {
            return includeParentContentRecursive;
        }
        
        public ContentTraverserConfig setIncludeParentContentRecursive(boolean includeParentContentRecursive) {
            this.includeParentContentRecursive = includeParentContentRecursive;
            return this;
        }
        
        public boolean isIncludeMediaData() {
            return includeMediaData;
        }
        
        public ContentTraverserConfig setIncludeMediaData(boolean includeMediaData) {
            this.includeMediaData = includeMediaData;
            return this;
        }
        
        public boolean isIncludeKeyword() {
            return includeKeyword;
        }
        
        public ContentTraverserConfig setIncludeKeyword(boolean includeKeywords) {
            this.includeKeyword = includeKeywords;
            return this;
        }
    }
    
    public ContentTraverserConfig newTravConfig() {
        return new ContentTraverserConfig();
    }

    public ContentTraverserConfig getTravConfig() {
        return travConfig;
    }
    
    public boolean isUseCache() {
        return travConfig.isUseCache();
    }
    
    /**
     * SCIPIO: Best-effort iterates the given Content records, its related Content records, all linked DataResource records,
     * and other records, and for each calls back the {@link ContentEntityVisitor#visitEntity(GenericValue)} method.
     * <p>
     * The relations and types of records included should be the same as the <code>removeContentAndRelated</code> service.
     * <p>
     * Added 2017-11-28.
     */
    public boolean traverseContentAndRelated(String contentId) throws GeneralException {
        GenericValue content = delegator.findOne("Content", UtilMisc.toMap("contentId", contentId), isUseCache());
        if (content == null) throw new GeneralException("content not found for contentId '" + contentId + "'");
        return traverseContentAndRelated(contentId, content);
    }
    
    /**
     * SCIPIO: Best-effort iterates the given Content records, its related Content records, all linked DataResource records,
     * and other records, and for each calls back the {@link ContentEntityVisitor#visitEntity(GenericValue)} method.
     * <p>
     * The relations and types of records included should be the same as the <code>removeContentAndRelated</code> service.
     * <p>
     * Added 2017-11-28.
     */
    public boolean traverseContentAndRelated(GenericValue content) throws GeneralException {
        return traverseContentAndRelated(content.getString("contentId"), content);
    }
    
    /**
     * SCIPIO: Best-effort iterates the given Content records, its related Content records, all linked DataResource records,
     * and other records, and for each calls back the {@link ContentEntityVisitor#visitEntity(GenericValue)} method.
     * <p>
     * The relations and types of records included should be the same as the <code>removeContentAndRelated</code> service.
     * <p>
     * Added 2017-11-28.
     */
    public boolean traverseContentAndRelated(String contentId, GenericValue content) throws GeneralException {
        try {
            traverseContentAndRelatedImpl(contentId, content);
            return true;
        } catch(StopContentTraversalException e) {
            ; // not an error
            return false;
        }
    }
    
    protected void traverseContentAndRelatedImpl(String contentId, GenericValue content) throws GeneralException {
        
        List<GenericValue> contentAssocChild = null;
        if (travConfig.isIncludeChildContentAssoc()) {
            contentAssocChild = EntityQuery.use(delegator).from("ContentAssoc")
                    .where("contentId", contentId).filterByDate(travConfig.isFilterByDate(), travConfig.getMoment())
                    .orderBy("sequenceNum").cache(isUseCache()).queryList();
            
            if (travConfig.isIncludeChildContentRecursive()) {
                for(GenericValue contentAssoc : contentAssocChild) {
                    String contentIdTo = contentAssoc.getString("contentIdTo");
                    GenericValue contentTo = delegator.findOne("Content", UtilMisc.toMap("contentId", contentIdTo), isUseCache());
                    if (contentTo == null) {
                        Debug.logError(getLogErrorPrefix()+"schema error: cannot find Content record for contentId '" + contentIdTo + "'", module);
                    } else {
                        traverseContentAndRelatedImpl(contentIdTo, contentTo);
                    }
                }
            }
        }
        
        List<GenericValue> contentAssocParent = null;
        if (travConfig.isIncludeParentContentAssoc()) {
            contentAssocParent = EntityQuery.use(delegator).from("ContentAssoc")
                    .where("contentIdTo", contentId).filterByDate(travConfig.isFilterByDate(), travConfig.getMoment())
                    .cache(isUseCache()).queryList(); // NOTE: here, .orderBy("sequenceNum") is meaningless
            
            if (travConfig.isIncludeParentContentRecursive()) {
                for(GenericValue contentAssoc : contentAssocChild) {
                    String contentIdStart = contentAssoc.getString("contentId");
                    GenericValue contentStart = delegator.findOne("Content", UtilMisc.toMap("contentId", contentIdStart), isUseCache());
                    if (contentStart == null) {
                        Debug.logError(getLogErrorPrefix()+"schema error: cannot find Content record for contentId '" + contentIdStart + "'", module);
                    } else {
                        traverseContentAndRelatedImpl(contentIdStart, contentStart);
                    }
                }
            }
        }
        
        if (travConfig.isIncludeAttr()) {
            List<GenericValue> contentAttrList = EntityQuery.use(delegator).from("ContentAttribute")
                    .where("contentId", contentId).cache(isUseCache()).queryList();
            for(GenericValue contentAttr : contentAttrList) {
                visitor.visitEntity(contentAttr);
            }
        }
        
        if (travConfig.isIncludeRole()) {
            List<GenericValue> contentRoleList = EntityQuery.use(delegator).from("ContentRole")
                    .where("contentId", contentId).filterByDate(travConfig.isFilterByDate(), travConfig.getMoment()).cache(isUseCache()).queryList();
            for(GenericValue contentAttr : contentRoleList) {
                visitor.visitEntity(contentAttr);
            }
        }
        
        if (travConfig.isIncludeKeyword()) {
            List<GenericValue> contentKeywordList = EntityQuery.use(delegator).from("ContentKeyword")
                    .where("contentId", contentId).filterByDate(travConfig.isFilterByDate(), travConfig.getMoment()).cache(isUseCache()).queryList();
            for(GenericValue contentkeyword : contentKeywordList) {
                visitor.visitEntity(contentkeyword);
            }
        }
        
        String dataResourceId = content.getString("dataResourceId");
        if (dataResourceId != null) {
            GenericValue dataResource = delegator.findOne("DataResource", UtilMisc.toMap("dataResourceId", dataResourceId), isUseCache());
            if (dataResource == null) {
                Debug.logError(getLogErrorPrefix()+"dataResource not found for dataResourceId '" + dataResourceId + "'", module);
            } else {
                if (travConfig.isIncludeCore()) {
                    visitor.visitEntity(dataResource);

                    GenericValue electronicText = delegator.findOne("ElectronicText", UtilMisc.toMap("dataResourceId", dataResourceId), isUseCache());
                    if (electronicText != null) {
                        visitor.visitEntity(electronicText);
                    }
                }
                
                if (travConfig.isIncludeAttr()) {
                    List<GenericValue> dataResAttrList = EntityQuery.use(delegator).from("DataResourceAttribute")
                            .where("dataResourceId", dataResourceId).cache(travConfig.isUseCache()).queryList();
                    for(GenericValue dataResAttr : dataResAttrList) {
                        visitor.visitEntity(dataResAttr);
                    }
                }
                
                if (travConfig.isIncludeRole()) {
                    List<GenericValue> dataResRoleList = EntityQuery.use(delegator).from("DataResourceRole")
                            .where("dataResourceId", dataResourceId).filterByDate(travConfig.isFilterByDate(), travConfig.getMoment()).cache(isUseCache()).queryList();
                    for(GenericValue dataResRole : dataResRoleList) {
                        visitor.visitEntity(dataResRole);
                    }
                }
                
                if (travConfig.isIncludeMediaData()) {
                    GenericValue mediaDataRes = DataResourceWorker.getMediaDataResourceFromDataResource(dataResource, isUseCache());
                    if (mediaDataRes != null) {
                        visitor.visitEntity(mediaDataRes);
                    }
                }
            }
        }
        
        if (travConfig.isIncludeCore()) {
            visitor.visitEntity(content);
        }
        
        if (contentAssocChild != null) {
            for(GenericValue contentAssoc : contentAssocChild) {
                visitor.visitEntity(contentAssoc);
            }
        }
        
        if (contentAssocParent != null) {
            for(GenericValue contentAssoc : contentAssocParent) {
                visitor.visitEntity(contentAssoc);
            }
        }
    }

    protected String getLogMsgPrefix() {
        return "Content: ";
    }
    
    protected String getLogErrorPrefix() {
        return "Error traversing Content records: ";
    }

}
