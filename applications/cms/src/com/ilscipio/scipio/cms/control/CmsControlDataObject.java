package com.ilscipio.scipio.cms.control;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.data.CmsDataException;
import com.ilscipio.scipio.cms.data.CmsDataObject;

public abstract class CmsControlDataObject extends CmsDataObject {

    private static final long serialVersionUID = -3604820756898342590L;

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected CmsControlDataObject(GenericValue entity) {
        super(entity);
    }

    public CmsControlDataObject(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
    }
    
    protected CmsControlDataObject(CmsControlDataObject other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
    }
    
    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
    }
    
    /**
     * 2016: Loads ALL this page's content and products into the current instance.
     * <p>
     * WARN: IMPORTANT: AFTER THIS CALL, 
     * NO FURTHER CALLS ARE ALLOWED TO MODIFY THE INSTANCE IN MEMORY
     * (EVEN if the instance is not physically made immutable!).
     * Essential for thread safety!!!
     */
    @Override
    public void preload(PreloadWorker preloadWorker) {
        super.preload(preloadWorker);
    }
    
    public static abstract class ControlDataObjectWorker<T extends CmsControlDataObject> extends DataObjectWorker<T> {
        
        protected ControlDataObjectWorker(Class<T> dataObjectClass) {
            super(dataObjectClass);
        }

        public abstract List<T> findByWebSiteId(Delegator delegator, String webSiteId, boolean useCache) throws CmsException;
        
        
        public abstract String getControlPageIdFieldName();
        
        /**
         * This logic is common to multiple data objects and mappings in particular. 
         * Performs the temporary app-level synchronization too (FIXME? @see getDataObjectOpsSyncObject).
         * <p>
         * This enforces logical PK fields during create/update at application level, at least for now.
         * <p>
         * For mappings, also handles management of the target page references, which currently all 3 mapping entities have.
         * <p>
         * Fields can contain unrelated entries as well (service context OK).
         * <p>
         * NOTE/TODO?: At the moment this will always throw an exception if no PK specified and
         * a value with logical/candidate PK already exists. Could have a flag that tells to use
         * the existing value if present instead; more friendly but becomes implicit...
         * <p>
         * 2016: moved this here from DataObjectWorker
         */
        public T createOrUpdateControlDataObject(Delegator delegator, String webSiteId, String pkValue, 
                Map<String, ?> inFields) throws CmsException {
            
            T dataObj;
            
            synchronized(getDataObjectOpsSyncObject()) {
                
                Map<String, Object> fields = UtilMisc.makeMapWritable(inFields);
                
                List<String> logicalPkFieldNames = getLogicalPkFieldNames(delegator);
                List<String> logicalPkFieldsAllowedEmptyNames = getLogicalPkFieldsAllowedEmptyNames(delegator);
                String pageIdFieldName = getControlPageIdFieldName();
                boolean hasPageAssoc = (pageIdFieldName != null);
                
                // 2016: we'll do this by pageId for local CMS
//                String pageCmsPageReqPath = null;
//                if (hasPageAssoc) {
//                    pageCmsPageReqPath = (String) fields.get(getPagePathInFieldName());
//                }
                String pageCmsPageId = null;
                if (hasPageAssoc) {
                    pageCmsPageId = (String) fields.get(getControlPageIdFieldName());
                }
                
                for(String fieldName : logicalPkFieldNames) {
                    if (!logicalPkFieldsAllowedEmptyNames.contains(fieldName) && UtilValidate.isEmpty((String) fields.get(fieldName))) {
                        throw new CmsException("Required identifying field " + fieldName + " cannot be empty");
                    }
                }
                
                boolean isNew = UtilValidate.isEmpty(pkValue);
                
                T existingDataObj = null;
                boolean changingLogicalPk;
                if (isNew) {
                    changingLogicalPk = true;
                } else {
                    existingDataObj = findById(delegator, pkValue, false);
                    
                    changingLogicalPk = false;
                    for(String fieldName : logicalPkFieldNames) {
                        String passedVal = (String) fields.get(fieldName);
                        if (passedVal != null && passedVal.length() == 0) {
                            passedVal = null;
                        }
                        String entityVal = existingDataObj.getEntity().getString(fieldName);
                        if (entityVal != null && entityVal.length() == 0) {
                            entityVal = null;
                        }
                        
                        if (passedVal == null) {
                            if (entityVal != null) {
                                changingLogicalPk = true;
                                break; 
                            }
                        } else {
                            if (!passedVal.equals(entityVal)) {
                                changingLogicalPk = true;
                                break;
                            }
                        }
                    }
                }
                
                // Only run candidate key check if we're trying to change the candidate key; least surprise.
                if (changingLogicalPk) {
                
                    Map<String, Object> logicalPkFields = new HashMap<>();
                    for(String fieldName : logicalPkFieldNames) {
                        String fieldVal = (String) fields.get(fieldName);
                        if (fieldVal != null && fieldVal.length() == 0) {
                            fieldVal = null;
                        }
                        logicalPkFields.put(fieldName, fieldVal);
                    }
                    
                    // Enforce candidate key manually by checking if already a record with it...
                    CmsDataObject logicalPkDataObj = findByCandidateKey(delegator, logicalPkFields, false);
                           
                    if (logicalPkDataObj != null) {
                        if (isNew || !pkValue.equals(logicalPkDataObj.getId())) {
                            throw new CmsDataException("A data entry with identifying fields " + logicalPkFields + " already exists");
                        }
                    }
                }
                
                controlHandleExisting(delegator, webSiteId, existingDataObj, fields);
                
                String deletePageId = null;
                
                CmsPage page = null;
                // 2016: doing this by page Id, and for local cms, 
                // the cms page MUST already exist for this to work; the pages 
                // hold their own and can't be just auto-created
//                if (hasPageAssoc && pageCmsPageReqPath != null) {
//                    page = CmsPage.findOrCreatePageByCmsPageReqPath(delegator, pageCmsPageReqPath);
//                    fields.put(pageIdFieldName, page.getId());
//                }
                if (hasPageAssoc && pageCmsPageId != null) {
                    page = CmsPage.getWorker().findById(delegator, pageCmsPageId, false);
                    if (page == null) {
                        throw new CmsDataException("CMS page does not exist for pageId '" + pageCmsPageId + "'");
                    }
                    fields.put(pageIdFieldName, page.getId());
                }
                
                if (isNew) {
                    GenericValue value = delegator.makeValue(getEntityName());
                    value.setNonPKFields(fields, true);
                    dataObj = makeFromValue(value);
                } else {
                    dataObj = existingDataObj;
                    
                    String oldPageId = null;
                    String newPageId = null;
                    if (hasPageAssoc) {
                        oldPageId = dataObj.getEntity().getString(pageIdFieldName);
                        newPageId = null;
                        if (page != null) {
                            newPageId = page.getId();
                        }
                    }
                    
                    dataObj.getEntity().setNonPKFields(fields, true);
                    
                    if (hasPageAssoc) {
                        // If the page has changed, must cleanly remove the old one
                        if (UtilValidate.isNotEmpty(oldPageId)) {
                            if (!oldPageId.equals(newPageId)) {
                                deletePageId = oldPageId;
                            }
                        }
                    }
                }
                
                dataObj.store();
                
                // If the page has changed, must cleanly remove the old one
                if (hasPageAssoc && deletePageId != null) {
                    CmsPage oldPage = CmsPage.getWorker().findById(delegator, deletePageId, false);
                    if (oldPage != null) {
                        oldPage.removeIfOrphan();
                    } else {
                        Debug.logWarning("CMS: Tried to remove old page " + deletePageId + ", " +
                                "but it was already removed, presumably by another process", module);
                    }
                }
                
            }
            
            return dataObj;
        }
        
        public void controlHandleExisting(Delegator delegator, String webSiteId, CmsDataObject existingDataObj, 
                Map<String, Object> fields) throws CmsException {
        }
    }
}
