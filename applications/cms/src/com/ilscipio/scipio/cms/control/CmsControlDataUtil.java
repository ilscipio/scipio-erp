package com.ilscipio.scipio.cms.control;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.data.CmsDataObject;

public abstract class CmsControlDataUtil {

    public static final String module = CmsControlDataUtil.class.getName();
    
    /**
     * Names of all Cms mapping-related entities.
     * <p>
     * Listed here in an order that produces no dependency issues if records are all deleted in this order.
     * <p>
     * NOTE: 2016: does NOT include CmsPage in local CMS
     */
    public static final Set<String> cmsMappingEntities = Collections.unmodifiableSet(new HashSet<>(Arrays.asList(new String[] {
            "CmsProcessViewMapping", "CmsProcessMapping", "CmsViewMapping" })));

    private CmsControlDataUtil() {}
   
    public static int deleteAllMappingRecords(Delegator delegator, boolean includePrimary) throws CmsException {
        int rowsAffected = 0;
        
        try {
            if (includePrimary) {
                for(String entityName : cmsMappingEntities) {
                    rowsAffected += delegator.removeAll(entityName);
                }
            } else {
                rowsAffected += delegator.removeAll("CmsViewMapping");
                rowsAffected += CmsDataObject.removeAll(
                        CmsProcessMapping.getWorker().findAll(delegator, UtilMisc.toMap("primaryForPageId", null), null, false));
            }
        }
        catch(GenericEntityException e) {
            throw new CmsException(e);
        }
        
        return rowsAffected;
    }
    
    public static int deleteWebSiteMappingRecords(Delegator delegator, String webSiteId, boolean includePrimary) throws CmsException {
        // These remove operations automatically remove no-longer-needed CmsPage and related values intelligently
        int rowsAffected = 0;
        
        if (includePrimary) {
            rowsAffected += CmsDataObject.removeAll(CmsViewMapping.getWorker().findByWebSiteId(delegator, webSiteId, false));
            rowsAffected += CmsDataObject.removeAll(CmsProcessMapping.getWorker().findByWebSiteId(delegator, webSiteId, false));
        } else {
            rowsAffected += CmsDataObject.removeAll(CmsViewMapping.getWorker().findByWebSiteId(delegator, webSiteId, false));
            rowsAffected += CmsDataObject.removeAll(
                    CmsProcessMapping.getWorker().findAll(delegator, UtilMisc.toMap("sourceWebSiteId", webSiteId, "primaryForPageId", null), 
                            null, false));
        }
        
        return rowsAffected;
    }
}
