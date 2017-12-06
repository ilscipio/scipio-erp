package com.ilscipio.scipio.cms.control;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.cms.CmsServiceUtil;
import com.ilscipio.scipio.cms.ServiceErrorFormatter;
import com.ilscipio.scipio.cms.ServiceErrorFormatter.FormattedError;
import com.ilscipio.scipio.cms.content.CmsPage;

/**
 * Cms control data-related Ofbiz services.
 */
public abstract class CmsControlDataServices {
    
    public static final String module = CmsControlDataServices.class.getName();
    private static final ServiceErrorFormatter errorFmt = CmsServiceUtil.getErrorFormatter();

    protected CmsControlDataServices() {}

    public static Map<String, Object> createUpdateProcessMapping(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        
        String processMappingId = (String) context.get("processMappingId");
        String sourceWebSiteId = (String) context.get("sourceWebSiteId");

        try {
            CmsProcessMapping cmsProcessMapping = CmsProcessMapping.getWorker()
                    .createOrUpdateControlDataObject(delegator, sourceWebSiteId, processMappingId, context);
            processMappingId = cmsProcessMapping.getId();
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error creating or updating process mapping", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
    
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("processMappingId", processMappingId);
        return result;
    }
    
    public static Map<String, Object> deleteProcessMapping(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();

        String processMappingId = (String) context.get("processMappingId");

        try {
            CmsProcessMapping.getWorker().deleteDataObject(delegator, processMappingId);
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error deleting process mapping", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
    
        Map<String, Object> result = ServiceUtil.returnSuccess();
        return result;
    }
    
    public static Map<String, Object> createUpdateProcessViewMapping(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();

        String processViewMappingId = (String) context.get("processViewMappingId");
        String webSiteId = (String) context.get("webSiteId");

        try {
            CmsProcessViewMapping processViewMapping = CmsProcessViewMapping.getWorker()
                    .createOrUpdateControlDataObject(delegator, webSiteId, processViewMappingId, context);
            processViewMappingId = processViewMapping.getId();
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error creating or updating process view mapping", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
    
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("processViewMappingId", processViewMappingId);
        return result;
    }
    
    public static Map<String, Object> deleteProcessViewMapping(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();

        String processViewMappingId = (String) context.get("processViewMappingId");

        try {
            CmsProcessViewMapping.getWorker().deleteDataObject(delegator, processViewMappingId);
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error deleting process view mapping", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
    
        Map<String, Object> result = ServiceUtil.returnSuccess();
        return result;
    }
    
    public static Map<String, Object> createUpdateViewMapping(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();

        String viewMappingId = (String) context.get("viewMappingId");
        String webSiteId = (String) context.get("webSiteId");
        
        try {
            CmsViewMapping viewMapping = CmsViewMapping.getWorker()
                    .createOrUpdateControlDataObject(delegator, webSiteId, viewMappingId, context);
            viewMappingId = viewMapping.getId();
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error creating or updating view mapping", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
    
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("viewMappingId", viewMappingId);
        return result;
    }
    
    public static Map<String, Object> deleteViewMapping(DispatchContext dctx, Map<String, Object> context) {
        
        Delegator delegator = dctx.getDelegator();

        String viewMappingId = (String) context.get("viewMappingId");

        try {
            CmsViewMapping.getWorker().deleteDataObject(delegator, viewMappingId);
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error deleting view mapping", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
    
        Map<String, Object> result = ServiceUtil.returnSuccess();
        return result;
    }

    
    public static Map<String, Object> clearMappingCaches(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        
        List<String> errMsgList = new ArrayList<>();
        
        Boolean clearMemoryCaches = (Boolean) context.get("clearMemoryCaches");
        if (clearMemoryCaches == null) {
            clearMemoryCaches = true;
        }
        Boolean clearEntityCaches = (Boolean) context.get("clearEntityCaches");
        if (clearEntityCaches == null) {
            clearEntityCaches = true;
        }
        
        if (clearEntityCaches) {
            try {
                CmsPage.getWorker().clearEntityCaches(delegator);
                CmsView.getWorker().clearEntityCaches(delegator);
                CmsViewMapping.getWorker().clearEntityCaches(delegator);
                CmsProcessViewMapping.getWorker().clearEntityCaches(delegator);
                CmsProcessMapping.getWorker().clearEntityCaches(delegator);
            } catch(Exception e) {
                final String errMsg = "Error clearing mapping entity cache: " + e.getMessage();
                errMsgList.add(errMsg);
                Debug.logError(e, "Cms: " + errMsg, module);
            }
        }
        
        if (clearMemoryCaches) {
            CmsPage.getWorker().clearMemoryCaches();
            CmsView.getWorker().clearMemoryCaches();
            CmsViewMapping.getWorker().clearMemoryCaches();
            CmsProcessViewMapping.getWorker().clearMemoryCaches();
            CmsProcessMapping.getWorker().clearMemoryCaches();
        }
        
        Map<String, Object> result;
        if (errMsgList.size() > 0) {
            result = ServiceUtil.returnFailure(errMsgList);
        } else {
            result = ServiceUtil.returnSuccess();
        }
        return result;
    }
    
    public static Map<String, Object> deleteAllMappingRecords(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        
        List<String> errMsgList = new ArrayList<>();
        boolean includePrimary = Boolean.TRUE.equals(context.get("includePrimary"));

        int numRemoved = 0;
        try {
            numRemoved = CmsControlDataUtil.deleteAllMappingRecords(delegator, includePrimary);
        } catch (Exception e) {
            final String errMsg = "Error trying to delete all mapping records: " + e.getMessage();
            errMsgList.add(errMsg);
            Debug.logError(e, "Cms: " + errMsg, module);
        }

        Map<String, Object> result;
        if (errMsgList.size() > 0) {
            result = ServiceUtil.returnError(errMsgList);
        } else {
            final String msg = "Deleted all mapping records; " + numRemoved + " removed";
            if (Debug.infoOn()) {
                Debug.logInfo("Cms: " + msg, module);
            }
            result = ServiceUtil.returnSuccess(msg);
        }
        return result;
    }
    
    public static Map<String, Object> deleteWebSiteMappingRecords(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        List<String> errMsgList = new ArrayList<>();
        
        String webSiteId = (String) context.get("webSiteId");
        boolean includePrimary = Boolean.TRUE.equals(context.get("includePrimary"));
        
        int numRemoved = 0;
        try {
            numRemoved = CmsControlDataUtil.deleteWebSiteMappingRecords(delegator, webSiteId, includePrimary);
        } catch(Exception e) {
            final String errMsg = "Error trying to delete mapping records for web site '" + webSiteId + "': " + e.getMessage();
            errMsgList.add(errMsg);
            Debug.logError(e, "Cms: " + errMsg, module);
        }

        Map<String, Object> result;
        if (errMsgList.size() > 0) {
            result = ServiceUtil.returnError(errMsgList);
        } else {
            final String msg = "Deleted mapping records for web site '" + webSiteId + "'; " + numRemoved + " removed";
            if (Debug.infoOn()) {
                Debug.logInfo("Cms: " + msg, module);
            }
            result = ServiceUtil.returnSuccess(msg);
        }
        return result;
    }

    public static Map<String, Object> addRemovePageViewMappings(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        
        // NOTE: 2016: perform auth here, this is a frontend service...
        Locale locale = (Locale) context.get("locale");

        String pageId = (String) context.get("pageId"); // required
        String webSiteId = (String) context.get("webSiteId"); // optional
        if (UtilValidate.isEmpty(webSiteId)) {
            webSiteId = null;
        }
        List<String> addViewNameList = UtilGenerics.checkList(context.get("addViewNameList"));
        List<String> removeViewNameList = UtilGenerics.checkList(context.get("removeViewNameList"));
        List<String> removeIdList = UtilGenerics.checkList(context.get("removeIdList"));

        try {
            // NOTE: wrecking abstractions here, but CmsViewMapping is simple (CmsViewMapping.remove() is trivial)
            
            if (UtilValidate.isNotEmpty(addViewNameList)) {
                for(String viewNameExpr : addViewNameList) {
                    if (UtilValidate.isNotEmpty(viewNameExpr)) {
                        Map<String, Object> fields = makeViewMappingCandidateKeyFields(webSiteId, viewNameExpr, locale);
                        
                        // first, remove all existing mappings to other pages for this candidate key, to ensure we add only unique
                        delegator.removeByAnd("CmsViewMapping", fields);
                        
                        // then, add a record for our page
                        fields.put("pageId", pageId);
                        fields.put("active", CmsViewMapping.ACTIVE_INITIAL_VALUE); // NOTE: 2016-12-02: CURRENTLY ALWAYS ACTIVE - TODO? review in future
                        CmsViewMapping viewMapping = new CmsViewMapping(delegator, fields);
                        viewMapping.store();
                    }
                }
            }
            
            if (UtilValidate.isNotEmpty(removeViewNameList)) {
                for(String viewNameExpr : removeViewNameList) {
                    if (UtilValidate.isNotEmpty(viewNameExpr)) {
                        Map<String, Object> fields = makeViewMappingCandidateKeyFields(webSiteId, viewNameExpr, locale);

                        // remove, but only if match our pageId
                        // NOTE: we only do a page-blind remove in the previous add operation, because it
                        // must ensure a unique result, but here due to concurrent requests we should probably not blanket (not unless readding as above)
                        fields.put("pageId", pageId);
                        delegator.removeByAnd("CmsViewMapping", fields);
                    }
                }
            }
            
            if (UtilValidate.isNotEmpty(removeIdList)) {
                for(String viewMappingId : removeIdList) {
                    if (UtilValidate.isNotEmpty(viewMappingId)) {
                        delegator.removeByAnd("CmsViewMapping", UtilMisc.toMap("viewMappingId", viewMappingId));
                    }
                }
            }
        } catch (LocalizedIllegalArgumentException e) {
            Debug.logError(e, "Cms: " + e.getMessage(), module);
            return ServiceUtil.returnError(e.getLocalizedMsg());
        } catch (Exception e) {
            Debug.logError(e, "Cms: " + e.getMessage(), module);
            return ServiceUtil.returnError(e.getMessage());
        }
    
        return ServiceUtil.returnSuccess();
    }
    
    // TODO: localize
    private static Map<String, Object> makeViewMappingCandidateKeyFields(String webSiteId, String viewNameExpr, Locale locale) throws IllegalArgumentException {
        String targetViewName;
        String[] parts = viewNameExpr.split("::", 2);
        if (parts.length >= 2) {
            webSiteId = parts[0];
            targetViewName = parts[1];
        } else {
            if (webSiteId == null) {
                throw new LocalizedIllegalArgumentException("Could not identify CmsViewMapping record from candidate key '" + viewNameExpr + "' - no webSiteId designated");
            }
            targetViewName = viewNameExpr;
        }
        if (UtilValidate.isEmpty(webSiteId)) {
            throw new LocalizedIllegalArgumentException("Could not identify CmsViewMapping record from candidate key '" + viewNameExpr + "' - no webSiteId designated");
        }
        if (UtilValidate.isEmpty(targetViewName)) {
            throw new LocalizedIllegalArgumentException("Could not identify CmsViewMapping record from candidate key '" + viewNameExpr + "' - no targetViewName designated");
        }
        return UtilMisc.toMap("webSiteId", webSiteId, "targetViewName", targetViewName);
    }
    
    @SuppressWarnings("serial")
    public static class LocalizedIllegalArgumentException extends IllegalArgumentException {

        private final String localizedMsg;

        LocalizedIllegalArgumentException(String s, String localizedMsg) {
            super(s);
            this.localizedMsg = localizedMsg;
        }
        
        @Deprecated
        LocalizedIllegalArgumentException(String s) {
            super(s);
            this.localizedMsg = s;
        }

        public String getLocalizedMsg() {
            return localizedMsg;
        } 
    }
    
    public static Map<String, Object> getWebsiteIndexablePageUris(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        String webSiteId = (String) context.get("webSiteId");
        Locale contentLocale = (Locale) context.get("contentLocale");
        boolean useCache = Boolean.TRUE.equals(context.get("useCache"));

        try {
            List<String> uriList = CmsProcessMapping.getWebsiteActiveIndexableUris(delegator, webSiteId, contentLocale, useCache);
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("uriList", uriList);
            return result;
        } catch (Exception e) {
            Debug.logError(e, "Cms: " + e.getMessage(), module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }
}
