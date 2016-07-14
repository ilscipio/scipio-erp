package com.ilscipio.scipio.cms.template;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import javolution.util.FastList;
import javolution.util.FastMap;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * CmsPageTemplateServices - CMS Page Template and Page Template Version Services
 *
 */
public final class CmsPageTemplateServices {
    public static final String module = CmsPageTemplateServices.class.getName();
    
    
    /**
     * Gets a page template.
     * 
     * @param dctx
     *            The DispatchContext that this service is operating in
     * @param context
     *            Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> getPageTemplate(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String pageTemplateId = (String) context.get("pageTemplateId");
            
            // TODO: Replace with a call that doesn't involve throwing exceptions if not found
            Map<String, Object> pageTemplate = CmsPageTemplate.getPageTemplateAsMap(delegator, pageTemplateId);
            
            if (UtilValidate.isNotEmpty(pageTemplate)) {
                result.put("pageTemplate", pageTemplate);
            }
            else {
                result = ServiceUtil.returnFailure("Page template '" + pageTemplateId + "' not found"); // TODO: Localize
            }
        } 
        catch (Exception e) {
            Debug.logError(e, module);
            // Note: Why is our 'convention' to return failure here? Must be because
            // we throw exceptions from constructors that look DB values up by ID when they find nothing.
            // TODO: Change this behavior to use factory methods that return null so can differentiate
            // exceptions/errors from each other and return error when it's called for here instead.
            result = ServiceUtil.returnFailure(e.getMessage());
        }
        return result;
    }
    
    /**
     * Gets a page template version.
     * 
     * @param dctx
     *            The DispatchContext that this service is operating in
     * @param context
     *            Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> getPageTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            Delegator delegator = dctx.getDelegator();
            String pageTemplateId = (String) context.get("pageTemplateId");
            String versionId = (String) context.get("versionId");
            boolean minimalInfoOnly = EntityUtil.toBoolean((String) context.get("minimalInfoOnly"), false);
            
            CmsPageTemplateVersion tmpVer = CmsPageTemplateVersion.findSpecificTemplateVersion(delegator, pageTemplateId, versionId);
            
            if (tmpVer != null) {
                Map<String, Object> version = FastMap.newInstance();
                tmpVer.putIntoMap(version, minimalInfoOnly, null);
                result.put("version", version);
            }
            else {
                result = ServiceUtil.returnFailure("Could not find page template version '" + versionId + "' for page template '" +
                        pageTemplateId + "'"); // TODO: Localize
            }
        } 
        catch (Exception e) {
            Debug.logError(e, module);
            result = ServiceUtil.returnFailure(e.getMessage());
        }
        return result;
    }
    
    /**
     * Gets all page template versions. Currently returns in the order in which created.
     * 
     * @param dctx
     *            The DispatchContext that this service is operating in
     * @param context
     *            Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> getPageTemplateVersions(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            List<Map<String, Object>> allVersionsList = FastList.newInstance();
            
            String pageTemplateId = (String) context.get("pageTemplateId");
            boolean minimalInfoOnly = EntityUtil.toBoolean((String) context.get("minimalInfoOnly"), false);
            
            CmsPageTemplate pageTmp = new CmsPageTemplate(pageTemplateId);
            
            List<CmsPageTemplateVersion> versions = pageTmp.getAllVersions();
            
            if (UtilValidate.isNotEmpty(versions)) {
                CmsPageTemplateVersion.ExtendedInfo knownInfo = new CmsPageTemplateVersion.ExtendedInfo();
                
                // Optimization for loop (TODO: Move this to an abstracted iterator or other later)
                knownInfo.setActiveVersion(pageTmp.getActiveVersion());
                knownInfo.setLastVersion(versions.get(0));
                knownInfo.setFirstVersion(versions.get(versions.size() - 1));
                
                for(CmsPageTemplateVersion version : versions) {
                    Map<String, Object> versionMap = FastMap.newInstance();
                    version.putIntoMap(versionMap, minimalInfoOnly, knownInfo);
                    allVersionsList.add(versionMap);
                }
            }
            
            result.put("versions", allVersionsList);   
        } 
        catch (Exception e) {
            Debug.logError(e, module);
            result = ServiceUtil.returnFailure(e.getMessage());
        }
        return result;
    }
    
    /**
     * Returns a map of a page template and all its versions, as well as extra helpful fields.
     * 
     * @param dctx
     *            The DispatchContext that this service is operating in
     * @param context
     *            Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> getPageTemplateAndVersions(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            Delegator delegator = dctx.getDelegator();
            
            String pageTemplateId = (String) context.get("pageTemplateId");
            
            // The current template is almost a display-related concern, but is optional
            String currentVersionId = (String) context.get("currentVersionId");
            String useMarkedAsCurrent = (String) context.get("useMarkedAsCurrent");
            
            Map<String, Object> pageTmpAndVersions = FastMap.newInstance();
            
            CmsPageTemplate pageTmp = new CmsPageTemplate(pageTemplateId);
            pageTmp.putTemplateFieldsIntoMap(pageTmpAndVersions);
            
            CmsPageTemplateVersion currVer = null;

            if (UtilValidate.isNotEmpty(currentVersionId)) {
                currVer = CmsPageTemplateVersion.findSpecificTemplateVersion(delegator, pageTmp, currentVersionId);
                
                if (currVer == null) {
                    final String errMsg = "Could not find specified current page template version ('" + 
                    currentVersionId + "') for specified page template ('" + pageTemplateId + "'); not ";
                    Debug.logWarning(errMsg, module); // (Don't localize in this call)        
                    // Don't fail - permit this since the 'current' version marking is provided only as convenience.
                    //result = ServiceUtil.returnFailure(errMsg); // TODO: Localize
                }
            }
            
            Map<String, Object> pageTmpVersions = FastMap.newInstance();
            List<Map<String, Object>> allVersions = FastList.newInstance();
            
            CmsPageTemplateVersion.ExtendedInfo knownInfo = new CmsPageTemplateVersion.ExtendedInfo();
            
            knownInfo.setActiveVersion(pageTmp.getActiveVersion());
            
            List<CmsPageTemplateVersion> vers = pageTmp.getAllVersions();
            if (UtilValidate.isNotEmpty(vers)) {
                
                knownInfo.setLastVersion(vers.get(0));
                knownInfo.setFirstVersion(vers.get(vers.size() - 1));

                for(CmsPageTemplateVersion ver : vers) {
                    Map<String, Object> version = FastMap.newInstance();
                    ver.putIntoMap(version, false, knownInfo);
                    
                    // Must do this one manually; not done by CmsPageTemplateVersion (because not relevant to it)
                    version.put("isCurrent", ver.isSameVersion(currVer));
                    
                    if (UtilValidate.isNotEmpty(useMarkedAsCurrent) && currVer == null) {
                        // Override current 
                        if (isVersionBoolSet(version, useMarkedAsCurrent)) {
                            version.put("isCurrent", true);
                        }
                    }
                    
                    checkVersionBoolPropertyAndRecord(version, "isCurrent", pageTmpVersions, "current");
                    checkVersionBoolPropertyAndRecord(version, "isActive", pageTmpVersions, "active");
                    checkVersionBoolPropertyAndRecord(version, "isLast", pageTmpVersions, "last");
                    checkVersionBoolPropertyAndRecord(version, "isFirst", pageTmpVersions, "first");

                    allVersions.add(version);
                }
            }
            
            pageTmpVersions.put("all", allVersions);
            pageTmpAndVersions.put("pageTmpVersions", pageTmpVersions);
            result.put("pageTmpAndVersions", pageTmpAndVersions); 
        } 
        catch (Exception e) {
            Debug.logError(e, module);
            result = ServiceUtil.returnFailure(e.getMessage());
        }
        return result;
    }
    
    private static boolean isVersionBoolSet(Map<String, Object> version, String propName) {
        Boolean val = (Boolean) version.get(propName);
        return (val != null && val == true);
    }
    
    private static void checkVersionBoolPropertyAndRecord(Map<String, Object> version, String propName, 
            Map<String, Object> resultMap, String resultKey) {
        if (isVersionBoolSet(version, propName)) {
            resultMap.put(resultKey, version);
        }
    }
    
    /**
     * Creates a new page template version in the repository.
     * 
     * @param dctx
     *            The DispatchContext that this service is operating in
     * @param context
     *            Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> addPageTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            String partyId = getUserPartyId(context);
            String pageTemplateId = (String) context.get("pageTemplateId");
            String versionComment = (String) context.get("versionComment");
            String templateBody = (String) context.get("templateBody");
            
            CmsPageTemplate pageTmp = new CmsPageTemplate(pageTemplateId);
            
            Map<String, Object> versionFields = FastMap.newInstance();
            versionFields.put("pageTemplateId", pageTemplateId);
            versionFields.put("createdBy", partyId);
            versionFields.put("versionComment", versionComment);
            versionFields.put("templateBody", templateBody);
            
            CmsPageTemplateVersion tmpVer = pageTmp.createNewVersion(versionFields);
            
            tmpVer.store();
            
            result.put("versionId", tmpVer.getId());
        } 
        catch (Exception e) {
            Debug.logError(e, module);
            result = ServiceUtil.returnFailure(e.getMessage());
        }
        return result;
    }
    
    /**
     * Sets a page template version as live version. The live version is the content that
     * will be displayed to regular page visitors.
     * 
     * @param dctx
     *            The DispatchContext that this service is operating in
     * @param context
     *            Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> activatePageTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            Delegator delegator = dctx.getDelegator();
            
            String pageTemplateId = (String) context.get("pageTemplateId");
            String versionId = (String) context.get("versionId");
            
            CmsPageTemplateVersion version = CmsPageTemplateVersion.findSpecificTemplateVersion(delegator, 
                    pageTemplateId, versionId);
            if (version != null) {
                version.setAsActiveVersion();
                version.store();
                
                result.put("pageTemplateId", pageTemplateId);
                result.put("versionId", version.getVersionId());
            }
            else {
                result = ServiceUtil.returnFailure("Page template version '" + versionId + "' not found"); // TODO: Localize
            }
        } 
        catch (Exception e) {
            Debug.logError(e, module);
            result = ServiceUtil.returnFailure(e.getMessage());
        }
        return result;
    }
    
    /**
     * Updates basic page template info.
     * 
     * @param dctx
     *            The DispatchContext that this service is operating in
     * @param context
     *            Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> updatePageTemplateInfo(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            Delegator delegator = dctx.getDelegator();
            
            String pageTemplateId = (String) context.get("pageTemplateId");
            
            CmsPageTemplate pageTmp = new CmsPageTemplate(pageTemplateId);
            pageTmp.updateTemplateFields(context);
            pageTmp.store();
            
        } 
        catch (Exception e) {
            Debug.logError(e, module);
            result = ServiceUtil.returnFailure(e.getMessage());
        }
        return result;
    }
    
    // Helper methods
    
    private static GenericValue getUserLogin(Map<String, ?> context) {
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        if (UtilValidate.isNotEmpty(request)) {
            return (GenericValue) request.getSession().getAttribute("userLogin");
        }
        else {
            return (GenericValue) context.get("userLogin");
        }
    }
    
    private static String getUserId(Map<String, ?> context) {
        // Modified version
        GenericValue userLogin = getUserLogin(context);
        if (userLogin != null) {
            return userLogin.getString("userLoginId");
        }
        return null;
    }  
    
    private static String getUserPartyId(Map<String, ?> context) {
     // Modified version
        GenericValue userLogin = getUserLogin(context);
        if (userLogin != null) {
            return userLogin.getString("partyId");
        }
        return null;
    }

    private static List<Map<String, ?>> transformVariables(Map<String, Object> root) {
        List<Map<String, ?>>  children = FastList.<Map<String, ?>>newInstance();
        Set<Entry<String, Object>> entries = root.entrySet();
        for (Entry<String, ?> child:  entries) {
            children.add(transformVariables(child));
        }
        return children;
    }
    
    private static Map<String, ?> transformVariables(Entry<String, ?> root) {
        Map<String, Object> wrapperMap = FastMap.<String, Object>newInstance();
        wrapperMap.put("name", root.getKey());        
        Object value = root.getValue();
        
        if (value instanceof String) {
            wrapperMap.put("value", value);
        } else if (value instanceof Map) {
            List<Map<String, ?>>  children = FastList.<Map<String, ?>>newInstance();
            Set<Entry<String, Object>> entries = ((Map<String, Object>)value).entrySet();
            for (Entry<String, Object> child:  entries) {
                children.add(transformVariables(child));
            }
            wrapperMap.put("children", children);            
        }
        
        
        return wrapperMap;
    }
    
    
}
