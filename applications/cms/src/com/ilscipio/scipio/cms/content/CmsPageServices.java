package com.ilscipio.scipio.cms.content;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericDelegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.cms.CmsServiceUtil;
import com.ilscipio.scipio.cms.content.CmsPage.CmsPageScriptAssoc;
import com.ilscipio.scipio.cms.content.CmsPage.UserRole;
import com.ilscipio.scipio.cms.template.CmsPageTemplate;
import com.ilscipio.scipio.cms.template.RendererType;

public abstract class CmsPageServices {

    public static final String module = CmsPageServices.class.getName();

    protected CmsPageServices() {
    }
    
    /**
     * Returns the page content, template and information of a page.
     * 
     * @param dctx
     * @param context
     * @return
     */
    public static Map<String, Object> getPage(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");

        Map<String, Object> result = ServiceUtil.returnSuccess();
        String pageId = (String) context.get("pageId");
        String webSiteId = (String) context.get("webSiteId");
        String primaryPath = (String) context.get("primaryPath");
        
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        HttpServletResponse response = (HttpServletResponse) context.get("response");
        // NOTE: request.getServletContext() new in servlet API 3.0
        ServletContext servletContext = request != null ? request.getServletContext() : null;
        
        boolean verifyWebSite = Boolean.TRUE.equals(context.get("verifyWebSite"));
        boolean useStaticWebSite = Boolean.TRUE.equals(context.get("useStaticWebSite"));

        CmsPage page = null;
        try {
            if (UtilValidate.isNotEmpty(pageId)) {
                page = CmsPage.getWorker().findById(delegator, pageId, false);
                if (page == null) {
                    return ServiceUtil.returnFailure("Page not found for pageId '" + pageId + "'");
                }
            } else if (UtilValidate.isNotEmpty(primaryPath) && UtilValidate.isNotEmpty(webSiteId)) {
                page = CmsPage.getWorker().findByPath(delegator, primaryPath, webSiteId, false);
                if (page == null) {
                    return ServiceUtil.returnFailure("Page not found for primaryPath '" + primaryPath + "' and webSiteId '" + webSiteId + "'");
                }
            } else {
                return ServiceUtil.returnFailure("Page not found. PageId or a combination of primaryPath and webSiteId are required.");
            }
    
            result.put("pageId", page.getId());
            CmsPageTemplate template = page.getTemplate();
            Set<String> candidateWebSiteIds = page.getCandidateWebSiteIds();

            // 2016: this isn't used in rendering, and the backend app doesn't have a webSiteId, 
            // so it doesn't make sense here. instead if webSiteId is null we'll get the best
            // candidate, for the page context variables stuff...
//                if (webSiteId == null && request != null) {
//                    webSiteId = WebSiteWorker.getWebSiteId(request);
//                }
            if (UtilValidate.isEmpty(webSiteId)) {
                if (useStaticWebSite) {
                    // TODO: REVIEW: for now, try the one stored in CmsPage first.
                    // NOTE: this should already be in candidateWebSiteIds as first one or so,
                    // but until code clarifies just do this for now.
                    webSiteId = page.getWebSiteId();
                    if (UtilValidate.isEmpty(webSiteId) && candidateWebSiteIds.size() > 0) {
                        webSiteId = candidateWebSiteIds.iterator().next();
                    }
                    if (UtilValidate.isEmpty(webSiteId)) {
                        return ServiceUtil.returnFailure("Could not fetch data for pageId '" + pageId + "' "
                                + "because no webSiteId passed and no static placeholder found associated to page to use");
                    }
                    Debug.logWarning("CMS: During cmsGetPage, no webSiteId was passed; using stored "
                            + "value instead (" + webSiteId + ") (for context emulation)", module);
                } else {
                    return ServiceUtil.returnFailure("Could not fetch data for pageId '" + pageId + "' "
                            + "because no webSiteId passed");
                }
            } else {
                // 2016: make sure the webSiteId matches something associated to the page
                if (verifyWebSite && !(webSiteId.equals(page.getWebSiteId()) || candidateWebSiteIds.contains(webSiteId))) {
                    return ServiceUtil.returnFailure("Could not fetch data for pageId '" + pageId + "'; "
                            + "specified webSiteId '" + webSiteId + "' has no mappings or associations to page");
                }
            }
            
            Map<String, ?> pageDesc = page.getDescriptor(webSiteId, locale);
            result.put("meta", pageDesc);

            String userId = CmsServiceUtil.getUserId(context);
            UserRole userRole = page.getUserAuthorization(userId, delegator, dispatcher);
            result.put("permission", userRole.toString());

            List<GenericValue> visitorsList = CmsPage.findUsersByPageRole(delegator, page, UserRole.CMS_VISITOR, false);
            String[] visitors = new String[visitorsList.size() + 1];
            visitors[0] = userId;
            for (int i = 1; i <= visitorsList.size(); i++) {
                visitors[i] = visitorsList.get(i - 1).getString("userId");
            }
            result.put("visitors", visitors);

            Map<String, Object> tempDesc = template.getDescriptor(locale);
            result.put("template", tempDesc);

            CmsPageVersion version;
            if (context.get("versionId") != null) {
                version = page.getVersion((String) context.get("versionId"));
            } else {
                version = page.getLastVersionOrNewVersion();
            }

            CmsPageContent pageVars = new CmsPageContent(page);
            
            if (request.getAttribute("delegator") == null) {
                Debug.logError("CMS: Delegator not found in request during getPage setup", module);
            }
            
            page.getTemplate().getRenderer().populateBasicContextVariables(pageVars, 
                    new CmsPageContext(request, response, servletContext, webSiteId, false, RendererType.CMS_EDITOR));
            result.put("variables", getPageVariablesDescriptor(pageVars));

            result.put("versionId", version.getId());
            result.put("content", version.getContent());
            result.put("pageModel", page);
            result.put("webSiteId", webSiteId);
            result.put("scriptTemplates", page.getSortedScriptTemplates());
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnFailure("Error while querying for page (pageId: " + pageId + ")");
        }

        return result;
    }


    public static List<Map<String, ?>> getPageVariablesDescriptor(Map<String, Object> root) {
        List<Map<String, ?>> children = new ArrayList<>();
        Set<Entry<String, Object>> entries = root.entrySet();
        for (Entry<String, ?> child : entries) {
            children.add(getPageVariablesDescriptor(child));
        }
        return children;
    }

    private static Map<String, ?> getPageVariablesDescriptor(Entry<String, ?> root) {
        Map<String, Object> wrapperMap = new HashMap<>();
        wrapperMap.put("name", root.getKey());
        Object value = root.getValue();

        if (value instanceof String) {
            wrapperMap.put("value", value);
        } else if (value instanceof Map) {
            List<Map<String, ?>> children = new ArrayList<>();
            Set<Entry<String, Object>> entries = (UtilGenerics.<String, Object> checkMap(value)).entrySet();
            for (Entry<String, Object> child : entries) {
                children.add(getPageVariablesDescriptor(child));
            }
            wrapperMap.put("children", children);
        }

        return wrapperMap;
    }
    
    /**
     * Creates a new page version in the repository. The content is not live
     * until it is approved.
     */
    public static Map<String, Object> addPageVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            CmsPage page = CmsPage.getWorker().findByIdAlways(delegator, (String) context.get("pageId"), false);
            CmsPageVersion version = page.addVersion((String) context.get("content"));
            String partyId = CmsServiceUtil.getUserPartyId(context);
            if (partyId != null) {
                version.setCreatedBy(partyId);
            }
            if (context.get("comment") != null) {
                version.setVersionComment((String) context.get("comment"));
            }
            version.store();
            result.put("versionId", version.getId());
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }

    /**
     * Creates a new page in the repository. Adds an empty version to it
     * afterwards.
     */
    public static Map<String, Object> createPage(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = null;
        
        String primaryPath = (String) context.get("primaryPath");
        if (primaryPath == null) {
            primaryPath = (String) context.get("path"); // DEPRECATED: TODO: REMOVE
        }
        
        String primaryTargetPath = (String) context.get("primaryTargetPath");
        
        try {
            userLogin = (GenericValue) context.get("userLogin");
            if (userLogin == null) {
                userLogin = delegator.findOne("UserLogin", true, UtilMisc.toMap("userLoginId", "system"));
            }

            // Create empty page
            Map<String, Object> fields = ServiceUtil.setServiceFields(dispatcher, "cmsCreatePage", 
                    UtilGenerics.<String, Object> checkMap(context), userLogin, null, null);
            fields.put("primaryPath", primaryPath);
            fields.put("primaryTargetPath", primaryTargetPath);
            
            // 2017-11-29: new pages will have their primary process mapping set active false,
            // then first publish operation will then toggle it to true.
            // see activePageVersion service below
            Object initialActive = fields.get("active");
            if (CmsPage.newPagePrimaryProcessMappingActive != null && (initialActive == null || (initialActive instanceof String && ((String) initialActive).isEmpty()))) {
                fields.put("active", CmsPage.newPagePrimaryProcessMappingActive);
            }
            
            CmsPage page = CmsPage.createAndStoreWithPrimaryProcessMapping(delegator, fields);

            // Add Base user authorization
            // page.setUserAuthorization(userLogin.getString("userLoginId"),
            // "CMS_ADMIN");

            // Store page
            CmsPageVersion version = page.addVersion("{}");
            version.store();
            page.store();

            result.put("pageId", page.getId());
            result.put("primaryPath", primaryPath);
            result.put("path", primaryPath); // DEPRECATED: TODO: REMOVE
            result.put("webSiteId", context.get("webSiteId"));
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }

    public static Map<String, Object> copyPage(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        Map<String, Object> copyArgs = new HashMap<>();
        copyArgs.put("webSiteId", context.get("webSiteId"));
        copyArgs.put("primaryPath", context.get("primaryPath"));
        copyArgs.put("primaryPathFromContextRoot", context.get("primaryPathFromContextRoot"));
        copyArgs.put("copyVersionId", context.get("srcVersionId"));
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (userLogin != null) {
            copyArgs.put("copyCreatorId", userLogin.get("partyId"));
        }
        try {
            String srcPageId = (String) context.get("srcPageId");
            CmsPage srcPage = CmsPage.getWorker().findByIdAlways(delegator, srcPageId, false);
            CmsPage page = srcPage.copy(copyArgs);
            page.update(context, false); // update pageName, description IF not empty
            page.store();
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("pageId", page.getId());
            return result;
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }
    
    public static Map<String, Object> updatePageInfo(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            Map<String, Object> fields = UtilMisc.makeMapWritable(context);
            
            // 2016: FIXME?: see com.ilscipio.scipio.cms.content.CmsPage.setPrimaryProcessMappingFields(Map<String, ?>)
            fields.put("primaryWebSiteIdOverride", "Y");
            
            String pageId = (String) context.get("pageId");
            CmsPage page = CmsPage.getWorker().findByIdAlways(delegator, pageId, false);
            page.update(fields);
            page.store();
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }
    
    /**
     * Sets a page version as live version. The live version is the content that
     * will be displayed to regular page visitors.
     */
    public static Map<String, Object> activatePageVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String pageId = (String) context.get("pageId");
            String versionId = (String) context.get("versionId");
            CmsPage page = CmsPage.getWorker().findByIdAlways(delegator, pageId, false);
            page.setActiveVersion(versionId);
            
            // 2017-11-29: new pages will have their primary process mapping set active false,
            // then first publish operation will then toggle it to true.
            // see createPage service above
            Map<String, Object> fields = new HashMap<>();
            fields.put("active", "Y");
            Collection<String> webSiteIds = page.getPrimaryProcessMappingsWebSiteIds();
            if (webSiteIds!= null && webSiteIds.size() > 0) {
                for(String webSiteId : webSiteIds) {
                    fields.put("webSiteId", webSiteId);
                    page.setPrimaryProcessMappingFields(fields, true);
                }
            } else {
                Debug.logWarning("Cms: activatePageVersion: Page '" + pageId 
                        + "' appears to have no primary process mapping webSiteIds"
                        + " - cannot activate primary process mapping - activation may be incomplete", module);
            }
            
            page.store();
            result.put("pageId", pageId);
            result.put("versionId", versionId);
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }

    public static Map<String, Object> updatePageScript(DispatchContext dctx, Map<String, ? extends Object> context) {
        GenericDelegator delegator = (GenericDelegator) dctx.getDelegator();
        Map<String, Object> result = ServiceUtil.returnSuccess();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        
        try {
            CmsPageScriptAssoc.getWorker().createUpdateScriptTemplateAndAssoc(delegator, context, userLogin);
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError("Error while updating Script Template Assoc: " + e.getMessage()); // TODO?: Localize
        }
        return result;
    }
    
    public static Map<String, Object> deletePage(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String pageId = (String) context.get("pageId");
            CmsPage page = CmsPage.getWorker().findByIdAlways(delegator, pageId, false);
            page.remove();
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }
    
    /**
     * Returns all pages for the current user.
     */
    public static Map<String, Object> getPages(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        Map<String, Object> result = ServiceUtil.returnSuccess();

        String webSiteId = (String) context.get("webSiteId");
        if (UtilValidate.isEmpty(webSiteId)) {
            webSiteId = null;
        }
        boolean shortDesc = !Boolean.FALSE.equals(context.get("short"));
        boolean editable = Boolean.TRUE.equals(context.get("editable"));
        
        // NOTE: if webSiteId set we'll manually filter in loop for now... (TODO: optimize later) 
        List<CmsPage> pages = CmsPage.getWorker().findAll(delegator, false); // findAllWithWebsite(delegator, false)
        
        List<Map<String, ?>> pagesList = new ArrayList<>();

        for (CmsPage page : pages) {
            String pageWebSiteId;
            if (webSiteId != null) {
                // CHECK to make sure this page is linked against the specified webSiteId.
                // TODO: turn this into faster query later.
                if (!page.isLinkedToWebSiteId(webSiteId)) {
                    continue;
                }

                pageWebSiteId = webSiteId;
            } else {
                // For organization purposes in this method (ONLY), prefer the CmsPage.webSiteId; if empty get from primary mapping
                pageWebSiteId = page.getWebSiteId();
                if (UtilValidate.isEmpty(pageWebSiteId)) {
                    pageWebSiteId = page.getPrimaryWebSiteId();
                }
            }

            Map<String, Object> pageMap;
            if (shortDesc) {
                pageMap = page.getShortDescriptor(pageWebSiteId, locale);
            } else {
                pageMap = page.getDescriptor(pageWebSiteId, locale);
            }

            if (editable) {
                String userId = CmsServiceUtil.getUserId(context);
                UserRole userRole = page.getUserAuthorization(userId, delegator, dispatcher);
                if (userRole != UserRole.CMS_VISITOR) {
                    pageMap.put("permission", userRole.toString());
                    pagesList.add(pageMap);
                }

            } else {
                pagesList.add(pageMap);
            }
        }
        result.put("pages", pagesList);
        return result;
    }
}
