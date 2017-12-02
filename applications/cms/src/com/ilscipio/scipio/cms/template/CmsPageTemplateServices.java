package com.ilscipio.scipio.cms.template;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericDelegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.cms.CmsServiceUtil;
import com.ilscipio.scipio.cms.ServiceErrorFormatter;
import com.ilscipio.scipio.cms.ServiceErrorFormatter.FormattedError;
import com.ilscipio.scipio.cms.template.CmsPageTemplate.CmsPageTemplateAssetAssoc;
import com.ilscipio.scipio.cms.template.CmsPageTemplate.CmsPageTemplateScriptAssoc;
import com.ilscipio.scipio.cms.template.CmsTemplate.TemplateBodySource;

/**
 * CmsPageTemplateServices - CMS Page Template and Page Template Version Services
 *
 */
public abstract class CmsPageTemplateServices {
    
    public static final String module = CmsPageTemplateServices.class.getName();
    private static final ServiceErrorFormatter errorFmt = 
            CmsServiceUtil.getErrorFormatter().specialize().setDefaultLogMsgGeneral("Page Template Error").build();

    protected CmsPageTemplateServices() {
    }
    
    /**
     * Creates a new template in the repository.
     */
    public static Map<String, Object> createPageTemplate(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        try {
            GenericValue userLogin = CmsServiceUtil.getUserLoginOrSystem(dctx, context);

            // Create empty template
            Map<String, Object> fields = ServiceUtil.setServiceFields(dispatcher, "cmsCreatePageTemplate", 
                    UtilGenerics.<String, Object> checkMap(context), userLogin, null, null);
            CmsPageTemplate pageTmp = new CmsPageTemplate(delegator, fields);
            pageTmp.store();

            result.put("pageTemplateId", pageTmp.getId());
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    public static Map<String, Object> copyPageTemplate(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        Map<String, Object> copyArgs = new HashMap<>();
        copyArgs.put("copyVersionId", context.get("srcVersionId"));
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (userLogin != null) {
            copyArgs.put("copyCreatorId", userLogin.get("partyId"));
        }
        try {
            String srcPageTemplateId = (String) context.get("srcPageTemplateId");
            CmsPageTemplate srcPageTemplate = CmsPageTemplate.getWorker().findByIdAlways(delegator, srcPageTemplateId, false);
            CmsPageTemplate pageTemplate = srcPageTemplate.copy(copyArgs);
            pageTemplate.update(context, false); // update templateName, description IF not empty
            // NOTE: store() now updates the version automatically using pageTemplate.lastVersion
            pageTemplate.store();
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("pageTemplateId", pageTemplate.getId());
            return result;
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
    }
    
    /**
     * Updates basic page template info.
     */
    public static Map<String, Object> updatePageTemplateInfo(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        
        try {
            String pageTemplateId = (String) context.get("pageTemplateId");
            CmsPageTemplate pageTmp = CmsPageTemplate.getWorker().findByIdAlways(delegator, pageTemplateId, false);
            pageTmp.update(context);
            pageTmp.store();
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    /**
     * Gets a page template.
     */
    public static Map<String, Object> getPageTemplate(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String pageTemplateId = (String) context.get("pageTemplateId");
            
            // TODO: Replace with a call that doesn't involve throwing exceptions if not found
            Map<String, Object> pageTemplate = CmsPageTemplate.getTemplateAsMap(delegator, pageTemplateId);
            
            if (UtilValidate.isNotEmpty(pageTemplate)) {
                result.put("pageTemplate", pageTemplate);
            } else {
                result = ServiceUtil.returnFailure("Page template '" + pageTemplateId + "' not found"); // TODO: Localize
            }
        } catch (Exception e) {
            // Note: Why is our 'convention' to return failure here? Must be because
            // we throw exceptions from constructors that look DB values up by ID when they find nothing.
            // TODO: Change this behavior to use factory methods that return null so can differentiate
            // exceptions/errors from each other and return error when it's called for here instead.
            // -
            // DEV NOTE: 2016: returning failure is needed to prevent transaction aborts during
            // screen renders (a.k.a render crash) when this is called from scripts.
            // only the getXxx reader services should return failure; update services must return error.
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }
        return result;
    }
    
    /**
     * Gets a page template version.
     */
    public static Map<String, Object> getPageTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            Delegator delegator = dctx.getDelegator();
            String pageTemplateId = (String) context.get("pageTemplateId");
            String versionId = (String) context.get("versionId");
            boolean minimalInfoOnly = EntityUtil.toBoolean((String) context.get("minimalInfoOnly"), false);
            
            CmsPageTemplateVersion tmpVer = CmsPageTemplate.getVerComTemplateWorker().findSpecificTemplateVersion(delegator, pageTemplateId, versionId);
            
            if (tmpVer != null) {
                Map<String, Object> version = new HashMap<>();
                tmpVer.putIntoMap(version, minimalInfoOnly, null);
                result.put("version", version);
            } else {
                result = ServiceUtil.returnFailure("Could not find page template version '" + versionId + "' for page template '" +
                        pageTemplateId + "'"); // TODO: Localize
            }
        } catch(Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }
        return result;
    }
    
    /**
     * Gets all page template versions. Currently returns in the order in which created.
     */
    public static Map<String, Object> getPageTemplateVersions(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            List<Map<String, Object>> allVersionsList = new ArrayList<>();
            
            String pageTemplateId = (String) context.get("pageTemplateId");
            boolean minimalInfoOnly = EntityUtil.toBoolean((String) context.get("minimalInfoOnly"), false);
            
            CmsPageTemplate pageTmp = CmsPageTemplate.getWorker().findByIdAlways(delegator, pageTemplateId, false);
            
            List<CmsPageTemplateVersion> versions = pageTmp.getAllVersions();
            
            if (UtilValidate.isNotEmpty(versions)) {
                CmsPageTemplateVersion.ExtendedInfo knownInfo = new CmsPageTemplateVersion.ExtendedInfo();
                
                // Optimization for loop (TODO: Move this to an abstracted iterator or other later)
                knownInfo.setActiveVersion(pageTmp.getActiveVersion());
                knownInfo.setLastVersion(versions.get(0));
                knownInfo.setFirstVersion(versions.get(versions.size() - 1));
                
                for(CmsPageTemplateVersion version : versions) {
                    Map<String, Object> versionMap = new HashMap<>();
                    version.putIntoMap(versionMap, minimalInfoOnly, knownInfo);
                    allVersionsList.add(versionMap);
                }
            }
            
            result.put("versions", allVersionsList);   
        } catch(Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }
        return result;
    }
    
    /**
     * Returns a map of a page template and all its versions, as well as extra helpful fields.
     */
    public static Map<String, Object> getPageTemplateAndVersions(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        try {
            String pageTemplateId = (String) context.get("pageTemplateId");
            
            // The current template is almost a display-related concern, but is optional
            String currentVersionId = (String) context.get("currentVersionId");
            String useMarkedAsCurrent = (String) context.get("useMarkedAsCurrent");
            
            Map<String, Object> pageTmpAndVersions = new HashMap<>();
            
            CmsPageTemplate pageTmp = CmsPageTemplate.getWorker().findByIdAlways(delegator, pageTemplateId, false);
            CmsPageTemplateVersion version = null;
            CmsPageTemplate currPageTmp = pageTmp;
            if (context.get("versionId") != null) {
                version = pageTmp.getVersion((String) context.get("versionId"));
                currPageTmp = version.getPageTemplate();
            } else {
                version = currPageTmp.getActiveVersion();
            }
            
            // FIXME: shouldn't be using entity values like this
            pageTmpAndVersions.putAll(currPageTmp.getEntity());
            // FIXME: shouldn't have to do this
            pageTmpAndVersions.put("description", currPageTmp.getDescription(locale));
            
            TemplateBodySource tmplBodySrc = (version != null) ? version.getTemplateBodySource() : TemplateBodySource.getUndefined();
            tmplBodySrc.toFields(pageTmpAndVersions);
                
            // note the version we just looked up
            pageTmpAndVersions.put("versionId", (version != null) ? version.getId() : null);
            
            // Add asset assocs
            pageTmpAndVersions.put("assetTemplates", pageTmp.getSortedAssetTemplates());
            
            // Add attributes assocs
            pageTmpAndVersions.put("attrTemplates", pageTmp.getLocallySortedAttributeTemplates());
            
            // add script assocs
            pageTmpAndVersions.put("scriptTemplates", pageTmp.getSortedScriptTemplates());
            
            CmsPageTemplateVersion currVer = null;

            if (UtilValidate.isNotEmpty(currentVersionId)) {
                currVer = CmsPageTemplate.getVerComTemplateWorker().findSpecificTemplateVersion(delegator, pageTmp, currentVersionId);
                
                if (currVer == null) {
                    final String errMsg = "Could not find specified current page template version ('" + 
                    currentVersionId + "') for specified page template ('" + pageTemplateId + "'); not ";
                    Debug.logWarning(errMsg, module); // (Don't localize in this call)        
                    // Don't fail - permit this since the 'current' version marking is provided only as convenience.
                    //result = ServiceUtil.returnFailure(errMsg); // TODO: Localize
                }
            }
            
            Map<String, Object> pageTmpVersions = new HashMap<>();
            List<Map<String, Object>> allVersions = new ArrayList<>();
            
            CmsPageTemplateVersion.ExtendedInfo knownInfo = new CmsPageTemplateVersion.ExtendedInfo();
            
            knownInfo.setActiveVersion(pageTmp.getActiveVersion());
            
            List<CmsPageTemplateVersion> vers = pageTmp.getAllVersions();
            if (UtilValidate.isNotEmpty(vers)) {
                knownInfo.setLastVersion(vers.get(0));
                knownInfo.setFirstVersion(vers.get(vers.size() - 1));

                for(CmsPageTemplateVersion ver : vers) {
                    Map<String, Object> versionMap = new HashMap<>();
                    ver.putIntoMap(versionMap, false, knownInfo);
                    
                    // Must do this one manually; not done by CmsPageTemplateVersion (because not relevant to it)
                    versionMap.put("isCurrent", ver.isSameVersion(currVer));
                    
                    if (UtilValidate.isNotEmpty(useMarkedAsCurrent) && currVer == null) {
                        // Override current 
                        if (isVersionBoolSet(versionMap, useMarkedAsCurrent)) {
                            versionMap.put("isCurrent", true);
                        }
                    }
                    
                    checkVersionBoolPropertyAndRecord(versionMap, "isCurrent", pageTmpVersions, "current");
                    checkVersionBoolPropertyAndRecord(versionMap, "isActive", pageTmpVersions, "active");
                    checkVersionBoolPropertyAndRecord(versionMap, "isLast", pageTmpVersions, "last");
                    checkVersionBoolPropertyAndRecord(versionMap, "isFirst", pageTmpVersions, "first");

                    allVersions.add(versionMap);
                }
            }
            
            pageTmpVersions.put("all", allVersions);
            pageTmpAndVersions.put("pageTmpVersions", pageTmpVersions);
            result.put("pageTmpAndVersions", pageTmpAndVersions); 
        } catch(Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
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
     * Gets all available page templates. Currently returns in the order in which created.
     */
    public static Map<String, Object> getAvailablePageTemplates(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            Delegator delegator = dctx.getDelegator();
            List<GenericValue> allTemplatesList = new ArrayList<>();
            if ((String) context.get("webSiteId") != null ){
                allTemplatesList = delegator.findByAnd("CmsPageTemplate", UtilMisc.toMap("webSiteId",context.get("webSiteId")),null, false);
            } else {
                allTemplatesList = delegator.findAll("CmsPageTemplate", false);
            }
            result.put("pageTemplates", allTemplatesList);   
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }
        return result;
    }
    
    /**
     * Creates a new page template version in the repository.
     */
    public static Map<String, Object> addPageTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String partyId = CmsServiceUtil.getUserPartyId(context);
            String pageTemplateId = (String) context.get("pageTemplateId");
            String versionComment = (String) context.get("versionComment");
            String templateBody = (String) context.get("templateBody");
            String templateLocation = (String) context.get("templateLocation");
            String templateSource = (String) context.get("templateSource");
            
            CmsPageTemplate pageTmp = CmsPageTemplate.getWorker().findByIdAlways(delegator, pageTemplateId, false);
            
            Map<String, Object> versionFields = new HashMap<>();
            versionFields.put("pageTemplateId", pageTemplateId);
            versionFields.put("createdBy", partyId);
            versionFields.put("versionComment", versionComment);
            versionFields.put("templateBody", templateBody);
            versionFields.put("templateLocation", templateLocation);
            versionFields.put("templateSource", templateSource);
            
            CmsPageTemplateVersion tmpVer = pageTmp.createNewVersion(versionFields);
            
            tmpVer.store();
            
            result.put("versionId", tmpVer.getId());
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    /**
     * Sets a page template version as live version. The live version is the content that
     * will be displayed to regular page visitors.
     */
    public static Map<String, Object> activatePageTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            Delegator delegator = dctx.getDelegator();
            
            String pageTemplateId = (String) context.get("pageTemplateId");
            String versionId = (String) context.get("versionId");
            
            CmsPageTemplateVersion version = CmsPageTemplate.getVerComTemplateWorker().findSpecificTemplateVersion(delegator, 
                    pageTemplateId, versionId);
            if (version != null) {
                version.setAsActiveVersion();
                version.store();
                
                result.put("pageTemplateId", pageTemplateId);
                result.put("versionId", version.getVersionId());
            } else {
                result = ServiceUtil.returnError("Page template version '" + versionId + "' not found"); // TODO: Localize
            }
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }

    /**
     * Service to update and add a new script file to a page template.
     */
    public static Map<String, Object> updatePageTemplateScript(DispatchContext dctx, Map<String, ? extends Object> context) {
        GenericDelegator delegator = (GenericDelegator) dctx.getDelegator();
        Map<String, Object> result = ServiceUtil.returnSuccess();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        try {
            CmsPageTemplateScriptAssoc.getWorker().createUpdateScriptTemplateAndAssoc(delegator, context, userLogin);
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error while updating Script Template Assoc", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    public static Map<String, Object> deletePageTemplate(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String pageTemplateId = (String) context.get("pageTemplateId");
            CmsPageTemplate template = CmsPageTemplate.getWorker().findByIdAlways(delegator, pageTemplateId, false);
            template.remove();
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    /**
     * Adds an available asset to a page template.
     */
    public static Map<String, Object> createUpdateAssetAssoc(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            // not needed
            //String pageTemplateId = (String) context.get("pageTemplateId");
            //CmsPageTemplate pageTmp = CmsPageTemplate.getWorker().findByIdAlways(delegator, pageTemplateId, false);
            CmsPageTemplateAssetAssoc.getWorker().createUpdatePageTemplateAssetAssoc(delegator, context);
        } catch(Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    /**
     * Creates or updates an attribute for a given template (page or asset).
     */
    public static Map<String, Object> createUpdateAttribute(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String attributeTemplateId = (String) context.get("attributeTemplateId");

            CmsAttributeTemplate attr = null;

            if (UtilValidate.isNotEmpty(attributeTemplateId)) {
                attr = CmsAttributeTemplate.getWorker().findByIdAlways(delegator, attributeTemplateId, false);
                attr.update(context, true);
            } else {
                attr = new CmsAttributeTemplate(delegator, context);
            }

            attr.store();
            result.put("attributeTemplateId", attr.getId());
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
}
