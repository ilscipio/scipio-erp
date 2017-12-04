package com.ilscipio.scipio.cms.template;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericDelegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.cms.CmsServiceUtil;
import com.ilscipio.scipio.cms.ServiceErrorFormatter;
import com.ilscipio.scipio.cms.ServiceErrorFormatter.FormattedError;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.data.CmsDataUtil;
import com.ilscipio.scipio.cms.template.CmsAssetTemplate.CmsAssetTemplateScriptAssoc;

/**
 * CmsAssetTemplateServices - CMS Asset Template and Asset Template Version Services
 *
 */
public abstract class CmsAssetTemplateServices {
    
    public static final String module = CmsAssetTemplateServices.class.getName();
    private static final ServiceErrorFormatter errorFmt = 
            CmsServiceUtil.getErrorFormatter().specialize().setDefaultLogMsgGeneral("Asset Template Error").build();

    protected CmsAssetTemplateServices() {
    }
    
    /**
     * Creates or updates an Asset.
     */
    public static Map<String, Object> createUpdateAsset(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        try {
            GenericValue userLogin = CmsServiceUtil.getUserLoginOrSystem(dctx, context);
            //Debug.logInfo("createUpdateAsset triggered",module);
            String assetTemplateId = (String) context.get("assetTemplateId");
            
            // Create empty template
            Map<String, Object> fields = ServiceUtil.setServiceFields(dispatcher, "cmsCreateUpdateAsset", 
                    UtilGenerics.<String, Object> checkMap(context), userLogin, null, null);
            
            CmsAssetTemplate assetTmp = null;
            if (UtilValidate.isNotEmpty(assetTemplateId)) {
                assetTmp = CmsAssetTemplate.getWorker().findByIdAlways(delegator, assetTemplateId, false);
                fields.put("createdBy", userLogin.getString("userLoginId"));
                assetTmp.update(fields);
            } else {
                fields.put("lastUpdatedBy", (String) userLogin.get("userLoginId"));
                assetTmp = new CmsAssetTemplate(delegator, fields);
            }
            
            assetTmp.store();
            result.put("assetTemplateId", assetTmp.getId());
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    public static Map<String, Object> copyAsset(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        Map<String, Object> copyArgs = new HashMap<>();
        copyArgs.put("copyVersionId", context.get("srcVersionId"));
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (userLogin != null) {
            copyArgs.put("copyCreatorId", userLogin.get("partyId"));
        }
        try {
            String srcAssetTemplateId = (String) context.get("srcAssetTemplateId");
            CmsAssetTemplate srcAssetTemplate = CmsAssetTemplate.getWorker().findByIdAlways(delegator, srcAssetTemplateId, false);
            CmsAssetTemplate assetTemplate = srcAssetTemplate.copy(copyArgs);
            assetTemplate.update(context, false); // update templateName, description IF not empty
            // NOTE: store() now updates the version automatically using assetTemplate.lastVersion
            assetTemplate.store();
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("assetTemplateId", assetTemplate.getId());
            return result;
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
    }
    
    public static Map<String, Object> updateAssetTemplateInfo(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String assetTemplateId = (String) context.get("assetTemplateId");
            CmsAssetTemplate assetTmp = CmsAssetTemplate.getWorker().findByIdAlways(delegator, assetTemplateId, false);
            assetTmp.update(context);
            assetTmp.store();
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }

    
    /**
     * NOTE: 2016: TODO?: I moved this here because it's where it belongs, but there
     * is no service def for this and the description that was here was wrong.
     * @deprecated this looks like the old versioning code; will be supplanted by CmsAssetTemplateVersion now already in place.
     */
    @Deprecated
    static Map<String, Object> updateAssetTemplate(DispatchContext dctx, Map<String, ? extends Object> origContext) {
        // create copy of map, so we can add items without creating side effects
        Map<String, Object> context = UtilMisc.makeMapWritable(origContext);
        Delegator delegator = (GenericDelegator) dctx.getDelegator();

        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            GenericValue userLogin = CmsServiceUtil.getUserLoginOrSystem(dctx, context);

            // check if you want update the actual thang
            if (context.get("assetTemplateId") != null) {
                // Partos unos : Copy original template
                CmsAssetTemplate originalTemplate = CmsAssetTemplate.getWorker().findByIdAlways(delegator, 
                        (String) context.get("assetTemplateId"), false);
                // Update the original template so it is inactive;
                // originalTemplate.setInactive(true);
                // originalTemplate.store();
                // Paritas dos tacos: Update original template and set to
                // inactive (assetTemplateId)
                CmsAssetTemplate newTemplate = (CmsAssetTemplate) originalTemplate.copy(new HashMap<String, Object>());
                if (context.get("templateName") != null) {
                    newTemplate.setName((String) context.get("templateName"));
                }
                newTemplate.setTemplateBodySource(context);

                newTemplate.setCreatedBy((String) userLogin.get("userLoginId"));

                // Partunoertes trois!
                // make sure this is active (in case we are restoring an old
                // version ;) )
                newTemplate.setActive(false);
                newTemplate.store();
                result.put("newAssetTemplateId", newTemplate.getId());
            } else {
                // ToDo: Check for templateName being unique (though we may not
                // want to do that)
                //GenericValue gv = delegator.makeValue("CmsAssetTemplate",
                //        EntityUtil.makeFields("templateName", context.get("templateName"), "templateBody", context.get("templateBody")));
                //CmsAssetTemplate newTemplate = new CmsAssetTemplate(gv);
            }
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    /**
     * Sets an asset template version as live version. The live version is the content that
     * will be displayed to regular page visitors.
     */
    public static Map<String, Object> activateAssetTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            Delegator delegator = dctx.getDelegator();
            
            String versionId = (String) context.get("versionId");
            CmsAssetTemplateVersion version = CmsAssetTemplateVersion.getWorker().findFirst(delegator, UtilMisc.toMap("versionId", versionId), false);
            
            if (version != null) {
                version.setAsActiveVersion();
                version.store();
            } else {
                return ServiceUtil.returnError("Asset template version '" + version + "' not found"); // TODO: Localize
            }
        } catch(Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    /**
     * Gets the active/default asset template version of the given asset template.
     */
    public static Map<String, Object> getActiveAssetTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String webSiteId = (String) context.get("webSiteId");
            String templateName = (String) context.get("templateName");
            
            CmsAssetTemplateVersion assetTemplateVersion = CmsAssetTemplate.getVerComTemplateWorker().findActiveVersion(delegator,
                    templateName, webSiteId, false);
            
            if (assetTemplateVersion != null) {
                result.put("assetTemplate", assetTemplateVersion.getAssetTemplate().getEntity());
                result.put("assetTemplateModel", assetTemplateVersion.getAssetTemplate());
                result.put("assetTemplateVersion", assetTemplateVersion.getEntity());
                result.put("assetTemplateVersionModel", assetTemplateVersion);
            } else {
                result = ServiceUtil.returnFailure("Could not find active template version for webSiteId '" +
                        webSiteId + "' and templateName '" + templateName + "'"); // TODO: Localize
            }
        } catch(Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }
        return result;
    }
    
    /**
     * Gets all asset information
     */
    public static Map<String, Object> getAssetTemplate(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String webSiteId = (String) context.get("webSiteId");
            String templateName = (String) context.get("templateName");
            String assetTemplateId = (String) context.get("assetTemplateId");
            CmsAssetTemplate assetTemplate = null;
            if (UtilValidate.isNotEmpty(assetTemplateId)) {
                assetTemplate = CmsAssetTemplate.getWorker().findByIdAlways(delegator, assetTemplateId, false);
//                if (assetTemplateModel == null) {
//                    return ServiceUtil.returnFailure("Could not find active template version for assetTemplateId '" + assetTemplateId + "'"); // TODO: Localize
//                }
            } else {
                assetTemplate = CmsAssetTemplate.getVerComTemplateWorker().findByName(delegator, templateName, webSiteId, false);
                if (assetTemplate == null) {
                    return ServiceUtil.returnFailure("Could not find active template version for webSiteId '" +
                            webSiteId + "' and templateName '" + templateName + "'"); // TODO: Localize
                }
            }
            result.put("assetTemplateValue", assetTemplate.getEntity());
            result.put("assetTemplate", assetTemplate);
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }
        return result;
    }

    public static Map<String, Object> updateAssetTemplateScript(DispatchContext dctx, Map<String, ? extends Object> context) {
        GenericDelegator delegator = (GenericDelegator) dctx.getDelegator();
        Map<String, Object> result = ServiceUtil.returnSuccess();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        
        try {
            CmsAssetTemplateScriptAssoc.getWorker().createUpdateScriptTemplateAndAssoc(delegator, context, userLogin);
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Error while updating Script Template Assoc", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    public static Map<String, Object> deleteAssetTemplate(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String assetTemplateId = (String) context.get("assetTemplateId");
            CmsAssetTemplate template = CmsAssetTemplate.getWorker().findByIdAlways(delegator, assetTemplateId, false);
            template.remove();
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
    /**
     * Gets the active/default asset template version of the given asset template.
     */
    public static Map<String, Object> getRelatedActiveAssetTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String relatedVersionId = (String) context.get("relatedVersionId");
            String assetTemplateId = (String) context.get("assetTemplateId");

            CmsAssetTemplate assetTemplate = null;
            if (UtilValidate.isNotEmpty(relatedVersionId)) {
                CmsAssetTemplateVersion relatedVersion = CmsAssetTemplateVersion.getWorker().findOne(delegator, UtilMisc.toMap("versionId", relatedVersionId), false);
                if (relatedVersion == null) {
                    return ServiceUtil.returnFailure("Could not find related active asset template version with versionId '" + relatedVersionId + "'");
                }
                assetTemplate = relatedVersion.getAssetTemplate();
            } else if (UtilValidate.isNotEmpty(assetTemplateId)) {
                assetTemplate = CmsAssetTemplate.getWorker().findOne(delegator, UtilMisc.toMap("assetTemplateId", assetTemplateId), false);
                if (assetTemplate == null) {
                    return ServiceUtil.returnFailure("Could not find asset template with assetTemplateId '" + assetTemplateId + "'");
                }
            } else {
                return ServiceUtil.returnFailure("Could not find related active template version; no relatedVersionId or assetTemplateId specified");
            }

            result.put("assetTemplate", assetTemplate.getEntity());
            result.put("assetTemplateModel", assetTemplate);
            result.put("assetTemplateVersion", assetTemplate.getActiveVersion() != null ? assetTemplate.getActiveVersion().getEntity() : null);
            result.put("assetTemplateVersionModel", assetTemplate.getActiveVersion());
        } catch(Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }
        return result;
    }    

    public static Map<String, Object> getAssetTemplateTypes(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            Boolean deep = !Boolean.FALSE.equals(context.get("deep"));
            List<GenericValue> types = new ArrayList<>();
            CmsDataUtil.getContentTypes(delegator, "SCP_TEMPLATE_PART", deep, types);
            result.put("contentTypeValues", types);
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }
        return result;
    }
    
    public static Map<String, Object> getAssetTemplates(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String webSiteId = (String) context.get("webSiteId");
            Boolean webSiteOptional = !Boolean.FALSE.equals("webSiteOptional");
            String contentTypeId = (String) context.get("contentTypeId");
            
            List<EntityCondition> condList = new ArrayList<>();
            if (context.containsKey("webSiteId")) {
                if (UtilValidate.isEmpty(webSiteId)) {
                    webSiteId = null;
                }
                if (webSiteOptional && UtilValidate.isNotEmpty(webSiteId)) {
                    condList.add(EntityCondition.makeCondition(
                                    EntityCondition.makeCondition("webSiteId", null),
                                    EntityOperator.OR,
                                    EntityCondition.makeCondition("webSiteId", webSiteId)));
                } else {
                    condList.add(EntityCondition.makeCondition("webSiteId", webSiteId));
                }
            }
            if (context.containsKey("contentTypeId")) {
                if (UtilValidate.isEmpty(contentTypeId)) {
                    contentTypeId = null;
                }
                condList.add(EntityCondition.makeCondition("contentTypeId", contentTypeId));
            }
            List<GenericValue> values = delegator.findList("CmsAssetTemplate", 
                    EntityCondition.makeCondition(condList, EntityOperator.AND), 
                    null, UtilMisc.toList("templateName"), null, false);
            result.put("assetTemplateValues", values);
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }
        return result;
    }
    
    public static Map<String, Object> getAssetTemplateAttributes(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String assetTemplateId = (String) context.get("assetTemplateId");
            CmsAssetTemplate assetTmpl = CmsAssetTemplate.getWorker().findByIdAlways(delegator, assetTemplateId, false);
            result.put("attributeValues", CmsDataObject.getEntityValues(assetTmpl.getAttributeTemplates()));
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
        }
        return result;
    }
    
}
