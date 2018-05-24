package com.ilscipio.scipio.cms.data.importexport;

import java.io.StringWriter;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webtools.WebToolsServices;

import com.ilscipio.scipio.cms.CmsServiceUtil;
import com.ilscipio.scipio.cms.ServiceErrorFormatter;
import com.ilscipio.scipio.cms.ServiceErrorFormatter.FormattedError;
import com.ilscipio.scipio.cms.data.CmsEntityInfo;
import com.ilscipio.scipio.cms.data.importexport.CmsDataExportWorker.GenericWorkerArgs;

public abstract class CmsImportExportServices {
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final ServiceErrorFormatter errorFmt = CmsServiceUtil.getErrorFormatter();

    protected CmsImportExportServices() {
    }
    
    public static Map<String, Object> exportDataAsXmlInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        //GenericValue userLogin = (GenericValue) context.get("userLogin");
        //Locale locale = (Locale) context.get("locale");
        //Security security = dctx.getSecurity();
        // TODO: we may need to check PageAuthorization at some point, but ENTITY_MAINT is too restrictive
        //if (!security.hasPermission("ENTITY_MAINT", userLogin)) {
        //    return ServiceUtil.returnError(UtilProperties.getMessage("WebtoolsUiLabels", "WebtoolsPermissionMaint", locale));
        //}
        try {
            GenericWorkerArgs workerArgs = new GenericWorkerArgs(delegator);
            workerArgs.setAllFromMap(context); // SEE IMPLEMENTATION - service params should match map names

            // TODO: remove singlePkfEntityName && singlePkfIdList once entityPkMap works better...
            Map<String, Collection<String>> entityPkMap = UtilGenerics.<String, Collection<String>>checkMap(context.get("entityPkMap"));
            String singlePkfEntityName = (String) context.get("singlePkfEntityName");
            LinkedHashSet<String> singlePkfIdList = new LinkedHashSet<String>(UtilGenerics.<String>checkCollection(context.get("singlePkfIdList")));

            boolean entityNoPkFindAll = Boolean.TRUE.equals(context.get("entityNoPkFindAll"));
            Set<String> namedEntities = null;
            Map<String, EntityCondition> entityCondMap = workerArgs.getEntityCondMap();
            if (UtilValidate.isNotEmpty(entityPkMap)) {
                CmsDataExportWorker.addEntityPkFilterConds(delegator, entityPkMap, entityCondMap);
                if (namedEntities == null) namedEntities = new HashSet<>();
                namedEntities.addAll(entityPkMap.keySet());
            }
            if (UtilValidate.isNotEmpty(singlePkfEntityName) && UtilValidate.isNotEmpty(singlePkfIdList)) {
                CmsDataExportWorker.addEntityPkFilterConds(delegator, singlePkfEntityName, singlePkfIdList, entityCondMap);
                if (namedEntities == null) namedEntities = new HashSet<>();
                namedEntities.add(singlePkfEntityName);
            }
            
            // WARN: this only works right in object group mode right now... will break other modes... 
            // hence why entityNoPkFindAll true by default
            if (!entityNoPkFindAll && namedEntities != null) {
                // remove the target
                Collection<String> targetEntityNames = workerArgs.getTargetEntityNames();
                if (UtilValidate.isNotEmpty(targetEntityNames)) {
                    Set<String> newNames = new LinkedHashSet<>(targetEntityNames);
                    newNames.retainAll(namedEntities);
                    workerArgs.setTargetEntityNames(newNames);
                }
            }
            
            workerArgs.setEntityCondMap(entityCondMap).setCommonEfo();
            
            CmsDataExportWorker worker = CmsDataExportWorker.makeSingleFileWorker(workerArgs);
            
            StringWriter writer = new StringWriter();
            worker.executeExport(writer);
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("resultText", writer.toString());
            return result;
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Data Export error", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
    }
    
    public static Map<String, Object> exportDataAsXml(DispatchContext dctx, Map<String, Object> context) {
        //Delegator delegator = dctx.getDelegator();
        //GenericValue userLogin = (GenericValue) context.get("userLogin");
        //Locale locale = (Locale) context.get("locale");
        //Security security = dctx.getSecurity();
        // TODO: we may need to check PageAuthorization at some point, but ENTITY_MAINT is too restrictive
        //if (!security.hasPermission("ENTITY_MAINT", userLogin)) {
        //    return ServiceUtil.returnError(UtilProperties.getMessage("WebtoolsUiLabels", "WebtoolsPermissionMaint", locale));
        //}
        try {
            if (null == null) throw new UnsupportedOperationException("INCOMPLETE");
            
            // TODO: 
//            try {
//                outputMode.checkAllowed(security, userLogin);
//                
//            } catch (Exception e) {
//                Debug.logError(e, "Cms: Data Export: " + e.getMessage(), module);
//                return ServiceUtil.returnError(e.getMessage());
//            }
            
            // TODO: NOT IMPLEMENTED

            Map<String, Object> result = ServiceUtil.returnSuccess();
            //result.put("resultText", writer.toString());
            return result;
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, "Data Export error", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
    }
    
    public static Map<String, Object> importXmlData(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Security security = dctx.getSecurity();
        
        boolean hasEntityMaintPerm = security.hasPermission("ENTITY_MAINT", userLogin);
        
        Set<String> allowedEntityNames = new HashSet<>();
        allowedEntityNames.addAll(CmsEntityInfo.getInst(delegator).getCmsEntityNames());
        allowedEntityNames.addAll(CmsEntityInfo.getInst(delegator).getExtCmsEntityNames());
        Set<String> explAllowedEntityNames = UtilGenerics.checkSet(context.get("allowedEntityNames")); // SCIPIO: new 2017-06-15
        if (UtilValidate.isNotEmpty(explAllowedEntityNames)) {
            allowedEntityNames.retainAll(explAllowedEntityNames);
        }
        
        context.put("allowedEntityNames", allowedEntityNames);
        context.put("allowLocations", hasEntityMaintPerm);
        return WebToolsServices.entityImport(dctx, context);
    }
}
