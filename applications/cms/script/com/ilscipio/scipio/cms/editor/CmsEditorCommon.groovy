/**
 * Scipio CMS Editor Common - Sets some utility classes/instances in context 
 * for use in other editor groovy scripts and/or templates.
 * AUTOMATICALLY included at start of every cms backend app screen render 
 * through CommonScreens.xml#static-common-actions.
 */

import java.util.ArrayList;

import java.util.List;
import java.util.Map;
import java.util.HashMap;

import javax.servlet.*;
import javax.servlet.http.*;

import org.apache.juli.ClassLoaderLogManager.ClassLoaderLogInfo
import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.WebAppUtil;
import com.ilscipio.scipio.cms.control.CmsWebSiteInfo;
import com.ilscipio.scipio.cms.control.CmsWebSiteConfig;
import com.ilscipio.scipio.cms.webapp.CmsWebappUtil;

public class CmsErrorHandler {
    def addContextErrors(context, errorMessages) {
        if (!errorMessages) return;
        def errorMessageList = context.errorMessageList;
        if (errorMessageList == null) {
            errorMessageList = new ArrayList();
        }
        errorMessageList.addAll(errorMessages);
        context.errorMessageList = errorMessageList;
        context.isError = true;
    }
    
    def addContextError(context, errorMessage) {
        this.addContextErrors(context, [errorMessage]);
    }
    
    def addContextReadErrors(context, errorMessages) {
        if (!errorMessages) return;
        this.addContextErrors(context, errorMessages);
        context.isCmsReadError = true;
    }
    
    def addContextReadError(context, errorMessage) {
        this.addContextReadErrors(context, [errorMessage]);
    }
    
    def addContextErrorFromServiceResult(context, serviceResult) {
        this.addContextError(context, ServiceUtil.getErrorMessage(serviceResult));
    }
    
    def addContextReadErrorFromServiceResult(context, serviceResult) {
        this.addContextErrorFromServiceResult(context, serviceResult);
        context.isCmsReadError = true;
    }
    
    def addContextEventMsgs(context, eventMessages) {
        if (!eventMessages) return;
        def eventMessageList = context.eventMessageList;
        if (eventMessageList == null) {
            eventMessageList = new ArrayList();
        }
        eventMessageList.addAll(eventMessages);
        context.eventMessageList = eventMessageList;
    }
    
    def addContextEventMsg(context, eventMessage) {
        this.addContextEventMsgs(context, [eventMessage]);
    }
}

public class CmsContentTreeUtil {
    private static final String module = CmsContentTreeUtil.class.getName();
    
    def addWebSiteSettingsToMap(targetMap, webSiteId) {
        // NEW: store website configs
        String requestServletPath = null;
        String realControlPath = null; // not sure if not needed yet, get it anyway
        String primaryPathFromContextRootDefault = null;
        
        if (webSiteId) {
            CmsWebSiteInfo webSiteInfo = CmsWebSiteInfo.getWebSiteInfo(webSiteId);
            CmsWebSiteConfig webSiteConfig = webSiteInfo.getWebSiteConfig();
            
            requestServletPath = webSiteConfig.getRequestServletPath();
            realControlPath = webSiteInfo.getControlServletMapping();
            primaryPathFromContextRootDefault = webSiteConfig.getPrimaryPathFromContextRootDefaultIndicator();
        }
    
        targetMap.put("requestServletPath", requestServletPath);
        targetMap.put("realControlPath", realControlPath);
        targetMap.put("primaryPathFromContextRootDefault", primaryPathFromContextRootDefault);
        
        // THE EDITOR REQUEST PREFIX FOR SCREENS IS AS FOLLOWS:
        // if primaryPathFromContextRootDefault is "Y", then we have to list requests
        // with the full path prefix so new pages based on them have the same path;
        // only omit path if primary paths were configured in system or webapp to be relative (to control/servlet)
        String editorRequestPathPrefix = "Y".equals(primaryPathFromContextRootDefault) ? requestServletPath : "";
        if (!editorRequestPathPrefix || editorRequestPathPrefix == "/") {
            editorRequestPathPrefix = "";
        }
        
        targetMap.put("editorRequestPathPrefix", editorRequestPathPrefix);
    }

}

public class CmsWebSiteHelper {
    private static final String module = CmsWebSiteHelper.class.getName();
    
    /**
     * Find out which webapps have control URIs root-aliased
     * can be manually indicated using web.xml cmsControlUrisRootAlias flag
     */
    def getWebSitesControlRootAliasMsgs(context) {
        def msgMap = new HashMap();
        CmsWebSiteInfo.getAllCmsRegWebSitesInfoList().each{ webSiteInfo ->
            def webSiteId = webSiteInfo.getWebSiteId();
            if (!webSiteInfo.isControlRootAlias()) return;
            def extInfo = webSiteInfo.getExtWebappInfo();
            if (extInfo == null) return;
            Debug.logInfo("Cms: Website '" + webSiteId + "' is control root alias enabled", module);
            try {
                def controlMapping = extInfo.getControlServletMapping();
                def srcPath = extInfo.getFullControlPath() ?: "";
                srcPath += (srcPath.endsWith("/") ? "..." : "/...");
                def destPath = extInfo.getContextRoot() ?: "";
                destPath += (destPath.endsWith("/") ? "..." : "/...");
                
                def msgArgs = [webSiteId:webSiteId, controlMapping:controlMapping,
                    srcPath:srcPath, destPath:destPath];
                def msg = UtilProperties.getMessage("CMSUiLabels", "CmsWebSiteConfiguredControlRootAlias", msgArgs, context.locale);
                msgMap[webSiteId] = msg;
            } catch(Exception e) {
                Debug.logError("Cms: Error looking up webapp info for website '" + webSiteId + "': " + e.getMessage(), module)
            }
        };
        return msgMap;
    }
}

public class CmsScriptUtil {
    public static final module = CmsScriptUtil.class.getName();
    
    def getScriptTemplateFromEntity(scriptEntity) {
        try {
            return com.ilscipio.scipio.cms.template.CmsScriptTemplate.getWorker().makeFromValue(scriptEntity);
        } catch(Exception e) {
            Debug.logError(e, "Cms: " + e.getMessage(), module);
        }
    }
}

if (!context.cmsEditorCommonIncluded) { // include guard
    context.cmsEditorCommonIncluded = true;
    
    context.cmsErrorHandler = new CmsErrorHandler();
    context.cmsContentTreeUtil = new CmsContentTreeUtil();
    context.cmsScriptUtil = new CmsScriptUtil();
    context.cmsWebSiteHelper = new CmsWebSiteHelper();

    final validMediaDataResourceTypeIdList = [
        "AUDIO_OBJECT",
        "IMAGE_OBJECT",
        "VIDEO_OBJECT",
        "DOCUMENT_OBJECT"
    ];
    context.validMediaDataResourceTypeIdList = validMediaDataResourceTypeIdList;
    context.validMediaDataResourceTypeIdSet = validMediaDataResourceTypeIdList as Set;
        
    webSiteIdSet = CmsWebSiteInfo.getAllCmsRegWebSitesInfo().keySet() as Set; // copy
    cmsWebSiteList = CmsWebappUtil.getWebSiteList(delegator, webSiteIdSet);
    context.cmsWebSiteList = cmsWebSiteList;
    context.cmsWebSiteIdSet = webSiteIdSet;
}
