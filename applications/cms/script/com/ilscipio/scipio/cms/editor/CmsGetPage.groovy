/**
 * Scipio CMS Get Page - script
 * Fetches page information based on available parameters and puts them in the page context. Can be used
 * to determine whether or not the new page dialog should be displayed instead
 */

import java.util.ArrayList;

import java.util.List;
import java.util.Map;
import java.util.HashMap;

import javax.servlet.*
import javax.servlet.http.*

import org.ofbiz.base.util.*
import org.ofbiz.entity.*
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.LocalDispatcher
import com.ilscipio.scipio.cms.control.CmsWebSiteConfig;
import com.ilscipio.scipio.cms.control.CmsWebSiteInfo;

final String module = "CmsGetPage.groovy";

// TODO: REVIEW: 2017: it would simplify the code and template if used only pageId in the URL
// instead of allowing pageName+webSiteId

/*Parameters*/
pageId = parameters.pageId;
primaryPath = parameters.primaryPath ?: parameters.path;
webSiteId = parameters.webSiteId;
versionId = parameters.versionId;

isNewPage = "Y".equals(parameters.isNewPage); // this forces new form, always
isCreate = "Y".equals(parameters.isCreate); // this falls back to new form IF there was an error
if (isCreate && Boolean.TRUE.equals(context.isError)) {
    isNewPage = true;
}
context.isNewPage = isNewPage;

/*Process request information*/
if ((pageId || primaryPath) && !isNewPage) { // edit mode requires either pageId or primary path
    context.webSiteId=webSiteId;
    context.versionId=versionId;
    context.pagePrimaryPath = primaryPath;
    
    servCtx = [
        "request": request,
        "response": response,
        "userLogin": context.userLogin,
        "locale": context.locale,
        "pageId": pageId,
        "primaryPath": primaryPath,
        "webSiteId": webSiteId,
        "versionId": versionId,
        "verifyWebSite": true, // 2016: make sure the webSiteId is one mapped to the page
        "useStaticWebSite": true // 2016: if webSiteId is missing, will try to get one from page
    ];
    pageResult = dispatcher.runSync("cmsGetPage", servCtx);
    if (ServiceUtil.isSuccess(pageResult)) {
        meta = pageResult.meta;
        context.meta = meta;
        context.variables = pageResult.variables;
        context.content = pageResult.content;
        context.visitors = pageResult.visitors;
        context.template = pageResult.template;
        context.currVersionId = pageResult.versionId;
        context.permission = pageResult.permission;
        context.pageId = pageResult.pageId;
        context.scriptTemplates = pageResult.scriptTemplates;
        webSiteId = pageResult.webSiteId;
        context.webSiteId = webSiteId;
        versionId = pageResult.versionId;
        context.versionId = versionId;
        if (!context.pagePrimaryPath) {
            context.pagePrimaryPath = pageResult.meta?.primaryPath;
        }
        context.pagePrimaryPathExpanded = pageResult.meta?.primaryPathExpanded;
        
        // SPECIAL CHECK: 2016: FIXME: print warning if website not matching because UI
        // can't handle multiple primary mappings
        if (webSiteId) {
            if (meta.webSiteId != meta.defaultWebSiteId) {
                Debug.logError("Cms: DATA ERROR: page '" + context.pageId + "' has "
                    + "a default webSiteId (" + meta.defaultWebSiteId + ") different from"
                    + "its primary webSiteId (" + meta.webSiteId + "); "
                    + "this is invalid and is result of either bad seed "
                    + "data or corruption (possibly concurrency issue)", module);
            }
            if (meta.primaryMappingCount > 1) {
                Debug.logWarning("Cms: DATA WARNING: page '" + context.pageId + "' has "
                    + "more than one primary process mappings in the system; although "
                    + "supported by the schema, the current CMS interface does not support "
                    + "multiple primary process mappings. Recommend manually fixing data to "
                    + "remove all but one CmsProcessMapping records with primaryForPageId field "
                    + "pointing to this page.", module);
            }
        }
    } else {
        context.cmsErrorHandler.addContextReadErrorFromServiceResult(context, pageResult);
    }
}

if (webSiteId) {
    webSiteConfig = CmsWebSiteInfo.getWebSiteConfig(webSiteId);
    context.primaryPathFromContextRootDefault = webSiteConfig?.getPrimaryPathFromContextRootDefaultIndicator();
}

tmplResult = dispatcher.runSync("cmsGetAvailablePageTemplates", ["userLogin": context.userLogin]);
if (ServiceUtil.isSuccess(tmplResult)) {
    context.availableTemplates = tmplResult.pageTemplates;
} else {
    context.cmsErrorHandler.addContextReadErrorFromServiceResult(context, tmplResult);
}

webSiteInfo = (webSiteId) ? CmsWebSiteInfo.getWebSiteInfo(webSiteId) : null;
webSiteConfig = CmsWebSiteInfo.getWebSiteConfigOrDefault(webSiteId);
webSiteContextParams = webSiteInfo?.getContextParams();
webSiteAllowPreview = webSiteConfig?.isAllowPreviewMode();
context.webSiteInfo = webSiteInfo;
context.webSiteConfig = webSiteConfig;
context.webSiteContextParams = webSiteContextParams;
context.webSiteAllowPreview = webSiteAllowPreview;

cmsCtrlRootAliasMsgs = context.cmsWebSiteHelper.getWebSitesControlRootAliasMsgs(context);
context.cmsCtrlRootAliasMsgs = cmsCtrlRootAliasMsgs;
