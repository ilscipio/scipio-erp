/**
 * Scipio CMS Get Template - script
 * Fetches template information based on available parameters and puts them in the page context. Can be used
 * to determine whether or not the new page dialog should be displayed instead
 */

import java.util.ArrayList;

import java.util.List;
import java.util.Map;
import java.util.HashMap;

import javax.servlet.*;
import javax.servlet.http.*;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.LocalDispatcher;
 
final String module = "CmsGetTemplate.groovy";

/*Parameters*/
pageTemplateId = parameters.pageTemplateId;
versionId = parameters.versionId;
context.pageTemplateId = pageTemplateId;

if (pageTemplateId) {
    servCtx = ["userLogin": context.userLogin, "locale": context.locale, "pageTemplateId":pageTemplateId, "versionId":versionId];
    pageTmplResult = dispatcher.runSync("cmsGetPageTemplateAndVersions", servCtx);
    if (ServiceUtil.isSuccess(pageTmplResult)) {
        pageTemplate = pageTmplResult.pageTmpAndVersions;
        context.pageTemplate = pageTemplate;
        context.versionId = pageTemplate.versionId;
    } else {
        context.cmsErrorHandler.addContextReadErrorFromServiceResult(context, pageTmplResult);
    }
}
