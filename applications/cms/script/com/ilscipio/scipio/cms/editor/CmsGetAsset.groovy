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

import com.ilscipio.scipio.cms.template.CmsTemplate.TemplateBodySource;

final String module = "CmsGetAsset.groovy";
 
/* Parameters */
assetTemplateId = parameters.assetTemplateId;
webSiteId = parameters.webSiteId;
templateName = parameters.templateName;
versionId = parameters.versionId;

if(assetTemplateId) {
        servCtx = ["userLogin": context.userLogin, "locale": context.locale, "assetTemplateId": assetTemplateId];
        pageTmplResult = dispatcher.runSync("cmsGetAssetTemplate", servCtx);
    if (ServiceUtil.isSuccess(pageTmplResult)) {
        // FIXME: why are we using GenericValue here instead of model?
        assetTemplate = pageTmplResult.assetTemplateValue;
        assetTemplateModel  = pageTmplResult.assetTemplate;
        context.assetTemplate = assetTemplate;
        context.assetTemplateModel = assetTemplateModel;
        
        // 2016-12-02: TODO?: UI CURRENTLY DOES NOT SUPPORT VERSIONING ON ASSET TEMPLATES (BUT SCHEMA DOES)
        // - ASSUMING ACTIVE VERSION AS ONLY VERSION
        tmplBodySrc = assetTemplateModel?.getActiveVersion()?.getTemplateBodySource() ?: TemplateBodySource.getUndefined();
        tmplBodySrc.toFields(context);
        
        context.assetAttr = assetTemplateModel.getLocallySortedAttributeTemplates();
        context.scriptTemplates = assetTemplateModel.getSortedScriptTemplates();
    } else {
        context.cmsErrorHandler.addContextReadErrorFromServiceResult(context, pageTmplResult);
    }
}
