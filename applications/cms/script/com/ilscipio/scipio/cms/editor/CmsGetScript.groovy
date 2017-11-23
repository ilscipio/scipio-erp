/**
 * Scipio CMS Get Script - script
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

final String module = "CmsGetScript.groovy";
 
// NOTE: 2016-12: Neither UI nor schema support versioning for scripts, but it could be implemented in the future.

/* Parameters */
scriptTemplateId = parameters.scriptTemplateId;

// the (internal) language name of our script, resolved from file if applicable (best-effort)
resolvedScriptLang = "none"; 
// flag for whether we support editing and storing the script lang named in resolvedScriptLang
resolvedScriptLangEditable = false; 
// map of internal script names to code editor language modes
scriptLangEditorModeMap = context.scriptLangEditorModeMap; 

if(scriptTemplateId) {
        servCtx = ["userLogin": context.userLogin, "locale": context.locale, "scriptTemplateId": scriptTemplateId];
        servResult = dispatcher.runSync("cmsGetScriptTemplate", servCtx);
    if (ServiceUtil.isSuccess(servResult)) {
        // FIXME: why are we using scriptTemplate GenericValue here?
        scriptTemplate = servResult.scriptTemplateValue;
        scriptTemplateModel  = servResult.scriptTemplate;
        // FIXME: why are we setting scriptTemplate GenericValue here?
        context.scriptTemplate = scriptTemplate;
        context.scriptTemplateModel = scriptTemplateModel;
        
        tmplBodySrc = scriptTemplateModel?.getTemplateBodySource() ?: TemplateBodySource.getUndefined();
        tmplBodySrc.toFields(context);
        
        if (scriptTemplateModel != null) {
            resolvedScriptLang = scriptTemplateModel.getResolvedScriptLang() ?: "invalid";
            resolvedScriptLangEditable = (context.supportedScriptBodyLangs ?: []).contains(resolvedScriptLang);
        } 
    } else {
        context.cmsErrorHandler.addContextReadErrorFromServiceResult(context, servResult);
    }
}

context.resolvedScriptLangEditable = resolvedScriptLangEditable;
context.resolvedScriptLang = resolvedScriptLang;

// determine the CodeMirror lang mode with provided mapping
context.editorLangMode = scriptLangEditorModeMap[resolvedScriptLang] ?: scriptLangEditorModeMap["default"];
