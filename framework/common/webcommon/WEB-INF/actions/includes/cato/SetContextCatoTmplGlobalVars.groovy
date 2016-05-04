import org.ofbiz.base.util.template.FreeMarkerWorker;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateScalarModel;
import org.ofbiz.base.util.*;
import com.ilscipio.cato.ce.webapp.ftl.lang.LangFtlUtil;
import com.ilscipio.cato.ce.webapp.ftl.context.ContextFtlUtil;

final module = "getContextCatoStyles.groovy"

setContextCatoStylesAsGlobal = context.setContextCatoStylesAsGlobal

// 2016-05-04: we use catoTmplGlobalVarsAdapted instead of catoTmplGlobalVars as an optimization

// get the cached variables
tmplGlobalVars = LangFtlUtil.unwrapOrNull(ContextFtlUtil.getRequestVar("catoTmplGlobalVarsAdapted", context.request, context));

if (tmplGlobalVars == null) {
    // nothing from cache, so there probably hasn't been a render yet... so we must trigger a dummy FTL render

    ftlContext = context
    useCache = true
    StringWriter sw = new StringWriter();
    env = FreeMarkerWorker.renderTemplateFromString("getContextCatoTmplGlobalVarsDummyTemplate", "", ftlContext, sw, useCache)
    
    if (env != null) {
        tmplGlobalVars = LangFtlUtil.unwrapOrNull(ContextFtlUtil.getRequestVar("catoTmplGlobalVarsAdapted", context.request, context));

    }
    else {
        Debug.logError("Missing Freemarker environment", module);
    }
}

if (tmplGlobalVars) {
    // Debug.logInfo("Cato templating vars: " + tmplGlobalVars, module);
    
    if (context.setContextCatoTmplGlobalVarsAsGlobal == true) {
        globalContext.putAll(tmplGlobalVars);
    }
    else {
        context.putAll(tmplGlobalVars);
    }
}
else {
    Debug.logWarning("No valid Cato theme/template variables were found in freemarker environment - is theme set up correctly?", module);
}



