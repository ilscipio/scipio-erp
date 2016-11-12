import org.ofbiz.base.util.template.FreeMarkerWorker;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateScalarModel;
import org.ofbiz.base.util.*;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;

final module = "getContextScipioStyles.groovy"

setContextScipioStylesAsGlobal = context.setContextScipioStylesAsGlobal

// 2016-05-04: we use scipioTmplGlobalVarsAdapted instead of scipioTmplGlobalVars as an optimization

// get the cached variables
tmplGlobalVars = LangFtlUtil.unwrapOrNull(ContextFtlUtil.getRequestVar("scipioTmplGlobalVarsAdapted", context.request, context));

if (tmplGlobalVars == null) {
    // nothing from cache, so there probably hasn't been a render yet... so we must trigger a dummy FTL render

    ftlContext = context
    useCache = true
    StringWriter sw = new StringWriter();
    env = FreeMarkerWorker.renderTemplateFromString("getContextScipioTmplGlobalVarsDummyTemplate", "", ftlContext, sw, useCache)
    
    if (env != null) {
        tmplGlobalVars = LangFtlUtil.unwrapOrNull(ContextFtlUtil.getRequestVar("scipioTmplGlobalVarsAdapted", context.request, context));

    }
    else {
        Debug.logError("Missing Freemarker environment", module);
    }
}

if (tmplGlobalVars) {
    // Debug.logInfo("Scipio templating vars: " + tmplGlobalVars, module);
    
    if (context.setContextScipioTmplGlobalVarsAsGlobal == true) {
        globalContext.putAll(tmplGlobalVars);
    }
    else {
        context.putAll(tmplGlobalVars);
    }
}
else {
    Debug.logWarning("No valid Scipio theme/template variables were found in freemarker environment - is theme set up correctly?", module);
}



