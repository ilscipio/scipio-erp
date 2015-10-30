import org.ofbiz.base.util.template.FreeMarkerWorker;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateScalarModel;
import org.ofbiz.base.util.*;
import com.ilscipio.cato.webapp.ftl.FtlTransformUtil;
import com.ilscipio.cato.webapp.ftl.CommonFtlUtil;

final module = "getContextCatoStyles.groovy"

setContextCatoStylesAsGlobal = context.setContextCatoStylesAsGlobal

// get the cached styles
styles = FtlTransformUtil.unwrapOrNull(CommonFtlUtil.getRequestVar("catoTmplVarStyles", context.request, context));

if (styles == null) {
    // nothing from cache, so there probably hasn't been a render yet... so we must trigger a dummy FTL render

    ftlContext = context
    useCache = true
    StringWriter sw = new StringWriter();
    env = FreeMarkerWorker.renderTemplateFromString("getContextCatoStylesDummyTemplate", "", ftlContext, sw, useCache)
    
    if (env != null) {
        // try again; get FTL global directly in case context is weird, so this should always work
        styles = FtlTransformUtil.unwrapOrNull(env.getGlobalVariable("styles"));
    }
    else {
        Debug.logError("Missing Freemarker environment", module);
    }
}

if (!styles) {
    Debug.logWarning("No valid Cato styles were found in freemarker environment - is theme set up correctly?", module)
}


if (setContextCatoStylesAsGlobal == true) {
    globalContext.styles = styles
}
else {
    context.styles = styles
}
