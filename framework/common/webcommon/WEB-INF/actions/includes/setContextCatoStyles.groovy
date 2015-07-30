import org.ofbiz.base.util.template.FreeMarkerWorker;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateScalarModel;
import org.ofbiz.base.util.*;

final module = "getContextCatoStyles.groovy"

setContextCatoStylesAsGlobal = context.setContextCatoStylesAsGlobal

styles = [:]

ftlContext = [:]
useCache = true
StringWriter sw = new StringWriter();
env = FreeMarkerWorker.renderTemplateFromString("getContextCatoStylesDummyTemplate", "", ftlContext, sw, useCache)
if (env != null) {
    stylesFtl = env.getGlobalVariable("styles")
    if (stylesFtl != null && stylesFtl instanceof TemplateHashModelEx) {
        keysFtl = stylesFtl.keys()
        keysItFtl = keysFtl.iterator()
        while (keysItFtl.hasNext()) {
            keyFtl = keysItFtl.next()
            key = keyFtl.getAsString()
            valFtl = stylesFtl.get(key)
            val = null
            if (valFtl != null && valFtl instanceof TemplateScalarModel) {
                val = valFtl.getAsString()
            }
            
            //Debug.logInfo(key + ": " + val, module)
            styles[key] = val
        }
    }
    else {
        Debug.logError("No styles found in freemarker environment", module)
    }
}
else {
    Debug.logError("No freemarker environment found", module)
}

if (setContextCatoStylesAsGlobal == true) {
    globalContext.styles = styles
}
else {
    context.styles = styles
}
