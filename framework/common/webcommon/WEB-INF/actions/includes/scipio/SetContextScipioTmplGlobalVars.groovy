/**
 * Sets the ftl library variables (styles hash) into render context, for use even outside of ftl (SCIPIO).
 */
import org.ofbiz.base.util.*;
import org.ofbiz.widget.renderer.VisualThemeWorker;

final module = "SetContextScipioTmplGlobalVars.groovy"

scpLibVarsRaw = VisualThemeWorker.getFtlLibVariables(context);
if (scpLibVarsRaw != null) {
    if (Boolean.TRUE.equals(context.setContextScipioTmplGlobalVarsAsGlobal)) {
        globalContext.putAll(scpLibVarsRaw);
    } else {
        context.putAll(scpLibVarsRaw);
    }
} else {
    Debug.logWarning("No valid Scipio theme/template variables found in theme or system - is theme or system set up correctly?", module);
}



