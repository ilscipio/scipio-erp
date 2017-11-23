/**
 * SCIPIO: Demo template dedicated script.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.widget.util.*;

final module = "DemoTemplate.groovy";

layoutSettings = globalContext.layoutSettings ?: [:];
styleSheets = layoutSettings.styleSheets ?: [];
javaScripts = layoutSettings.javaScripts ?: [];
if (context.customCssFile) {
    styleSheets.add(context.customCssFile);
}
if (context.customJsFile) {
    javaScripts.add(context.customJsFile);
}
//Debug.logInfo("title: " + context.title, module);
//Debug.logInfo("customCssFile: " + context.customCssFile, module);
//Debug.logInfo("customJsFile: " + context.customJsFile, module);
//Debug.logInfo("cmsContent: " + context.cmsContent, module);
layoutSettings.styleSheets = styleSheets;
layoutSettings.javaScripts = javaScripts;
globalContext.layoutSettings = layoutSettings;

// TODO: utility still needed to reuse CmsScriptTemplate definitions from inside scripts (here)
WidgetScriptUtil.runScreenActionsAtLocation("component://shop/widget/CommonScreens.xml", "ShopActions", context);
WidgetScriptUtil.runScreenActionsAtLocation("component://common/widget/CommonScreens.xml", "AllGlobalActions", context);
