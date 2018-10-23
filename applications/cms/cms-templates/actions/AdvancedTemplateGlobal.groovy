/**
 * SCIPIO: Advanced global (backend) template dedicated/self-titled script.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.widget.util.*;
 
final module = "AdvancedTemplateGlobal.groovy";

WidgetScriptUtil.runScreenActionsAtLocation("component://common/widget/CommonScreens.xml", "AllGlobalActions", context);
