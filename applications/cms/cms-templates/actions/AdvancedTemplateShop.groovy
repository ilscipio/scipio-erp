/**
 * SCIPIO: Advanced shop (frontend) template dedicated/self-titled script.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.widget.util.*;

final module = "AdvancedTemplateShop.groovy";

WidgetScriptUtil.runScreenActionsAtLocation("component://shop/widget/CommonScreens.xml", "ShopActions", context);
WidgetScriptUtil.runScreenActionsAtLocation("component://common/widget/CommonScreens.xml", "AllGlobalActions", context);
