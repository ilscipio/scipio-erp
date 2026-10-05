/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
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
