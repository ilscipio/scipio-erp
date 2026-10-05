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
 * CommonScriptAssocActions - for editTemplate & editAsset
 * Extracted from inline script in CMSScreens.xml
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;
import com.ilscipio.scipio.cms.template.CmsScriptTemplate;

final String module = "CommonScriptAssocActions.groovy";

standaloneScriptTemplates = [];
slaveScriptTemplates = [];
allScriptTemplates = [];
try {
    for (st in CmsScriptTemplate.getWorker().findAll(delegator, (EntityCondition) null, ["templateName"], false)) {
        if (st.isStandalone()) {
            standaloneScriptTemplates.add(st);
        } else {
            slaveScriptTemplates.add(st);
        }
    }
    allScriptTemplates.addAll(standaloneScriptTemplates);
    allScriptTemplates.addAll(slaveScriptTemplates);
} catch (Exception e) {
    Debug.logError(e, "Cms: Could not read script templates", module);
}
context.standaloneScriptTemplates = standaloneScriptTemplates;
context.slaveScriptTemplates = slaveScriptTemplates;
context.allScriptTemplates = allScriptTemplates;
