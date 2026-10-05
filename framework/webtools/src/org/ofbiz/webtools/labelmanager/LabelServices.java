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
package org.ofbiz.webtools.labelmanager;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;

import java.util.Map;
import java.util.TreeMap;

/** LabelServices (SCIPIO). */
public abstract class LabelServices {

    // SCIPIO: FIXME: dependency issues: belongs in com.ilscipio.scipio.common.label.PropertyServices
    public static Map<String, Object> getLocalizedPropertyValues(ServiceContext ctx) {
        String resourceId = ctx.attr("resourceId");
        String propertyId = ctx.attr("propertyId");
        LabelFile labelFile = LabelManagerFactory.getLabelFileNoExtStatic(resourceId);
        if (labelFile == null) {
            return ServiceUtil.returnError("Resource [" + resourceId + "] not found");
        }
        labelFile = labelFile.toLocal(); // could skip here
        Map<String, String> staticLangValueMap = labelFile.getStaticPropertyValues(propertyId);
        if (staticLangValueMap == null) {
            return ServiceUtil.returnFailure("Property [" + propertyId + "] not found in resource [" + resourceId + "]");
        }
        // NOTE: Currently no tenant delegator support
        Map<String, String> entityLangValueMap = labelFile.getEntityPropertyValues(propertyId);
        Map<String, String> langValueMap = new TreeMap<>(staticLangValueMap);
        if (entityLangValueMap != null) {
            langValueMap.putAll(entityLangValueMap);
        }
        return UtilMisc.put(ServiceUtil.returnSuccess(),
                "langValueMap", langValueMap,
                "staticLangValueMap", staticLangValueMap,
                "entityLangValueMap", entityLangValueMap);
    }

}
