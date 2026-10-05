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
package com.ilscipio.scipio.common.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PropertyServices {

    /**
     * Get label name-value map from a property resource bundle for given locale
     */
    @Service(
        name = "updateLocalizedProperty",
        location = "com.ilscipio.scipio.common.label.PropertyServices",
        invoke = "updateLocalizedProperty",
        description = "Get label name-value map from a property resource bundle for given locale",
        attributes = {
            @Attribute(name = "preventEmpty", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "resourceId", type = "String", mode = "IN"),
            @Attribute(name = "propertyId", type = "String", mode = "IN"),
            @Attribute(name = "lang", type = "String", mode = "IN"),
            @Attribute(name = "value", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useEmpty", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateLocalizedProperty {}

    /**
     * Get label name-value map from a property resource bundle for given locale
     */
    @Service(
        name = "updateLocalizedPropertyOptional",
        location = "com.ilscipio.scipio.common.label.PropertyServices",
        invoke = "updateLocalizedPropertyOptional",
        description = "Get label name-value map from a property resource bundle for given locale",
        implemented = {@Implements(service = "updateLocalizedProperty")},
        overrideAttributes = {
            @OverrideAttribute(name = "lang", optional = "true")
        }
    )
    public interface UpdateLocalizedPropertyOptional {}

    /**
     * clearLocalizedPropertyCache for all Servers listening to the topic (SCIPIO)
     */
    @Service(
        name = "distributedClearLocalizedPropertyCaches",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "clearLocalizedPropertyCaches",
        description = "clearLocalizedPropertyCache for all Servers listening to the topic (SCIPIO)",
        auth = "true",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet",
        hideResultInLog = "true",
        attributes = {
            @Attribute(name = "resourceId", type = "String", mode = "IN")
        }
    )
    public interface DistributedClearLocalizedPropertyCaches {}

    /**
     * Clear UtilCaches and LocalizedProperty caches for the given resource (SCIPIO)
     */
    @Service(
        name = "clearLocalizedPropertyCaches",
        location = "com.ilscipio.scipio.common.label.PropertyServices",
        invoke = "clearLocalizedPropertyCaches",
        description = "Clear UtilCaches and LocalizedProperty caches for the given resource (SCIPIO)",
        attributes = {
            @Attribute(name = "resourceId", type = "String", mode = "IN"),
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        }
    )
    public interface ClearLocalizedPropertyCaches {}

    /**
     * Get label name-value map from a property resource bundle for given locale
     */
    @Service(
        name = "getLocalizedPropertyValues",
        location = "org.ofbiz.webtools.labelmanager.LabelServices",
        invoke = "getLocalizedPropertyValues",
        description = "Get label name-value map from a property resource bundle for given locale",
        attributes = {
            @Attribute(name = "resourceId", type = "String", mode = "IN"),
            @Attribute(name = "propertyId", type = "String", mode = "IN"),
            @Attribute(name = "langValueMap", type = "Map", mode = "OUT", optional = "true", description = "Lang-value map from combined maps (staticLangValueMap + entityLangValueMap)"),
            @Attribute(name = "staticLangValueMap", type = "Map", mode = "OUT", optional = "true", description = "Lang-value map from static file"),
            @Attribute(name = "entityLangValueMap", type = "Map", mode = "OUT", optional = "true", description = "Lang-value map from LocalizedProperty entity")
        }
    )
    public interface GetLocalizedPropertyValues {}

}
