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
public class MediaServices {

    /**
     * clearMediaProfileCaches for all Servers listening to the topic (SCIPIO)
     */
    @Service(
        name = "distributedClearMediaProfileCaches",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "clearMediaProfileCaches",
        description = "clearMediaProfileCaches for all Servers listening to the topic (SCIPIO)",
        auth = "true",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet",
        hideResultInLog = "true"
    )
    public interface DistributedClearMediaProfileCaches {}

    /**
     * Clear media profile caches
     */
    @Service(
        name = "clearMediaProfileCaches",
        location = "org.ofbiz.common.image.MediaProfile",
        invoke = "clearCaches",
        description = "Clear media profile caches",
        attributes = {
            @Attribute(name = "type", type = "String", mode = "IN", optional = "true", description = "TODO: currently ignored, but callers should still specify"),
            @Attribute(name = "tenantOnly", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        }
    )
    public interface ClearMediaProfileCaches {}

    /**
     * Clear media profile caches
     */
    @Service(
        name = "clearImageProfileCaches",
        location = "org.ofbiz.common.image.MediaProfile",
        invoke = "clearCaches",
        description = "Clear media profile caches",
        implemented = {@Implements(service = "clearMediaProfileCaches")},
        overrideAttributes = {
            @OverrideAttribute(name = "type", defaultValue = "IMAGE_OBJECT")
        }
    )
    public interface ClearImageProfileCaches {}

}
