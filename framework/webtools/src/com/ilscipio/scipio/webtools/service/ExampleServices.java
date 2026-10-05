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
package com.ilscipio.scipio.webtools.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ExampleServices {

    @Service(
        name = "sendExamplePushNotifications",
        location = "org.ofbiz.example.ExampleServices",
        invoke = "sendExamplePushNotifications",
        auth = "true",
        attributes = {
            @Attribute(name = "exampleId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "successMessage", type = "String", mode = "IN", optional = "true")
        },
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "OFBTOOLS", action = "_VIEW")})}
    )
    public interface SendExamplePushNotifications {}

    @Service(
        name = "testAdminService",
        location = "org.ofbiz.example.ExampleServices",
        invoke = "testAdminService",
        auth = "true",
        attributes = {
            @Attribute(name = "inParam1", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inParam2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inParam3", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inParam4Internal", type = "String", mode = "IN", optional = "true", access = "internal"),
            @Attribute(name = "inListParam3", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "inMapParam4", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "outResult1", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "outResult2", type = "String", mode = "OUT", optional = "true")
        },
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "OFBTOOLS", action = "_VIEW")})}
    )
    public interface TestAdminService {}

}
