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
public class ScipioServices {

    /**
     * SCIPIO: Reload visual theme definitions of specified theme or all themes if not specified
     */
    @Service(
        name = "reloadVisualThemeResources",
        location = "com.ilscipio.scipio.common.CommonServices",
        invoke = "reloadVisualThemeResources",
        description = "SCIPIO: Reload visual theme definitions of specified theme or all themes if not specified",
        attributes = {
            @Attribute(name = "visualThemeId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "visualThemePermissionCheck", mainAction = "UPDATE")
    )
    public interface ReloadVisualThemeResources {}

    @Service(
        name = "demoDataGenerator",
        engine = "interface",
        attributes = {
            @Attribute(name = "dataGeneratorProviderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "num", type = "Integer", mode = "IN", optional = "true", defaultValue = "50"),
            @Attribute(name = "minDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "maxDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "dataIntervalMs", type = "Long", mode = "IN", optional = "true", defaultValue = "0"),
            @Attribute(name = "dataSeparateTrans", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "generatedDataStats", type = "java.util.List", mode = "OUT", optional = "true")
        }
    )
    public interface DemoDataGenerator {}

    /**
     * Service to boot up FileListeners on the system
     */
    @Service(
        name = "startFileListener",
        location = "com.ilscipio.scipio.common.CommonServices",
        invoke = "startFileListener",
        description = "Service to boot up FileListeners on the system"
    )
    public interface StartFileListener {}

    /**
     * An empty service, which is triggered once a file has changed inside of a filesystem. Can be used to trigger further events down the line.
     */
    @Service(
        name = "triggerFileEvent",
        location = "com.ilscipio.scipio.common.CommonServices",
        invoke = "triggerFileEvent",
        description = "An empty service, which is triggered once a file has changed inside of a filesystem. Can be used to trigger further events down the line.",
        attributes = {
            @Attribute(name = "fileEvent", type = "String", mode = "IN"),
            @Attribute(name = "fileLocation", type = "String", mode = "IN"),
            @Attribute(name = "fileType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "eventName", type = "String", mode = "IN"),
            @Attribute(name = "eventRoot", type = "String", mode = "IN")
        }
    )
    public interface TriggerFileEvent {}

    /**
     * Looks up and clears cache based on a file location.
     */
    @Service(
        name = "clearFileCaches",
        location = "com.ilscipio.scipio.common.CommonServices",
        invoke = "clearFileCaches",
        description = "Looks up and clears cache based on a file location.",
        auth = "true",
        export = "true",
        attributes = {
            @Attribute(name = "fileLocation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fileType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cacheName", type = "String", mode = "IN")
        }
    )
    public interface ClearFileCaches {}

    /**
     * Creates a new system message
     */
    @Service(
        name = "createSystemMessage",
        engine = "entity-auto",
        invoke = "create",
        description = "Creates a new system message",
        defaultEntityName = "SystemMessages",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk")
        }
    )
    public interface CreateSystemMessage {}

    /**
     * Sends a message (preferrably json object) to connected clients
     */
    @Service(
        name = "sendWebsocketMessage",
        location = "com.ilscipio.scipio.common.CommonServices",
        invoke = "sendWebsocketMessage",
        description = "Sends a message (preferrably json object) to connected clients",
        auth = "true",
        attributes = {
            @Attribute(name = "channel", type = "String", mode = "IN"),
            @Attribute(name = "message", type = "String", mode = "IN")
        }
    )
    public interface SendWebsocketMessage {}

    /**
     * Converts a map to JSON and sends it to websocket clients
     */
    @Service(
        name = "sendWebsocketObject",
        location = "com.ilscipio.scipio.common.CommonServices",
        invoke = "sendWebsocketObject",
        description = "Converts a map to JSON and sends it to websocket clients",
        auth = "true",
        attributes = {
            @Attribute(name = "channel", type = "String", mode = "IN"),
            @Attribute(name = "data", type = "Map", mode = "IN")
        }
    )
    public interface SendWebsocketObject {}

}
