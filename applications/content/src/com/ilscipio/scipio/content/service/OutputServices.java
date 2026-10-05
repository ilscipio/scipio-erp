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
package com.ilscipio.scipio.content.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OutputServices {

    /**
     * Send Print From Screen Widget Service
     */
    @Service(
        name = "sendPrintFromScreen",
        engine = "java",
        location = "org.ofbiz.content.output.OutputServices",
        invoke = "sendPrintFromScreen",
        description = "Send Print From Screen Widget Service",
        maxRetry = "0",
        attributes = {
            @Attribute(name = "screenLocation", type = "String", mode = "IN"),
            @Attribute(name = "screenContext", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "printerContentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "printerName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "docAttributes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "printRequestAttributes", type = "List", mode = "IN", optional = "true")
        }
    )
    public interface SendPrintFromScreen {}

    /**
     * Create a File From Screen Widget Service
     */
    @Service(
        name = "createFileFromScreen",
        engine = "java",
        location = "org.ofbiz.content.output.OutputServices",
        invoke = "createFileFromScreen",
        description = "Create a File From Screen Widget Service",
        maxRetry = "0",
        attributes = {
            @Attribute(name = "screenLocation", type = "String", mode = "IN"),
            @Attribute(name = "screenContext", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filePath", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fileName", type = "String", mode = "IN"),
            @Attribute(name = "fileOutput", type = "java.io.File", mode = "OUT", optional = "true")
        }
    )
    public interface CreateFileFromScreen {}

}
