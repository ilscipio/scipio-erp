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
package com.ilscipio.scipio.accounting.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class DatevServices {

    @Service(
        name = "importDatevInterface",
        engine = "interface",
        attributes = {
            @Attribute(name = "dataCategory", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "uploadedFile", type = "java.nio.ByteBuffer", mode = "IN"),
            @Attribute(name = "_uploadedFile_size", type = "String", mode = "IN"),
            @Attribute(name = "_uploadedFile_fileName", type = "String", mode = "IN"),
            @Attribute(name = "_uploadedFile_contentType", type = "String", mode = "IN"),
            @Attribute(name = "operationResults", type = "com.ilscipio.scipio.accounting.external.BaseOperationResults", mode = "OUT", optional = "true"),
            @Attribute(name = "operationStats", type = "java.util.List", mode = "OUT")
        }
    )
    public interface ImportDatevInterface {}

    /**
     * Imports transactions entries in Datev format from a csv
     */
    @Service(
        name = "importDatevTransactionEntries",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.external.datev.DatevServices",
        invoke = "importDatev",
        description = "Imports transactions entries in Datev format from a csv",
        auth = "true",
        implemented = {@Implements(service = "importDatevInterface")},
        attributes = {
            @Attribute(name = "orgPartyId", type = "String", mode = "INOUT"),
            @Attribute(name = "topGlAccountId", type = "String", mode = "INOUT")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "CREATE")
    )
    public interface ImportDatevTransactionEntries {}

    /**
     * Imports contacts in Datev format from a csv
     */
    @Service(
        name = "importDatevContacts",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.external.datev.DatevServices",
        invoke = "importDatev",
        description = "Imports contacts in Datev format from a csv",
        auth = "true",
        implemented = {@Implements(service = "importDatevInterface")},
        attributes = {
            @Attribute(name = "orgPartyId", type = "String", mode = "INOUT"),
            @Attribute(name = "topGlAccountId", type = "String", mode = "INOUT")
        },
        permissionService = @PermissionService(service = "partyBasePermissionCheck", mainAction = "CREATE")
    )
    public interface ImportDatevContacts {}

    /**
     * Exports transactions entries in Datev format to a csv
     */
    @Service(
        name = "exportDatevTransactionEntries",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.external.datev.DatevServices",
        invoke = "exportDatevTransactionEntries",
        description = "Exports transactions entries in Datev format to a csv",
        auth = "true",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "transactionEntries", type = "java.nio.ByteBuffer", mode = "OUT")
        },
        permissionService = @PermissionService(service = "acctgTransactionPermissionCheck", mainAction = "CREATE")
    )
    public interface ExportDatevTransactionEntries {}

}
