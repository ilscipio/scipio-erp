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
package com.ilscipio.scipio.commonext.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Services {

    /**
     * Create a system info note
     */
    @Service(
        name = "createSystemInfoNote",
        engine = "simple",
        location = "component://commonext/script/org/ofbiz/SystemInfoServices.xml",
        invoke = "createSystemInfoNote",
        description = "Create a system info note",
        defaultEntityName = "NoteData",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateSystemInfoNote {}

    /**
     * Delete a system info note
     */
    @Service(
        name = "deleteSystemInfoNote",
        engine = "simple",
        location = "component://commonext/script/org/ofbiz/SystemInfoServices.xml",
        invoke = "deleteSystemInfoNote",
        description = "Delete a system info note",
        defaultEntityName = "NoteData",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSystemInfoNote {}

    /**
     * Delete all system notes for the logged on party
     */
    @Service(
        name = "deleteAllSystemNotes",
        engine = "simple",
        location = "component://commonext/script/org/ofbiz/SystemInfoServices.xml",
        invoke = "deleteAllSystemNotes",
        description = "Delete all system notes for the logged on party",
        auth = "true"
    )
    public interface DeleteAllSystemNotes {}

    /**
     * Get system notes for the logged on party
     */
    @Service(
        name = "getSystemInfoNotes",
        engine = "simple",
        location = "component://commonext/script/org/ofbiz/SystemInfoServices.xml",
        invoke = "getSystemInfoNotes",
        description = "Get system notes for the logged on party",
        auth = "true",
        attributes = {
            @Attribute(name = "viewIndex", type = "Integer", mode = "INOUT", optional = "true"),
            @Attribute(name = "viewSize", type = "Integer", mode = "INOUT", optional = "true"),
            @Attribute(name = "showAll", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "systemInfoNotes", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetSystemInfoNotes {}

    /**
     * Get last system note for the logged on party
     */
    @Service(
        name = "getLastSystemInfoNote",
        engine = "simple",
        location = "component://commonext/script/org/ofbiz/SystemInfoServices.xml",
        invoke = "getLastSystemInfoNote",
        description = "Get last system note for the logged on party",
        attributes = {
            @Attribute(name = "lastSystemInfoNote1", type = "GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "lastSystemInfoNote2", type = "GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "lastSystemInfoNote3", type = "GenericValue", mode = "OUT", optional = "true")
        }
    )
    public interface GetLastSystemInfoNote {}

    /**
     * Get system status for the logged on party
     */
    @Service(
        name = "getSystemInfoStatus",
        engine = "simple",
        location = "component://commonext/script/org/ofbiz/SystemInfoServices.xml",
        invoke = "getSystemInfoStatus",
        description = "Get system status for the logged on party",
        auth = "true",
        attributes = {
            @Attribute(name = "systemInfoStatus", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetSystemInfoStatus {}

    /**
     * Get system messages for an authenticated party
     */
    @Service(
        name = "getSystemMessages",
        engine = "simple",
        location = "component://commonext/script/org/ofbiz/SystemInfoServices.xml",
        invoke = "getSystemMessages",
        description = "Get system messages for an authenticated party",
        auth = "true",
        attributes = {
            @Attribute(name = "toPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "typeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "viewIndex", type = "Integer", mode = "INOUT", optional = "true"),
            @Attribute(name = "viewSize", type = "Integer", mode = "INOUT", optional = "true"),
            @Attribute(name = "showAll", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "messages", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "count", type = "Long", mode = "OUT", optional = "true")
        }
    )
    public interface GetSystemMessages {}

    /**
     * Get system messages for an authenticated party
     */
    @Service(
        name = "convertSystemMessageFromNoteData",
        engine = "simple",
        location = "component://commonext/script/org/ofbiz/SystemInfoServices.xml",
        invoke = "convertSystemMessageFromNoteData",
        description = "Get system messages for an authenticated party",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "NoteData", mode = "IN", optional = "true")
        }
    )
    public interface ConvertSystemMessageFromNoteData {}

    /**
     * SCIPIO: Send E-Mail From Screen Widget Service, application-level enhanced             This version provides improved webSiteId/productStoreId/orderId handling.
     */
    @Service(
        name = "sendMailFromScreenExt",
        engine = "java",
        location = "org.ofbiz.commonext.email.ExtEmailServices",
        invoke = "sendMailFromScreen",
        description = "SCIPIO: Send E-Mail From Screen Widget Service, application-level enhanced\n            This version provides improved webSiteId/productStoreId/orderId handling.",
        maxRetry = "3",
        implemented = {@Implements(service = "sendMailFromScreenStd")}
    )
    public interface SendMailFromScreenExt {}

    /**
     * SCIPIO: Send E-Mail hidden in log (password, etc.) From Screen Widget Service, application-level enhanced             This version provides improved webSiteId/productStoreId/orderId handling.
     */
    @Service(
        name = "sendMailHiddenInLogFromScreenExt",
        engine = "java",
        location = "org.ofbiz.commonext.email.ExtEmailServices",
        invoke = "sendMailHiddenInLogFromScreen",
        description = "SCIPIO: Send E-Mail hidden in log (password, etc.) From Screen Widget Service, application-level enhanced\n            This version provides improved webSiteId/productStoreId/orderId handling.",
        maxRetry = "3",
        implemented = {@Implements(service = "sendMailFromScreenInterface")}
    )
    public interface SendMailHiddenInLogFromScreenExt {}

    /**
     * SCIPIO: Send E-Mail From Screen Widget Service, application-level enhanced (CommonExt override)             This version provides improved webSiteId/productStoreId/orderId handling.
     */
    @Service(
        name = "sendMailFromScreen",
        engine = "java",
        location = "org.ofbiz.commonext.email.ExtEmailServices",
        invoke = "sendMailFromScreen",
        description = "SCIPIO: Send E-Mail From Screen Widget Service, application-level enhanced (CommonExt override)\n            This version provides improved webSiteId/productStoreId/orderId handling.",
        maxRetry = "3",
        implemented = {@Implements(service = "sendMailFromScreenExt")}
    )
    public interface SendMailFromScreen {}

    /**
     * SCIPIO: Send E-Mail hidden in log (password, etc.) From Screen Widget Service, application-level enhanced (CommonExt override)             This version provides improved webSiteId/productStoreId/orderId handling.
     */
    @Service(
        name = "sendMailHiddenInLogFromScreen",
        engine = "java",
        location = "org.ofbiz.commonext.email.ExtEmailServices",
        invoke = "sendMailHiddenInLogFromScreen",
        description = "SCIPIO: Send E-Mail hidden in log (password, etc.) From Screen Widget Service, application-level enhanced (CommonExt override)\n            This version provides improved webSiteId/productStoreId/orderId handling.",
        maxRetry = "3",
        implemented = {@Implements(service = "sendMailHiddenInLogFromScreenExt")}
    )
    public interface SendMailHiddenInLogFromScreen {}

}
