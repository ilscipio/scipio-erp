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
public class EmailServices {

    /**
     * Interface service for mail services.  contentType defaults to "text/html", sendType defaults to             "mail.smtp.host".  sendVia must be specified if sendType is different.  Configured in general.properties
     */
    @Service(
        name = "sendMailInterface",
        engine = "interface",
        description = "Interface service for mail services.  contentType defaults to \"text/html\", sendType defaults to\n            \"mail.smtp.host\".  sendVia must be specified if sendType is different.  Configured in general.properties",
        attributes = {
            @Attribute(name = "sendTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sendCc", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sendBcc", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sendFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authUser", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authPass", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "port", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sendVia", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sendType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "socketFactoryClass", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "socketFactoryPort", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "socketFactoryFallback", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sendFailureNotification", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "sendPartial", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "startTLSEnabled", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "replyTo", type = "java.util.List", mode = "IN", optional = "true"),
            @Attribute(name = "allowCustomHeaders", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "customHeaders", type = "java.util.Map", mode = "IN", optional = "true"),
            @Attribute(name = "sendAs", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "storeName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subject", type = "String", mode = "INOUT", optional = "true", allowHtml = "any"),
            @Attribute(name = "contentType", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "messageId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "emailType", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "custRequestId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "messageWrapper", type = "org.ofbiz.service.mail.MimeMessageWrapper", mode = "OUT", optional = "true"),
            @Attribute(name = "communicationEventId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface SendMailInterface {}

    /**
     * Interface service for sendMail* services.
     */
    @Service(
        name = "sendMailOnePartInterface",
        engine = "interface",
        description = "Interface service for sendMail* services.",
        implemented = {@Implements(service = "sendMailInterface")},
        attributes = {
            @Attribute(name = "body", type = "String", mode = "INOUT", allowHtml = "any")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentType", mode = "INOUT"),
            @OverrideAttribute(name = "subject", mode = "INOUT", optional = "false"),
            @OverrideAttribute(name = "emailType", type = "String", mode = "INOUT", optional = "true"),
            @OverrideAttribute(name = "sendTo", optional = "false")
        }
    )
    public interface SendMailOnePartInterface {}

    /**
     * Interface service for sendMailMultiPart* services
     */
    @Service(
        name = "sendMailMultiPartInterface",
        engine = "interface",
        description = "Interface service for sendMailMultiPart* services",
        implemented = {@Implements(service = "sendMailInterface")},
        attributes = {
            @Attribute(name = "bodyParts", type = "java.util.List", mode = "INOUT"),
            @Attribute(name = "subject", type = "String", mode = "INOUT")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentType", mode = "INOUT")
        }
    )
    public interface SendMailMultiPartInterface {}

    /**
     * Send E-Mail Service.  partyId and communicationEventId aren't used by sendMail             but are passed down to storeEmailAsCommunication during the SECA chain.  See sendMailInterface for more comments.
     */
    @Service(
        name = "sendMail",
        location = "org.ofbiz.common.email.EmailServices",
        invoke = "sendMail",
        description = "Send E-Mail Service.  partyId and communicationEventId aren't used by sendMail\n            but are passed down to storeEmailAsCommunication during the SECA chain.  See sendMailInterface for more comments.",
        implemented = {@Implements(service = "sendMailOnePartInterface")}
    )
    public interface SendMail {}

    /**
     * Send E-Mail Service.  partyId and communicationEventId aren't used by sendMail             but are passed down to storeEmailAsCommunication during the SECA chain.  See sendMailInterface for more comments.
     */
    @Service(
        name = "sendMailHiddenInLog",
        location = "org.ofbiz.common.email.EmailServices",
        invoke = "sendMail",
        description = "Send E-Mail Service.  partyId and communicationEventId aren't used by sendMail\n            but are passed down to storeEmailAsCommunication during the SECA chain.  See sendMailInterface for more comments.",
        hideResultInLog = "true",
        implemented = {@Implements(service = "sendMailOnePartInterface")},
        attributes = {
            @Attribute(name = "hideInLog", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface SendMailHiddenInLog {}

    /**
     * Send Multi-Part E-Mail Service
     */
    @Service(
        name = "sendMailMultiPart",
        location = "org.ofbiz.common.email.EmailServices",
        invoke = "sendMail",
        description = "Send Multi-Part E-Mail Service",
        implemented = {@Implements(service = "sendMailMultiPartInterface")}
    )
    public interface SendMailMultiPart {}

    /**
     * Send Multi-Part E-Mail Service
     */
    @Service(
        name = "sendMailMultiPartHiddenInLog",
        location = "org.ofbiz.common.email.EmailServices",
        invoke = "sendMail",
        description = "Send Multi-Part E-Mail Service",
        hideResultInLog = "true",
        implemented = {@Implements(service = "sendMailMultiPartInterface")}
    )
    public interface SendMailMultiPartHiddenInLog {}

    /**
     * Send E-Mail From URL Service
     */
    @Service(
        name = "sendMailFromUrl",
        location = "org.ofbiz.common.email.EmailServices",
        invoke = "sendMailFromUrl",
        description = "Send E-Mail From URL Service",
        implemented = {@Implements(service = "sendMailInterface")},
        attributes = {
            @Attribute(name = "bodyUrl", type = "String", mode = "IN"),
            @Attribute(name = "bodyUrlParameters", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "body", type = "String", mode = "OUT", allowHtml = "any")
        }
    )
    public interface SendMailFromUrl {}

    /**
     * Interface service for E-Mail sent From Screen Widget
     */
    @Service(
        name = "sendMailFromScreenInterface",
        engine = "interface",
        description = "Interface service for E-Mail sent From Screen Widget",
        implemented = {@Implements(service = "sendMailInterface")},
        attributes = {
            @Attribute(name = "bodyText", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "bodyScreenUri", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "xslfoAttachScreenLocation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "attachmentName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "xslfoAttachScreenLocationList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "attachmentNameList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "bodyParameters", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true", description = "\n                The webSiteId of the WebSite this email should appear to be from, or\n                in other words - from the point of view of the screen - the \"current\" WebSite.\n                Used for building links to the web site, themeing, etc.\n                NOTE: This is REQUIRED for store frontend emails (WARNING: Cannot be enforced! Caller must ensure).\n                Backend emails can leave this empty.\n            "),
            @Attribute(name = "autoInferParams", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "\n                SCIPIO: If true (default), allows sendMailFromScreen implementations to automatically try\n                to determine common services parameters and bodyParameters from each other.\n                2019-02-04: This is now enabled by default and done by the Scipio sendMailFromScreen/sendMailFromScreenExt\n                override in commonext component; it simply tries to determine the following fields from each other and\n                put them in both service context and the bodyParameters: webSiteId, productStoreId, orderId.\n                This is catch-all measure to prevent a number of bugs.\n            "),
            @Attribute(name = "subject", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "body", type = "String", mode = "OUT")
        }
    )
    public interface SendMailFromScreenInterface {}

    /**
     * Send E-Mail From Screen Widget Service -             SCIPIO: 2019-02-01: This service is now overridden in the CommonExt component             for extended webSiteId/productStoreId/orderId handling; if you need the             original behavior, call sendMailFromScreenStd instead.
     */
    @Service(
        name = "sendMailFromScreen",
        location = "org.ofbiz.common.email.EmailServices",
        invoke = "sendMailFromScreen",
        description = "Send E-Mail From Screen Widget Service -\n            SCIPIO: 2019-02-01: This service is now overridden in the CommonExt component\n            for extended webSiteId/productStoreId/orderId handling; if you need the\n            original behavior, call sendMailFromScreenStd instead.",
        maxRetry = "3",
        implemented = {@Implements(service = "sendMailFromScreenInterface")},
        attributes = {
            @Attribute(name = "hideInLog", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface SendMailFromScreen {}

    /**
     * Send E-Mail From Screen Widget Service -              SCIPIO: This invokes the original implementation of the sendMailFromScreen service,             without the supplemental webSiteId/productStoreId/orderId processing.
     */
    @Service(
        name = "sendMailFromScreenStd",
        location = "org.ofbiz.common.email.EmailServices",
        invoke = "sendMailFromScreen",
        description = "Send E-Mail From Screen Widget Service - \n            SCIPIO: This invokes the original implementation of the sendMailFromScreen service,\n            without the supplemental webSiteId/productStoreId/orderId processing.",
        maxRetry = "3",
        implemented = {@Implements(service = "sendMailFromScreenInterface")},
        attributes = {
            @Attribute(name = "hideInLog", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface SendMailFromScreenStd {}

    /**
     * Send E-Mail hidden in log (password, etc.) From Screen Widget Service
     */
    @Service(
        name = "sendMailHiddenInLogFromScreen",
        location = "org.ofbiz.common.email.EmailServices",
        invoke = "sendMailHiddenInLogFromScreen",
        description = "Send E-Mail hidden in log (password, etc.) From Screen Widget Service",
        maxRetry = "3",
        hideResultInLog = "true",
        implemented = {@Implements(service = "sendMailFromScreenInterface")}
    )
    public interface SendMailHiddenInLogFromScreen {}

    /**
     * Send Email From Email Template Setting Service
     */
    @Service(
        name = "sendMailFromTemplateSetting",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/email/EmailServices.xml",
        invoke = "sendMailFromTemplateSetting",
        description = "Send Email From Email Template Setting Service",
        implemented = {@Implements(service = "sendMailInterface")},
        attributes = {
            @Attribute(name = "emailTemplateSettingId", type = "String", mode = "IN"),
            @Attribute(name = "partyIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "bodyText", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "attachmentName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "bodyParameters", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "body", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SendMailFromTemplateSetting {}

    /**
     * Send Template Based Notification Service
     */
    @Service(
        name = "prepareNotificationInterface",
        engine = "interface",
        description = "Send Template Based Notification Service",
        implemented = {@Implements(service = "sendMailInterface")},
        attributes = {
            @Attribute(name = "body", type = "String", mode = "INOUT", optional = "true", allowHtml = "any"),
            @Attribute(name = "baseUrl", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "templateName", type = "String", mode = "IN"),
            @Attribute(name = "templateData", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PrepareNotificationInterface {}

    /**
     * Send Template Based Notification Service
     */
    @Service(
        name = "sendNotificationInterface",
        engine = "interface",
        description = "Send Template Based Notification Service",
        implemented = {@Implements(service = "prepareNotificationInterface")},
        attributes = {
            @Attribute(name = "body", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "templateName", type = "String", mode = "IN"),
            @Attribute(name = "templateData", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SendNotificationInterface {}

    /**
     * Generic Template Based Notification Service
     */
    @Service(
        name = "sendGenericNotificationEmail",
        location = "org.ofbiz.common.email.NotificationServices",
        invoke = "sendNotification",
        description = "Generic Template Based Notification Service",
        implemented = {@Implements(service = "sendNotificationInterface")}
    )
    public interface SendGenericNotificationEmail {}

    /**
     * Create a EmailTemplateSetting record
     */
    @Service(
        name = "createEmailTemplateSetting",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a EmailTemplateSetting record",
        defaultEntityName = "EmailTemplateSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateEmailTemplateSetting {}

    /**
     * Update a EmailTemplateSetting record
     */
    @Service(
        name = "updateEmailTemplateSetting",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a EmailTemplateSetting record",
        defaultEntityName = "EmailTemplateSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateEmailTemplateSetting {}

    /**
     * Delete a EmailTemplateSetting record
     */
    @Service(
        name = "deleteEmailTemplateSetting",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a EmailTemplateSetting record",
        defaultEntityName = "EmailTemplateSetting",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteEmailTemplateSetting {}

}
