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
public class EmailServices {

    /**
     * Send E-Mail From Screen Widget Service (CommonExt version) (SCIPIO)             - attempts to auto-determine (autoInferParams) and normalize the webSiteId and productStoreId fields in             service context and bodyParameters, using each other as well as orderId.
     */
    @Service(
        name = "sendMailFromScreenExt",
        engine = "java",
        location = "org.ofbiz.commonext.email.ExtEmailServices",
        invoke = "sendMailFromScreen",
        description = "Send E-Mail From Screen Widget Service (CommonExt version) (SCIPIO)\n            - attempts to auto-determine (autoInferParams) and normalize the webSiteId and productStoreId fields in\n            service context and bodyParameters, using each other as well as orderId.",
        maxRetry = "3",
        implemented = {@Implements(service = "sendMailFromScreenStd")}
    )
    public interface SendMailFromScreenExt {}

    /**
     * Send E-Mail From Screen Widget Service (CommonExt override) (SCIPIO)             - attempts to auto-determine (autoInferParams) and normalize the webSiteId and productStoreId fields in             service context and bodyParameters, using each other as well as orderId.
     */
    @Service(
        name = "sendMailFromScreen",
        engine = "java",
        location = "org.ofbiz.commonext.email.ExtEmailServices",
        invoke = "sendMailFromScreen",
        description = "Send E-Mail From Screen Widget Service (CommonExt override) (SCIPIO)\n            - attempts to auto-determine (autoInferParams) and normalize the webSiteId and productStoreId fields in\n            service context and bodyParameters, using each other as well as orderId.",
        maxRetry = "3",
        implemented = {@Implements(service = "sendMailFromScreenExt")}
    )
    public interface SendMailFromScreen {}

}
