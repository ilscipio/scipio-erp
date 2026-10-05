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
package com.ilscipio.scipio.securityext.service;

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
     * Import an x.509 certificate into a defined keystore and create the provision data
     */
    @Service(
        name = "importIssuerProvision",
        engine = "java",
        location = "org.ofbiz.securityext.cert.CertificateServices",
        invoke = "importIssuerCertificate",
        description = "Import an x.509 certificate into a defined keystore and create the provision data",
        auth = "true",
        attributes = {
            @Attribute(name = "componentName", type = "String", mode = "IN"),
            @Attribute(name = "keystoreName", type = "String", mode = "IN"),
            @Attribute(name = "certString", type = "String", mode = "IN"),
            @Attribute(name = "importIssuer", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "alias", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "securityPermissionCheck", mainAction = "CREATE")
    )
    public interface ImportIssuerProvision {}

}
