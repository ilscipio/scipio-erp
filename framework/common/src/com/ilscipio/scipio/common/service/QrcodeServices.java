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
public class QrcodeServices {

    /**
     * Generate a QRCode image according to 
     */
    @Service(
        name = "generateQRCodeImage",
        location = "org.ofbiz.common.qrcode.QRCodeServices",
        invoke = "generateQRCodeImage",
        description = "Generate a QRCode image according to ",
        requireNewTransaction = "true",
        maxRetry = "3",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN"),
            @Attribute(name = "format", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "height", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "width", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "encoding", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "logoImage", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "logoImageMaxWidth", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "logoImageMaxHeight", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "verifyOutput", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "bufferedImage", type = "java.awt.image.BufferedImage", mode = "OUT", optional = "true"),
            @Attribute(name = "useLogo", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "SCIPIO: Explicit logo enable (stock did not permit false). Default: true"),
            @Attribute(name = "ecLevel", type = "String", mode = "IN", optional = "true", description = "SCIPIO: Error correction level. Possible values: L, M, Q, H. Default: from qrcode.properties"),
            @Attribute(name = "logoImageSize", type = "String", mode = "IN", optional = "true", description = "SCIPIO: Logo image target size. Supports formats such as: 100%, 50%, 100x100, 80%x80%\n                NOTE: Aspect ratio is never changed; two dimensions mean two limits."),
            @Attribute(name = "logoImageMaxSize", type = "String", mode = "IN", optional = "true", description = "SCIPIO: Logo image max size. Supports formats such as: 100%, 50%, 100x100, 80%x80%\n                NOTE: Aspect ratio is never changed; two dimensions mean two limits."),
            @Attribute(name = "scalingOptions", type = "Map", mode = "IN", optional = "true", description = "SCIPIO: Scaling options; scalerName can name a scaler from imageops.properties.")
        }
    )
    public interface GenerateQRCodeImage {}

}
