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
package org.ofbiz.common.image.scaler;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Map;

import org.ofbiz.common.image.ImageOp;

/**
 * SCIPIO: Simple image scaling interface, to allow to plugin different scaling algorithms
 * from different libraries.
 * Added 2017-07-10.
 */
public interface ImageScaler extends ImageOp {

    BufferedImage scaleImage(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException;

    BufferedImage scaleImage(BufferedImage image, int targetWidth, int targetHeight) throws IOException;

    public interface ImageScalerFactory extends ImageOpFactory<ImageScaler> {
    }
}
