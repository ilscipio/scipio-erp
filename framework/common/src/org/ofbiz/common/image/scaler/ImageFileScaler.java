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

import org.ofbiz.common.image.ImageOp;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Map;

/**
 * Simple file-encoded image scaling interface for image scalers, to allow to plugin different scaling algorithms
 * from different libraries (SCIPIO).
 * TODO: not yet implemented, requires a separate integration from ImageScaler that uses exclusively BufferedImage
 *  OR integrate this into ImageScaler as new methods with abstract defaults.
 */
public interface ImageFileScaler extends ImageOp {

    /** Scales a full encoded image (file path, File or byte[] array) to byte array. */
    byte[] scaleImageFileToBytes(Object image, String targetFormat, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException;

    /** Scales a full encoded image (file path, File or byte[] array) to file storage (file path, File). */
    Object scaleImageFileToFile(Object image, Object targetFile, String targetFormat, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException;

    interface ImageFileScalerFactory extends ImageOpFactory<ImageFileScaler> {
    }
}
