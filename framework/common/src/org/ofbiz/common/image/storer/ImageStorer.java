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
package org.ofbiz.common.image.storer;

import org.ofbiz.common.image.ImageOp;
import org.ofbiz.entity.Delegator;

import java.awt.image.RenderedImage;
import java.io.IOException;
import java.util.Map;

/**
 * Image storer interface, based on ImageIO.write.
 * Configured in imageops.properties.
 * TODO: This should probably implement ImageOp interface if it can be refitted.
 */
public interface ImageStorer extends ImageOp {

    boolean write(RenderedImage im, String formatName, Object output, String imageProfile, Map<String, Object> options, Delegator delegator) throws IOException;

    boolean isApplicable(RenderedImage im, String formatName, Object output, String imageProfile, Map<String, Object> options, Delegator delegator);

    interface ImageStorerFactory extends ImageOp.ImageOpFactory<ImageStorer> {
    }

}
