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
package org.ofbiz.common.image;

import java.io.Serializable;

import org.ofbiz.base.lang.ThreadSafe;

/**
 * SCIPIO: Simple width x height image dimensions class for return values.
 * Added 2018-08-23.
 */
@SuppressWarnings("serial")
@ThreadSafe
public class ImageDim<N extends Number> implements Serializable {

    protected final N width;
    protected final N height;

    public ImageDim(N width, N height) {
        this.width = width;
        this.height = height;
    }

    /**
     * @return the width
     */
    public N getWidth() {
        return width;
    }

    /**
     * @return the height
     */
    public N getHeight() {
        return height;
    }

    @Override
    public String toString() {
        return width + "x" + height;
    }
}
