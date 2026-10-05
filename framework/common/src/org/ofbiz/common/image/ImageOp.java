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

import java.util.Map;

/**
 * SCIPIO: Image operation base interface (scaling, etc.).
 * Added 2017-07-10.
 */
public interface ImageOp {

    String getName();

    Map<String, Object> getConfiguredOptions();
    Map<String, Object> getDefaultOptions();
    Map<String, Object> getConfiguredAndDefaultOptions();

    /**
     * Returns a new Map with options parsed in the format recognized by
     * this ImageOp. In other words, converts strings to numbers, discards unrecognized, etc.
     * <p>
     * This is a convenience method for {@link #getFactory()} + {@link ImageOpFactory#makeValidOptions(Map)}.
     */
    Map<String, Object> makeValidOptions(Map<String, Object> options);

    /**
     * Returns the factory that created this instance.
     * NOTE: best to avoid this in client code; usually requires casting.
     */
    ImageOpFactory<?> getFactory();

    // DEV NOTE: The T parameter is intended to be a sub-interface like ImageScaler, NOT an implementing or abstract class.
    public interface ImageOpFactory<T extends ImageOp> {
        /**
         * Returns new ImageOp instance.
         * The defaultScalingOptions may be in non-validated format (e.g. strings instead of ints, where applicable).
         */
        T getImageOpInst(String name, Map<String, Object> defaultOptions);

        /**
         * Returns new ImageOp instance.
         * The defaultScalingOptions must be in validated format and types, in other words
         * must have been processed by {@link #makeValidOptions(Map)}.
         */
        T getImageOpInstStrict(String name, Map<String, Object> defaultOptions);

        /**
         * Derives or extends the given op, usually simply replacing its default options.
         * The defaultScalingOptions may be in non-validated format.
         */
        T getDerivedImageOpInst(String name, Map<String, Object> defaultOptions, ImageOp other);

        /**
         * Returns a new Map with options parsed in the format recognized by
         * this ImageOp. In other words, converts strings to numbers, discards unrecognized, etc.
         */
        Map<String, Object> makeValidOptions(Map<String, Object> options);

        Map<String, Object> getDefaultOptions();
    }
}
