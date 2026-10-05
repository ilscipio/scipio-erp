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
package com.ilscipio.scipio.product.category;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.GenericValue;

import java.io.Serializable;

/**
 * Used by *some* implementations of CatalogVisitor (optional support) - through {@link CatalogTraverser} to filter out categories and products.
 * <p>
 * Known supported by:
 * <ul>
 * <li>{@link com.ilscipio.scipio.product.seo.sitemap.SitemapGenerator}</li>
 * </ul>
 */
public interface CatalogFilter {

    /**
     * Returns true if category should be included; false to exclude.
     */
    default boolean filterCategory(GenericValue productCategory, CatalogTraverser.TraversalState state) throws GeneralException { return true; }

    /**
     * Returns false if product should be included; false to exclude.
     */
    default boolean filterProduct(GenericValue product, CatalogTraverser.TraversalState state) throws GeneralException { return true; }

}
