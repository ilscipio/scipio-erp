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
import java.util.Collections;
import java.util.Set;

/**
 * Common filters, specific implementations.
 */
public class CatalogFilters {

    public static class AllowAllFilter implements CatalogFilter, Serializable { // trivial filter
        private static final AllowAllFilter INSTANCE = new AllowAllFilter();
        public static AllowAllFilter getInstance() { return INSTANCE; }

        @Override
        public boolean filterCategory(GenericValue productCategory, CatalogTraverser.TraversalState state) throws GeneralException {
            return true;
        }

        @Override
        public boolean filterProduct(GenericValue product, CatalogTraverser.TraversalState state) throws GeneralException {
            return true;
        }
    }

    public static class LoggingFilter implements CatalogFilter, Serializable {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
        private static final LoggingFilter INSTANCE = new LoggingFilter();
        public static LoggingFilter getInstance() { return INSTANCE; }

        @Override
        public boolean filterCategory(GenericValue productCategory, CatalogTraverser.TraversalState state) throws GeneralException {
            Debug.logInfo("Allowing category: " + productCategory.get("productCategoryId"), module);
            return true;
        }

        @Override
        public boolean filterProduct(GenericValue product, CatalogTraverser.TraversalState state) throws GeneralException {
            Debug.logInfo("Allowing product: " + product.get("productId"), module);
            return true;
        }
    }

    public static class ViewAllowCategoryProductFilter implements CatalogFilter, Serializable {
        private static final ViewAllowCategoryProductFilter INSTANCE = new ViewAllowCategoryProductFilter();
        public static ViewAllowCategoryProductFilter getInstance() { return INSTANCE; }

        @Override
        public boolean filterProduct(GenericValue product, CatalogTraverser.TraversalState state) throws GeneralException {
            return state.getTarverser().isViewAllowProduct(product);
        }
    }

    public static class ExcludeVariantsProductFilter implements CatalogFilter, Serializable {
        private static final ExcludeVariantsProductFilter INSTANCE = new ExcludeVariantsProductFilter();
        public static ExcludeVariantsProductFilter getInstance() { return INSTANCE; }

        @Override
        public boolean filterProduct(GenericValue product, CatalogTraverser.TraversalState state) throws GeneralException {
            return !Boolean.TRUE.equals(product.getBoolean("isVariant"));
        }
    }

    public static class ExcludeSpecificCategoryFilter implements CatalogFilter, Serializable {
        protected final Set<String> excludeIds;

        public ExcludeSpecificCategoryFilter(Set<String> excludeIds) {
            this.excludeIds = (excludeIds != null) ? excludeIds : Collections.emptySet();
        }

        @Override
        public boolean filterCategory(GenericValue productCategory, CatalogTraverser.TraversalState state) throws GeneralException {
            return !excludeIds.contains(productCategory.get("productCategoryId"));
        }
    }

    public static class ExcludeSpecificProductFilter implements CatalogFilter, Serializable {
        protected final Set<String> excludeIds;

        public ExcludeSpecificProductFilter(Set<String> excludeIds) {
            this.excludeIds = (excludeIds != null) ? excludeIds : Collections.emptySet();
        }

        @Override
        public boolean filterProduct(GenericValue product, CatalogTraverser.TraversalState state) throws GeneralException {
            return !excludeIds.contains(product.get("productId"));
        }
    }
}
