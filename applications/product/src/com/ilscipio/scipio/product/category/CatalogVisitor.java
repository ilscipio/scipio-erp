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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.product.category.CatalogTraverser.TraversalState;

/**
 * Versatile visitor interface for {@link CatalogTraverser}.
 * @see CatalogTraverser
 */
public interface CatalogVisitor {

    void pushCategory(GenericValue productCategory, TraversalState state) throws GeneralException;

    void popCategory(GenericValue productCategory, TraversalState state) throws GeneralException;

    void visitCategory(GenericValue productCategory, TraversalState state) throws GeneralException;

    void visitProduct(GenericValue product, TraversalState state) throws GeneralException;

    public static abstract class AbstractCatalogVisitor implements CatalogVisitor {
        @Override public void pushCategory(GenericValue productCategory, TraversalState state) throws GeneralException { ; }
        @Override public void popCategory(GenericValue productCategory, TraversalState state) throws GeneralException { ; }
        @Override public void visitCategory(GenericValue productCategory, TraversalState state) throws GeneralException { ; }
        @Override public void visitProduct(GenericValue product, TraversalState state) throws GeneralException { ; }
    }

    public static class LoggingCatalogVisitor extends AbstractCatalogVisitor implements Serializable {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

        protected List<String> trailIds = new ArrayList<>();
        protected String lastId = null;


        @Override
        public void pushCategory(GenericValue productCategory, TraversalState state) {
            trailIds.add(productCategory.getString("productCategoryId"));
        }

        @Override
        public void popCategory(GenericValue productCategory, TraversalState state) {
            trailIds.remove(trailIds.size() - 1);
        }

        @Override
        public void visitCategory(GenericValue productCategory, TraversalState state) {
            Debug.logInfo(getTrailPrefix() + productCategory.get("productCategoryId") + " [category]", module);
        }

        @Override
        public void visitProduct(GenericValue product, TraversalState state) {
            Debug.logInfo(getTrailPrefix() + product.get("productId") + " [product]", module);
        }

        protected String getTrailPrefix() {
            if (trailIds.isEmpty()) return "/";
            else return "/" + StringUtils.join(trailIds, "/") + "/";
        }
    }
}