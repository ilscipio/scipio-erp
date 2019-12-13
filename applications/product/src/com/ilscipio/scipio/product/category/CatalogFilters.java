package com.ilscipio.scipio.product.category;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.GenericValue;

import java.io.Serializable;

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
}
