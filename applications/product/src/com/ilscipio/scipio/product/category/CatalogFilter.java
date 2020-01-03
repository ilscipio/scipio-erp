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
