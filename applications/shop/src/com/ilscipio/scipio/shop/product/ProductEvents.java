package com.ilscipio.scipio.shop.product;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.product.category.CatalogUrlFilter;
import org.ofbiz.product.category.CatalogUrlServlet;
import org.ofbiz.product.product.ProductSearchSession;
import org.ofbiz.product.product.ProductSearchSession.DefaultKeywordOverrideHandler;

/**
 * SCIPIO: Shop product events.
 * Added 2018-10-18.
 */
public class ProductEvents {

    protected ProductEvents() {
    }

    /** A ControlServlet event method used to check to see if there is an override for any of the current keywords in the search */
    public static final String checkDoKeywordOverride(HttpServletRequest request, HttpServletResponse response) {
        return ProductSearchSession.checkDoKeywordOverride(request, response, ShopKeywordOverrideHandler.INSTANCE);
    }

    /**
     * SCIPIO: Overrides the keyword handling to build proper catalog and product links.
     */
    public static class ShopKeywordOverrideHandler extends DefaultKeywordOverrideHandler {
        public static final ShopKeywordOverrideHandler INSTANCE = new ShopKeywordOverrideHandler();

        @Override
        public String handleCategoryLink(HttpServletRequest request, HttpServletResponse response, 
                String target, String keyword, GenericValue productStoreKeywordOvrd) {
            return CatalogUrlServlet.makeCatalogLink(request, response, UtilHttp.getLocale(request), null, target, null, null,
                    null, null, null, null);
        }

        @Override
        public String handleProductLink(HttpServletRequest request, HttpServletResponse response, 
                String target, String keyword, GenericValue productStoreKeywordOvrd) {
            return CatalogUrlFilter.makeCatalogAltLink(request, response, UtilHttp.getLocale(request), null, target, null, null, null,
                    null, null, null, null, null, null, null);
        }
    }
}
