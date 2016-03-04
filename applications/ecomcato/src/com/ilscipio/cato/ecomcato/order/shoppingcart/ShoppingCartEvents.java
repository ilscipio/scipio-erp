package com.ilscipio.cato.ecomcato.order.shoppingcart;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.order.shoppingcart.ShoppingCartItem;
import org.ofbiz.product.catalog.CatalogWorker;

public final class ShoppingCartEvents {

    private ShoppingCartEvents() {
    }
    
    /**
     * Ensures the catalog ID is stored in session attributes.
     */
    public static String ensureCurrentCatalogId(HttpServletRequest request, HttpServletResponse response) {
        //String catalogId = CatalogWorker.getCurrentCatalogId(request);
        CatalogWorker.getCurrentCatalogId(request);
        return "success";
    }
    
    
    /** 
     * Customized additem event method, demonstrating necessity of Cato View Parameters.
     */
    public static String addToCart(HttpServletRequest request, HttpServletResponse response) {
        String addResult = org.ofbiz.order.shoppingcart.ShoppingCartEvents.addToCart(request, response);
        
        /*
         * Here is a Cato example of complex data type passing from controller events
         * over to CMS rendering. This is done using (Serializable) Cato View Parameters.
         * 
         * Here we store a complex list of maps of simple types, all of which are Serializable,
         * including the List and Map themselves - they all must be to pass the CMS invocation barrier.
         * For demonstration we build a list of item info found in the cart after a successful
         * cart update. We store it in the session (the same would be needed if stored in request or application attributes).
         * 
         * This list cannot be passed automatically to the CMS invocation, and the Cato Request Maps
         * application must be used to specify this request attribute by name for passing over.
         * Everything passed to the CMS must be serializable and it is our job to ensure this here;
         * once named as a View Parameter, Cato will simply attempt to serialize it and pass it to the CMS.
         * To ensure this here we simply stick to simple Java types and collections for now.
         */
        
        if (!"error".equals(addResult)) {
            ShoppingCart cart = org.ofbiz.order.shoppingcart.ShoppingCartEvents.getCartObject(request);
            
            List<Map<String, Object>> demoAddToCartItems = new ArrayList<Map<String, Object>>(cart.size());
            for(ShoppingCartItem item : cart.items()) {
                Map<String, Object> itemMap = new HashMap<String, Object>();
                
                itemMap.put("productId", item.getProductId());
                itemMap.put("name", item.getName());
                
                demoAddToCartItems.add(itemMap);
            }
            
            request.getSession().setAttribute("demoAddToCartItems", demoAddToCartItems);
            
            Map<String, Object> demoAddToCartInfo = new HashMap<String, Object>();
            
            demoAddToCartInfo.put("webSiteId", cart.getWebSiteId());
            demoAddToCartInfo.put("quoteId", cart.getQuoteId());
            demoAddToCartInfo.put("size", cart.size());
            demoAddToCartInfo.put("currency", cart.getCurrency());
            
            request.getSession().setAttribute("demoAddToCartInfo", demoAddToCartInfo);
        }
        
        return addResult;
    }
    
}
