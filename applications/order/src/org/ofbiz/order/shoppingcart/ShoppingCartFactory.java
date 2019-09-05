package org.ofbiz.order.shoppingcart;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.product.store.ProductStoreWorker;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import java.io.Serializable;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

/**
 * SCIPIO: Cart factory to replace hardcoded ShoppingCart and WebShoppingCart constructors.
 * <p>
 * To use this class, create a file named <code>orderstoreconfig.properties</code> in your component's config folder
 * and the following entry using your store's ID (the default factory class is given here):
 * <ul>
 * <li>Per-store config: <code>store.PRODUCT_STORE_ID.cart.factoryClass=org.ofbiz.order.shoppingcart.ShoppingCartFactory$DefaultShoppingCartFactory</code></li>
 * <li>Global default config: <code>store.DEFAULT.cart.factoryClass=org.ofbiz.order.shoppingcart.ShoppingCartFactory$DefaultShoppingCartFactory</code></li>
 * </ul>
 */
public abstract class ShoppingCartFactory implements Serializable {

    public static final String ORDER_STORE_CONFIG_PROPFILE = "orderstoreconfig";

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final Map<String, ShoppingCartFactory> storeIdCache = readInstances();
    private static final ShoppingCartFactory defaultFactory = readDefaultInstance(storeIdCache);

    public static ShoppingCartFactory get(String productStoreId) {
        ShoppingCartFactory factory = storeIdCache.get(productStoreId);
        return (factory != null) ? factory : defaultFactory;
    }

    public static ShoppingCartFactory get(HttpServletRequest request) {
        return get(ProductStoreWorker.getProductStoreId(request));
    }

    /**
     * Full web shopping cart constructor.
     * <p>
     * SCIPIO: NOTE: 2018-11-30: This constructor should ONLY be inside a {@link CartSync#synchronizedSection(HttpServletRequest)}
     * block, because it modifies session variables that must match the cart contents.
     */
    public abstract ShoppingCart createWebShoppingCart(HttpServletRequest request, Locale locale, String currencyUom);

    /**
     * Common web shopping cart constructor.
     * <p>
     * SCIPIO: NOTE: 2018-11-30: This constructor should ONLY be inside a {@link CartSync#synchronizedSection(HttpServletRequest)}
     * block, because it modifies session variables that must match the cart contents.
     */
    public abstract ShoppingCart createWebShoppingCart(HttpServletRequest request);

    /** Creates a new cloned ShoppingCart Object, using legacy (partial) cloning. */
    public abstract ShoppingCart copyWebShoppingCart(ShoppingCart cart);

    /** Creates a new cloned ShoppingCart Object, with option between legacy (partial) and full/exact cloning the whole cart. */
    public abstract ShoppingCart copyWebShoppingCart(ShoppingCart cart, boolean exactCopy);

    /** Performs an exact, deep copy of the cart. Changes to this copy do not affect the main cart. */
    public abstract ShoppingCart copyWebShoppingCart(HttpServletRequest request, Locale locale, String currencyUom);

    /** Creates a new cloned ShoppingCart Object, using legacy (partial) cloning. */
    public abstract ShoppingCart copyShoppingCart(ShoppingCart cart);

    /** Creates a new cloned ShoppingCart Object, with option between legacy (partial) and full/exact cloning the whole cart. */
    public abstract ShoppingCart copyShoppingCart(ShoppingCart cart, boolean exactCopy);

    /** Creates new empty ShoppingCart object. */
    public abstract ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, String webSiteId, Locale locale, String currencyUom, String billToCustomerPartyId, String billFromVendorPartyId);

    /** Creates new empty ShoppingCart object. */
    public abstract ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, String webSiteId, Locale locale, String currencyUom);

    /** Creates a new empty ShoppingCart object. */
    public abstract ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, Locale locale, String currencyUom);

    private static Map<String, ShoppingCartFactory> readInstances() {
        Map<String, ShoppingCartFactory> factoryMap = new HashMap<>();
        Properties props = UtilProperties.readMergedPropertiesFromAllComponents(ORDER_STORE_CONFIG_PROPFILE);
        Map<String, Map<String, String>> configs = new LinkedHashMap<>();
        UtilProperties.extractPropertiesWithPrefixAndId(configs, props, "store.");
        for(Map.Entry<String, Map<String, String>> entry : configs.entrySet()) {
            String productStoreId = entry.getKey();
            String factoryClsName = entry.getValue().get("cart.factoryClass");
            if (UtilValidate.isNotEmpty(factoryClsName)) {
                try {
                    Class<? extends ShoppingCartFactory> factoryCls = (Class<? extends ShoppingCartFactory>) Thread.currentThread().getContextClassLoader().loadClass(factoryClsName);
                    ShoppingCartFactory factory = factoryCls.newInstance();
                    factoryMap.put(productStoreId, factory);
                } catch(Exception e) {
                    Debug.logError("Could not load factory [" + factoryClsName + "] for store [" + productStoreId + "]", module);
                }
            }
        }
        Debug.logInfo("Read shopping cart factories: " + factoryMap, module);
        return factoryMap;
    }

    private static ShoppingCartFactory readDefaultInstance(Map<String, ShoppingCartFactory> storeIdCache) {
        ShoppingCartFactory factory = storeIdCache.get("DEFAULT");
        if (factory == null) {
            factory = DefaultShoppingCartFactory.INSTANCE;
        }
        Debug.logInfo("Read default shopping cart factory: " + factory, module);
        return factory;
    }

    public static abstract class BasicShoppingCartFactory extends ShoppingCartFactory {
        @Override
        public ShoppingCart copyShoppingCart(ShoppingCart cart) {
            return new ShoppingCart(cart);
        }

        @Override
        public ShoppingCart copyShoppingCart(ShoppingCart cart, boolean exactCopy) {
            return new ShoppingCart(cart, exactCopy);
        }

        @Override
        public ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, String webSiteId, Locale locale, String currencyUom, String billToCustomerPartyId, String billFromVendorPartyId) {
            return new ShoppingCart(delegator, productStoreId, webSiteId, locale, currencyUom, billToCustomerPartyId, billFromVendorPartyId);
        }

        @Override
        public ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, String webSiteId, Locale locale, String currencyUom) {
            return new ShoppingCart(delegator, productStoreId, webSiteId, locale, currencyUom);
        }

        @Override
        public ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, Locale locale, String currencyUom) {
            return new ShoppingCart(delegator, productStoreId, locale, currencyUom);
        }
    }

    public static class DefaultShoppingCartFactory extends BasicShoppingCartFactory {
        private static final DefaultShoppingCartFactory INSTANCE = new DefaultShoppingCartFactory();

        @Override
        public ShoppingCart createWebShoppingCart(HttpServletRequest request, Locale locale, String currencyUom) {
            return new WebShoppingCart(request, locale, currencyUom);
        }

        @Override
        public ShoppingCart createWebShoppingCart(HttpServletRequest request) {
            return new WebShoppingCart(request);
        }

        @Override
        public ShoppingCart copyWebShoppingCart(ShoppingCart cart) {
            return new WebShoppingCart(cart);
        }

        @Override
        public ShoppingCart copyWebShoppingCart(ShoppingCart cart, boolean exactCopy) {
            return new WebShoppingCart(cart, exactCopy);
        }

        @Override
        public ShoppingCart copyWebShoppingCart(HttpServletRequest request, Locale locale, String currencyUom) {
            return new WebShoppingCart(request, locale, currencyUom);
        }

    }

}
