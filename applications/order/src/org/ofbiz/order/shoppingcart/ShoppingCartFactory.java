package org.ofbiz.order.shoppingcart;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.LocalDispatcher;

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
 * NOTE: The actual instance is represented by {@link ShoppingCartFactory.Factory}, due to the static
 * methods being the exact same as the instance methods which will lead to obscure-bug-making-errors
 * if try to put both in same class with slightly different names (this way the compiler will catch everything).
 * <p>
 * To use this class, create a file named <code>orderstoreconfig.properties</code> in your component's config folder
 * and the following entry using your store's ID (the default factory class is given here):
 * <ul>
 * <li>Per-store config: <code>store.PRODUCT_STORE_ID.cart.factoryClass=org.ofbiz.order.shoppingcart.ShoppingCartFactory$DefaultShoppingCartFactory</code></li>
 * <li>Global default config: <code>store.DEFAULT.cart.factoryClass=org.ofbiz.order.shoppingcart.ShoppingCartFactory$DefaultShoppingCartFactory</code></li>
 * </ul>
 */
public abstract class ShoppingCartFactory {

    public static final String ORDER_STORE_CONFIG_PROPFILE = "orderstoreconfig";

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final Map<String, Factory> storeIdCache = readInstances();
    private static final Factory defaultFactory = readDefaultInstance(storeIdCache);

    /** The instance is represented by {@link Factory} nested class. */
    protected ShoppingCartFactory() {}

    public static Factory get(Delegator delegator, String productStoreId) {
        Factory factory = storeIdCache.get(productStoreId);
        return (factory != null) ? factory : defaultFactory;
    }

    public static Factory get(HttpServletRequest request) {
        return get(null, ProductStoreWorker.getProductStoreId(request));
    }

    /** @deprecated Not passing delegator was a bad idea */
    @Deprecated
    public static Factory get(String productStoreId) {
        return get(null, productStoreId);
    }

    private static Map<String, Factory> readInstances() {
        Map<String, Factory> factoryMap = new HashMap<>();
        Properties props = UtilProperties.readMergedPropertiesFromAllComponents(ORDER_STORE_CONFIG_PROPFILE);
        Map<String, Map<String, String>> configs = new LinkedHashMap<>();
        UtilProperties.extractPropertiesWithPrefixAndId(configs, props, "store.");
        for(Map.Entry<String, Map<String, String>> entry : configs.entrySet()) {
            String productStoreId = entry.getKey();
            String factoryClsName = entry.getValue().get("cart.factoryClass");
            if (UtilValidate.isNotEmpty(factoryClsName)) {
                try {
                    Class<? extends Factory> factoryCls = (Class<? extends Factory>) Thread.currentThread().getContextClassLoader().loadClass(factoryClsName);
                    Factory factory = factoryCls.newInstance();
                    factoryMap.put(productStoreId, factory);
                } catch(Exception e) {
                    Debug.logError("Could not load factory [" + factoryClsName + "] for store [" + productStoreId + "]", module);
                }
            }
        }
        Debug.logInfo("Read shopping cart factories: " + factoryMap, module);
        return factoryMap;
    }

    private static Factory readDefaultInstance(Map<String, Factory> storeIdCache) {
        Factory factory = storeIdCache.get("DEFAULT");
        if (factory == null) {
            factory = DefaultFactory.INSTANCE;
        }
        Debug.logInfo("Read default shopping cart factory: " + factory, module);
        return factory;
    }

    // Static standard helper wrapper methods around the Factory methods, to help simplify code
    // (the get() calls are unnecessary to write manually in code in all known cases, because delegator and productStoreId are always already known to the factory methods)

    /** Creates new empty (non-web) ShoppingCart object. */
    public static ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, String webSiteId, Locale locale, String currencyUom, String billToCustomerPartyId, String billFromVendorPartyId) {
        return get(delegator, productStoreId).createShoppingCart(delegator, productStoreId, webSiteId, locale, currencyUom, billToCustomerPartyId, billFromVendorPartyId);
    }

    /** Creates new empty (non-web) ShoppingCart object. */
    public static ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, String webSiteId, Locale locale, String currencyUom) {
        return get(delegator, productStoreId).createShoppingCart(delegator, productStoreId, webSiteId, locale, currencyUom);
    }

    /** Creates a new empty (non-web) ShoppingCart object. */
    public static ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, Locale locale, String currencyUom) {
        return get(delegator, productStoreId).createShoppingCart(delegator, productStoreId, locale, currencyUom);
    }

    /** Creates a new cloned (non-web) ShoppingCart Object, using legacy (partial) cloning. */
    public static ShoppingCart copyShoppingCart(ShoppingCart cart) {
        return get(cart.getDelegator(), cart.getProductStoreId()).copyShoppingCart(cart);
    }

    /** Creates a new cloned (non-web) ShoppingCart Object, with option between legacy (partial) and full/exact cloning the whole cart. */
    public static ShoppingCart copyShoppingCart(ShoppingCart cart, boolean exactCopy) {
        return get(cart.getDelegator(), cart.getProductStoreId()).copyShoppingCart(cart, exactCopy);
    }

    /**
     * Creates a new web ShoppingCart Object - full web shopping cart constructor.
     * <p>
     * SCIPIO: NOTE: 2018-11-30: This constructor should ONLY be inside a {@link CartSync#synchronizedSection(HttpServletRequest)}
     * block, because it modifies session variables that must match the cart contents.
     */
    public static ShoppingCart createWebShoppingCart(HttpServletRequest request, Locale locale, String currencyUom) {
        return get(request).createWebShoppingCart(request, locale, currencyUom);
    }

    /**
     * Creates a new web ShoppingCart Object - common web shopping cart constructor.
     * <p>
     * SCIPIO: NOTE: 2018-11-30: This constructor should ONLY be inside a {@link CartSync#synchronizedSection(HttpServletRequest)}
     * block, because it modifies session variables that must match the cart contents.
     */
    public static ShoppingCart createWebShoppingCart(HttpServletRequest request) {
        return get(request).createWebShoppingCart(request);
    }

    /** Creates a new cloned web ShoppingCart Object, using legacy (partial) cloning. */
    public static ShoppingCart copyWebShoppingCart(ShoppingCart cart) {
        return get(cart.getDelegator(), cart.getProductStoreId()).copyWebShoppingCart(cart);
    }

    /** Creates a new cloned web ShoppingCart Object, with option between legacy (partial) and full/exact cloning the whole cart. */
    public static ShoppingCart copyWebShoppingCart(ShoppingCart cart, boolean exactCopy) {
        return get(cart.getDelegator(), cart.getProductStoreId()).copyWebShoppingCart(cart, exactCopy);
    }

    /** Creates a new cloned web ShoppingCart Object - performs an exact, deep copy of the cart. Changes to this copy do not affect the main cart. */
    public static ShoppingCart copyWebShoppingCart(HttpServletRequest request, Locale locale, String currencyUom) {
        return get(request).copyWebShoppingCart(request, locale, currencyUom);
    }

    /** Creates a new empty (web) ShoppingCartHelper object. */
    public static ShoppingCartHelper createShoppingCartHelper(Delegator delegator, LocalDispatcher dispatcher, ShoppingCart cart) {
        return get(delegator, cart.getProductStoreId()).createShoppingCartHelper(delegator, dispatcher, cart);
    }

    // Instance methods and interfaces

    /** Actual ShoppingCart factory and instance methods (mostly identical to the static methods) */
    public interface Factory extends Serializable {

        /** Creates new empty (non-web) ShoppingCart object. */
        ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, String webSiteId, Locale locale, String currencyUom, String billToCustomerPartyId, String billFromVendorPartyId);

        /** Creates new empty (non-web) ShoppingCart object. */
        ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, String webSiteId, Locale locale, String currencyUom);

        /** Creates a new empty (non-web) ShoppingCart object. */
        ShoppingCart createShoppingCart(Delegator delegator, String productStoreId, Locale locale, String currencyUom);

        /** Creates a new cloned (non-web) ShoppingCart Object, using legacy (partial) cloning. */
        ShoppingCart copyShoppingCart(ShoppingCart cart);

        /** Creates a new cloned (non-web) ShoppingCart Object, with option between legacy (partial) and full/exact cloning the whole cart. */
        ShoppingCart copyShoppingCart(ShoppingCart cart, boolean exactCopy);

        /**
         * Creates a new web ShoppingCart Object - full web shopping cart constructor.
         * <p>
         * SCIPIO: NOTE: 2018-11-30: This constructor should ONLY be inside a {@link CartSync#synchronizedSection(HttpServletRequest)}
         * block, because it modifies session variables that must match the cart contents.
         */
        ShoppingCart createWebShoppingCart(HttpServletRequest request, Locale locale, String currencyUom);

        /**
         * Creates a new web ShoppingCart Object - common web shopping cart constructor.
         * <p>
         * SCIPIO: NOTE: 2018-11-30: This constructor should ONLY be inside a {@link CartSync#synchronizedSection(HttpServletRequest)}
         * block, because it modifies session variables that must match the cart contents.
         */
        ShoppingCart createWebShoppingCart(HttpServletRequest request);

        /** Creates a new cloned ShoppingCart Object, using legacy (partial) cloning. */
        ShoppingCart copyWebShoppingCart(ShoppingCart cart);

        /** Creates a new cloned ShoppingCart Object, with option between legacy (partial) and full/exact cloning the whole cart. */
        ShoppingCart copyWebShoppingCart(ShoppingCart cart, boolean exactCopy);

        /** Creates a new cloned web ShoppingCart Object - performs an exact, deep copy of the cart. Changes to this copy do not affect the main cart. */
        ShoppingCart copyWebShoppingCart(HttpServletRequest request, Locale locale, String currencyUom);

        /** Creates a new empty (web) ShoppingCartHelper object. */
        ShoppingCartHelper createShoppingCartHelper(Delegator delegator, LocalDispatcher dispatcher, ShoppingCart cart);
    }

    public static abstract class BasicFactory implements Factory {
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

        @Override
        public ShoppingCart copyShoppingCart(ShoppingCart cart) {
            return new ShoppingCart(cart);
        }

        @Override
        public ShoppingCart copyShoppingCart(ShoppingCart cart, boolean exactCopy) {
            return new ShoppingCart(cart, exactCopy);
        }

        @Override
        public ShoppingCartHelper createShoppingCartHelper(Delegator delegator, LocalDispatcher dispatcher, ShoppingCart cart) {
            return new ShoppingCartHelper(delegator, dispatcher, cart);
        }
    }

    public static class DefaultFactory extends BasicFactory {
        private static final DefaultFactory INSTANCE = new DefaultFactory();

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
