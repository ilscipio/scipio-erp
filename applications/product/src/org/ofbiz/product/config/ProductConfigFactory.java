package org.ofbiz.product.config;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
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
 * SCIPIO: Factory to replace hardcoded ProductConfigWrapper constructors.
 * <p>
 * NOTE: The actual instance is represented by {@link ProductConfigFactory.Factory}, due to the static
 * methods being the exact same as the instance methods which will lead to obscure-bug-making-errors
 * if try to put both in same class with slightly different names (this way the compiler will catch everything).
 * <p>
 * To use this class, create a file named <code>orderstoreconfig.properties</code> in your component's config folder
 * and the following entry using your store's ID (the default factory class is given here):
 * <ul>
 * <li>Per-store config: <code>store.PRODUCT_STORE_ID.productConfig.factoryClass=org.ofbiz.product.config.ProductConfigFactory$DefaultFactory</code></li>
 * <li>Global default config: <code>store.DEFAULT.productConfig.factoryClass=org.ofbiz.product.config.ProductConfigFactory$DefaultFactory</code></li>
 * </ul>
 */
public abstract class ProductConfigFactory {

    public static final String ORDER_STORE_CONFIG_PROPFILE = "orderstoreconfig";

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final Map<String, Factory> storeIdCache = readInstances();
    private static final Factory defaultFactory = readDefaultInstance(storeIdCache);

    /** The instance is represented by {@link Factory} nested class. */
    protected ProductConfigFactory() {}

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
            String factoryClsName = entry.getValue().get("productConfig.factoryClass");
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
        Debug.logInfo("Read ProductConfigWrapper factories: " + factoryMap, module);
        return factoryMap;
    }

    private static Factory readDefaultInstance(Map<String, Factory> storeIdCache) {
        Factory factory = storeIdCache.get("DEFAULT");
        if (factory == null) {
            factory = DefaultFactory.INSTANCE;
        }
        Debug.logInfo("Read default ProductConfigWrapper factory: " + factory, module);
        return factory;
    }

    // Static standard helper wrapper methods around the Factory methods, to help simplify code
    // (the get() calls are unnecessary to write manually in code in all known cases, because delegator and productStoreId are always already known to the factory methods)

    //** Creates new ProductConfigWrapper. extraArgs is for optional extra arguments for client code. */
    public static <T extends ProductConfigWrapper> T createProductConfigWrapper(Delegator delegator, LocalDispatcher dispatcher, String productId, String productStoreId,
                                                                  String catalogId, String webSiteId, String currencyUomId, Locale locale, GenericValue autoUserLogin,
                                                                  Map<String, Object> extraArgs) throws Exception {
        return get(delegator, productStoreId).createProductConfigWrapper(delegator, dispatcher, productId, productStoreId, catalogId, webSiteId, currencyUomId, locale, autoUserLogin, extraArgs);
    }

    //** Creates new ProductConfigWrapper. extraArgs is for optional extra arguments for client code. */
    public static <T extends ProductConfigWrapper> T createProductConfigWrapper(Delegator delegator, LocalDispatcher dispatcher, String productId, String productStoreId,
                                                                                String catalogId, String webSiteId, String currencyUomId, Locale locale, GenericValue autoUserLogin) throws Exception {
        return get(delegator, productStoreId).createProductConfigWrapper(delegator, dispatcher, productId, productStoreId, catalogId, webSiteId, currencyUomId, locale, autoUserLogin, null);
    }

    /** Creates a new cloned ProductConfigWrapper Object, using legacy (partial) cloning. */
    public static <T extends ProductConfigWrapper> T copyProductConfigWrapper(ProductConfigWrapper pcw) {
        return get(pcw.getDelegator(), pcw.getProductStoreId()).copyProductConfigWrapper(pcw);
    }

    /** Creates a new cloned ProductConfigWrapper Object, with option between legacy (partial) and full/exact cloning the whole wrapper. */
    public static <T extends ProductConfigWrapper> T copyProductConfigWrapper(ProductConfigWrapper pcw, boolean exactCopy) {
        return get(pcw.getDelegator(), pcw.getProductStoreId()).copyProductConfigWrapper(pcw, exactCopy);
    }

    // Instance methods and interfaces

    /** Actual ProductConfigWrapper factory and instance methods (mostly identical to the static methods) */
    public interface Factory extends Serializable {

        /** Creates new ProductConfigWrapper. extraArgs is for optional extra arguments for client code. */
        <T extends ProductConfigWrapper> T createProductConfigWrapper(Delegator delegator, LocalDispatcher dispatcher, String productId, String productStoreId,
                                                                      String catalogId, String webSiteId, String currencyUomId, Locale locale, GenericValue autoUserLogin,
                                                                      Map<String, Object> extraArgs) throws Exception;

        /** Creates a new cloned ProductConfigWrapper Object, using legacy (partial) cloning. */
        <T extends ProductConfigWrapper> T copyProductConfigWrapper(ProductConfigWrapper pcw);

        /** Creates a new cloned ProductConfigWrapper Object, with option between legacy (partial) and full/exact cloning the whole wrapper. */
        <T extends ProductConfigWrapper> T copyProductConfigWrapper(ProductConfigWrapper pcw, boolean exactCopy);

    }

    @SuppressWarnings("unchecked")
    public static class DefaultFactory implements Factory {
        private static final DefaultFactory INSTANCE = new DefaultFactory();

        @Override
        public <T extends ProductConfigWrapper> T createProductConfigWrapper(Delegator delegator, LocalDispatcher dispatcher, String productId, String productStoreId, String catalogId, String webSiteId, String currencyUomId, Locale locale, GenericValue autoUserLogin,
                                                                     Map<String, Object> extraArgs) throws Exception {
            return (T) new ProductConfigWrapper(delegator, dispatcher, productId, productStoreId,  catalogId,  webSiteId, currencyUomId, locale, autoUserLogin);
        }

        @Override
        public <T extends ProductConfigWrapper> T copyProductConfigWrapper(ProductConfigWrapper pcw) {
            return (T) new ProductConfigWrapper(pcw);
        }

        @Override
        public <T extends ProductConfigWrapper> T copyProductConfigWrapper(ProductConfigWrapper pcw, boolean exactCopy) {
            return (T) new ProductConfigWrapper(pcw, exactCopy);
        }
    }

}
