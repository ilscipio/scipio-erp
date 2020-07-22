package com.ilscipio.scipio.solr;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.GenericValue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Base class for solr product indexer-derived data, hooked in solrhooks.properties files as numbered entries.
 * Instantiated from factory before every indexing iteration (instances not cached).
 * See SolrProductSearch.updateToSolrCoreMultiAdd for reference calls.
 */
public interface IndexingHookHandler {

    enum HookType { ECA, REINDEX, MANUAL }

    interface Factory {
        IndexingHookHandler getHookHandler();
        Class<? extends IndexingHookHandler> getHookHandlerClass();
    }

    default void begin(IndexingStatus indexingStatus) throws Exception {}

    default void beginBatch(IndexingStatus indexingStatus) throws Exception {}

    default void processDocAdd(IndexingStatus indexingStatus, Map<String, Object> doc, SolrProductIndexer.ProductDocBuilder docBuilder,
                               SolrProductIndexer.ProductUpdateRequest productUpdateRequest) throws Exception {}

    default void processDocRemove(IndexingStatus indexingStatus, String productId,
                                  SolrProductIndexer.ProductUpdateRequest productUpdateRequest) throws Exception {}

    /** Batch end commit, called after solr docs are sent to server. NOTE: endBatch may be called even if no docs added/removed */
    default void endBatch(IndexingStatus indexingStatus) throws Exception {}

    default void end(IndexingStatus indexingStatus) throws Exception {}

    /** Loads handlers from solrhooks.properties. */
    abstract class Handlers {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

        private static final Map<HookType, List<Factory>> factories = readHookHandlerFactories();

        public static List<IndexingHookHandler> getHookHandlers(Collection<? extends Factory> factories) {
            List<IndexingHookHandler> hookHandlers = new ArrayList<>(factories.size());
            for(Factory factory : factories) {
                hookHandlers.add(factory.getHookHandler());
            }
            return hookHandlers;
        }

        public static List<Factory> getHookHandlerFactories(IndexingHookHandler.HookType hookType) {
            return factories.get(hookType);
        }

        public static Map<HookType, List<Factory>> getHookHandlerFactoryMap(IndexingHookHandler.HookType hookType) {
            return factories;
        }

        public static List<Factory> getHookHandlerFactoriesByType(Class<? extends IndexingHookHandler> cls) {
            List<Factory> hookHandlers = new ArrayList<>();
            for(Map.Entry<HookType, List<Factory>> entry : factories.entrySet()) {
                for(Factory factory : entry.getValue()) {
                    if (cls.isAssignableFrom(factory.getHookHandlerClass())) {
                        hookHandlers.add(factory);
                    }
                }
            }
            return hookHandlers;
        }

        private static Map<HookType, List<Factory>> readHookHandlerFactories() {
            Map<HookType, List<Factory>> factories = new EnumMap<>(HookType.class);
            for(HookType hookType : HookType.values()) {
                factories.put(hookType, new ArrayList<>());
            }

            List<Map<String, String>> handlerDefs = new ArrayList<>(UtilProperties.extractPropertiesWithPrefixAndId(new LinkedHashMap<String, Map<String, String>>(),
                    UtilProperties.getMergedPropertiesFromAllComponents("solrhooks"), "solr.hooks.indexing.").values());
            Collections.sort(handlerDefs, new Comparator<Map<String, String>>() {
                @Override
                public int compare(Map<String, String> o1, Map<String, String> o2) {
                    Integer prio1 = 1000; // higher = lower prio
                    try {
                        prio1 = Integer.parseInt(o1.get("prio"));
                    } catch(NumberFormatException e) {}
                    Integer prio2 = 1000;
                    try {
                        prio2 = Integer.parseInt(o2.get("prio"));
                    } catch(NumberFormatException e) {}
                    return prio1.compareTo(prio2);
                }
            });

            for(Map<String, String> props : handlerDefs) {
                String factoryClassStr = props.get("factoryClass");
                Factory factory;
                try {
                    @SuppressWarnings("unchecked")
                    Class<? extends IndexingHookHandler.Factory> factoryCls =
                            (Class<? extends IndexingHookHandler.Factory>) Thread.currentThread().getContextClassLoader().loadClass(factoryClassStr);
                    factory = factoryCls.newInstance();
                } catch(Exception e) {
                    Debug.logError("Could not load factory [" + factoryClassStr + "] from solrhooks.properties", module);
                    continue;
                }

                String hookTypesStr = props.get("hookTypes");
                if (hookTypesStr == null) {
                    hookTypesStr = "ALL";
                }
                Set<HookType> hookTypes = new LinkedHashSet<>();
                for(String hookTypeStr : hookTypesStr.split("\\*,\\*")) {
                    if (hookTypeStr.length() > 0) {
                        if ("ALL".equals(hookTypeStr)) {
                            hookTypes.addAll(Arrays.asList(HookType.values()));
                            break;
                        } else {
                            try {
                                HookType hookType = HookType.valueOf(hookTypeStr);
                                hookTypes.add(hookType);
                            } catch (IllegalArgumentException e) {
                                Debug.logError(e, "Invalid solr hook in solrhooks.properties: " + hookTypeStr, module);
                            }
                        }
                    }
                }

                for(HookType hookType : hookTypes) {
                    factories.get(hookType).add(factory);
                }
            }
            for(Map.Entry<HookType, List<Factory>> entry : factories.entrySet()) {
                entry.setValue(Collections.unmodifiableList(entry.getValue()));
            }
            return Collections.unmodifiableMap(factories);
        }
    }

    class HookHelper {

    }
}
