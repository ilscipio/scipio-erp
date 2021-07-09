package com.ilscipio.scipio.solr;

import org.ofbiz.base.SystemState;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ProcessSignals;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericPK;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityFindOptions;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContainer;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceOptions;
import org.ofbiz.service.ServiceSyncRegistrations;
import org.ofbiz.service.ServiceUtil;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.Semaphore;
import java.util.stream.Collectors;

/**
 * Implements Solr ECA, manual and scripted solr reindexing through the scheduleEntityIndexing service.
 *
 * <p>This is a replacement for the legacy hook system {@link IndexingHookHandler} as EntityIndexer uses callback
 * async services as consumers instead of hooks, but IndexingHookHandler is still in use for now because the handlers are reusable.</p>
 *
 * <p>Each indexer has a queue of incoming entity IDs or value (with delegator) in a queue. The stock Scipio schema implements
 * product updates.
 * This implementation is currently tied to the solr component but is generic in nature; the classes not having a "Solr"
 * prefix are generic. By default the indexer name is the entity name. (1 entity = 1 indexer = 1 queue)</p>
 *
 * <p>scheduleDataIndexing and registerUpdateToSolr services add the productId or other ID to a global FIFO queue which is
 * processed by a data worker thread created on-demand. The data worker thread removes productIds from the queue for
 * which it creates instances of {@link SolrDocBuilder.ProductDocBuilder} (FIXME: for legacy reasons this class is
 * tied to a solr class). These are passed to handlers implementing {@link IndexingHookHandler}.</p>
 *
 * <p>NOTE: It is now possible to disable the solr commit hook in <code>applications/solr/config/solrhooks.properties</code>
 * files and add new hooks by defining a <code>config/solrhooks.properties</code> file in any component's config folder.
 * The solr commit hook itself honors the solr control properties in any case. You can do this and also disable solr
 * more thoroughly in <code></code>applications/solr/config/solrconfig.properties</code> as well as its webapp in
 * <code>applications/solr/scipio-component.xml</code>, or point them to a shared Solr instance on a dedicated
 * Scipio server for Solr.</p>
 *
 * TODO?: This class could be decoupled from the solr component and moved to the entity(ext?) component.
 */
public class EntityIndexer implements Runnable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final boolean DEBUG = UtilProperties.getPropertyAsBoolean("entityindexing", "entity.indexer.debug", false);
    private static final int STATS_INTERVAL = UtilProperties.getPropertyAsInteger("entityindexing", "entity.indexer.log.stats.interval", -1);
    /**
     * Maps a data source name (by convention the entity name) and data element ID (primary key) to an indexer. The
     * The indexer refreshes the data with hooks using {@link IndexingHookHandler}.
     */
    private static final Map<String, EntityIndexer.Factory> factoryMap = new ConcurrentHashMap<>();
    private static final Object factoryMapLock = new Object();

    /**
     * Maps a data source name (by convention the entity name) and data element ID (primary key) to an indexer. The
     * The indexer refreshes the data with hooks using {@link IndexingHookHandler}.
     */
    private static final Map<String, EntityIndexer> indexerMap = new ConcurrentHashMap<>();
    private static final Object indexerMapLock = new Object();

    private static final Map<String, ?> defaultProperties = Collections.unmodifiableMap(UtilProperties.getPropertiesWithPrefix(
            UtilProperties.getMergedPropertiesFromAllComponents("entityindexing"), "entity.indexer.default."));
    private static final Factory defaultFactory = loadFactory("default", defaultProperties, new Factory());

    protected final String name; // entity name by default
    protected final String entityName;
    protected final Queue<Entry> queue;
    protected ModelEntity modelEntity;
    protected final Semaphore runSemaphore;
    protected Map<String, ?> properties;
    protected List<Consumer> consumers;
    protected Long runServicePriority;

    protected int flushTime;
    protected int maxRunTime;
    protected int bufSize;
    protected int sleepTime;
    protected long lastRunStatsTime = 0;

    protected EntityIndexer(String name, Map<String, ?> properties, Queue<Entry> queue) {
        if (properties == null) {
            properties = Collections.emptyMap();
        }
        this.name = name;
        this.queue = queue;
        String entityName = (String) properties.get("entityName");
        if (UtilValidate.isEmpty(entityName)) {
            entityName = name;
        }
        this.entityName = entityName;
        this.properties = Collections.unmodifiableMap(new HashMap<>(properties));
        this.flushTime = UtilMisc.toInteger(properties.get("flushTime"), 3000);
        this.maxRunTime = UtilMisc.toInteger(properties.get("maxRunTime"), 60000);
        this.bufSize = UtilMisc.toInteger(properties.get("bufSize"), 1000);
        this.sleepTime = UtilMisc.toInteger(properties.get("sleepTime"), 500);
        this.runSemaphore = new Semaphore(1);
        this.lastRunStatsTime = 0;
    }

    public static class Factory {
        public EntityIndexer makeIndexer(String name, Map<String, ?> properties) {
            return new EntityIndexer(name, properties, makeQueue(properties));
        }

        public Queue<Entry> makeQueue(Map<String, ?> properties) {
            return new PriorityBlockingQueue<>(1000);
        }
    }

    /**
     * Gets a data indexer for the given data source name (entity name) and data element ID (primary key).
     * NOTE: The data source name must not contain the string "::".
     */
    public static <I extends EntityIndexer> I getIndexer(String name) {
        EntityIndexer indexer = indexerMap.get(name);
        if (indexer == null) {
            synchronized(indexerMapLock) {
                indexer = indexerMap.get(name);
                if (indexer == null) {
                    indexer = makeIndexer(name);
                    indexerMap.put(name, indexer);
                }
            }
        }
        return UtilGenerics.cast(indexer);
    }

    protected static <I extends EntityIndexer> I makeIndexer(String name) {
        Factory factory = factoryMap.get(name);
        if (factory == null) {
            factory = getFactory(name);
            if (factory == null) {
                return null;
            }
        }
        return UtilGenerics.cast(factory.makeIndexer(name, loadProperties(name)));
    }

    /**
     * Gets a data indexer factory for the given data source name (entity name).
     * NOTE: The data source name must not contain the string "::".
     */
    public static <F extends Factory> F getFactory(String name) {
        Factory factory = factoryMap.get(name);
        if (factory == null) {
            synchronized(factoryMapLock) {
                factory = factoryMap.get(name);
                if (factory == null) {
                    factory = loadFactory(name, loadProperties(name), defaultFactory);
                    factoryMap.put(name, factory);
                }
            }
        }
        return UtilGenerics.cast(factory);
    }

    protected static Map<String, ?> getDefaultProperties() {
        return defaultProperties;
    }

    protected static Map<String, ?> loadProperties(String name) {
        Map<String, ?> properties = new HashMap<>(getDefaultProperties());
        return UtilProperties.putPropertiesWithPrefix(UtilGenerics.cast(properties),
                UtilProperties.getMergedPropertiesFromAllComponents("entityindexing"), "entity.indexer." + name + ".");
    }

    /**
     * Returns the default indexer factory.
     */
    private static Factory getDefaultFactory(String name, Map<String, ?> properties) {
        return defaultFactory;
    }

    protected static Factory loadFactory(String name, Map<String, ?> properties, Factory defaultFactory) {
        String factoryClass = (String) properties.get("factoryClass");
        if (UtilValidate.isEmpty(factoryClass)) {
            return defaultFactory;
        }
        try {
            return UtilGenerics.cast(Thread.currentThread().getContextClassLoader().loadClass(factoryClass)
                    .getConstructor().newInstance());
        } catch(Exception e) {
            Debug.logError(e, "Could not load entity indexer factoryClass [" + factoryClass + "] from properties", module);
            return defaultFactory;
        }
    }

    /**
     * Registers a new factory in the factoryMap registry if none exists but does not replace an existing factory in
     * which case it returns the existing factory.
     */
    public static <F extends Factory> F register(String name, Factory factory) {
        Factory prevFactory = factoryMap.putIfAbsent(name, factory);
        return UtilGenerics.cast((prevFactory != null) ? prevFactory : factory);
    }

    /**
     * Registers a new factory in the factoryMap registry.
     * Replaces any existing factory with the one passed and returns the new factory.
     */
    public static <F extends Factory> F forceRegister(String name, Factory factory) {
        factoryMap.put(name, factory);
        return UtilGenerics.cast(factory);
    }

    public String getName() {
        return name;
    }

    /**
     * Returns the entity name for the indexer (for lookups), by default the indexer name.
     */
    public String getEntityName() {
        return entityName;
    }

    public Queue<Entry> getQueue() {
        return queue;
    }

    public Map<String, ?> getProperties() {
        return properties;
    }

    public long getRunServicePriority() {
        Long runServicePriority = this.runServicePriority;
        if (runServicePriority == null) {
            Object runServicePriorityObj = getProperties().get("runServicePriority");
            if (runServicePriorityObj instanceof Long) {
                runServicePriority = (Long) runServicePriorityObj;
            } else if (runServicePriorityObj instanceof Number) {
                runServicePriority = ((Number) runServicePriorityObj).longValue();
            } else if (runServicePriorityObj instanceof String) {
                try {
                    runServicePriority = Long.parseLong((String) runServicePriorityObj);
                } catch(Exception e) {
                    Debug.logError(e, "Cannot parse runServicePriority value [" + runServicePriorityObj + "]", module);
                }
            }
            if (runServicePriority == null) {
                runServicePriority = 100L;
            }
            this.runServicePriority = runServicePriority;
        }
        return runServicePriority;
    }

    @Override
    public String toString() {
        return getName();
    }

    public Entry add(Entry entry) {
        getQueue().add(entry);
        return entry;
    }

    public int poll(List<Entry> entries, int max) {
        int count;
        if (getQueue() instanceof PriorityBlockingQueue) {
            count = ((PriorityBlockingQueue<Entry>) getQueue()).drainTo(entries, max);
        } else {
            for(int i = 0; i < max; i++) {
                Entry entry = getQueue().poll();
                if (entry == null) {
                    return i;
                }
                entries.add(entry);
            }
            count = max;
        }
        // TODO: optimization: the entries are grouped by PK, so the last ones can get cut off, for now this will complicate
        //  because have to read the queue twice and in between another element can be added
        /*
        // Consume as many sequential duplicate PKs as possible because the last batch of same-id-ordered entries may get cut in half.
        Set<Entry> pks = null;
        while(true) {
            Entry entry = getQueue().peek();
            if (entry == null) {
                break;
            }
            if (pks == null) {
                // NOTE: this uses the implicit pk equals/hashcode for duplicates, not perfect
                pks = new HashSet<>(entries);
            }
            if (!pks.contains(entry)) {

            }
        }
        */
        return count;
    }

    public int getBufSize() {
        return bufSize;
    }

    public long getFlushTime() {
        return flushTime;
    }

    public long getMaxRunTime() {
        return maxRunTime;
    }

    public int getSleepTime() {
        return sleepTime;
    }

    @Override
    public void run() {
        try {
            run(getDefaultDctx(), Collections.emptyMap());
        } catch(Exception e) {
            Debug.logError(e, "Error during entity indexing: " + e.getMessage(), module);
        }
    }

    public void run(DispatchContext dctx, Map<String, Object> context) throws GeneralException, InterruptedException, IOException {
        long startTime = System.currentTimeMillis();
        long lastReadTime = 0;
        int numRead;
        List<Entry> entries = new ArrayList<>(getBufSize());
        List<DocEntry> docs = new ArrayList<>(getBufSize());
        Set<Entry> docsToRemove = new LinkedHashSet<>(getBufSize());

        int processedEntries = 0;
        int totalProcessedEntries = 0;
        int oldQueueSize = getQueue().size();
        int docsCommitted = 0;
        int docsRemoved = 0;

        // Read entries up to maxRunTime until there are no more or as long as elements were read but the buffer is still waiting for more
        while (((System.currentTimeMillis()) - startTime) < getMaxRunTime()) {
            // Gather the queue as long as new elements become available or wait briefly if flush time not reached
            // TODO: REVIEW: At the very end unless maxRunTime reached we always do an extra poll() back here to minimize the number of
            //  times this thread gets reactivated, and combined with flush time this helps buffer JobManager overhead, however, but
            //  a few minor thread synchronization issues exist with the semaphore
            boolean flush = false;
            while (!flush && (((System.currentTimeMillis()) - startTime) < getMaxRunTime()) &&
                    ((numRead = poll(entries, getBufSize())) > 0
                            || ((docs.size() > 0 || docsToRemove.size() > 0) && (System.currentTimeMillis() - lastReadTime) < getFlushTime()))) {
                // commit if buffer full
                if (entries.size() > 0) {
                    lastReadTime = entries.get(0).getEntryTime();
                    if (Debug.infoOn()) {
                        StringBuilder pkList = new StringBuilder();
                        int pkCount = 0;
                        for(Entry entry : entries) {
                            if (pkCount > 0) {
                                pkList.append(", ");
                            }
                            pkList.append(entry.getShortPk());
                            pkCount++;
                            if (pkCount >= SolrProductSearch.getMaxLogIds()) {
                                break;
                            }
                        }
                        if (entries.size() > SolrProductSearch.getMaxLogIds()) {
                            pkList.append("...");
                        }
                        Debug.logInfo("Reading docs " + (processedEntries + 1) + "-" + (processedEntries + entries.size()) +
                                " [" + getName() + "]: " + pkList + " (runTime: " + UtilDateTime.formatDurationHMS((System.currentTimeMillis()) - startTime) + ")", module);
                    }
                    processedEntries += entries.size();
                    totalProcessedEntries += entries.size();
                    for(Entry entry : entries) {
                        if ("all".equals(entry.getFlush())) {
                            flush = true;
                            break;
                        }
                    }
                    readDocs(dctx, context, entries, docs, docsToRemove);
                    entries.clear();
                } else if (!flush) {
                    Thread.sleep(getSleepTime());
                }
            }
            if (!docs.isEmpty() || !docsToRemove.isEmpty()) {
                commit(dctx, context, docs, docsToRemove);
                docsCommitted += docs.size();
                docsRemoved += docsToRemove.size();
                docs.clear();
                docsToRemove.clear();
                processedEntries = 0;
            }
        }
        if (getStatsInterval() >= 0) {
            long nowTime = System.currentTimeMillis();
            if ((nowTime - lastRunStatsTime) > getStatsInterval()) {
                lastRunStatsTime = nowTime;
                Debug.logInfo("Entity indexer [" + getName() + "] run doc stats: [committed=" + docsCommitted + ", removed=" + docsRemoved +
                        ", entries=" + totalProcessedEntries + ", runTime=" + UtilDateTime.formatDurationHMS(nowTime - startTime) + "ms]", module);
            }
        }
    }

    public void tryRun(DispatchContext dctx, Map<String, Object> context) throws GeneralException, InterruptedException, IOException {
        if (runSemaphore.tryAcquire()) {
            Debug.logInfo("Entity indexer [" + getName() + "]: beginning run (queued: " + getQueue().size() + ")", module);
            try {
                run(dctx, context);
            } finally {
                runSemaphore.release();
                if (isDebug()) {
                    Debug.logInfo("Entity indexer [" + getName() + "]: finished run", module);
                }
            }
        }
    }

    public boolean isRunning() {
        return (runSemaphore.availablePermits() == 0);
    }

    /**
     * Main processing method, may be overridden; default implementation dispatches documents to consumers.
     */
    public Object readDocs(DispatchContext dctx, Map<String, Object> context, Iterable<Entry> entries, List<DocEntry> docs,
                           Set<Entry> docsToRemove) throws GeneralException, InterruptedException, IOException {
        for(Entry entry : entries) {
            if (entry.isExplicitRemove()) {
                docsToRemove.add(entry);
            } else {
                // FIXME: missing auto-detect for non-explicit removal here
                docs.add(makeDocEntry(entry, null, null));
            }
        }
        return null;
    }

    public Object readDocsAndCommit(DispatchContext dctx, Map<String, Object> context, Iterable<Entry> entries) throws GeneralException, InterruptedException, IOException { // special cases/reuse
        List<DocEntry> docs = new ArrayList<>(getBufSize());
        Set<Entry> docsToRemove = new LinkedHashSet<>(getBufSize());
        Object result = readDocs(dctx, context, entries, docs, docsToRemove);
        if (docs.size() > 0 || docsToRemove.size() > 0) {
            commit(dctx, context, docs, docsToRemove);
        }
        return result;
    }

    public Object extractEntriesToDocs(DispatchContext dctx, Map<String, Object> context, long entryTime, Object properties,
                                               List<DocEntry> docs, Set<Entry> docsToRemove) throws GeneralException, InterruptedException, IOException {
        Collection<EntityIndexer.Entry> entries = extractEntries(dctx.getDelegator(), context, entryTime, properties);
        if (UtilValidate.isEmpty(entries)) {
            return null;
        }
        return readDocs(dctx, context, entries, docs, docsToRemove);
    }

    public Object extractEntriesToDocs(DispatchContext dctx, Map<String, Object> context, List<DocEntry> docs,
                                       Set<Entry> docsToRemove) throws GeneralException, InterruptedException, IOException {
        return extractEntriesToDocs(dctx, context, System.currentTimeMillis(), null, docs, docsToRemove);
    }

    /**
     * Returns the delegator, by default the default system delegator (not the "current").
     */
    protected Delegator getDefaultDelegator() {
        return DelegatorFactory.getDefaultDelegator();
    }

    /**
     * Returns the dispatcher, by default the default system dispatcher (not the "current").
     */
    protected LocalDispatcher getDefaultDispatcher() {
        return ServiceContainer.getLocalDispatcher(getDefaultDelegator().getDelegatorName(), getDefaultDelegator());
    }

    /**
     * Returns the dispatch context, by default default system dispatch context (not the "current").
     */
    protected DispatchContext getDefaultDctx() {
        return getDefaultDispatcher().getDispatchContext();
    }

    protected ModelEntity getModelEntity(String entityName) {
        ModelEntity modelEntity = getDefaultDelegator().getModelEntity(entityName);
        if (modelEntity == null) {
            throw new IllegalArgumentException("Could not find model for entity [" + entityName + "]");
        }
        return modelEntity;
    }

    public ModelEntity getModelEntity() {
        ModelEntity modelEntity = this.modelEntity;
        if (modelEntity == null) {
            modelEntity = getModelEntity(getEntityName());
            this.modelEntity = modelEntity;
        }
        return modelEntity;
    }

    /**
     * Creates indexer entity entries, one per entity/product, from the given maps compatible with the
     * scheduleEntityIndexing (scheduleProductIndexing) service interface.
     */
    public Collection<Entry> extractEntries(Delegator delegator, Map<String, Object> context, long entryTime, Object properties) {
        Object entityRef = extractEntryRef(delegator, context);
        if (entityRef == null) {
            return null;
        }
        Action action = extractEntryAction(delegator, context);
        Collection<GenericPK> pks = extractEntryPKs(delegator, context);
        if (UtilValidate.isEmpty(pks)) {
            return null;
        }
        Collection<Entry> entries = new ArrayList<>(pks.size());
        Collection<String> topics = UtilGenerics.cast(context.get("topics"));
        String flush = UtilGenerics.cast(context.get("flush"));
        for(GenericPK pk : pks) {
            Entry entry = makeEntry(pk, entityRef, action, entryTime, topics, flush, context, properties);
            if (entry != null) {
                entries.add(entry);
            }
        }
        return entries;
    }

    public Object extractEntryRef(Delegator delegator, Map<String, Object> context) {
        Object entityRef = context.get("instance");
        if (entityRef != null) {
            return entityRef;
        }
        entityRef = context.get("id");
        if (entityRef != null) {
            return entityRef;
        }
        Object idField = context.get("idField");
        if (idField != null) {
            entityRef = extractFirstIdField(idField, context);
            if (entityRef != null) {
                return entityRef;
            }
        }
        return null;
    }

    /**
     * Default algorithm for extracting target PKs from passed entity values and pks, best-effort.
     * Usually 1 is returned but more may be needed in some cases.
     */
    public Collection<GenericPK> extractEntryPKs(Delegator delegator, Map<String, Object> context) {
        Object entityRef = context.get("instance");
        if (entityRef instanceof GenericEntity) {
            GenericEntity entity = (GenericEntity) entityRef;
            if (getEntityName().equals(entity.getEntityName())) {
                return UtilMisc.toList((entity instanceof GenericPK) ? (GenericPK) entity : entity.getPrimaryKey());
            }
            String relationName = (String) context.get("relationName");
            if (relationName != null) {
                return UtilMisc.toList(entity.getRelatedOnePk(relationName));
            }
            List<GenericPK> pkMaps = entity.getRelatedOnePksForEntity(new ArrayList<>(), getEntityName());
            if (UtilValidate.isNotEmpty(pkMaps)) {
                return pkMaps;
            }
        }
        Object idRef = context.get("id");
        if (idRef == null) {
            if (entityRef instanceof Map) { // may also be GenericEntity (some entities may use idField when no relation defined)
                Map<String, Object> mapEntityRef = UtilGenerics.cast(entityRef);
                Object idField = context.get("idField");
                if (idField != null) {
                    idRef = extractFirstIdField(idField, mapEntityRef); // FIXME: support PKs greater than size 1 for idField
                }
            }
            if (idRef == null) {
                Object idField = context.get("idField");
                if (idField != null) {
                    idRef = extractFirstIdField(idField, context); // FIXME: support PKs greater than size 1 for idField
                }
                if (idRef == null) {
                    return null;
                }
            }
        }
        if (getModelEntity().getPksSize() == 1) {
            return UtilMisc.toList(GenericPK.create(delegator, getModelEntity(), idRef));
        } else if (idRef instanceof String) {
            GenericPK pk = GenericPK.create(delegator, getModelEntity(), modelEntity.getPkMapFromId((String) idRef, delegator)); // TODO: optimize
            if (pk.size() == getModelEntity().getPksSize()) {
                return UtilMisc.toList(pk);
            }
        }
        return null;
    }

    protected static Object extractIdField(Object idField, Map<String, ?> map) {
        if (idField == null) {
            return null;
        } else if (idField instanceof String) {
            return map.get(idField);
        } else if (idField instanceof Collection) {
            Map<String, Object> fields = new LinkedHashMap<>();
            for(String id : UtilGenerics.<Collection<String>>cast(idField)) {
                fields.put(id, map.get(id));
            }
            return fields;
        } else {
            throw new IllegalArgumentException("Invalid idField: " + idField);
        }
    }

    protected static Object extractFirstIdField(Object idField, Map<String, ?> map) { // TODO: remove in future
        return UtilMisc.firstOrSelfSafe(extractIdField(idField, map));
    }

    public Action extractEntryAction(Delegator delegator, Map<String, Object> context) {
        Object action = context.get("action");
        if (action instanceof Action) {
            return (Action) action;
        } else if (action instanceof Boolean) {
            return Boolean.FALSE.equals(action) ? Action.REMOVE : Action.ADD;
        } else if (action instanceof String) {
            if ("add".equals(action)) {
                return Action.ADD;
            } else if ("remove".equals(action)) {
                return Action.REMOVE;
            } else if (!((String) action).isEmpty()) {
                throw new IllegalArgumentException("Invalid action string value: " + action);
            }
        } else if (action != null) {
            throw new IllegalArgumentException("Invalid action value type: " + action.getClass());
        }
        return null;
    }

    public enum Action {
        ADD, REMOVE;
    }

    public GenericPK toPk(Object value) {
        if (value instanceof GenericPK) {
            return (GenericPK) value;
        } else if (value instanceof GenericEntity) {
            return ((GenericEntity) value).getPrimaryKey();
        } else if (value instanceof Map) {
            return GenericPK.create(getDefaultDelegator(), getModelEntity(), UtilGenerics.<Map<String, Object>>cast(value), getModelEntity().getPkFieldsUnmodifiable());
        } else {
            return GenericPK.create(getDefaultDelegator(), getModelEntity(), value);
        }
    }

    public Entry makeEntry(GenericPK pk, Object entityRef, Action action, long entryTime, Collection<String> topics, String flush, Map<String, Object> context, Object properties) {
        return new Entry(pk, entityRef, action, entryTime, topics, flush, context);
    }

    public Entry makeEntry(Object pk, Action action, Collection<String> topics) {
        return makeEntry(toPk(pk), null, action, System.currentTimeMillis(), topics, null, null, properties);
    }

    public Entry makeEntry(Entry other, Object pk, Long entryTime) {
        return makeEntry(other, toPk(pk), entryTime);
    }

    public static class Entry implements Comparable<Entry>, Serializable {
        protected final GenericPK pk;
        protected transient String shortPk;
        protected final Object entityRef;
        protected final Action action;
        protected long entryTime;
        protected final Collection<String> topics;
        protected String flush;

        protected Entry(GenericPK pk, Object entityRef, Action action, long entryTime, Collection<String> topics, String flush, Map<String, Object> context) {
            this.pk = pk;
            this.entityRef = entityRef;
            this.action = action;
            this.entryTime = entryTime;
            this.topics = (topics != null) ? topics : Collections.emptySet();
            this.flush = flush;
        }

        public Entry(GenericPK pk, Object entityRef, Action action, long entryTime, Collection<String> topics, String flush) {
            this.pk = pk;
            this.entityRef = entityRef;
            this.action = action;
            this.entryTime = entryTime;
            this.topics = (topics != null) ? topics : Collections.emptySet();
            this.flush = flush;
        }

        protected Entry(Entry other, GenericPK pk, Long entryTime) {
            this.pk = (pk != null) ? pk : other.getPk();
            this.entityRef = other.entityRef;
            this.action = other.action;
            this.entryTime = (entryTime != null) ? entryTime : other.getEntryTime();
            this.topics = other.topics;
        }

        public GenericPK getPk() {
            return pk;
        }

        public String getShortPk() {
            String shortPk = this.shortPk;
            if (shortPk == null) {
                shortPk = pk.getPkShortValueString();
                this.shortPk = shortPk;
            }
            return shortPk;
        }

        public Object getEntityRef() {
            return entityRef;
        }

        public Action getAction() {
            return action;
        }

        public boolean isExplicitAdd() { return getAction() == Action.ADD; }
        public boolean isExplicitRemove() { return getAction() == Action.REMOVE; }

        public long getEntryTime() {
            return entryTime;
        }

        public Collection<String> getTopics() {
            return topics;
        }

        public boolean hasTopic() {
            return !topics.isEmpty();
        }

        public String getFlush() {
            return flush;
        }

        @Override
        public int compareTo(Entry o) {
            if (pk.equals(o.pk)) {
                return 0; // same ID = same value = same priority, time doesn't matter
            }
            return Long.compare(entryTime, o.entryTime); // oldest entries should get picked first
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Entry entry = (Entry) o;
            return getPk().equals(entry.getPk());
        }

        @Override
        public int hashCode() {
            return Objects.hash(getPk());
        }
    }

    public DocEntry makeDocEntry(Entry entry, Map<String, Object> doc, Object docBuilder) {
        return new DocEntry(entry, doc, docBuilder);
    }

    public DocEntry makeDocEntry(GenericPK pk, Map<String, Object> doc, Object docBuilder) {
        return new DocEntry(pk, doc, docBuilder);
    }

    /**
     * Processed document entry (commit).
     */
    public class DocEntry implements Serializable {
        private final GenericPK pk;
        private transient String shortPk;
        private final Entry entry;
        private final Map<String, Object> doc;
        private final Object data;

        protected DocEntry(Entry entry, Map<String, Object> doc, Object data) {
            this.entry = entry;
            this.pk = entry.getPk();
            this.doc = doc;
            this.data = data;
        }

        protected DocEntry(GenericPK pk, Map<String, Object> doc, Object data) {
            this.entry = null;
            this.pk = pk;
            this.doc = doc;
            this.data = data;
        }

        /** The entry used to request this indexing, or null if generic event or callback. */
        public Entry getEntry() {
            return entry;
        }

        /** Makes a new Entry reflecting this DocEntry. */
        public Entry makeEntry(Action action, long entryTime, Collection<String> topics, String flush, Map<String, Object> context, Object properties) {
            return EntityIndexer.this.makeEntry(getPk(), (getEntry() != null) ? getEntry().getEntityRef() : null, action, entryTime, topics, flush, context, properties);
        }

        public Map<String, Object> getDoc() {
            return doc;
        }

        public Object getData() {
            return data;
        }

        public GenericPK getPk() {
            return pk;
        }

        public String getShortPk() {
            String shortPk = this.shortPk;
            if (shortPk == null) {
                shortPk = pk.getPkShortValueString();
                this.shortPk = shortPk;
            }
            return shortPk;
        }

        public Collection<String> getTopics() {
            return (entry != null) ? entry.getTopics() : Collections.emptySet();
        }

        public boolean hasTopic() {
            return (entry != null) ? entry.hasTopic() : false;
        }

    }

    public Object commit(DispatchContext dctx, Map<String, Object> context, Collection<? extends DocEntry> docs,
                         Collection<? extends Entry> docsToRemove) {
        for(Consumer consumer : getConsumers()) {
            Collection<? extends DocEntry> consumerDocs = docs.stream().filter(consumer::acceptsDoc).collect(Collectors.toList());
            Collection<? extends Entry> consumerDocsToRemove = docsToRemove.stream().filter(consumer::acceptsDoc).collect(Collectors.toList());
            try {
                Map<String, Object> servCtx = UtilMisc.toMap(
                        "userLogin", dctx.getDelegator().from("UserLogin").where("userLoginId", "system").cache().queryOneSafe(),
                        "docs", consumerDocs, "docsToRemove", consumerDocsToRemove, "onError", consumer.getOnError());
                if (consumer.isAsync()) {
                    dctx.getDispatcher().runAsync(consumer.getServiceName(), servCtx,
                            ServiceOptions.async(consumer.isPersist()).priority(consumer.getPriority()));
                    //Debug.logInfo("Jobs after starting " + consumer.getServiceName() + ": " + JobPoller.getInstance().getPoolState(false, true, 16), module);
                } else {
                    Map<String, Object> servResult = dctx.getDispatcher().runSync(consumer.getServiceName(), servCtx);
                    if (ServiceUtil.isError(servResult)) {
                        Debug.logError("Error dispatching " + docs.size() + " documents to entity indexing consumer " +
                                consumer + ": " + ServiceUtil.getErrorMessage(servResult), module);
                    }
                }
            } catch(GeneralException e) {
                Debug.logError(e, "Could not dispatch " + docs.size() + " documents to entity indexing consumer " + consumer, module);
            }
        }
        return null;
    }

    public List<Consumer> getConsumers() {
        List<Consumer> consumers = this.consumers;
        if (consumers == null) {
            Map<String, Map<String, Object>> consumerProps = UtilProperties.extractPropertiesWithPrefixAndId(new LinkedHashMap<>(),
                    getProperties(),"consumer.");
            consumers = new ArrayList<>(consumerProps.size());
            for(Map.Entry<String, Map<String, Object>> entry : consumerProps.entrySet()) {
                Consumer consumer = makeConsumer(entry.getKey(), entry.getValue());
                if (consumer != null) {
                    consumers.add(consumer);
                }
            }
            Collections.sort(consumers, (o1, o2) -> Integer.compare(o1.getSequenceNum(), o2.getSequenceNum()));
            Debug.logInfo("Consumers for entity indexer [" + getName() + "]: " + consumers, module);
            consumers = Collections.unmodifiableList(consumers);
            this.consumers = consumers;
        }
        return consumers;
    }

    protected Consumer makeConsumer(String name, Map<String, ?> props) {
        name = (name != null) ? name : (String) props.get("name");
        String serviceName = (String) props.get("service");
        Object topicsObj = props.get("topics");
        String mode = (String) props.get("mode");
        boolean persist = UtilMisc.booleanValue(props.get("persist"), false);
        Set<String> topics = null;
        if (topicsObj instanceof Collection) {
            topics = UtilGenerics.cast(topicsObj);
        } else if (topicsObj instanceof String) {
            topics = StringUtil.splitRegex(new LinkedHashSet<>(), (String) props.get("topics"), ",", null, true, false);
        } else if (topicsObj != null) {
            Debug.logError("Invalid entity indexing consumer topics configuration: " + topicsObj + ": " + props, module);
            return null;
        }
        if (UtilValidate.isEmpty(name) || UtilValidate.isEmpty(serviceName) || UtilValidate.isEmpty(topicsObj)) {
            Debug.logError("Invalid entity indexing consumer configuration: " + props, module);
            return null;
        }
        long priority = UtilMisc.toLong(props.get("priority"), 99L);
        String onError = (String) props.get("onError");
        return makeConsumer(name, UtilMisc.toInteger(props.get("sequenceNum"), 100), serviceName, topics, mode, persist, priority, onError);
    }

    protected Consumer makeConsumer(String name, int sequenceNum, String serviceName, Set<String> topics, String mode, boolean persist, long priority, String onError) {
        return new Consumer(name, sequenceNum, serviceName, topics, mode, persist, priority, onError);
    }

    public static class Consumer {
        private final String name;
        private final int sequenceNum;
        private final String serviceName;
        private final Set<String> topics;
        private final String mode;
        private final boolean persist;
        private final long priority;
        private final String onError;

        protected Consumer(String name, int sequenceNum, String serviceName, Set<String> topics, String mode, boolean persist, long priority, String onError) {
            this.name = name;
            this.sequenceNum = sequenceNum;
            this.serviceName = serviceName;
            this.topics = (topics != null) ? Collections.unmodifiableSet(new LinkedHashSet<>(topics)) : Collections.emptySet();
            this.mode = mode;
            this.persist = persist;
            this.priority = priority;
            this.onError = onError;
        }

        public String getName() {
            return name;
        }

        public int getSequenceNum() {
            return sequenceNum;
        }

        public String getServiceName() {
            return serviceName;
        }

        public Set<String> getTopics() {
            return topics;
        }

        public boolean hasTopic(Collection<String> topics) {
            for(String topic : topics) {
                if (this.topics.contains(topic)) {
                    return true;
                }
            }
            return false;
        }

        public boolean acceptsDoc(DocEntry docEntry) {
            return !docEntry.hasTopic() || hasTopic(docEntry.getTopics());
        }

        public boolean acceptsDoc(Entry entry) {
            return !entry.hasTopic() || hasTopic(entry.getTopics());
        }

        public boolean isAsync() {
            return true;
        }

        public boolean isPersist() {
            return false;
        }

        public long getPriority() {
            return priority;
        }

        public String getOnError() {
            return onError;
        }

        @Override
        public String toString() {
            return "{" +
                    "name='" + name + '\'' +
                    ", sequenceNum=" + sequenceNum +
                    ", serviceName='" + serviceName + '\'' +
                    ", topics=" + topics +
                    ", mode='" + mode + '\'' +
                    ", persist=" + persist +
                    ", priority=" + priority +
                    ", onError='" + onError + '\'' +
                    '}';
        }
    }

    /**
     * Service that registers the given entity PK to the current thread's transactions for queueing with
     * queueEntityIndex, otherwise performs queueEntityIndex immediately. queueEntityIndex then appends to the queue
     * for the EntityIndexer.
     */
    public static Map<String, Object> scheduleEntityIndexing(ServiceContext ctx) {
        if (!SystemState.getInstance().isServerExecution()) {
            if (Debug.verboseOn()) {
                Debug.logInfo("Not scheduling entity indexing; not a regular server execution", module);
            }
            return ServiceUtil.returnSuccess("Not scheduling entity indexing; not a regular server execution");
        }

        if ("trans-commit".equals(ctx.attr("event")) && TransactionUtil.isTransactionInPlaceSafe()) {
            if (TransactionUtil.getStatusSafe() == TransactionUtil.STATUS_MARKED_ROLLBACK) {
                return ServiceUtil.returnFailure("Current transaction is marked for rollback; aborting scheduleEntityIndexing");
            }
            return scheduleTransCommitEntityIndexing(ctx);
        } else {
            return queueEntityIndexing(ctx);
        }
    }

    /**
     * Extracts an entity PK from context and adds it to entitiesToIndex map to the transaction commit registration service entry for scheduleEntityIndexing.
     */
    public static Map<String, Object> scheduleTransCommitEntityIndexing(ServiceContext ctx) {
        try {
            LocalDispatcher dispatcher = ctx.dispatcher();
            ServiceSyncRegistrations regs = dispatcher.getServiceSyncRegistrations();
            EntityIndexer indexer = EntityIndexer.getIndexer(ctx.attr("entityName"));
            if (indexer == null) {
                throw new IllegalArgumentException("Could not get indexer for entity [" + ctx.attr("entityName") + "]");
            }
            Collection<Entry> entries = indexer.extractEntries(ctx.delegator(), ctx.context(), System.currentTimeMillis(), null);
            if (UtilValidate.isEmpty(entries)) {
                Debug.logWarning("No entity values identified for scheduling for context: " + ctx.context(), module);
                return ServiceUtil.returnFailure("No entity values identified for scheduling");
            }
            Collection<ServiceSyncRegistrations.ServiceSyncRegistration> commitRegs = regs.getCommitRegistrationsForService("scheduleEntityIndexing");
            if (commitRegs.size() >= 1) {
                if (commitRegs.size() >= 2) {
                    Debug.logError("scheduleTransCommitEntityIndexing: Found more than one transaction commit registration"
                            + " for scheduleEntityIndexing; should not happen! (coding or ECA config error)", module);
                }
                ServiceSyncRegistrations.ServiceSyncRegistration reg = commitRegs.iterator().next();
                // WARN: editing existing registration's service context in-place
                Map<String, Map<GenericPK, Entry>> entitiesToIndex = UtilGenerics.cast(reg.getContext().get("entitiesToIndex"));
                Map<GenericPK, Entry> entryMap = UtilGenerics.cast(entitiesToIndex.get(indexer.getName()));
                if (entryMap == null) {
                    entryMap = new LinkedHashMap<>();
                    entitiesToIndex.put(indexer.getName(), entryMap);
                }
                for(Entry entry : entries) {
                    entryMap.remove(entry.getPk()); // this is a LinkedHashMap, so remove existing first so we keep the "real" order
                    entryMap.put(entry.getPk(), entry);
                }
                String msg = "Scheduled transaction commit entity indexing for " + entries.size() + " entity " + indexer.getEntityName();
                Debug.logInfo("scheduleEntityIndexing: " + msg, module);
                return ServiceUtil.returnSuccess(msg);
            } else {
                // register the service
                Map<String, Object> servCtx = UtilMisc.toMap("locale", ctx.locale(), "userLogin", ctx.userLogin(),
                        "timeZone", ctx.timeZone(), "event", "global-queue");
                Map<String, Map<GenericPK, Entry>> entitiesToIndex = new LinkedHashMap<>();
                // IMPORTANT: LinkedHashMap keeps order of changes across transaction
                Map<GenericPK, Entry> entryMap = new LinkedHashMap<>();
                for(Entry entry : entries) {
                    entryMap.put(entry.getPk(), entry);
                }
                entitiesToIndex.put(indexer.getName(), entryMap);
                servCtx.put("entitiesToIndex", entitiesToIndex);
                regs.addCommitService(ctx.dctx(), "scheduleEntityIndexing", null, servCtx, false, false);
            }
            //StringBuilder pkList = new StringBuilder();
            //int pkCount = 0;
            //for(Entry entry : entries) {
            //    if (pkList.length() > 0) {
            //        pkList.append(", ");
            //    }
            //    pkList.append(entry.getShortPk());
            //    pkCount++;
            //    if (pkCount >= SolrProductSearch.maxLogIds) {
            //        break;
            //    }
            //}
            //if (entries.size() > SolrProductSearch.maxLogIds) {
            //    pkList.append("...");
            //}

            // This looks redundant because the queue below already does it and accumulates them
            //String msg = "Scheduled transaction commit entity indexing for " + entries.size() + " entries of entity " + indexer.getEntityName()
            //        + ": " + pkList;
            //Debug.logInfo("scheduleEntityIndexing: " + msg, module);
            return ServiceUtil.returnSuccess("Scheduled transaction commit entity indexing for " + entries.size() +
                    " entries of entity " + indexer.getEntityName());
        } catch (Exception e) {
            final String errMsg = "Could not schedule transaction global-commit entity indexing";
            Debug.logError(e, "scheduleEntityIndexing: " + errMsg, module);
            return ServiceUtil.returnError(errMsg + ": " + e.toString());
        }
    }

    /**
     * Queues the given entity PKs for indexing in the appropriate {@link EntityIndexer}.
     */
    public static Map<String, Object> queueEntityIndexing(ServiceContext ctx) {
        try {
            Map<String, Map<GenericPK, Entry>> entitiesToIndex = ctx.attr("entitiesToIndex");
            String entityName = ctx.attr("entityName");
            if (UtilValidate.isNotEmpty(entityName)) { // Only needed for "inline" queues (inline here is a convenience)
                EntityIndexer indexer = EntityIndexer.getIndexer(entityName);
                Collection<Entry> entries = indexer.extractEntries(ctx.delegator(), ctx.context(), System.currentTimeMillis(), null);
                if (UtilValidate.isNotEmpty(entries)) {
                    if (entitiesToIndex == null) {
                        entitiesToIndex = new LinkedHashMap<>();
                    }
                    Map<GenericPK, Entry> entryMap = entitiesToIndex.get(indexer.getName());
                    if (entryMap == null) {
                        entryMap = new LinkedHashMap<>();
                        entitiesToIndex.put(indexer.getName(), entryMap);
                    }
                    for (Entry entry : entries) {
                        entryMap.remove(entry.getPk()); // this is a LinkedHashMap, so remove existing first so we keep the "real" order
                        entryMap.put(entry.getPk(), entry);
                    }
                }
            }
            Map<String, Object> newEntitiesToIndex = ctx.attr("newEntitiesToIndex");
            Set<String> entityNames = UtilMisc.keySet(entitiesToIndex, newEntitiesToIndex);

            int scheduled = 0;
            StringBuilder pkList = new StringBuilder();
            int pkCount = 0;
            for(String targetEntityName : entityNames) {
                EntityIndexer targetIndexer = getIndexer(targetEntityName);
                if (pkList.length() > 0) {
                    pkList.append("; ");
                }
                pkList.append(targetIndexer.getEntityName());
                pkList.append(": ");
                boolean firstPk = true;

                Map<GenericPK, Entry> targetEntry = (entitiesToIndex != null) ? entitiesToIndex.get(targetEntityName) : null;
                if (targetEntry != null) {
                    for (Entry entry : targetEntry.values()) {
                        targetIndexer.add(entry);
                        if (pkCount < SolrProductSearch.getMaxLogIds()) {
                            if (!firstPk) {
                                pkList.append(", ");
                            }
                            pkList.append(entry.getShortPk());
                            pkCount++;
                        } else if (pkCount == SolrProductSearch.getMaxLogIds()) {
                            pkList.append("...");
                            pkCount++;
                        }
                        scheduled++;
                        firstPk = false;
                    }
                }

                Object rawEntities = (newEntitiesToIndex != null) ? newEntitiesToIndex.get(targetEntityName) : null;
                if (rawEntities != null) {
                    Map<String, Object> extractCtx = new HashMap<>(ctx.context());
                    extractCtx.remove("entityName");
                    extractCtx.remove("idField");
                    Iterator<?> it;
                    if (rawEntities instanceof Iterable || rawEntities instanceof Iterator) {
                        it = UtilMisc.asIterator(rawEntities);
                    } else {
                        it = UtilMisc.toList(rawEntities).iterator();
                    }

                    if (rawEntities instanceof EntityListIterator) {
                        // in this case trigger running early due to high number
                        if (!targetIndexer.isRunning()) {
                            // Start the async service to create a new thread and prioritize in system at same time
                            ctx.dispatcher().runAsync("runEntityIndexing",
                                    UtilMisc.toMap("userLogin", ctx.attr("userLogin"), "entityName", targetIndexer.getName()),
                                    ServiceOptions.asyncMemory().priority(targetIndexer.getRunServicePriority()));
                        }
                    }

                    Object value;
                    while((value = UtilMisc.next(it)) != null) {
                        if (value instanceof GenericEntity) {
                            extractCtx.put("instance", value);
                            extractCtx.remove("id");
                        } else if (value instanceof String) {
                            extractCtx.remove("instance");
                            extractCtx.put("id", value);
                        } else {
                            throw new IllegalArgumentException("invalid entityInstances for entity [" + targetEntityName + "]: " + value.getClass().getName());
                        }
                        Collection<Entry> entries = targetIndexer.extractEntries(ctx.delegator(), extractCtx, System.currentTimeMillis(), null);
                        if (UtilValidate.isNotEmpty(entries)) {
                            for(Entry entry : entries) {
                                targetIndexer.add(entry);
                                if (pkCount < SolrProductSearch.getMaxLogIds()) {
                                    if (!firstPk) {
                                        pkList.append(", ");
                                    }
                                    pkList.append(entry.getShortPk());
                                    pkCount++;
                                } else if (pkCount == SolrProductSearch.getMaxLogIds()) {
                                    pkList.append("...");
                                    pkCount++;
                                }
                                scheduled++;
                                firstPk = false;
                            }
                        }
                    }
                }

                if (!targetIndexer.isRunning()) {
                    // Start the async service to create a new thread and prioritize in system at same time
                    ctx.dispatcher().runAsync("runEntityIndexing",
                        UtilMisc.toMap("userLogin", ctx.attr("userLogin"), "entityName", targetIndexer.getName()),
                            ServiceOptions.asyncMemory().priority(targetIndexer.getRunServicePriority()));
                }
            }
            String msg = "Queued entity indexing for " + scheduled + " entries: " + pkList;
            Debug.logInfo("scheduleEntityIndexing: " + msg, module);
            return ServiceUtil.returnSuccess(msg);
        } catch (Exception e) {
            final String errMsg = "Could not queue entity indexing";
            Debug.logError(e, "scheduleEntityIndexing: " + errMsg, module);
            return ServiceUtil.returnError(errMsg + ": " + e.toString());
        }
    }

    public static Map<String, Object> scheduleAllEntityIndexing(ServiceContext ctx) {
        Delegator delegator = ctx.delegator();
        ProcessSignals processSignals = null;

        String entityName = ctx.attr("entityName");
        Integer maxRows = ctx.attr("maxRows");

        EntityListIterator prodIt = null;
        try {
            if (processSignals != null && processSignals.isSet("stop")) {
                return ServiceUtil.returnFailure(processSignals.getProcess() + " aborted");
            }

            EntityFindOptions findOptions = new EntityFindOptions();
            if (maxRows != null) {
                findOptions.setMaxRows(maxRows);
            }
            prodIt = delegator.find(entityName, null, null, null, null, findOptions);
            if (processSignals != null && processSignals.isSet("stop")) {
                return ServiceUtil.returnFailure(processSignals.getProcess() + " aborted");
            }

            /* TODO: filters
            Collection<String> includeMainStoreIds = UtilMisc.nullIfEmpty(ctx.<Collection<String>>attr("includeMainStoreIds"));
            Collection<String> includeAnyStoreIds = UtilMisc.nullIfEmpty(ctx.<Collection<String>>attr("includeAnyStoreIds"));
            List<SolrDocBuilder.ProductFilter> productFilters = null;
            SolrDocBuilder.ProductFilter storeProductFilter = docBuilder.makeStoreProductFilter(includeMainStoreIds, includeAnyStoreIds);
            if (storeProductFilter != null) {
                productFilters = (productFilters != null) ? new ArrayList<>(productFilters) : new ArrayList<>();
                productFilters.add(storeProductFilter);
            }
            */

            Map<String, Object> queueCtx = new HashMap<>(ctx.context());
            queueCtx.put("newEntitiesToIndex", UtilMisc.toMap(entityName, prodIt));
            Map<String, Object> queueResult = queueEntityIndexing(ctx.from(queueCtx));
            if (ServiceUtil.isError(queueResult)) {
                return ServiceUtil.returnResultSysFields(queueResult);
            }
            return ServiceUtil.returnResultSysFields(queueResult);
        } catch (Exception e) {
            return ServiceUtil.returnError(e.toString());
        } finally {
            if (prodIt != null) {
                try {
                    prodIt.close();
                } catch(Exception e) {
                }
            }
        }
    }

    /**
     * Re-queues the given entity PKs for indexing in the appropriate {@link EntityIndexer} after failure.
     * May be called directly by entityIndexingConsumer implementations.
     * NOTE: Currently no service interface to avoid transaction handling.
     */
    public static Map<String, Object> requeueEntityIndexing(DispatchContext dctx, Map<String, Object> context) {
        try {
            Collection<? extends EntityIndexer.DocEntry> allDocs = UtilGenerics.cast(context.get("docs"));
            Collection<String> topics = UtilGenerics.cast(context.get("topics"));
            if (topics == null && !context.containsKey("topics")) {
                String onError = UtilGenerics.cast(context.get("onError"));
                Collection<String> srcTopics = UtilGenerics.cast(context.get("srcTopics"));
                if ("ignore".equals(onError)) {
                    return ServiceUtil.returnSuccess();
                } else if ("requeue-topic".equals(onError)) {
                    topics = srcTopics;
                } else if ("requeue-all".equals(onError)) {
                    topics = null;
                }
            }
            if (UtilValidate.isEmpty(topics)) {
                topics = null;
            }
            String entityName = (String) context.get("entityName");
            int scheduled = 0;
            StringBuilder pkList = new StringBuilder();
            int pkCount = 0;
            EntityIndexer targetIndexer = getIndexer(entityName);
            if (targetIndexer == null) {
                throw new IllegalArgumentException("Invalid entity indexer name: " + entityName);
            }
            if (pkList.length() > 0) {
                pkList.append("; ");
            }
            pkList.append(targetIndexer.getEntityName());
            pkList.append(": ");
            boolean firstPk = true;
            long entryTime = System.currentTimeMillis();
            for(DocEntry docEntry : allDocs) {
                Entry entry = docEntry.makeEntry(Action.ADD, entryTime, topics, null, null, null);
                targetIndexer.add(entry);
                if (pkCount < SolrProductSearch.getMaxLogIds()) {
                    if (!firstPk) {
                        pkList.append(", ");
                    }
                    pkList.append(entry.getShortPk());
                    pkCount++;
                } else if (pkCount == SolrProductSearch.getMaxLogIds()) {
                    pkList.append("...");
                    pkCount++;
                }
                scheduled++;
                firstPk = false;
            }
            if (!targetIndexer.isRunning()) {
                // Start the async service to create a new thread and prioritize in system at same time
                dctx.getDispatcher().runAsync("runEntityIndexing",
                        UtilMisc.toMap("userLogin", context.get("userLogin"), "entityName", targetIndexer.getName()),
                        ServiceOptions.asyncMemory().priority(targetIndexer.getRunServicePriority()));
            }
            String msg = "Requeued entity indexing for " + scheduled + " entries: " + pkList;
            Debug.logInfo("requeueEntityIndexing: " + msg, module);
            return ServiceUtil.returnSuccess(msg);
        } catch (Exception e) {
            final String errMsg = "Could not requeue entity indexing";
            Debug.logError(e, "requeueEntityIndexing: " + errMsg, module);
            return ServiceUtil.returnError(errMsg + ": " + e.toString());
        }
    }

    /**
     * Runs entity indexing for an entity/indexer IF not already started; if started does nothing for that indexer.
     * Must be run as an async service to let the job manager determine best time to run processing.
     * NOTE: This must strictly be an async service without transaction because the thread may get recycled up to
     * a maximum.
     * WARN: If multiple entityNames are passed they try-run sequentially - avoiding this for now, instead doing multiple
     * runAsync with single entityName.
     */
    public static Map<String, Object> runEntityIndexing(ServiceContext ctx) {
        Collection<String> entityNames = ctx.attr("entityNames");
        if (UtilValidate.isEmpty(entityNames)) {
            String entityName = ctx.attr("entityName");
            if (UtilValidate.isEmpty(entityName)) {
                return ServiceUtil.returnError("No entity names (indexer names) specified");
            }
            entityNames = UtilMisc.toList(entityName);
        }

        for(String entityName : entityNames) {
            EntityIndexer targetIndexer = getIndexer(entityName);
            if (targetIndexer != null) {
                try {
                    // Run with default dispatch context so everyone is equal
                    //targetIndexer.tryRun(dctx, context);
                    targetIndexer.tryRun(targetIndexer.getDefaultDctx(), ctx.context());
                } catch(Exception e) {
                    Debug.logError(e, module);
                    return ServiceUtil.returnError(e.toString());
                }

                // If there are elements in the queue, queue another invocation using a separate service call.
                // NOTE: On a live system with a constant flux of ECAs this is not needed, but for development this can be needed.
                if (!targetIndexer.isRunning() && targetIndexer.getQueue().size() > 0) {
                    // Start the async service to create a new thread and prioritize in system at same time
                    try {
                        ctx.dispatcher().runAsync("runEntityIndexing",
                                UtilMisc.toMap("userLogin", ctx.attr("userLogin"), "entityName", targetIndexer.getName()),
                                ServiceOptions.asyncMemory().priority(targetIndexer.getRunServicePriority()));
                    } catch (GenericServiceException e) {
                        String msg = "Could not reschedule runEntityIndexing: " + e.toString();
                        Debug.logError(e, "runEntityIndexing: " + msg, module);
                        return ServiceUtil.returnFailure(msg);
                    }
                }
            }
        }
        return ServiceUtil.returnSuccess();
    }

    public static boolean isDebug() {
        return DEBUG;
    }

    public static int getStatsInterval() {
        return STATS_INTERVAL;
    }
}
