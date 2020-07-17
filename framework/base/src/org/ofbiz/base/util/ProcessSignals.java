package org.ofbiz.base.util;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Used to stop rebuildSolrIndex (SCIPIO).
 */
public class ProcessSignals implements Map<String, Object> {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final Map<String, Object> signals = new ConcurrentHashMap<>();
    private final String process;
    private final boolean verbose;

    protected ProcessSignals(String process, boolean verbose) {
        this.process = process;
        this.verbose = verbose;
    }

    public static ProcessSignals make(String process) {
        return make(process, false);
    }

    public static ProcessSignals make(String process, boolean verbose) {
        // TODO: REVIEW: this method may want to hold a central reference later...
        return new ProcessSignals(process, verbose);
    }

    public String getProcess() {
        return process;
    }

    @Override
    public Object get(Object name) {
        return signals.get(name);
    }

    public boolean isSet(String name) {
        return signals.containsKey(name);
    }

    @Override
    public Object put(String name, Object value) {
        if (verbose) {
            Debug.logInfo("Sent signal [" + name + "] to process [" + process + "]", module);
        }
        return signals.put(name, value);
    }

    public void put(String name) {
        put(name, true);
    }

    @Override
    public Object remove(Object name) {
        return signals.remove(name);
    }

    @Override
    public void clear() {
        signals.clear();
    }

    @Override
    public int size() {
        return signals.size();
    }

    @Override
    public boolean isEmpty() {
        return signals.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return signals.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return signals.containsValue(value);
    }

    @Override
    public void putAll(Map<? extends String, ?> m) {
        signals.putAll(m);
    }

    @Override
    public Set<String> keySet() {
        return signals.keySet();
    }

    @Override
    public Collection<Object> values() {
        return signals.values();
    }

    @Override
    public Set<Entry<String, Object>> entrySet() {
        return signals.entrySet();
    }
}
