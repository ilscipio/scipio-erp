package com.ilscipio.scipio.ce.webapp.ftl.lang;

import java.util.AbstractMap;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;

import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * NOTE: These are mainly for transform implementations.
 */
public abstract class TemplateModelContainer {

    /**
     * SCIPIO: This is the half-reverse of
     * {@link freemarker.template.DefaultMapAdapter.DefaultMapAdapter(Map, ObjectWrapper)};
     * except that {@link #get(Object)} returns TemplateModels, not unwrapped objects.
     * <p>
     * WARNING: This implementation is partial only! Some methods throw UnsupportedOperationException
     * even if violates {@link java.util.Map} interface.
     * AVOID USING the values
     */
    public static class TemplateModelMap<T extends TemplateModel> extends TemplateModelContainer implements Map<String, T> {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

        private final TemplateHashModelEx hash;
        private Set<String> keys; // cache
        private Collection<T> values; // cache
        private Set<Map.Entry<String, T>> entries; // cache

        protected TemplateModelMap(TemplateHashModelEx hash) {
            this.hash = hash;
        }

        @Override
        public int size() {
            try {
                return hash.size();
            } catch (TemplateModelException e) {
                Debug.logError(e, module);
                return 0;
            }
        }

        @Override
        public boolean isEmpty() {
            try {
                return hash.isEmpty();
            } catch (TemplateModelException e) {
                Debug.logError(e, module);
                return true;
            }
        }

        @Override
        public boolean containsKey(Object key) {
            return keySet().contains(key);
        }

        @Override
        public boolean containsValue(Object value) {
            return values().contains(value);
        }

        @SuppressWarnings("unchecked")
        @Override
        public T get(Object key) {
            try {
                return (T) hash.get((String) key);
            } catch (TemplateModelException e) {
                Debug.logError(e, module);
                return null;
            }
        }

        @Override
        public T put(String key, T value) {
            throw new UnsupportedOperationException();
        }

        @Override
        public T remove(Object key) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void putAll(Map<? extends String, ? extends T> m) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void clear() {
            throw new UnsupportedOperationException();
        }

        @Override
        public Set<String> keySet() {
            Set<String> keys = this.keys;
            if (keys == null) {
                try {
                    // FIXME: IMPROVE WITH ADAPTER WHERE POSSIBLE (forces copy!)
                    keys = LangFtlUtil.toStringSet(hash.keys());
                    this.keys = keys;
                } catch (TemplateModelException e) {
                    Debug.logError(e, module);
                }
            }
            return keys;
        }

        @Override
        public Collection<T> values() {
            Collection<T> values = this.values;
            if (values == null) {
                try {
                    // FIXME: IMPROVE WITH ADAPTER WHERE POSSIBLE (forces copy!)
                    values = LangFtlUtil.toListNoUnwrap(hash.values());
                    this.values = values;
                } catch (TemplateModelException e) {
                    Debug.logError(e, module);
                }
            }
            return values;
        }

        @Override
        public Set<Entry<String, T>> entrySet() {
            Set<Entry<String, T>> entries = this.entries;
            if (entries == null) {
                entries = new LinkedHashSet<>();
                // FIXME: IMPROVE WITH ADAPTER WHERE POSSIBLE (forces copy - very slow!)
                for(String key : keySet()) {
                    entries.add(new AbstractMap.SimpleEntry<String, T>(key, get(key)));
                }
                this.entries = entries;
            }
            return entries;
        }
    }

    /* TODO: requires a different class for each of TemplateCollectionModel, TemplateSequenceModel, etc.
     * until this is done, MapModelAdapter.keySet() will keep making copies!
    public static class TemplateModelSet<V extends TemplateModel> extends TemplateModelAdapter implements Map<String, V> {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

        private final TemplateModel collOrSeq;
        private final ObjectWrapper objectWrapper;

        protected TemplateModelSet(TemplateModel collOrSeq, ObjectWrapper objectWrapper) {
            this.collOrSeq = collOrSeq;
            this.objectWrapper = objectWrapper;
        }

        @Override
        public int size() {
            return col;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public boolean containsKey(Object key) {
            return false;
        }

        @Override
        public boolean containsValue(Object value) {
            return false;
        }

        @Override
        public V get(Object key) {
            return null;
        }

        @Override
        public V put(String key, V value) {
            return null;
        }

        @Override
        public V remove(Object key) {
            return null;
        }

        @Override
        public void putAll(Map<? extends String, ? extends V> m) {
        }

        @Override
        public void clear() {
        }

        @Override
        public Set<String> keySet() {
            return null;
        }

        @Override
        public Collection<V> values() {
            return null;
        }

        @Override
        public Set<Entry<String, V>> entrySet() {
            return null;
        }
    }
    */

}
