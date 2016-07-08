package com.ilscipio.scipio.cms.util;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import javolution.util.FastList;

/**
 * CmsRecorderMap - Map wrapper that records names of keys added or overwritten.
 *
 * @param <T>
 */
public class CmsRecorderMap<T extends Object>  implements Map<String, T> {
    private List<String> modifiedNames = FastList.newInstance();
    private Map<String, T> realMap;
    public CmsRecorderMap(Map<String, T> realMap) {
        this.realMap = realMap;
    }
    public List<String> getAddedNames() {
        return modifiedNames;
    }
    @Override
    public int size() {
        return realMap.size();
    }
    @Override
    public boolean isEmpty() {
        return realMap.isEmpty();
    }
    @Override
    public boolean containsKey(Object key) {
        return realMap.containsKey(key);
    }
    @Override
    public boolean containsValue(Object value) {
        return realMap.containsValue(value);
    }
    @Override
    public T get(Object key) {
        return realMap.get(key);
    }
    @Override
    public T put(String key, T value) {
        modifiedNames.add(key);
        return realMap.put(key, value);
    }
    @Override
    public T remove(Object key) {
        return realMap.remove(key);
    }
    @Override
    public void putAll(Map<? extends String, ? extends T> m) {
        modifiedNames.addAll(m.keySet());
        realMap.putAll(m);
    }
    @Override
    public void clear() {
        realMap.clear();
    }
    @Override
    public Set<String> keySet() {
        return realMap.keySet();
    }
    @Override
    public Collection<T> values() {
        return realMap.values();
    }
    @Override
    public Set<Map.Entry<String, T>> entrySet() {
        return realMap.entrySet();
    }
}