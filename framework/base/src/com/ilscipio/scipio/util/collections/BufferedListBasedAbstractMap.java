package com.ilscipio.scipio.util.collections;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * Simplification of AbstractMap for read-only implementations (mostly).
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public abstract class BufferedListBasedAbstractMap<K, V> extends AbstractMap<K, V> {

    protected Set<Entry<K, V>> entrySet;
    protected List<Entry<K, V>> entryList;

    protected abstract List<Entry<K, V>> makeEntryList();

    public List<Entry<K, V>> entryList() {
        List<Entry<K, V>> entryList = this.entryList;
        if (entryList == null) {
            entryList = makeEntryList();
            this.entryList = entryList;
        }
        return entryList;
    }

    @Override
    public Set<Entry<K, V>> entrySet() {
        Set<Entry<K, V>> entrySet = this.entrySet;
        if (entrySet == null) {
            entrySet = new AbstractSet<>() {
                private final List<Entry<K, V>> entryList = entryList();
                @Override
                public Iterator<Entry<K, V>> iterator() {
                    return entryList.iterator();
                }
                @Override
                public int size() {
                    return entryList.size();
                }
            };
            this.entrySet = entrySet;
        }
        return entrySet;
    }

}
