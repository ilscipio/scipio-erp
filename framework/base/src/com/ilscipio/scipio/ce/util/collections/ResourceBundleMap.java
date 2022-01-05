package com.ilscipio.scipio.ce.util.collections;

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.ResourceBundle;
import java.util.Set;

/**
 * Immutable simple map wrapper around java ResourceBundle.
 * <p>SCIPIO: 2.1.0: Added for UtilProperties/UtilCache.</p>
 */
public class ResourceBundleMap extends AbstractMap<String, Object> implements Serializable {
    private final ResourceBundle rb;

    public ResourceBundleMap(ResourceBundle rb) {
        this.rb = rb;
    }

    @Override
    public Object get(Object key) { // optimization
        return rb.getObject(key != null ? key.toString() : null);
    }

    @Override
    public Set<Entry<String, Object>> entrySet() {
        return new AbstractSet<Entry<String, Object>>() {
            private final Set<String> keySet = rb.keySet();
            @Override
            public Iterator<Entry<String, Object>> iterator() {
                final Iterator<String> keySetIt = keySet.iterator();
                return new Iterator<Entry<String, Object>>() {
                    @Override
                    public boolean hasNext() {
                        return keySetIt.hasNext();
                    }

                    @Override
                    public Entry<String, Object> next() {
                        String key = keySetIt.next();
                        return new SimpleEntry<>(key, rb.getObject(key));
                    }
                };
            }

            @Override
            public int size() {
                return keySet.size();
            }
        };
    }
}
