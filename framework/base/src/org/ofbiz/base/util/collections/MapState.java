package org.ofbiz.base.util.collections;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Small helper to save the state of certain map keys (context fields) or other, to restore them accurately after a call (SCIPIO),
 * without the need for {@link MapContext#push}.
 * Automatically handles MapContext/MapStack where only the top/current layer must be saved/restored.
 * WARN: Assumes the MapContext is at the same level at the restore as it was at the save.
 */
public abstract class MapState<K, V> {

    private static class SavedKeysMapState<K, V> extends MapState<K, V> {
        private final List<StateEntry<K, V>> savedEntries;

        private SavedKeysMapState(List<StateEntry<K, V>> savedEntries) { // NOTE: may get subclassed for new methods
            this.savedEntries = savedEntries;
        }

        protected static <K, V> MapState saveKeysStd(Map<K, V> map, List<K> keys) {
            Map<K, V> savedValues = new HashMap<>();
            List<StateEntry<K, V>> savedEntries = new ArrayList<>(keys.size());
            for(K key : keys) {
                V value = map.get(key);
                savedEntries.add(new StateEntry<>(key, value, value != null || map.containsKey(key)));
            }
            return new SavedKeysMapState(savedEntries);
        }

        @Override
        public void restore(Map<K, V> map) {
            restoreKeysStd(MapContext.getCurrentMap(map));
        }

        protected void restoreKeysStd(Map<K, V> map) {
            for(StateEntry<K, V> entry : savedEntries) {
                if (entry.present) {
                    map.put(entry.getKey(), entry.getValue());
                } else {
                    map.remove(entry.getKey());
                }
            }
        }

        private static class StateEntry<K, V> {
            protected final K key;
            protected final V value;
            protected final boolean present;

            private StateEntry(K key, V value, boolean present) {
                this.key = key;
                this.value = value;
                this.present = present;
            }

            public K getKey() { return key; }
            public V getValue() { return value; }
            public boolean isPresent() { return present; }
        }
    }

    /**
     * Saves the key values and presence of
     */
    public static <K, V> MapState saveKeys(Map<K, V> map, List<K> keys) {
        return SavedKeysMapState.saveKeysStd(MapContext.getCurrentMap(map), keys);
    }

    public static <K, V> MapState saveKeys(Map<K, V> map, K... keys) {
        return SavedKeysMapState.saveKeysStd(MapContext.getCurrentMap(map), Arrays.asList(keys));
    }

    /**
     * Restores the original map based on this saved state, using the passed map, which usually should be the original map/context passed.
     * Usually this will be in a finally block.
     */
    public abstract void restore(Map<K, V> map);

}
