/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.base.util.collections;

import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.UtilGenerics;


/**
 * Map Stack
 * <p>
 * SCIPIO: This class has been modified for:
 * <ul>
 * <li>new traversal operations (for partial preservation of key insert order of underlying maps) 
 * <li>better copy construction and construction with initial map
 * <li>making the main {@link #stackList} variable private
 * <li>optimization: switching the {@link #stackList} to {@link java.util.ArrayList} instead of LinkedList
 * <li>optimization: reversing {@link #stackList} structure so that {@link #push} appends to end of list, 
 *     for ArrayList to avoid insert-firsts (NOTE: now {@link #addToBottom(Map)} for property-maps/uiLabelMap uses
 *     insert-firsts, but it is better because there is no removeFromBotton compared to push/pop; in any case,
 *     in practice ArrayList with small capacity is faster for insert-first than LinkedList anyway)
 * <li>optimization: using indexed loops for ArrayList (avoids iterator creation)
 * </ul>
 * <p>
 * Optimization notes: These were sorely needed because this class performs expensive multi-map lookups 
 * for every {@link #get(Object)} and similar call, while being used for hundreds of map lookups at each render, 
 * through its use as main context and property-maps (uiLabelMap). While we cannot improve on the
 * basic algorithm, switching to ArrayList and preventing iterator usage can double the speed, according to practical
 * tests with recent JDKs. Only a straight array would be faster (but cannot handle growing well).
 */
public class MapContext<K, V> implements Map<K, V>, LocalizedMap<V> {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * SCIPIO: Initial stack capacity for {@link #stackList} {@link java.util.ArrayList}.
     * <p>
     * We overestimate this for most cases so that the ArrayList almost never needs
     * to resize. In typical screen renders, the context never grows far into
     * the double-digits, and if it does, small capacity resize is fast anyway.
     * Similarly, property-maps stack size never gets this high in practice.
     */
    private static final int STACK_INITIAL_CAPACITY = 32;

    public static final <K, V> MapContext<K, V> getMapContext() {
        return new MapContext<K, V>();
    }

    public static <K, V> MapContext<K, V> createMapContext() {
        MapContext<K, V> newValue = MapContext.getMapContext();
        // initialize with a single entry
        newValue.push();
        return newValue;
    }

    //@SuppressWarnings("unchecked")
    public static <K, V> MapContext<K, V> createMapContext(Map<K, V> baseMap) {
        // SCIPIO: use constructors
        //MapContext<K, V> newValue = MapContext.getMapContext();
        MapContext<K, V> newValue;
        if (baseMap instanceof MapContext) {
            //newValue.stackList.addAll(((MapContext) baseMap).stackList);
            newValue = new MapContext<>((MapContext<K, V>) baseMap);
        } else {
            //newValue.stackList.add(0, baseMap);
            newValue = new MapContext<>(baseMap);
        }
        return newValue;
    }

    /** Does a shallow copy of the internal stack of the passed MapContext; enables simultaneous stacks that share common parent Maps */
    public static <K, V> MapContext<K, V> createMapContext(MapContext<K, V> source) {
        // SCIPIO: use copy constructor
        //MapContext<K, V> newValue = MapContext.getMapContext();
        //newValue.stackList.addAll(source.stackList);
        //return newValue;
        return new MapContext<>(source);
    }

    protected MapContext() {
        super();
        stackList = new ArrayList<Map<K, V>>(STACK_INITIAL_CAPACITY); // SCIPIO: switched to ArrayList
    }
    
    /**
     * SCIPIO: Shallow copy constructor - copies the stackList.
     */
    protected MapContext(MapContext<K, V> source) {
        stackList = new ArrayList<Map<K, V>>(source.stackList); // SCIPIO: switched to ArrayList
    }

    /**
     * SCIPIO: Constructor with initial map.
     */
    protected MapContext(Map<K, V> baseMap) {
        stackList = new ArrayList<Map<K, V>>(STACK_INITIAL_CAPACITY); // SCIPIO: switched to ArrayList
        stackList.add(baseMap);
    }

    // SCIPIO: 2018-08-10: protected was a bad idea for this - added constructor for subclasses instead
    //protected List<Map<K, V>> stackList = new LinkedList<Map<K, V>>();
    private List<Map<K, V>> stackList;

    public void reset() {
        stackList = new ArrayList<Map<K, V>>(STACK_INITIAL_CAPACITY); // SCIPIO: switched to ArrayList
    }

    /** Puts a new Map on the top of the stack */
    public void push() {
        Map<K, V> newMap = new HashMap<K, V>();
        // SCIPIO: reversed order
        //this.stackList.add(0,newMap);
        this.stackList.add(newMap);
    }

    /** Puts an existing Map on the top of the stack (top meaning will override lower layers on the stack) */
    public void push(Map<K, V> existingMap) {
        if (existingMap == null) {
            throw new IllegalArgumentException("Error: cannot push null existing Map onto a MapContext");
        }
        // SCIPIO: reversed order
        //this.stackList.add(0, existingMap);
        this.stackList.add(existingMap);
    }

    /** Puts an existing Map on the BOTTOM of the stack (bottom meaning will be overriden by lower layers on the stack, ie everything else already there) */
    public void addToBottom(Map<K, V> existingMap) {
        if (existingMap == null) {
            throw new IllegalArgumentException("Error: cannot add null existing Map to bottom of a MapContext");
        }
        // SCIPIO: reversed order
        //this.stackList.add(existingMap);
        this.stackList.add(0, existingMap);
    }

    /** Remove and returns the Map from the top of the stack; if there is only one Map on the stack it returns null and does not remove it */
    public Map<K, V> pop() {
        // always leave at least one Map in the List, ie never pop off the last Map
        if (this.stackList.size() > 1) {
            // SCIPIO: reversed order
            //return stackList.remove(0);
            return stackList.remove(stackList.size() - 1);
        } else {
            return null;
        }
    }

    /**
     * Creates a MapContext object that has the same Map objects on its stack;
     * meant to be used to enable a
     * situation where a parent and child context are operating simultaneously
     * using two different MapContext objects, but sharing the Maps in common
     */
    public MapContext<K, V> standAloneStack() {
        MapContext<K, V> standAlone = MapContext.createMapContext(this);
        return standAlone;
    }

    /**
     * Creates a MapContext object that has the same Map objects on its stack,
     * but with a new Map pushed on the top; meant to be used to enable a
     * situation where a parent and child context are operating simultaneously
     * using two different MapContext objects, but sharing the Maps in common
     */
    public MapContext<K, V> standAloneChildStack() {
        MapContext<K, V> standAloneChild = MapContext.createMapContext(this);
        standAloneChild.push();
        return standAloneChild;
    }

    /* (non-Javadoc)
     * @see java.util.Map#size()
     */
    public int size() {
        // a little bit tricky; to represent the apparent size we need to aggregate all keys and get a count of unique keys
        // this is a bit of a slow way, but gets the best number possible
        Set<K> keys = this.keySet();
        return keys.size();
    }

    /* (non-Javadoc)
     * @see java.util.Map#isEmpty()
     */
    public boolean isEmpty() {
        // walk the stackList and if any is not empty, return false; otherwise return true
        // SCIPIO: reversed order
        //for (Map<K, V> curMap: this.stackList) {
        for(int i = this.stackList.size() - 1; i >= 0; i--) {
            Map<K, V> curMap = this.stackList.get(i);
            if (!curMap.isEmpty()) {
                return false;
            }
        }
        return true;
    }

    /* (non-Javadoc)
     * @see java.util.Map#containsKey(java.lang.Object)
     */
    public boolean containsKey(Object key) {
        // walk the stackList and for the first place it is found return true; otherwise refurn false
        // SCIPIO: reversed order
        //for (Map<K, V> curMap: this.stackList) {
        for(int i = this.stackList.size() - 1; i >= 0; i--) {
            Map<K, V> curMap = this.stackList.get(i);
            if (curMap.containsKey(key)) {
                return true;
            }
        }
        return false;
    }

    /* (non-Javadoc)
     * @see java.util.Map#containsValue(java.lang.Object)
     */
    public boolean containsValue(Object value) {
        // walk the stackList and the entries for each Map and if nothing is in for the current key, consider it an option, otherwise ignore
        Set<K> resultKeySet = new HashSet<K>();
        // SCIPIO: reversed order
        //for (Map<K, V> curMap: this.stackList) {
        for(int i = this.stackList.size() - 1; i >= 0; i--) {
            Map<K, V> curMap = this.stackList.get(i);
            for (Map.Entry<K, V> curEntry: curMap.entrySet()) {
                if (!resultKeySet.contains(curEntry.getKey())) {
                    resultKeySet.add(curEntry.getKey());
                    if (value == null) {
                        if (curEntry.getValue() == null) {
                            return true;
                        }
                    } else {
                        if (value.equals(curEntry.getValue())) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    /* (non-Javadoc)
     * @see java.util.Map#get(java.lang.Object)
     */
    public V get(Object key) {
        // walk the stackList and for the first place it is found return true; otherwise refurn false
        // SCIPIO: reversed order
        //for (Map<K, V> curMap: this.stackList) {
        for(int i = this.stackList.size() - 1; i >= 0; i--) {
            Map<K, V> curMap = this.stackList.get(i);
            // only return if the curMap contains the key, rather than checking for null; this allows a null at a lower level to override a value at a higher level
            if (curMap.containsKey(key)) {
                return curMap.get(key);
            }
        }
        return null;
    }

    /* (non-Javadoc)
     * @see org.ofbiz.base.util.collections.LocalizedMap#get(java.lang.String, java.util.Locale)
     */
    public V get(String name, Locale locale) {
        // walk the stackList and for the first place it is found return true; otherwise refurn false
        // SCIPIO: reversed order
        //for (Map<K, V> curMap: this.stackList) {
        for(int i = this.stackList.size() - 1; i >= 0; i--) {
            Map<K, V> curMap = this.stackList.get(i);
            // only return if the curMap contains the key, rather than checking for null; this allows a null at a lower level to override a value at a higher level
            if (curMap.containsKey(name)) {
                if (curMap instanceof LocalizedMap<?>) {
                    LocalizedMap<V> lmap = UtilGenerics.cast(curMap);
                    return lmap.get(name, locale);
                } else {
                    return curMap.get(name);
                }
            }
        }
        return null;
    }

    /* (non-Javadoc)
     * @see java.util.Map#put(java.lang.Object, java.lang.Object)
     */
    public V put(K key, V value) {
        // all write operations are local: only put in the Map on the top of the stack
        // SCIPIO: reversed order
        //Map<K, V> currentMap = this.stackList.get(0);
        Map<K, V> currentMap = this.stackList.get(this.stackList.size() - 1);
        return currentMap.put(key, value);
    }

    /* (non-Javadoc)
     * @see java.util.Map#remove(java.lang.Object)
     */
    public V remove(Object key) {
        // all write operations are local: only remove from the Map on the top of the stack
        // SCIPIO: reversed order
        //Map<K, V> currentMap = this.stackList.get(0);
        Map<K, V> currentMap = this.stackList.get(this.stackList.size() - 1);
        return currentMap.remove(key);
    }

    /* (non-Javadoc)
     * @see java.util.Map#putAll(java.util.Map)
     */
    public void putAll(Map<? extends K, ? extends V> arg0) {
        // all write operations are local: only put in the Map on the top of the stack
        // SCIPIO: reversed order
        //Map<K, V> currentMap = this.stackList.get(0);
        Map<K, V> currentMap = this.stackList.get(this.stackList.size() - 1);
        currentMap.putAll(arg0);
    }

    /* (non-Javadoc)
     * @see java.util.Map#clear()
     */
    public void clear() {
        // all write operations are local: only clear the Map on the top of the stack
        // SCIPIO: reversed order
        //this.stackList.get(0).clear();
        this.stackList.get(this.stackList.size() - 1).clear();
    }

    /* (non-Javadoc)
     * @see java.util.Map#keySet()
     */
    public Set<K> keySet() {
        // walk the stackList and aggregate all keys
        // SCIPIO: 2018-06-14: forced to use LinkedHashSet because otherwise cannot preserve
        // insert order when caller decides to use LinkedHashMap as stack entries.
        //Set<K> resultSet = new HashSet<K>();
        Set<K> resultSet = new LinkedHashSet<K>();
        // SCIPIO: reversed order
        //for (Map<K, V> curMap: this.stackList) {
        for(int i = this.stackList.size() - 1; i >= 0; i--) {
            Map<K, V> curMap = this.stackList.get(i);
            resultSet.addAll(curMap.keySet());
        }
        return Collections.unmodifiableSet(resultSet);
    }

    /* (non-Javadoc)
     * @see java.util.Map#values()
     */
    public Collection<V> values() {
        // walk the stackList and the entries for each Map and if nothing is in for the current key, put it in
        Set<K> resultKeySet = new HashSet<K>();
        List<V> resultValues = new ArrayList<V>(STACK_INITIAL_CAPACITY); // SCIPIO: switched to ArrayList
        // SCIPIO: reversed order
        //for (Map<K, V> curMap: this.stackList) {
        for(int i = this.stackList.size() - 1; i >= 0; i--) {
            Map<K, V> curMap = this.stackList.get(i);
            for (Map.Entry<K, V> curEntry: curMap.entrySet()) {
                if (!resultKeySet.contains(curEntry.getKey())) {
                    resultKeySet.add(curEntry.getKey());
                    resultValues.add(curEntry.getValue());
                }
            }
        }
        return Collections.unmodifiableCollection(resultValues);
    }

    /* (non-Javadoc)
     * @see java.util.Map#entrySet()
     */
    public Set<Map.Entry<K, V>> entrySet() {
        // walk the stackList and the entries for each Map and if nothing is in for the current key, put it in
        Set<K> resultKeySet = new HashSet<K>();
        Set<Map.Entry<K, V>> resultEntrySet = new ListSet<Map.Entry<K, V>>();
        // SCIPIO: reversed order
        //for (Map<K, V> curMap: this.stackList) {
        for(int i = this.stackList.size() - 1; i >= 0; i--) {
            Map<K, V> curMap = this.stackList.get(i);
            for (Map.Entry<K, V> curEntry: curMap.entrySet()) {
                if (!resultKeySet.contains(curEntry.getKey())) {
                    resultKeySet.add(curEntry.getKey());
                    resultEntrySet.add(curEntry);
                }
            }
        }
        return Collections.unmodifiableSet(resultEntrySet);
    }

    @Override
    public String toString() {
        StringBuilder fullMapString = new StringBuilder();
        int curLevel = 0;
        // SCIPIO: reversed order
        //for (Map<K, V> curMap: this.stackList) {
        for(int i = this.stackList.size() - 1; i >= 0; i--) {
            Map<K, V> curMap = this.stackList.get(i);
            fullMapString.append("============================== Start stack level " + curLevel + "\n");
            for (Map.Entry<K, V> curEntry: curMap.entrySet()) {

                fullMapString.append("==>[");
                fullMapString.append(curEntry.getKey());
                fullMapString.append("]:");
                // skip the instances of MapContext to avoid infinite loop
                if (curEntry.getValue() instanceof MapContext<?, ?>) {
                    fullMapString.append("<Instance of MapContext, not printing to avoid infinite recursion>");
                } else {
                    fullMapString.append(curEntry.getValue());
                }
                fullMapString.append("\n");
            }
            fullMapString.append("============================== End stack level " + curLevel + "\n");
            curLevel++;
        }
        return fullMapString.toString();
    }

    private static final class ListSet<E> extends AbstractSet<E> implements Set<E> {

        protected final List<E> listImpl;

        public ListSet() {
            this.listImpl = new ArrayList<E>();
        }

        public int size() {
            return this.listImpl.size();
        }

        public Iterator<E> iterator() {
            return this.listImpl.iterator();
        }

        public boolean add(final E obj) {
            boolean added = false;

            if (!this.listImpl.contains(obj)) {
                added = this.listImpl.add(obj);
            }

            return added;
        }

        public boolean isEmpty() {
            return this.listImpl.isEmpty();
        }

        public boolean contains(final Object obj) {
            return this.listImpl.contains(obj);
        }

        public boolean remove(final Object obj) {
            return this.listImpl.remove(obj);
        }

        public void clear() {
            this.listImpl.clear();
        }

    }

    /**
     * SCIPIO: Alternative to {@link #keySet()} that builds the collection by visiting
     * the deepest (oldest) map stacks first.
     * <p>
     * Partially preserves insert order if the underlying stack maps are insert-order preserving.
     * <p>
     * May help preserve order when using LinkedHashMap in stacks.
     * <p>
     * Added 2018-06-14.
     */
    public Set<K> keySetDeepFirst() {
        Set<K> resultSet = new LinkedHashSet<K>();
        // SCIPIO: reversed order
        //ListIterator<Map<K, V>> it = this.stackList.listIterator(this.stackList.size());
        //while(it.hasPrevious()) {
        //    Map<K, V> curMap = it.previous();
        for(int i = 0; i < this.stackList.size(); i++) {
            Map<K, V> curMap = this.stackList.get(i);
            resultSet.addAll(curMap.keySet());
        }
        return Collections.unmodifiableSet(resultSet);
    }

    /**
     * SCIPIO: Alternative to {@link #values()} that builds the collection by visiting
     * the deepest (oldest) map stacks first.
     * <p>
     * Partially preserves insert order if the underlying stack maps are insert-order preserving.
     * <p>
     * May help preserve order when using LinkedHashMap in stacks.
     * <p>
     * Added 2018-06-14.
     */
    public Collection<V> valuesDeepFirst() {
        Map<K, V> resultMap = new LinkedHashMap<K, V>();
        // SCIPIO: reversed order
        //ListIterator<Map<K, V>> it = this.stackList.listIterator(this.stackList.size());
        //while(it.hasPrevious()) {
        //    Map<K, V> curMap = it.previous();
        for(int i = 0; i < this.stackList.size(); i++) {
            Map<K, V> curMap = this.stackList.get(i);
            resultMap.putAll(curMap);
        }
        return Collections.unmodifiableCollection(resultMap.values());
    }

    /**
     * SCIPIO: Alternative to {@link #entrySet()} that builds the collection by visiting
     * the deepest (oldest) map stacks first.
     * <p>
     * Partially preserves insert order if the underlying stack maps are insert-order preserving.
     * <p>
     * May help preserve order when using LinkedHashMap in stacks.
     * <p>
     * Added 2018-06-14.
     */
    public Set<Map.Entry<K, V>> entrySetDeepFirst() {
        Map<K, V> resultMap = new LinkedHashMap<K, V>();
        // SCIPIO: reversed order
        //ListIterator<Map<K, V>> it = this.stackList.listIterator(this.stackList.size());
        //while(it.hasPrevious()) {
        //    Map<K, V> curMap = it.previous();
        for(int i = 0; i < this.stackList.size(); i++) {
            Map<K, V> curMap = this.stackList.get(i);
            resultMap.putAll(curMap);
        }
        return Collections.unmodifiableSet(resultMap.entrySet());
    }

    /**
     * SCIPIO: Returns a MapContext whose {@link #keySet()}, {@link #values()}, {@link #entrySet()}
     * methods iterate the oldest (deepest) maps in the stack first.
     * <p>
     * Partially preserves insert order if the underlying stack maps are insert-order preserving.
     * <p>
     * Useful for when the stacked maps are LinkedHashMaps.
     * <p>
     * Added 2018-06-14.
     */
    public static <K, V> MapContext<K, V> getIterDeepFirstMapContext() {
        return new IterDeepFirstMapContext<>();
    }

    /**
     * SCIPIO: Returns an adapter around this MapContext
     * whose {@link #keySet()}, {@link #values()}, {@link #entrySet()} methods
     * iterate the oldest (deepest) maps in the stack first.
     * <p>
     * Partially preserves insert order if the underlying stack maps are insert-order preserving.
     * <p>
     * Useful for when the stacked maps are LinkedHashMaps.
     * <p>
     * Added 2018-06-14.
     */
    public Map<K, V> getIterDeepFirstAdapter() {
        return new IterDeepFirstAdapter<>(this);
    }

    protected static class IterDeepFirstAdapter<K, V> implements Map<K, V> {
        private final MapContext<K, V> mapCtx;
        protected IterDeepFirstAdapter(MapContext<K, V> mapCtx) { this.mapCtx = mapCtx; }
        @Override public int size() { return mapCtx.size(); }
        @Override public boolean isEmpty() { return mapCtx.isEmpty(); }
        @Override public boolean containsKey(Object key) { return mapCtx.containsKey(key); }
        @Override public boolean containsValue(Object value) { return mapCtx.containsValue(value); }
        @Override public V get(Object key) { return mapCtx.get(key); }
        @Override public V put(K key, V value) { return mapCtx.put(key, value); }
        @Override public V remove(Object key) { return mapCtx.remove(key); }
        @Override public void putAll(Map<? extends K, ? extends V> m) { mapCtx.putAll(m); }
        @Override public void clear() { mapCtx.clear(); }
        @Override public Set<K> keySet() { return mapCtx.keySetDeepFirst(); } // SPECIAL
        @Override public Collection<V> values() { return mapCtx.valuesDeepFirst(); } // SPECIAL
        @Override public Set<Entry<K, V>> entrySet() { return mapCtx.entrySetDeepFirst(); } // SPECIAL
        @Override public boolean equals(Object o) { return mapCtx.equals(o); }
        @Override public int hashCode() { return mapCtx.hashCode(); }
    }

    protected static class IterDeepFirstMapContext<K, V> extends MapContext<K, V> {
        @Override public Set<K> keySet() { return keySetDeepFirst(); }
        @Override public Collection<V> values() { return valuesDeepFirst(); }
        @Override public Set<Entry<K, V>> entrySet() { return entrySetDeepFirst(); }
    }
}
