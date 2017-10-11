package org.ofbiz.entity.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;

/**
 * SCIPIO: Read-only special entity sequence ID list implementations which can
 * return new sequence IDs on-demand using the {@link #get(int)} method.
 * <p>
 * IMPORTANT: The main operation that matters here is the {@link #get(int)}
 * call; other calls may currently force preloading in undesirable way (FIXME).
 * <p>
 * This works slightly differently than the standard List interface;
 * get(xxx) calls don't throw IndexOutOfBoundsExceptions and instead cause the
 * list to resize.
 */
public abstract class EntitySeqIdList<E> implements List<E> {

    public static final String module = EntitySeqIdList.class.getName();
    
    private static final boolean DEBUG = false;
    
    protected EntitySeqIdList() {
    }

    /**
     * Returns new sequence IDs from the delegator through the {@link #get(int)} method.
     * <p>
     * The list automatically grows as needed through access, though it needs an initialSize. 
     */
    public static abstract class DynamicEntitySeqIdList<E> extends EntitySeqIdList<E> {
        protected final Delegator delegator;
        protected final String seqName;
        protected final ArrayList<E> seqIdList;
        protected int size;
        
        protected DynamicEntitySeqIdList(Delegator delegator, String seqName, int initialSize) {
            this.delegator = delegator;
            this.seqName = seqName;
            this.seqIdList = new ArrayList<>(initialSize);
            //ensureIndexAccessible(initialSize - 1); // we can delay this, only needed by some operations, cleaner without
            this.size = initialSize;
        }

        protected abstract E getNextSeqId();
        
        /**
         * Automatically grows the list to the given index (both capacity AND size).
         */
        protected void ensureIndexAccessible(int index) {
            while(seqIdList.size() <= index) {
                seqIdList.add(null);
            }
            // NOTE: this is the only place that this.size is adjusted, and size is never decreased.
            if (size < seqIdList.size()) size = seqIdList.size();
        }
        
        protected void forcePreload() {
            forcePreload(0, size);
        }
        
        protected void forcePreload(int fromIndex, int toIndex) {
            // TODO: optimize this
            if (toIndex > seqIdList.size()) {
                ensureIndexAccessible(toIndex - 1);
            }
            for(int i=fromIndex; i < toIndex; i++) {
                if (seqIdList.get(i) == null) {
                    seqIdList.set(i, getNextSeqId());
                }
            }
        }
        
        @Override
        public E get(int index) {
            E value = null;
            if (index >= seqIdList.size()) {
                ensureIndexAccessible(index);
            } else {
                value = seqIdList.get(index);
            }
            if (value == null) {
                value = getNextSeqId();
                seqIdList.set(index, value);
            }
            if (DEBUG) Debug.logInfo("Entity seq list internal after get at index " + index + ": " + seqIdList, module);
            return value;
        }
        

        @Override
        public int size() {
            return size;
        }

        @Override
        public boolean isEmpty() { // NOTE: will basically always be falses
            return (size == 0); // seqIdList.isEmpty();
        }

        /**
         * NOTE: this only returns true for the ID if it was already loaded
         * using a {@link #get(int)} call.
         */
        @Override
        public boolean contains(Object o) { // WARN: poorly-defined
            return seqIdList.contains(o);
        }

        /**
         * WARN: FIXME: this currently forces a preload of the whole collection.
         */
        @Override
        public Iterator<E> iterator() {
            forcePreload();
            return Collections.unmodifiableList(seqIdList).iterator();
        }

        /**
         * WARN: FIXME: this currently forces a preload of the whole collection.
         */
        @Override
        public Object[] toArray() {
            forcePreload();
            return Collections.unmodifiableList(seqIdList).toArray();
        }

        /**
         * WARN: FIXME: this currently forces a preload of the whole collection.
         */
        @Override
        public <T> T[] toArray(T[] a) {
            forcePreload();
            return Collections.unmodifiableList(seqIdList).toArray(a);
        }

        @Override
        public boolean add(E e) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean remove(Object o) {
            throw new UnsupportedOperationException();
        }

        /**
         * NOTE: this only returns true for the ID if it was already loaded
         * using a {@link #get(int)} call.
         */
        @Override
        public boolean containsAll(Collection<?> c) {
            return seqIdList.containsAll(c);
        }

        @Override
        public boolean addAll(Collection<? extends E> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean addAll(int index, Collection<? extends E> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean removeAll(Collection<?> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean retainAll(Collection<?> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void clear() {
            throw new UnsupportedOperationException();
        }

        @Override
        public E set(int index, E element) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void add(int index, E element) {
            throw new UnsupportedOperationException();
        }

        @Override
        public E remove(int index) {
            throw new UnsupportedOperationException();
        }

        /**
         * NOTE: this only returns the index for the ID if it was already loaded
         * using a {@link #get(int)} call.
         */
        @Override
        public int indexOf(Object o) {
            return seqIdList.indexOf(o);
        }

        /**
         * NOTE: this only returns the index for the ID if it was already loaded
         * using a {@link #get(int)} call.
         */
        @Override
        public int lastIndexOf(Object o) {
            return seqIdList.lastIndexOf(o);
        }

        /**
         * WARN: FIXME: this currently forces a preload of the whole collection.
         */
        @Override
        public ListIterator<E> listIterator() {
            forcePreload();
            return Collections.unmodifiableList(seqIdList).listIterator();
        }

        /**
         * WARN: FIXME: this currently forces a preload of the whole collection.
         */
        @Override
        public ListIterator<E> listIterator(int index) {
            forcePreload();
            return Collections.unmodifiableList(seqIdList).listIterator(index);
        }

        /**
         * WARN: FIXME: this currently forces a preload of the whole collection.
         */
        @Override
        public List<E> subList(int fromIndex, int toIndex) {
            forcePreload(fromIndex, toIndex);
            return Collections.unmodifiableList(seqIdList).subList(fromIndex, toIndex);
        }
    }
    
    public static class DynamicEntitySeqIdStringList extends DynamicEntitySeqIdList<String> {
        public DynamicEntitySeqIdStringList(Delegator delegator, String seqName, int initialSize) {
            super(delegator, seqName, initialSize);
        }

        @Override
        protected String getNextSeqId() {
            return delegator.getNextSeqId(seqName);
        }
    }
    
    public static class DynamicEntitySeqIdLongList extends DynamicEntitySeqIdList<Long> {
        public DynamicEntitySeqIdLongList(Delegator delegator, String seqName, int initialSize) {
            super(delegator, seqName, initialSize);
        }

        @Override
        protected Long getNextSeqId() {
            return delegator.getNextSeqIdLong(seqName);
        }
    }
    
    /**
     * Returns a consecutive ID through the {@link #get(int)} method, so that
     * callers don't have to allocate huge arrays with predictable values.
     */
    public static abstract class FixedEntitySeqIdList<E> extends EntitySeqIdList<E> {
        protected long baseId;
        protected int size;

        protected FixedEntitySeqIdList(Object baseId, int size) {
            this.baseId = toLong(baseId);
            this.size = size;
        }

        protected abstract E toIdType(long value);
        
        protected Long toLong(Object obj) {
            if (obj instanceof Long) return (Long) obj;
            else if (obj instanceof String) return Long.parseLong((String) obj);
            else if (obj instanceof Integer) return ((Integer) obj).longValue();
            else if (obj == null) throw new NullPointerException("expected Long, String or Integer for entity sequence ID value, but got null instead");
            else throw new IllegalArgumentException("expected Long, String or Integer for entity sequence ID value, but instead got: " + obj.getClass().getName());
        }
        
        @Override
        public E get(int index) {
            if (index >= size) size = index + 1; // auto-grow
            if (DEBUG) Debug.logInfo("Returning fixed entity seq ID: " + (baseId + index), module);
            return toIdType(baseId + index);
        }
        
        @Override
        public int size() {
            return size;
        }

        @Override
        public boolean isEmpty() {
            return size > 0;
        }

        @Override
        public boolean contains(Object o) {
            long value = toLong(o);
            return (value >= baseId) && (value < (baseId + size));
        }

        @Override
        public Iterator<E> iterator() {
            throw new UnsupportedOperationException("TODO - NOT YET IMPLEMENTED"); // TODO
        }

        @Override
        public Object[] toArray() {
            throw new UnsupportedOperationException("TODO - NOT YET IMPLEMENTED"); // TODO
        }

        @Override
        public <T> T[] toArray(T[] a) {
            throw new UnsupportedOperationException("TODO - NOT YET IMPLEMENTED"); // TODO
        }

        @Override
        public boolean add(E e) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean remove(Object o) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean containsAll(Collection<?> c) {
            for(Object obj : c) {
                if (!contains(obj)) return false;
            }
            return true;
        }

        @Override
        public boolean addAll(Collection<? extends E> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean addAll(int index, Collection<? extends E> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean removeAll(Collection<?> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean retainAll(Collection<?> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void clear() {
            throw new UnsupportedOperationException();
        }

        @Override
        public E set(int index, E element) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void add(int index, E element) {
            throw new UnsupportedOperationException();
        }

        @Override
        public E remove(int index) {
            throw new UnsupportedOperationException();
        }

        @Override
        public int indexOf(Object o) {
            long value = toLong(o);
            
            long index = (value - baseId);
            
            if (index >= 0 && index < size) return (int) index;
            else return -1;
        }

        @Override
        public int lastIndexOf(Object o) {
            return indexOf(o);
        }

        @Override
        public ListIterator<E> listIterator() {
            throw new UnsupportedOperationException("TODO - NOT YET IMPLEMENTED"); // TODO
        }

        @Override
        public ListIterator<E> listIterator(int index) {
            throw new UnsupportedOperationException("TODO - NOT YET IMPLEMENTED"); // TODO
        }

        @Override
        public List<E> subList(int fromIndex, int toIndex) {
            throw new UnsupportedOperationException("TODO - NOT YET IMPLEMENTED"); // TODO
        }
    }
    
    public static class FixedEntitySeqIdStringList extends FixedEntitySeqIdList<String> {
        public FixedEntitySeqIdStringList(Object baseId, int size) {
            super(baseId, size);
        }

        @Override
        protected String toIdType(long value) {
            return String.valueOf(value);
        }
    }
    
    public static class FixedEntitySeqIdLongList extends FixedEntitySeqIdList<Long> {
        public FixedEntitySeqIdLongList(Object baseId, int size) {
            super(baseId, size);
        }

        @Override
        protected Long toIdType(long value) {
            return value;
        }
    }
    
    
    // OLD METHODS
    // for reference only - use above classes instead.
    
    static List<String> reserveEntitySeqIdList(Delegator delegator, String seqName, Integer count) {
        List<String> list = new ArrayList<>(count);
        for(int i = 0; i < count; i++) {
            list.add(delegator.getNextSeqId(seqName, 1));
        }
        return list;
    }
    
    static List<String> makeConsecutiveEntitySeqIdList(Long baseId, Integer count) {
        List<String> list = new ArrayList<>(count);
        for(long i = baseId; i < (baseId + count); i++) {
            list.add(String.valueOf(i));
        }
        return list;
    }
}
