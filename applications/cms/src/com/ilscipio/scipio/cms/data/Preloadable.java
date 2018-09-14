package com.ilscipio.scipio.cms.data;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.ofbiz.entity.GenericValue;


public interface Preloadable {

    /**
     * 2016: Loads ALL this object's content into the current instance,
     * and depending on the PreloadWorker configuration, may also make it immutable.
     * <p>
     * NOTE: this is for implementations. client code should call
     * {@link PreloadWorker#preload(Preloadable)} instead.
     * <p>
     * WARN: IMPORTANT: AFTER THIS CALL,
     * NO FURTHER CALLS ARE ALLOWED TO MODIFY THE INSTANCE IN MEMORY
     * (EVEN if the instance is not physically made immutable!).
     * Essential for thread safety!!!
     * <p>
     * NOTE: if any local caches, they must fully implement synchronization and publishing
     * on their own (using synchronized methods or blocks, {@link java.util.concurrent.ConcurrentHashMap} or
     * {@link CmsObjectCache#getLocalCache}).
     * <p>
     * NOTE: The implementation should mark itself as preloaded as early as possible in the preload
     * call implementation, so that the {@link #isPreloaded} method can be used by PreloadWorker
     * to prevent endless loops in preloads.
     * It should then use the {@link PreloadWorker} methods instead of direct calls in order to avoid
     * such endless loops.
     */
    public void preload(PreloadWorker preloadWorker);

    // DEV NOTE: these are not necessary as long as the first call is super.preload(preloadWorker)
    // so the flags always get set early before all other calls
    //public void markPreloaded();

    //public void markImmutable();


    /**
     * Returns whether this instance was preloaded, or whether a preload() call was already run
     * for the instance (regardless of whether it has returned yet or not - for endless loop prevention).
     */
    public boolean isPreloaded();

    /**
     * Returns whether this instance is immutable, or whether a preloadImmutable() call was already run
     * for the instance (regardless of whether it has returned yet or not - for endless loop prevention).
     */
    public boolean isImmutable();

    /**
     * Preload utility, especially for assisting in implementations of preload() and preloadImmutable().
     * <p>
     * NOTE:
     */
    public static class PreloadWorker {

        private static final PreloadWorker preloadWorker = new PreloadWorker(true);
        private static final PreloadWorker preloadImmutableWorker = new PreloadWorker(false);

        private final boolean mutable;

        public PreloadWorker(boolean mutable) {
            this.mutable = mutable;
        }

        /**
         * Returns a worker that preloads but without making instance immutable.
         */
        public static PreloadWorker getPreloadWorker() {
            return preloadWorker;
        }

        /**
         * Returns a worker that preloads AND makes instance immutable.
         */
        public static PreloadWorker getPreloadImmutableWorker() {
            return preloadImmutableWorker;
        }

        /**
         * Returns a worker that preloads with optional immutable instance making (if mutable false passed).
         */
        public static PreloadWorker getPreloadWorker(boolean mutable) {
            return mutable ? preloadWorker : preloadImmutableWorker;
        }

        /**
         * Returns true if this PreloadWorker is creating immutable instances in addition to preloads.
         */
        public boolean isImmutable() {
            return !mutable;
        }

        public boolean isMutable() {
            return mutable;
        }

        public boolean isHandled(Preloadable preloadable) {
            if (preloadable == null) {
                return true;
            } else if (this.isImmutable()) {
                return preloadable.isPreloaded() && preloadable.isImmutable();
            } else {
                return preloadable.isPreloaded();
            }
        }

        /**
         * Preloads a Preloadable instance if it is not null and it does not register as preloaded
         * (and immutable, if applicable) already.
         * <p>
         * NOTE: This should be used internally in preloadImmutable() implementations to prevent endless loops
         * due to circular references.
         * <p>
         * NOTE: for this overload, the return value is always the same as the parameter, so it
         * can be discarded.
         */
        public <T extends Preloadable> T preload(T preloadable) {
            if (!isHandled(preloadable)) {
                preloadable.preload(this);
            }
            return preloadable;
        }

        /**
         * Preloads a wrapped Preloadable instance if it is not null and it does not register as preloaded
         * (and immutable, if applicable) already.
         * <p>
         * NOTE: This should be used internally in preloadImmutable() implementations to prevent endless loops
         * due to circular references.
         * <p>
         * NOTE: for this overload, the return value is always the same as the parameter, so it
         * can be discarded.
         */
        public <T extends Preloadable> Optional<T> preload(Optional<T> preloadable) {
            if (preloadable != null && preloadable.isPresent()) {
                preload(preloadable.get());
            }
            return preloadable;
        }

        public <T extends Preloadable> Collection<T> preloadDeep(Collection<T> preloadables) {
            if (preloadables != null) {
                for(Preloadable preloadable : preloadables) {
                    preload(preloadable);
                }
                return transformContainer(preloadables);
            } else {
                return null;
            }
        }

        public <T extends Preloadable> Set<T> preloadDeep(Set<T> preloadables) {
            if (preloadables != null) {
                for(Preloadable preloadable : preloadables) {
                    preload(preloadable);
                }
                return transformContainer(preloadables);
            } else {
                return null;
            }
        }

        public <T extends Preloadable> List<T> preloadDeep(List<T> preloadables) {
            if (preloadables != null) {
                for(Preloadable preloadable : preloadables) {
                    preload(preloadable);
                }
                return transformContainer(preloadables);
            } else {
                return null;
            }
        }

        public <S, T extends Preloadable> Map<S, T> preloadDeep(Map<S, T> preloadables) {
            if (preloadables != null) {
                for(Preloadable preloadable : preloadables.values()) {
                    preload(preloadable);
                }
                return transformContainer(preloadables);
            } else {
                return null;
            }
        }


        /**
         * Preloads the preloadables in a map of lists and makes the containers immutable if applicable,
         * including both the inner and outer containers.
         * <p>
         * NOTE: name is not a typo.
         */
        public <S, T extends Preloadable> Map<S, List<T>> preloadDeepListMap(Map<S, List<T>> preloadables) {
            if (preloadables != null) {
                for(Map.Entry<S, List<T>> entry : preloadables.entrySet()) {
                    entry.setValue(preloadDeep(entry.getValue()));
                }
                return transformContainer(preloadables);
            } else {
                return null;
            }
        }

        public GenericValue preloadEntity(GenericValue entity) {
            if (entity != null) {
                // TODO: investigate if more could be needed some cases
                // at least, mark the entity immutable
                if (this.isImmutable()) {
                    entity.setImmutable();
                }
                return entity;
            } else {
                return null;
            }
        }

        /**
         * Checks a generic container for needed alterations for preload (non-deep, no Preloadable involved).
         * Currently, this returns unmodifiable container if we're preloading to immutable.
         */
        public <V> Collection<V> transformContainer(Collection<V> collection) {
            return (collection != null) ? (this.mutable ? collection : Collections.unmodifiableCollection(collection)) : null;
        }

        public <V> List<V> transformContainer(List<V> list) {
            return (list != null) ? (this.mutable ? list : Collections.unmodifiableList(list)) : null;
        }

        public <V> Set<V> transformContainer(Set<V> set) {
            return (set != null) ? (this.mutable ? set : Collections.unmodifiableSet(set)) : null;
        }

        public <K, V> Map<K, V> transformContainer(Map<K, V> map) {
            return (map != null) ? (this.mutable ? map : Collections.unmodifiableMap(map)) : null;
        }

    }

    /**
     * Reference/base implementation of Preloadable, can be extended directly when
     * inheritance is possible.
     */
    public static abstract class AbstractPreloadable implements Preloadable {

        protected boolean preloaded;
        protected boolean mutable;

        public AbstractPreloadable(boolean preloaded, boolean mutable) {
            this.preloaded = preloaded;
            this.mutable = mutable;
        }

        public AbstractPreloadable() {
            this.preloaded = false;
            this.mutable = true;
        }

        @Override
        public void preload(PreloadWorker preloadWorker) {
            this.preloaded = true;
            if (preloadWorker.isImmutable()) {
                mutable = false;
            }
        }

        @Override
        public boolean isPreloaded() {
            return this.preloaded;
        }

        @Override
        public boolean isImmutable() {
            return !this.mutable;
        }

        /**
         * Throws IllegalStateException if the object is marked immutable.
         * <p>
         * The purpose of this is to guarantee we don't accidentally call methods from the frontend
         * that were not written to be thread-safe. This needs to be explicit to prevent accidental
         * create of extremely difficult to find threading bugs.
         */
        protected void preventIfImmutable() throws IllegalStateException {
            if (this.isImmutable()) {
                throw new IllegalStateException("CMS: Fatal: Immutable CMS object subjected to code"
                        + " involving instance modification or otherwise inappropriate for immutable object"
                        + " (usually this is due to running editing-mode-only methods on a CMS object cached for live render)");
            }
        }
    }

}
