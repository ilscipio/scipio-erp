package com.ilscipio.scipio.ce.webapp.control.util;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/**
 * Provides generalized LoginWorker-like externalLoginKey-like
 * access token functionality.
 * <p>
 * Tokens are associated in the provider with a map of tokens to values.
 * If caller doesn't need the values, simply use a dummy Object.
 * <p>
 * All the current implementations are thread-safe
 * but differ on key weakness and performance.
 * <p>
 * Added 2018-05-05.
 */
public abstract class AccessTokenProvider<V> {

    private final EventHandler<V> eventHandler;

    protected AccessTokenProvider(EventHandler<V> eventHandler) {
        this.eventHandler = (eventHandler != null) ? eventHandler : NoopEventHandler.getInstance();
    }

    /**
     * Creates a new token string ID.
     * By default, uses same UUID generation as
     * {@link org.ofbiz.webapp.control.LoginWorker#getExternalLoginKey}.
     */
    public String newTokenString() {
        return UUID.randomUUID().toString();
    }

    /**
     * Creates a new token.
     * The token is NOT automatically stored in the provider (see {@link #put(AccessToken, Object)).
     */
    public abstract AccessToken newToken();
    public abstract AccessToken newToken(String tokenString);

    public abstract V put(AccessToken token, V value);
    public V put(String tokenString, V value) {
        return put(newToken(tokenString), value);
    }

    public abstract V get(AccessToken token);
    public V get(String tokenString) {
        return get(newToken(tokenString));
    }

    public abstract V remove(AccessToken token);
    public V remove(String tokenString) {
        return remove(newToken(tokenString));
    }

    protected EventHandler<V> getEventHandler() {
        return eventHandler;
    }

    /**
     * Access token, whose {@link #toString()} method provides the string
     * representation (for URLs).
     */
    @SuppressWarnings("serial")
    public static abstract class AccessToken implements Serializable {
        @Override
        public int hashCode() {
            return toString().hashCode();
        }

        @Override
        public boolean equals(Object other) {
            if (this == other) return true;
            if (other == null) return false;
            return toString().equals(other.toString());
        }
    }

    /**
     * Simple access token equivalent to a single string.
     * DEV NOTE: This wrapper is needed for weak hash map keys to work.
     * WARN: does not support null tokenString; use {@link NullSimpleAccessToken} for null token.
     */
    @SuppressWarnings("serial")
    public static class SimpleAccessToken extends AccessToken {
        protected final String tokenString;

        protected SimpleAccessToken(String tokenString) {
            this.tokenString = tokenString;
        }

        public static SimpleAccessToken newToken(String tokenString) {
            return (tokenString != null) ? new SimpleAccessToken(tokenString) : NullSimpleAccessToken.INSTANCE;
        }

        @Override
        public String toString() {
            return tokenString;
        }
    }

    @SuppressWarnings("serial")
    public static class NullSimpleAccessToken extends SimpleAccessToken {
        public static final NullSimpleAccessToken INSTANCE = new NullSimpleAccessToken();

        private NullSimpleAccessToken() {
            super(null);
        }

        @Override
        public int hashCode() {
            return 0;
        }

        @Override
        public boolean equals(Object other) {
            return (other == null) || (other.toString() == null);
        }
    }



    /**
     * Returns a new THREAD-SAFE token provider that does NOT have weak keys (may be faster).
     * <p>
     * WARN: Caller should ONLY use this if he knows he can always clear the keys himself.
     */
    public static <V> AccessTokenProvider<V> newAccessTokenProvider(EventHandler<V> eventHandler) {
        return new ConcurrentSimpleAccessTokenProvider<V>(eventHandler);
    }

    /**
     * Returns a new THREAD-SAFE token provider that uses weak keys (less chance of memory leaks, but slower).
     * <p>
     * With this, caller does not need to clear keys manually (though can for faster gc).
     * <p>
     * NOTE: This may be a little slower than the others due to synchronization needed.
     * <p>
     * FIXME: This implementation is slow because it synchronizes around WeakHashMap get accesses!
     * Needs a fast concurrent weak hash map implementation instead!
     */
    public static <V> AccessTokenProvider<V> newWeakAccessTokenProvider(EventHandler<V> eventHandler) {
        return new WeakSimpleAccessTokenProvider<V>(eventHandler);
    }

    /**
     * Returns a new NON-THREAD-SAFE token provider that does NOT have weak keys.
     */
    public static <V> AccessTokenProvider<V> newUnsafeAccessTokenProvider(EventHandler<V> eventHandler) {
        return new HashMapSimpleAccessTokenProvider<V>(eventHandler);
    }

    /**
     * Returns a new NON-THREAD-SAFE token provider with weak keys.
     */
    public static <V> AccessTokenProvider<V> newUnsafeWeakAccessTokenProvider(EventHandler<V> eventHandler) {
        return new UnsafeWeakSimpleAccessTokenProvider<V>(eventHandler);
    }

    public static abstract class SimpleAccessTokenProvider<V> extends AccessTokenProvider<V> {
        protected SimpleAccessTokenProvider(EventHandler<V> eventHandler) {
            super(eventHandler);
        }

        @Override
        public AccessToken newToken() {
            return new SimpleAccessToken(newTokenString());
        }

        @Override
        public AccessToken newToken(String tokenString) {
            return (tokenString != null) ? new SimpleAccessToken(tokenString) : NullSimpleAccessToken.INSTANCE;
        }
    }


    public static abstract class MapSimpleAccessTokenProvider<V> extends SimpleAccessTokenProvider<V> {
        protected final Map<AccessToken, V> tokens;

        protected MapSimpleAccessTokenProvider(EventHandler<V> eventHandler, Map<AccessToken, V> tokens) {
            super(eventHandler);
            this.tokens = tokens;
        }

        @Override
        public V get(AccessToken token) {
            return tokens.get(token);
        }

        @Override
        public V put(AccessToken token, V value) {
            return tokens.put(token, value);
        }

        @Override
        public V remove(AccessToken token) {
            return tokens.remove(token);
        }
    }

    /**
     * Map token provider that synchronizes around the map.
     * <p>
     * WARN: get accesses are synchronized - relatively slow.
     */
    public static abstract class SyncingMapSimpleAccessTokenProvider<V> extends MapSimpleAccessTokenProvider<V> {
        protected SyncingMapSimpleAccessTokenProvider(EventHandler<V> eventHandler, Map<AccessToken, V> tokens) {
            super(eventHandler, tokens);
        }

        @Override
        public V get(AccessToken token) {
            synchronized(tokens) {
                return tokens.get(token);
            }
        }

        @Override
        public V put(AccessToken token, V value) {
            synchronized(tokens) {
                return tokens.put(token, value);
            }
        }

        @Override
        public V remove(AccessToken token) {
            synchronized(tokens) {
                return tokens.remove(token);
            }
        }
    }

    /**
     * Thread-safe fast token provider with non-weak keys.
     */
    public static class ConcurrentSimpleAccessTokenProvider<V> extends MapSimpleAccessTokenProvider<V> {
        protected ConcurrentSimpleAccessTokenProvider(EventHandler<V> eventHandler) {
            super(eventHandler, new ConcurrentHashMap<AccessToken, V>());
        }
    }

    /**
     * Thread-safe slow token provider with weak keys.
     * <p>
     * WARN: get accesses are synchronized - relatively slow.
     */
    public static class WeakSimpleAccessTokenProvider<V> extends SyncingMapSimpleAccessTokenProvider<V> {
        protected WeakSimpleAccessTokenProvider(EventHandler<V> eventHandler) {
            super(eventHandler, new WeakHashMap<AccessToken, V>());
        }
    }

    /**
     * Non-thread-safe fast HashMap token provider with non-weak keys.
     */
    public static class HashMapSimpleAccessTokenProvider<V> extends MapSimpleAccessTokenProvider<V> {
        protected HashMapSimpleAccessTokenProvider(EventHandler<V> eventHandler) {
            super(eventHandler, new HashMap<>());
        }
    }

    /**
     * Non-thread-safe fast WeakHashMap token provider with weak keys.
     */
    public static class UnsafeWeakSimpleAccessTokenProvider<V> extends MapSimpleAccessTokenProvider<V> {
        protected UnsafeWeakSimpleAccessTokenProvider(EventHandler<V> eventHandler) {
            super(eventHandler, new WeakHashMap<AccessToken, V>());
        }
    }

    /**
     * Returns the session token - only if it's already set in session (no create).
     * <p>
     * NOTE: there is no value parameter, which indicates this cannot create token.
     * <p>
     * WARN: Even if this returns non-null, it is not guaranteed the token is properly
     * registered in the provider! Use {@link #getEnsureSessionToken} to guarantee that.
     */
    public AccessToken getLocalSessionToken(HttpSession session, HttpServletRequest request, String attrName) {
        AccessToken token;
        if (request != null) {
            token = (AccessToken) request.getAttribute(attrName);
            if (token != null) return token;
        }
        token = (AccessToken) session.getAttribute(attrName);
        if (token != null) {
            request.setAttribute(attrName, token);
        }
        return token;
    }

    /**
     * Gets session token; if not yet created or registered in the provider for whatever reason, does so.
     * <p>
     * After this call, the session is guaranteed to contain a token.
     * <p>
     * WARN: FIXME: Synchronization on session is not guaranteed due to servlet API -
     * e.g. will fail for session replication.
     * To prevent issues, only call this from controller after-login events or from
     * {@link javax.servlet.http.HttpSessionListener#sessionCreated} implementations.
     */
    public AccessToken getSessionToken(HttpSession session, HttpServletRequest request, String attrName, V initialValue) {
        // the following is an optimization to skip synchronized block, at least for ConcurrentHashMap
        AccessToken token = getLocalSessionToken(session, request, attrName);
        if (token != null) {
            if (this.get(token) != null) {
                return token;
            }
        }
        synchronized(session) { // FIXME: best-effort sync only - servlet API doesn't guarantee - can't work with session replication
            token = (AccessToken) session.getAttribute(attrName);
            if (token != null) {
                if (this.get(token) != null) {
                    return token;
                }
            }
            return createSessionToken(session, request, attrName, initialValue);
        }
    }

    /**
     * Creates token in session with given value, removing any prior
     * <p>
     * After this call, the session is guaranteed to contain a token.
     * <p>
     * WARN: FIXME: Synchronization on session is not guaranteed due to servlet API -
     * e.g. will fail for session replication.
     * To prevent issues, only call this from controller after-login events or from
     * {@link javax.servlet.http.HttpSessionListener#sessionCreated} implementations.
     */
    public AccessToken createSessionToken(HttpSession session, HttpServletRequest request, String attrName, V initialValue) {
        synchronized(session) { // FIXME: best-effort sync only - servlet API doesn't guarantee - can't work with session replication
            cleanupSessionToken(session, request, attrName);

            AccessToken token = newToken();
            put(token, initialValue);
            session.setAttribute(attrName, token);
            request.setAttribute(attrName, token);

            getEventHandler().sessionTokenCreated(session, request, attrName, token, initialValue);
            return token;
        }
    }

    public void cleanupSessionToken(HttpSession session, HttpServletRequest request, String attrName) {
        synchronized(session) { // FIXME: best-effort sync only - servlet API doesn't guarantee - can't work with session replication
            if (request != null) {
                AccessToken token = (AccessToken) request.getAttribute(attrName);
                if (token != null) {
                    request.removeAttribute(attrName);
                    remove(token);
                }
            }

            AccessToken token = (AccessToken) session.getAttribute(attrName);
            if (token != null) {
                session.removeAttribute(attrName);
                remove(token);
            }
        }
    }

    public interface EventHandler<V> {
        void sessionTokenCreated(HttpSession session, HttpServletRequest request,
                String attrName, AccessToken token, V value);
    }

    public static class NoopEventHandler<V> implements EventHandler<V> {
        private static final NoopEventHandler<?> INSTANCE = new NoopEventHandler<Object>();

        @SuppressWarnings("unchecked")
        public static <V> NoopEventHandler<V> getInstance() {
            // implemented via type erasure to avoid needless extra objects
            return (NoopEventHandler<V>) INSTANCE;
        }

        @Override
        public void sessionTokenCreated(HttpSession session, HttpServletRequest request,
                String attrName, AccessToken token, V value) {
        }
    }

}
