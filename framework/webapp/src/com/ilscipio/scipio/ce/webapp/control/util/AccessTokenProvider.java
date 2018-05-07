package com.ilscipio.scipio.ce.webapp.control.util;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;

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

    protected AccessTokenProvider() {
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
    public static <V> AccessTokenProvider<V> newAccessTokenProvider() {
        return new ConcurrentSimpleAccessTokenProvider<V>();
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
    public static <V> AccessTokenProvider<V> newWeakAccessTokenProvider() {
        return new WeakSimpleAccessTokenProvider<V>();
    }
    
    /**
     * Returns a new NON-THREAD-SAFE token provider that does NOT have weak keys.
     */
    public static <V> AccessTokenProvider<V> newUnsafeAccessTokenProvider() {
        return new HashMapSimpleAccessTokenProvider<V>();
    }
    
    /**
     * Returns a new NON-THREAD-SAFE token provider with weak keys.
     */
    public static <V> AccessTokenProvider<V> newUnsafeWeakAccessTokenProvider() {
        return new UnsafeWeakSimpleAccessTokenProvider<V>();
    }
    
    public static abstract class SimpleAccessTokenProvider<V> extends AccessTokenProvider<V> {
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

        protected MapSimpleAccessTokenProvider(Map<AccessToken, V> tokens) {
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
        protected SyncingMapSimpleAccessTokenProvider(Map<AccessToken, V> tokens) {
            super(tokens);
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
        protected ConcurrentSimpleAccessTokenProvider() {
            super(new ConcurrentHashMap<AccessToken, V>());
        }
    }

    /**
     * Thread-safe slow token provider with weak keys.
     * <p>
     * WARN: get accesses are synchronized - relatively slow.
     */
    public static class WeakSimpleAccessTokenProvider<V> extends SyncingMapSimpleAccessTokenProvider<V> {
        protected WeakSimpleAccessTokenProvider() {
            super(new WeakHashMap<AccessToken, V>());
        }
    }
    
    /**
     * Non-thread-safe fast HashMap token provider with non-weak keys.
     */
    public static class HashMapSimpleAccessTokenProvider<V> extends MapSimpleAccessTokenProvider<V> {
        protected HashMapSimpleAccessTokenProvider() {
            super(new HashMap<AccessToken, V>());
        }
    }
    
    /**
     * Non-thread-safe fast WeakHashMap token provider with weak keys.
     */
    public static class UnsafeWeakSimpleAccessTokenProvider<V> extends MapSimpleAccessTokenProvider<V> {
        protected UnsafeWeakSimpleAccessTokenProvider() {
            super(new WeakHashMap<AccessToken, V>());
        }
    }

    /**
     * Returns the session token - only if it's already set in session (no create).
     * <p>
     * NOTE: there is no value parameter, which indicates this cannot create token.
     */
    public AccessToken getSessionToken(HttpSession session, String attrName) {
        return (AccessToken) session.getAttribute(attrName);
    }
    
    /**
     * Returns the session token string - only if it's already set in session (no create).
     * <p>
     * NOTE: there is no value parameter, which indicates this cannot create token.
     */
    public String getSessionTokenString(HttpSession session, String attrName) {
        AccessToken token = getSessionToken(session, attrName);
        return (token != null) ? token.toString() : null;
    }
    
    private AccessToken updateSessionToken(HttpSession session, String attrName, V value, boolean onlyIfNewToken) {
        AccessToken token = (AccessToken) session.getAttribute(attrName);
        if (token != null) {
            if (!onlyIfNewToken) {
                put(token, value);
            }
            return token;
        }
        
        // FIXME: this session sync is best-effort and not guaranteed by the serlvet API -
        // will not work with session replication
        synchronized(session) {
            token = (AccessToken) session.getAttribute(attrName);
            if (token != null) {
                if (!onlyIfNewToken) {
                    put(token, value);
                }
                return token;
            }
            
            token = newToken();
            put(token, value);
            session.setAttribute(attrName, token);
            return token;
        }
    }
    
    /**
     * Updates current token in session to the given value, creating it if it doesn't already exist,
     * and returns the current token.
     * <p>
     * After this call, the session is guaranteed to contain a token.
     * <p>
     * NOTE: If you only need to set the value on creation (i.e. it doesn't change for a given token), 
     * it's better to call {@link #createOrGetSessionToken} instead, for performance reasons.
     * <p>
     * WARN: FIXME: Synchronization on session is not guaranteed due to servlet API -
     * e.g. will fail for session replication.
     * To prevent issues, only call this from controller after-login events or from
     * {@link javax.servlet.http.HttpSessionListener#sessionCreated} implementations.
     */
    public AccessToken updateAndGetSessionToken(HttpSession session, String attrName, V value) {
        return updateSessionToken(session, attrName, value, false);
    }
    
    /**
     * Creates token in session with given value if it doesn't already exist;
     * otherwise returns the current token.
     * <p>
     * After this call, the session is guaranteed to contain a token.
     * <p>
     * WARN: FIXME: Synchronization on session is not guaranteed due to servlet API -
     * e.g. will fail for session replication.
     * To prevent issues, only call this from controller after-login events or from
     * {@link javax.servlet.http.HttpSessionListener#sessionCreated} implementations.
     */
    public AccessToken createOrGetSessionToken(HttpSession session, String attrName, V value) {
        return updateSessionToken(session, attrName, value, true);
    }
    
    /**
     * Restores token stored in session to this provider's tokens map.
     * <p>
     * Needed only for persisted session deserialization.
     */
    public AccessToken restoreSessionTokenToProvider(HttpSession session, String attrName, V value) {
        AccessToken token = getSessionToken(session, attrName);
        if (token != null) {
            this.put(token, value);
        }
        return token;
    }
    
    public V cleanupSessionToken(HttpSession session, String attrName) {
        AccessToken token = (AccessToken) session.getAttribute(attrName);
        if (token != null) {
            return remove(token);
        }
        return null;
    }
    
}
