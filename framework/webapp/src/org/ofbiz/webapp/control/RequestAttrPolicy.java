package org.ofbiz.webapp.control;

import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import com.ilscipio.scipio.ce.util.collections.MapEntryAdapter;
import com.ilscipio.scipio.ce.util.servlet.ServletAttrContainer;

/**
 * SCIPIO: Helper utils and tag/callback interfaces to help integrate with the RequestHandler's
 * "view-last", "redirect-request*" and other responses that save request attributes into session attributes,
 * in addition to any other controller plugin-related functionality.
 * <p>
 * Added 2019-01-22.
 */
public final class RequestAttrPolicy {

    private RequestAttrPolicy() {}

    /*
     * ******************************************************************************
     * Request-to-session attribute saving plugins
     * ******************************************************************************
     */

    /**
     * Base interface for any webapp control and {@link RequestHandler}-related attribute store/restore callback handler,
     * but mostly focused on the request-to-session attribute saving behavior of controller features such as "view-last"
     * responses and redirects.
     * <p>
     * This defines a number of high-level identifying interfaces that classes can implement as simple tags
     * (like Serializable) to decide if they want to be included or not in the view-last and redirect-request
     * response saving of request attributes into session attributes maps ("_LAST_VIEW_PARAMS_", "_REQ_ATTR_MAP_", etc.),
     * in addition to being able to subscribe callbacks and modify default behavior.
     * <p>
     * To use, simply create a new class and have it implement one of the {@link AttrPolicy}-derived
     * child classes corresponding to the callback(s) to which you want to subscribe.
     * For advanced usage, you can override the callback for any plugin callback:
     * <pre><code>
     * class MyClass implements {@link SessionAttrPolicy.NotStorable} { // prevent any saving into session attributes
     * class MyClass implements {@link SessionAttrPolicy.Never} { // prevent any saving to OR restoring from session attributes
     * </code></pre>
     * <p>
     * This interface also defines the elementary <code>VALUE_*</code> return values that must be returned
     * by the callback store/restore methods.
     * <p>
     * NOTE: Implementing classes do NOT need to be serializable to implement these interfaces.
     */
    public interface AttrPolicy {

        /**
         * Special object values, represented by the <code>VALUE_*</code> constants.
         */
        public interface AttrValue extends java.io.Serializable {}

        /**
         * Special Object value that means "the current value should be ignored" or "not be set", when passed to helper
         * methods such as {@link RequestAttrPolicy#putMapValue}.
         * <p>
         * For any store/restore callback in the {@link SaveAttr} interfaces that must decide and return (or not return)
         * a value to be stored/restored, this value indicates that the current value should NOT be stored/restored.
         */
        @SuppressWarnings("serial")
        Object VALUE_IGNORE = new AttrValue() {
            @Override
            public String toString() {
                return "IGNORE";
            }
        };

        /**
         * Special Object value that means "the current value should be explicitly removed" from whatever container
         * is currently being updated, when passed to helper methods such as {@link RequestAttrPolicy#putMapValue}.
         * <p>
         * For any store/restore callback in the {@link SaveAttr} interfaces that must decide and return (or not return)
         * a value to be stored/restored, this value indicates that the target attributes or map for the store/restore
         * should have the target explicitly remove the entry for this attribute's key.
         */
        @SuppressWarnings("serial")
        Object VALUE_UNSET = new AttrValue() {
            @Override
            public String toString() {
                return "UNSET";
            }
        };
    }

    /**
     * Modifies all controller request-to/from-session-map ("_LAST_VIEW_PARAMS_"/"_SAVED_VIEW_PARAMS_"/"_HOME_VIEW_PARAMS_",
     * "_REQ_ATTR_MAP_", etc.) attribute saving behavior, both storing and restoring, for such features as
     * "view-last"/"view-saved"/"view-home" responses and controller redirects ("request-redirect*").
     * <p>
     * The default behavior is represented by {@link SessionAttrPolicy.Always} to permit both storing and restoring.
     * <p>
     * This interface is a high-level combination of the interfaces of:
     * <ul>
     * <li>{@link ViewLastAttrPolicy}</li>
     * <li>{@link RedirectAttrPolicy}</li>
     * </ul>
     * To use, simply create a new class and have it implement one of the {@link AttrPolicy}-derived
     * child classes corresponding to the callbacks(s) to which you want to subscribe.
     * For advanced usage, you can override the callback for any plugin callback:
     * <pre><code>
     * class MyClass implements {@link SessionAttrPolicy.NotStorable} { // prevent any saving into session attributes
     * class MyClass implements {@link SessionAttrPolicy.Never} { // prevent any saving to OR restoring from session attributes
     * </code></pre>
     */
    public interface SessionAttrPolicy extends AttrPolicy {

        /**
         * Handler for any {@link RequestHandler}-related request attribute to session store callback; unless overridden, by default
         * this returns/stores <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the store callbacks related to storage into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         * <p>
         * NOTE: This reflects the default system behavior.
         */
        public interface Storable extends ViewLastAttrPolicy.Storable, RedirectAttrPolicy.Storable {}

        /**
         * Handler for any {@link RequestHandler}-related request attribute to session store callback; unless overridden, by default
         * this prevents storing <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the store callbacks related to storage into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         */
        public interface NotStorable extends ViewLastAttrPolicy.NotStorable, RedirectAttrPolicy.NotStorable {}

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request restore callback; unless overridden, by default
         * this returns/restores <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the restore callbacks related to storage into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         * <p>
         * NOTE: This reflects the default system behavior.
         */
        public interface Restorable extends ViewLastAttrPolicy.Restorable, RedirectAttrPolicy.Restorable {}

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request restore callback; unless overridden, by default
         * this prevents storing <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the restore callbacks related to storage into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         */
        public interface NotRestorable extends ViewLastAttrPolicy.NotRestorable, RedirectAttrPolicy.NotRestorable {}

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request store OR restore callback; unless overridden, by default
         * this returns/stores/restores <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the store AND restore callbacks related to storage into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         * <p>
         * NOTE: This reflects the default system behavior.
         */
        public interface Always extends Storable, Restorable {}

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request store OR restore callback; unless overridden, by default
         * this prevents storing/restoring <code>this</code> for all callbacks.
         * <p>
         * Callbacks include all the store AND restore callbacks related to storage into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         */
        public interface Never extends NotStorable, NotRestorable {}
    }

    /**
     * Modifies controller "view-last"/"view-saved"/"view-home" request-to/from-session-map ("_LAST_VIEW_PARAMS_",
     * "_SAVED_VIEW_PARAMS_", "_HOME_VIEW_PARAMS_") attribute saving behavior, both storing and restoring.
     * <p>
     * The default behavior is represented by {@link ViewLastAttrPolicy.Always} to permit both storing and restoring.
     */
    public interface ViewLastAttrPolicy extends AttrPolicy {

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session whenever a controller "view"
         * is about to be rendered, in the implementing of "view-last"-type responses.
         */
        public interface StorePolicy extends ViewLastAttrPolicy {
            public static final StoringPolicyInvoker<StorePolicy> INVOKER = new StoringPolicyInvoker<StorePolicy>() {
                @Override
                public Object doAttrStoreRestore(StorePolicy policy, HttpServletRequest request, String attrName,
                        Map<String, Object> saveAttrMap) {
                    return policy.doViewLastAttrStore(request, attrName, saveAttrMap);
                }
                @Override
                public boolean valueApplies(Object value) {
                    return (value instanceof StorePolicy);
                }
            };

            /**
             * Invoked just before this object is about to be saved from request attributes to session, or whenever a controller "view"
             * is about to be rendered; returns the object that should be stored in session.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was stored as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> to save, {@link AttrPolicy#VALUE_IGNORE} to prevent, or a replacement value
             */
            Object doViewLastAttrStore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap);
        }

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session whenever a controller "view"
         * is about to be rendered, in the implementing of "view-last"-type responses; unless overridden, by default, saves <code>this</code>.
         */
        public interface Storable extends StorePolicy {
            /**
             * Invoked just before this object is about to be saved from request attributes to session, or whenever a controller "view"
             * is about to be rendered; returns the object that should be stored in session, by default <code>this</code>.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was stored as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> (default) to save, {@link AttrPolicy#VALUE_IGNORE} to prevent, or a replacement value
             */
            @Override
            default Object doViewLastAttrStore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return this;
            }
        }

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session whenever a controller "view"
         * is about to be rendered, in the implementing of "view-last"-type responses; unless overridden, by default, prevents saving <code>this</code>.
         */
        public interface NotStorable extends StorePolicy {
            /**
             * Called just before this object is about to be saved from request attributes to session, or
             * whenever a controller "view" is about to be rendered; by default returns {@link AttrPolicy#VALUE_IGNORE}.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was stored as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> to save, {@link AttrPolicy#VALUE_IGNORE} (default) to prevent or a replacement value
             */
            @Override
            default Object doViewLastAttrStore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return AttrPolicy.VALUE_IGNORE;
            }
        }

        /**
         * Callback interface invoked just before this object is about to be restored from session attributes to request, or whenever there is a controller
         * "view-(last|saved|home)" response, in the implementing of "view-last"-type responses; unless overridden, by default, restores <code>this</code>.
         */
        public interface RestorePolicy extends ViewLastAttrPolicy {
            public static final StoringPolicyInvoker<RestorePolicy> INVOKER = new StoringPolicyInvoker<RestorePolicy>() {
                @Override
                public Object doAttrStoreRestore(RestorePolicy policy, HttpServletRequest request, String attrName,
                        Map<String, Object> saveAttrMap) {
                    return policy.doViewLastAttrRestore(request, attrName, saveAttrMap);
                }
                @Override
                public boolean valueApplies(Object value) {
                    return (value instanceof RestorePolicy);
                }
            };

            /**
             * Invoked just before this object is about to be restored from session attributes to request whenever there is a controller
             * "view-(last|saved|home)" response; returns the object that should be stored in session.
             * @param request The request
             * @param attrName The map of saved attributes being read (from session)
             * @param saveAttrMap The map of attributes being restored (from session)
             * @return <code>this</code> to restore, {@link AttrPolicy#VALUE_IGNORE} to prevent or a replacement value
             */
            Object doViewLastAttrRestore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap);
        }

        /**
         * Callback interface invoked just before this object is about to be restored from session attributes to request, or whenever there is a controller
         * "view-(last|saved|home)" response, in the implementing of "view-last"-type responses; unless overridden, by default, restores <code>this</code>.
         */
        public interface Restorable extends RestorePolicy {
            /**
             * Invoked just before this object is about to be restored from session attributes to request whenever there is a controller
             * "view-(last|saved|home)" response; returns the object that should be stored in session, by default <code>this</code>.
             * @param request The request
             * @param attrName The map of saved attributes being read (from session)
             * @param saveAttrMap The map of attributes being restored (from session)
             * @return <code>this</code> (default) to restore, {@link AttrPolicy#VALUE_IGNORE} to prevent or a replacement value
             */
            @Override
            default Object doViewLastAttrRestore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return this;
            }
        }

        /**
         * Callback interface invoked just before this object is about to be restored from session attributes to request, or whenever there is a controller
         * "view-(last|saved|home)" response, in the implementing of "view-last"-type responses; unless overridden, by default, prevents restoring <code>this</code>.
         */
        public interface NotRestorable extends RestorePolicy {
            /**
             * Invoked just before this object is about to be restored from session attributes to request whenever there is a controller
             * "view-(last|saved|home)" response; returns the object that should be stored in session, by default {@link AttrPolicy#VALUE_IGNORE}.
             * @param request The request
             * @param attrName The map of saved attributes being read (from session)
             * @param saveAttrMap The map of attributes being restored (from session)
             * @return <code>this</code> to restore, {@link AttrPolicy#VALUE_IGNORE} (default) to prevent or a replacement value
             */
            @Override
            default Object doViewLastAttrRestore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return AttrPolicy.VALUE_IGNORE;
            }
        }

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request store OR restore callbacks for view-last logic;
         * unless overridden, by default this returns/stores/restores <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the store AND restore callbacks related to storage into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_SAVED_VIEW_PARAMS_" and "_HOME_VIEW_PARAMS_" used to implement the "view-last"/"view-saved"/"view-home"
         * controller request response.
         * <p>
         * NOTE: This reflects the default system behavior.
         */
        public interface Always extends Storable, Restorable {}

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request store OR restore callback for view-last logic;
         * unless overridden, by default this prevents storing/restoring <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the store AND restore callbacks related to storage into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_SAVED_VIEW_PARAMS_" and "_HOME_VIEW_PARAMS_" used to implement the "view-last"/"view-saved"/"view-home"
         * controller request response.
         */
        public interface Never extends NotStorable, NotRestorable {}
    }

    /**
     * Modifies controller redirect request-to/from-session-map ("_REQ_ATTR_MAP_") attribute saving behavior,
     * both storing and restoring, notably for "request-redirect*" request responses,
     * <p>
     * The default behavior is represented by {@link RedirectAttrPolicy.Always} to permit both storing and restoring.
     */
    public interface RedirectAttrPolicy extends AttrPolicy {

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session upon any controller
         * redirect.
         */
        public interface StorePolicy extends RedirectAttrPolicy {
            public static final StoringPolicyInvoker<StorePolicy> INVOKER = new StoringPolicyInvoker<StorePolicy>() {
                @Override
                public Object doAttrStoreRestore(StorePolicy policy, HttpServletRequest request, String attrName,
                        Map<String, Object> saveAttrMap) {
                    return policy.doRedirectAttrStore(request, attrName, saveAttrMap);
                }
                @Override
                public boolean valueApplies(Object value) {
                    return (value instanceof StorePolicy);
                }
            };

            /**
             * Invoked just before this object is about to be saved from request attributes to session upon any controller redirect;
             * returns the object that should be stored in session.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was stored as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> to save, {@link AttrPolicy#VALUE_IGNORE} to prevent or a replacement value
             */
            Object doRedirectAttrStore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap);
        }

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session upon any controller
         * redirect; unless overridden, by default, saves <code>this</code>.
         */
        public interface Storable extends StorePolicy {
            /**
             * Invoked just before this object is about to be saved from request attributes to session upon any controller redirect;
             * returns the object that should be stored in session, by default <code>this</code>.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was stored as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> (default) to save, {@link AttrPolicy#VALUE_IGNORE} to prevent or a replacement value
             */
            @Override
            default Object doRedirectAttrStore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return this;
            }
        }

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session upon any controller
         * redirect; unless overridden, by default, prevents saving <code>this</code>.
         */
        public interface NotStorable extends StorePolicy {
            /**
             * Invoked just before this object is about to be saved from request attributes to session upon any controller redirect;
             * returns the object that should be stored in session, by default {@link AttrPolicy#VALUE_IGNORE}.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was stored as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> to save, {@link AttrPolicy#VALUE_IGNORE} (default) to prevent or a replacement value
             */
            @Override
            default Object doRedirectAttrStore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return AttrPolicy.VALUE_IGNORE;
            }
        }

        /**
         * Callback interface invoked just before this object is about to be restored from session attributes to request after a controller
         * redirect.
         */
        public interface RestorePolicy extends RedirectAttrPolicy {
            public static final StoringPolicyInvoker<RestorePolicy> INVOKER = new StoringPolicyInvoker<RestorePolicy>() {
                @Override
                public Object doAttrStoreRestore(RestorePolicy policy, HttpServletRequest request, String attrName,
                        Map<String, Object> saveAttrMap) {
                    return policy.doRedirectAttrRestore(request, attrName, saveAttrMap);
                }
                @Override
                public boolean valueApplies(Object value) {
                    return (value instanceof RestorePolicy);
                }
            };

            /**
             * Invoked just before this object is about to be restored from session attributes to request upon any controller redirect;
             * returns the object that should be stored in session.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was stored as
             * @param saveAttrMap The map of saved attributes being restored (from session)
             * @return <code>this</code> to restore, {@link AttrPolicy#VALUE_IGNORE} to prevent or a replacement value
             */
            Object doRedirectAttrRestore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap);
        }

        /**
         * Callback interface invoked just before this object is about to be restored from session attributes to request after a controller
         * redirect; unless overridden, by default, restores <code>this</code>.
         */
        public interface Restorable extends RestorePolicy {
            /**
             * Invoked just before this object is about to be restored from session attributes to request upon any controller redirect;
             * returns the object that should be stored in session, by default <code>this</code>.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was stored as
             * @param saveAttrMap The map of saved attributes being restored (from session)
             * @return <code>this</code> (default) to restore, {@link AttrPolicy#VALUE_IGNORE} to prevent or a replacement value
             */
            @Override
            default Object doRedirectAttrRestore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return this;
            }
        }

        /**
         * Callback interface invoked just before this object is about to be restored from session attributes to request after a controller
         * redirect; unless overridden, by default, prevents restoring <code>this</code>.
         */
        public interface NotRestorable extends RestorePolicy {
            /**
             * Invoked just before this object is about to be restored from session attributes to request upon any controller redirect;
             * returns the object that should be stored in session, by default {@link AttrPolicy#VALUE_IGNORE}.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was stored as
             * @param saveAttrMap The map of saved attributes being restored (from session)
             * @return <code>this</code> to restore, {@link AttrPolicy#VALUE_IGNORE} (default) to prevent or a replacement value
             */
            @Override
            default Object doRedirectAttrRestore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return AttrPolicy.VALUE_IGNORE;
            }
        }

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request store OR restore callbacks for controller redirect logic;
         * unless overridden, by default this returns/stores/restores <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the store AND restore callbacks related to storage into the session attributes maps "_REQ_ATTR_MAP_"
         * used to implement controller redirects including the "request-redirect*" responses.
         * <p>
         * NOTE: This reflects the default system behavior.
         */
        public interface Always extends Storable, Restorable {}

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request store OR restore callback  for view-last logic;
         * unless overridden, by default this prevents storing/restoring <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the store AND restore callbacks related to storage into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_SAVED_VIEW_PARAMS_" and "_HOME_VIEW_PARAMS_" used to implement the "view-last"/"view-saved"/"view-home"
         * controller request response.
         */
        public interface Never extends NotStorable, NotRestorable {}
    }

// Not necessary; code can just use the INVOKER (StoringPolicyInvoker) variables directly.
//    public static class AttrPolicyRegistry {
//        private static final Map<Class<? extends AttrPolicy>, StoringPolicyInvoker<? extends AttrPolicy>> invokers;
//        static {
//            Map<Class<? extends AttrPolicy>, StoringPolicyInvoker<? extends AttrPolicy>> map = new HashMap<>();
//            map.put(ViewLastAttrPolicy.StorePolicyHandler.class, ViewLastAttrPolicy.StorePolicyHandler.INVOKER);
//            map.put(ViewLastAttrPolicy.RestorePolicyHandler.class, ViewLastAttrPolicy.RestorePolicyHandler.INVOKER);
//            map.put(RedirectAttrPolicy.StorePolicyHandler.class, RedirectAttrPolicy.StorePolicyHandler.INVOKER);
//            map.put(RedirectAttrPolicy.RestorePolicyHandler.class, RedirectAttrPolicy.RestorePolicyHandler.INVOKER);
//            invokers = map;
//        }
//
//        public static <T> T getStoringPolicyInvoker(Class<T> cls) {
//            return (T)
//        }
//    }

    public static abstract class StoringPolicyInvoker<T extends AttrPolicy> {

        public abstract boolean valueApplies(Object value);

        public abstract Object doAttrStoreRestore(T poli, HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap);

        @SuppressWarnings("unchecked")
        public boolean storeToMap(HttpServletRequest request, Map<String, Object> saveAttrMap, String attrName, Object value) {
            boolean hasSpecialHandler = valueApplies(value);
            return storeToMap(request, saveAttrMap, attrName, value, hasSpecialHandler, hasSpecialHandler ?
                    doAttrStoreRestore((T) value, request, attrName, saveAttrMap) : value); // NOTE: Default behavior is to allow
        }

        @SuppressWarnings("unchecked")
        public boolean storeToMap(HttpServletRequest request, Map<String, Object> saveAttrMap, Map.Entry<String, Object> attrEntry) {
            boolean hasSpecialHandler = valueApplies(attrEntry.getValue());
            return storeToMap(request, saveAttrMap, attrEntry, hasSpecialHandler, hasSpecialHandler ?
                    doAttrStoreRestore((T) attrEntry.getValue(), request, attrEntry.getKey(), saveAttrMap) : attrEntry.getValue()); // NOTE: Default behavior is to allow
        }

        @SuppressWarnings("unchecked")
        public boolean storeToMapEntryInPlace(HttpServletRequest request, Map.Entry<String, Object> attrEntry, Map<String, Object> saveAttrMap) {
            boolean hasSpecialHandler = valueApplies(attrEntry.getValue());
            return storeToMapEntryInPlace(request, attrEntry, saveAttrMap, hasSpecialHandler, hasSpecialHandler ?
                    doAttrStoreRestore((T) attrEntry.getValue(), request, attrEntry.getKey(), saveAttrMap) : attrEntry.getValue()); // NOTE: Default behavior is to allow
        }

        @SuppressWarnings("unchecked")
        public boolean restoreToRequest(HttpServletRequest request, String attrName, Object value, Map<String, Object> saveAttrMap) {
            boolean hasSpecialHandler = valueApplies(value);
            return restoreToRequest(request, attrName, value, saveAttrMap, hasSpecialHandler, hasSpecialHandler ?
                    doAttrStoreRestore((T) value, request, attrName, saveAttrMap) : value); // NOTE: Default behavior is to allow
        }

        @SuppressWarnings("unchecked")
        public boolean restoreToRequest(HttpServletRequest request, Map.Entry<String, Object> attrEntry, Map<String, Object> saveAttrMap) {
            boolean hasSpecialHandler = valueApplies(attrEntry.getValue());
            return restoreToRequest(request, attrEntry, saveAttrMap, hasSpecialHandler, hasSpecialHandler ?
                    doAttrStoreRestore((T) attrEntry.getValue(), request, attrEntry.getKey(), saveAttrMap) : attrEntry.getValue()); // NOTE: Default behavior is to allow
        }

        protected static boolean storeToMap(HttpServletRequest request, Map<String, Object> saveAttrMap, String attrName, Object value,
                boolean hasSpecialHandler, Object specialValue) {
            if (hasSpecialHandler) {
                return RequestAttrPolicy.putMapValue(saveAttrMap, attrName, specialValue);
            }
            saveAttrMap.put(attrName, value);
            return true;
        }

        protected static boolean storeToMap(HttpServletRequest request, Map<String, Object> saveAttrMap, Map.Entry<String, Object> attrEntry,
                boolean hasSpecialHandler, Object specialValue) {
            if (hasSpecialHandler) {
                return RequestAttrPolicy.putMapValue(saveAttrMap, attrEntry.getKey(), specialValue);
            }
            saveAttrMap.put(attrEntry.getKey(), attrEntry.getValue());
            return true;
        }

        protected static boolean storeToMapEntryInPlace(HttpServletRequest request, Map.Entry<String, Object> attrEntry, Map<String, Object> saveAttrMap,
                boolean hasSpecialHandler, Object specialValue) {
            if (hasSpecialHandler) {
                return RequestAttrPolicy.setEntryValue(attrEntry, specialValue);
            }
            return true; // do nothing, it's in-place
        }

        protected static boolean restoreToRequest(HttpServletRequest request, String attrName, Object value, Map<String, Object> saveAttrMap,
                boolean hasSpecialHandler, Object specialValue) {
            if (hasSpecialHandler) {
                return RequestAttrPolicy.setServletAttribute(request, attrName, specialValue);
            }
            request.setAttribute(attrName, value);
            return true;
        }

        protected static boolean restoreToRequest(HttpServletRequest request, Map.Entry<String, Object> attrEntry, Map<String, Object> saveAttrMap,
                boolean hasSpecialHandler, Object specialValue) {
            if (hasSpecialHandler) {
                return RequestAttrPolicy.setServletAttribute(request, attrEntry.getKey(), specialValue);
            }
            request.setAttribute(attrEntry.getKey(), attrEntry.getValue());
            return true;
        }
    }

    /*
     * ******************************************************************************
     * Implementation helpers
     * ******************************************************************************
     */

    public static boolean putMapValue(Map<String, Object> map, String attrName, Object value) {
        if (value == AttrPolicy.VALUE_IGNORE) {
            return false;
        }
        if (value == AttrPolicy.VALUE_UNSET) {
            map.remove(attrName);
            return false;
        }
        map.put(attrName, value);
        return true;
    }

    public static boolean setEntryValue(Map.Entry<String, Object> entry, Object value) {
        if (value == AttrPolicy.VALUE_IGNORE) {
            return false;
        }
        if (value == AttrPolicy.VALUE_UNSET) {
            MapEntryAdapter.removeValueOnly(entry);
            return false;
        }
        MapEntryAdapter.setValueOnly(entry, value);
        return true;
    }

    public static boolean setServletAttribute(ServletAttrContainer attrContainer, String attrName, Object value) {
        if (value == AttrPolicy.VALUE_IGNORE) {
            return false;
        }
        if (value == AttrPolicy.VALUE_UNSET) {
            attrContainer.removeAttribute(attrName);
            return false;
        }
        attrContainer.setAttribute(attrName, value);
        return true;
    }

    public static boolean setServletAttribute(ServletRequest attrContainer, String attrName, Object value) {
        if (value == AttrPolicy.VALUE_IGNORE) {
            return false;
        }
        if (value == AttrPolicy.VALUE_UNSET) {
            attrContainer.removeAttribute(attrName);
            return false;
        }
        attrContainer.setAttribute(attrName, value);
        return true;
    }

    public static boolean setServletAttribute(HttpSession attrContainer, String attrName, Object value) {
        if (value == AttrPolicy.VALUE_IGNORE) {
            return false;
        }
        if (value == AttrPolicy.VALUE_UNSET) {
            attrContainer.removeAttribute(attrName);
            return false;
        }
        attrContainer.setAttribute(attrName, value);
        return true;
    }

    public static boolean setServletAttribute(ServletContext attrContainer, String attrName, Object value) {
        if (value == AttrPolicy.VALUE_IGNORE) {
            return false;
        }
        if (value == AttrPolicy.VALUE_UNSET) {
            attrContainer.removeAttribute(attrName);
            return false;
        }
        attrContainer.setAttribute(attrName, value);
        return true;
    }
}
