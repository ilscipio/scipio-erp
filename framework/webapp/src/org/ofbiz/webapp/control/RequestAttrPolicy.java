package org.ofbiz.webapp.control;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Spliterator;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Stream;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.UtilMisc;

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

    /**
     * Base interface for any webapp control and {@link RequestHandler}-related attribute save/restore callback handler,
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
     * class MyClass implements {@link RequestSavingAttrPolicy.NotSaveable} { // prevent any saving into session attributes
     * class MyClass implements {@link RequestSavingAttrPolicy.Never} { // prevent any saving to OR restoring from session attributes
     * </code></pre>
     * <p>
     * This interface also defines the elementary <code>VALUE_*</code> return values that must be returned
     * by the callback save/restore methods.
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
         * For any save/restore callback in the {@link AttrPolicy} interfaces that must decide and return (or not return)
         * a value to be saved/restored, this value indicates that the current value should NOT be saved/restored.
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
         * For any save/restore callback in the {@link AttrPolicy} interfaces that must decide and return (or not return)
         * a value to be saved/restored, this value indicates that the target attributes or map for the save/restore
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
     * Represents an attribute saving or restoring event.
     * <p>
     * NOTE: This has NO relation to the definable controller "events" (e.g. pre-processor, post-processor, etc.).
     */
    public interface AttrEventPolicy extends AttrPolicy {}

    /*
     * ******************************************************************************
     * General policies (name- and type-based)
     * ******************************************************************************
     */

    /**
     * Modifies all controller request-to/from-session-map ("_LAST_VIEW_PARAMS_"/"_SAVED_VIEW_PARAMS_"/"_HOME_VIEW_PARAMS_",
     * "_REQ_ATTR_MAP_", etc.) attribute saving behavior, both saving and restoring, for such features as
     * "view-last"/"view-saved"/"view-home" responses and controller redirects ("request-redirect*").
     * <p>
     * The default behavior is represented by {@link RequestSavingAttrPolicy.Always} to permit both saving and restoring.
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
     * class MyClass implements {@link RequestSavingAttrPolicy.NotSaveable} { // prevent any saving into session attributes
     * class MyClass implements {@link RequestSavingAttrPolicy.Never} { // prevent any saving to OR restoring from session attributes
     * </code></pre>
     */
    public interface RequestSavingAttrPolicy extends AttrPolicy {

        /**
         * Handler for any {@link RequestHandler}-related request attribute to session save callback; unless overridden, by default
         * this returns/saves <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the save callbacks related to saving into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         * <p>
         * NOTE: This reflects the default system behavior.
         */
        public interface Saveable extends ViewLastAttrPolicy.Saveable, RedirectAttrPolicy.Saveable {}

        /**
         * Handler for any {@link RequestHandler}-related request attribute to session save callback; unless overridden, by default
         * this prevents saving <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the save callbacks related to saving into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         */
        public interface NotSaveable extends ViewLastAttrPolicy.NotSaveable, RedirectAttrPolicy.NotSaveable {
            /**
             * SCIPIO: Default list of attributes which should never be saved in session and transferred across
             * requests using view-last or request-redirect mechanisms.
             */
            public static final List<String> DEFAULT_ATTR_NAME_EXCL = UtilMisc.unmodifiableArrayList(
                    RequestAttrNamePolicy.ATTR, "_ALLOW_VIEW_SAVE_", "ftlServletContext", "_REQUEST_HANDLER_",
                    ConfigXMLReader.RequestResponse.AttributesSpec.REDIRECT_ATTR,
                    "shoppingCartChanged" // TODO: REVIEW: this does not reflect dependencies properly, but may help prevent problems this way
            );
        }

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request restore callback; unless overridden, by default
         * this returns/restores <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the restore callbacks related to saving into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         * <p>
         * NOTE: This reflects the default system behavior.
         */
        public interface Restorable extends ViewLastAttrPolicy.Restorable, RedirectAttrPolicy.Restorable {}

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request restore callback; unless overridden, by default
         * this prevents saving <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the restore callbacks related to saving into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         */
        public interface NotRestorable extends ViewLastAttrPolicy.NotRestorable, RedirectAttrPolicy.NotRestorable {}

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request save OR restore callback; unless overridden, by default
         * this returns/saves/restores <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the save AND restore callbacks related to saving into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         * <p>
         * NOTE: This reflects the default system behavior.
         */
        public interface Always extends Saveable, Restorable {}

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request save OR restore callback; unless overridden, by default
         * this prevents saving/restoring <code>this</code> for all callbacks.
         * <p>
         * Callbacks include all the save AND restore callbacks related to saving into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_REQ_ATTR_MAP_", etc. used to implement features such as "view-last"/"view-saved"/"view-home" and
         * "request-redirect*" controller request responses, respectively.
         */
        public interface Never extends NotSaveable, NotRestorable {}
    }

    @SuppressWarnings("serial")
    public static class RequestAttrNamePolicy implements AttrPolicy, Set<String>, Serializable {
        public static final String ATTR = "_SCP_NOSAVEREQATTR_";
        // TODO: REVIEW: unclear, but may have been creating a problem, see below
        //private static final RequestAttrNamePolicy DEFAULT = new RequestAttrNamePolicy();

        private static final Map<Class<? extends AttrPolicy>, Collection<String>> defaultAttrNameExclMap;
        static {
            Map<Class<? extends AttrPolicy>, Collection<String>> map = new HashMap<>();
            map.put(RequestSavingAttrPolicy.NotSaveable.class,
                    new ArrayList<>(RequestSavingAttrPolicy.NotSaveable.DEFAULT_ATTR_NAME_EXCL));
            defaultAttrNameExclMap = map;
        }

        private final Map<Class<? extends AttrPolicy>, Set<String>> attrNameExclMap = new HashMap<>(); // This was previously _SCP_NOSAVEREQATTR_ request attribute (HashSet)

        protected RequestAttrNamePolicy(Set<String> attrNameExcl) {
            if (attrNameExcl != null) {
                attrNameExclMap.put(RequestSavingAttrPolicy.NotSaveable.class, attrNameExcl); // backward-compat
            }
        }

        protected RequestAttrNamePolicy() {
            this(new HashSet<>());
        }

        /**
         * Returns set of request attribute/param names which should be excluded from saving
         * into session by "view-last", "request-redirect" and similar responses.
         * <p>
         * 2018-12-03: Previously this list was called "_SCP_VIEW_SAVE_ATTR_EXCL_", now is more generic
         * and called "_SCP_NOSAVEREQATTR_".
         * <p>
         * Intended to be callable from any client code. If does not exist yet,
         * automatically creates and sets in request attributes.
         */
        @SuppressWarnings("unchecked")
        public static RequestAttrNamePolicy from(HttpServletRequest request) {
            Object policyObj = request.getAttribute(ATTR);
            return (policyObj instanceof RequestAttrNamePolicy) ? (RequestAttrNamePolicy) policyObj :
                createAndSet(request, (Collection<String>) policyObj);
        }

        /**
         * Returns set of request attribute/param names which should be excluded from saving
         * into session by "view-last", "request-redirect" and similar responses; if missing,
         * returns a new default instance but does NOT set it in request.
         * <p>
         * 2018-12-03: Previously this list was called "_SCP_VIEW_SAVE_ATTR_EXCL_", now is more generic
         * and called "_SCP_NOSAVEREQATTR_".
         * <p>
         * WARN: The returned policy in this case should be treated as read-only!
         * This is mainly for code that calls the invokers.
         */
        public static RequestAttrNamePolicy fromOrDefault(HttpServletRequest request) {
            Object policyObj = request.getAttribute(ATTR);
            // TODO: REVIEW: Using a static DEFAULT here may cause problems, so make a new empty one lazily for now
            return (policyObj instanceof RequestAttrNamePolicy) ? (RequestAttrNamePolicy) policyObj : create();
        }

        private static RequestAttrNamePolicy create(Collection<String> noSaveAttrSet) {
            return new RequestAttrNamePolicy(UtilMisc.asHashSetNonNull(noSaveAttrSet));
        }

        public static RequestAttrNamePolicy create() {
            return new RequestAttrNamePolicy();
        }

        private static RequestAttrNamePolicy createAndSet(HttpServletRequest request, Collection<String> noSaveAttrSet) {
            RequestAttrNamePolicy policy = create(noSaveAttrSet);
            setAttrNamePolicy(request, policy);
            return policy;
        }

        public static RequestAttrNamePolicy createAndSet(HttpServletRequest request) {
            return createAndSet(request, null);
        }
        
        public static void setAttrNamePolicy(HttpServletRequest request, RequestAttrNamePolicy newPolicy) {
            request.setAttribute(ATTR, newPolicy);
        }

        /**
         * Adds to the request attribute/param names which should be excluded from saving
         * into session by "view-last", "request-redirect" OR similar responses; this collection is editable in-place and
         * caller may simply add names to it.
         * <p>
         * NOTE: In order to simply prevent attributes from being <em>saved</em> into all session maps,
         * simply pass <code>{@link RequestSavingAttrPolicy.NotSaveable}.class</code> as <code>targetEvent</code>.
         */
        public void addExclude(Class<? extends AttrPolicy> targetEvent, String attrName) {
            add(attrName);
        }

        /**
         * Adds to the request attribute/param names which should be excluded from saving
         * into session by "view-last", "request-redirect" OR similar responses; this collection is editable in-place and
         * caller may simply add names to it.
         * <p>
         * NOTE: In order to simply prevent attributes from being <em>saved</em> into all session maps,
         * simply pass <code>{@link RequestSavingAttrPolicy.NotSaveable}.class</code> as <code>targetEvent</code>.
         */
        public void addExcludes(Class<? extends AttrPolicy> targetEvent, Collection<String> attrNames) {
            addAll(attrNames);
        }

        /**
         * Adds to the request attribute/param names which should be excluded from saving
         * into session by "view-last", "request-redirect" OR similar responses; this collection is editable in-place and
         * caller may simply add names to it.
         * <p>
         * NOTE: In order to simply prevent attributes from being <em>saved</em> into all session maps,
         * simply pass <code>{@link RequestSavingAttrPolicy.NotSaveable}.class</code> as <code>targetEvent</code>.
         */
        public void addExcludes(Class<? extends AttrPolicy> targetEvent, String... attrNames) {
            addAll(Arrays.asList(attrNames));
        }

        /**
         * Gets the excludes set for the given single target event (policy).
         * This may return null, but never creates a new set.
         * DO NOT modify the result directly (with the only exception of
         */
        public Set<String> getExcludes(Class<? extends AttrPolicy> targetEvent) { // NOTE: may return null
            return attrNameExclMap.get(targetEvent);
        }

        /**
         * Gets the excludes set for the given target event (policy) as well as any of its child policies.
         * This always creates a new Set (copy), that may be modified directly.
         */
        public Set<String> getAllExcludes(HttpServletRequest request, Class<? extends AttrPolicy> targetEvent,
                Collection<String> extraExcludes) {
            Set<String> allExcludes = (extraExcludes != null) ? new HashSet<>(extraExcludes) : new HashSet<>();
            getExcludes(targetEvent, attrNameExclMap, allExcludes);
            getExcludes(targetEvent, defaultAttrNameExclMap, allExcludes);
            return allExcludes;
        }

        protected static void getExcludes(Class<? extends AttrPolicy> targetEvent,
                Map<Class<? extends AttrPolicy>, ? extends Collection<String>> attrNameExclMap, Set<String> allExcludes) {
            Collection<String> excludes = attrNameExclMap.get(targetEvent);
            if (excludes != null) {
                allExcludes.addAll(excludes);
            } else {
                for(Map.Entry<Class<? extends AttrPolicy>, ? extends Collection<String>> entry : attrNameExclMap.entrySet()) {
                    if (targetEvent.isAssignableFrom(entry.getKey())) { // TODO?: optimize: isAssignableFrom could be pre-calculated?
                        allExcludes.addAll(entry.getValue());
                    }
                }
            }
        }

        private Set<String> getNoSaveExclSet() {
            return attrNameExclMap.get(RequestSavingAttrPolicy.NotSaveable.class);
        }

        // Set<String> interface methods (backward-compat for _SCP_NOSAVEREQATTR_)
        @Override public void forEach(Consumer<? super String> action) { getNoSaveExclSet().forEach(action); }
        @Override public int size() { return getNoSaveExclSet().size(); }
        @Override public boolean isEmpty() { return getNoSaveExclSet().isEmpty(); }
        @Override public boolean contains(Object o) { return getNoSaveExclSet().contains(o); }
        @Override public Iterator<String> iterator() { return getNoSaveExclSet().iterator(); }
        @Override public Object[] toArray() { return getNoSaveExclSet().toArray(); }
        @Override public <T> T[] toArray(T[] a) { return getNoSaveExclSet().toArray(a); }
        @Override public boolean add(String e) { return getNoSaveExclSet().add(e); }
        @Override public boolean remove(Object o) { return getNoSaveExclSet().remove(o); }
        @Override public boolean containsAll(Collection<?> c) { return getNoSaveExclSet().containsAll(c); }
        @Override public boolean addAll(Collection<? extends String> c) { return getNoSaveExclSet().addAll(c); }
        @Override public boolean retainAll(Collection<?> c) { return getNoSaveExclSet().retainAll(c); }
        @Override public boolean removeAll(Collection<?> c) { return getNoSaveExclSet().removeAll(c); }
        @Override public void clear() { getNoSaveExclSet().clear(); }
        @Override public boolean equals(Object o) { return getNoSaveExclSet().equals(o); }
        @Override public int hashCode() { return getNoSaveExclSet().hashCode(); }
        @Override public Spliterator<String> spliterator() { return getNoSaveExclSet().spliterator(); }
        @Override public boolean removeIf(Predicate<? super String> filter) { return getNoSaveExclSet().removeIf(filter); }
        @Override public Stream<String> stream() { return getNoSaveExclSet().stream(); }
        @Override public Stream<String> parallelStream() { return getNoSaveExclSet().parallelStream(); }
    }

    /*
     * ******************************************************************************
     * Specific callback policies (name- and type-based)
     * ******************************************************************************
     */

    /**
     * Modifies controller "view-last"/"view-saved"/"view-home" request-to/from-session-map ("_LAST_VIEW_PARAMS_",
     * "_SAVED_VIEW_PARAMS_", "_HOME_VIEW_PARAMS_") attribute saving behavior, both saving and restoring.
     * <p>
     * The default behavior is represented by {@link ViewLastAttrPolicy.Always} to permit both saving and restoring.
     */
    public interface ViewLastAttrPolicy extends AttrPolicy {

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session whenever a controller "view"
         * is about to be rendered, in the implementing of "view-last"-type responses.
         */
        public interface SavePolicy extends ViewLastAttrPolicy, AttrEventPolicy {
            static class Invoker extends SaveAttrPolicyInvoker<SavePolicy> {
                protected Invoker(HttpServletRequest request) {
                    super(request);
                }
                @Override
                public Class<SavePolicy> getPolicyClass() {
                    return SavePolicy.class;
                }
                @Override
                public Object doSaveAttr(SavePolicy policy, HttpServletRequest request, String attrName,
                        Map<String, Object> saveAttrMap) {
                    return policy.doViewLastAttrSave(request, attrName, saveAttrMap);
                }
                public boolean isAttrNameExcluded(Map<String, Object> saveAttrMap, String attrName, Object value) {
                    // SCIPIO: 2020-01: Always exclude java servlet attributes, because transferring them across a redirect
                    // is a violation of their behaviors according to java servlet spec.
                    return isFilterAttrNames() && (getAttrNameExcludes().contains(attrName) || attrName.startsWith("javax.servlet."));
                }
            };

            public static SaveAttrPolicyInvoker<SavePolicy> getInvoker(HttpServletRequest request) {
                return new Invoker(request);
            }

            /**
             * Invoked just before this object is about to be saved from request attributes to session, or whenever a controller "view"
             * is about to be rendered; returns the object that should be saved in session.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was saved as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> to save, {@link AttrPolicy#VALUE_IGNORE} to prevent, or a replacement value
             */
            Object doViewLastAttrSave(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap);
        }

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session whenever a controller "view"
         * is about to be rendered, in the implementing of "view-last"-type responses; unless overridden, by default, saves <code>this</code>.
         */
        public interface Saveable extends SavePolicy {
            /**
             * Invoked just before this object is about to be saved from request attributes to session, or whenever a controller "view"
             * is about to be rendered; returns the object that should be saved in session, by default <code>this</code>.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was saved as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> (default) to save, {@link AttrPolicy#VALUE_IGNORE} to prevent, or a replacement value
             */
            @Override
            default Object doViewLastAttrSave(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return this;
            }
        }

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session whenever a controller "view"
         * is about to be rendered, in the implementing of "view-last"-type responses; unless overridden, by default, prevents saving <code>this</code>.
         */
        public interface NotSaveable extends SavePolicy {
            /**
             * Called just before this object is about to be saved from request attributes to session, or
             * whenever a controller "view" is about to be rendered; by default returns {@link AttrPolicy#VALUE_IGNORE}.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was saved as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> to save, {@link AttrPolicy#VALUE_IGNORE} (default) to prevent or a replacement value
             */
            @Override
            default Object doViewLastAttrSave(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return AttrPolicy.VALUE_IGNORE;
            }
        }

        /**
         * Callback interface invoked just before this object is about to be restored from session attributes to request, or whenever there is a controller
         * "view-(last|saved|home)" response, in the implementing of "view-last"-type responses; unless overridden, by default, restores <code>this</code>.
         */
        public interface RestorePolicy extends ViewLastAttrPolicy, AttrEventPolicy {
            static class Invoker extends RestoreAttrPolicyInvoker<RestorePolicy> {
                protected Invoker(HttpServletRequest request) {
                    super(request);
                }
                @Override
                public Class<RestorePolicy> getPolicyClass() {
                    return RestorePolicy.class;
                }
                @Override
                public Object doRestoreAttr(RestorePolicy policy, HttpServletRequest request, String attrName,
                        Map<String, Object> saveAttrMap) {
                    return policy.doViewLastRestoreAttr(request, attrName, saveAttrMap);
                }
            };

            public static RestoreAttrPolicyInvoker<RestorePolicy> getInvoker(HttpServletRequest request) {
                return new Invoker(request);
            }

            /**
             * Invoked just before this object is about to be restored from session attributes to request whenever there is a controller
             * "view-(last|saved|home)" response; returns the object that should be saved in session.
             * @param request The request
             * @param attrName The map of saved attributes being read (from session)
             * @param saveAttrMap The map of attributes being restored (from session)
             * @return <code>this</code> to restore, {@link AttrPolicy#VALUE_IGNORE} to prevent or a replacement value
             */
            Object doViewLastRestoreAttr(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap);
        }

        /**
         * Callback interface invoked just before this object is about to be restored from session attributes to request, or whenever there is a controller
         * "view-(last|saved|home)" response, in the implementing of "view-last"-type responses; unless overridden, by default, restores <code>this</code>.
         */
        public interface Restorable extends RestorePolicy {
            /**
             * Invoked just before this object is about to be restored from session attributes to request whenever there is a controller
             * "view-(last|saved|home)" response; returns the object that should be saved in session, by default <code>this</code>.
             * @param request The request
             * @param attrName The map of saved attributes being read (from session)
             * @param saveAttrMap The map of attributes being restored (from session)
             * @return <code>this</code> (default) to restore, {@link AttrPolicy#VALUE_IGNORE} to prevent or a replacement value
             */
            @Override
            default Object doViewLastRestoreAttr(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
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
             * "view-(last|saved|home)" response; returns the object that should be saved in session, by default {@link AttrPolicy#VALUE_IGNORE}.
             * @param request The request
             * @param attrName The map of saved attributes being read (from session)
             * @param saveAttrMap The map of attributes being restored (from session)
             * @return <code>this</code> to restore, {@link AttrPolicy#VALUE_IGNORE} (default) to prevent or a replacement value
             */
            @Override
            default Object doViewLastRestoreAttr(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return AttrPolicy.VALUE_IGNORE;
            }
        }

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request save OR restore callbacks for view-last logic;
         * unless overridden, by default this returns/saves/restores <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the save AND restore callbacks related to saving into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_SAVED_VIEW_PARAMS_" and "_HOME_VIEW_PARAMS_" used to implement the "view-last"/"view-saved"/"view-home"
         * controller request response.
         * <p>
         * NOTE: This reflects the default system behavior.
         */
        public interface Always extends Saveable, Restorable {}

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request save OR restore callback for view-last logic;
         * unless overridden, by default this prevents saving/restoring <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the save AND restore callbacks related to saving into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_SAVED_VIEW_PARAMS_" and "_HOME_VIEW_PARAMS_" used to implement the "view-last"/"view-saved"/"view-home"
         * controller request response.
         */
        public interface Never extends NotSaveable, NotRestorable {}
    }

    /**
     * Modifies controller redirect request-to/from-session-map ("_REQ_ATTR_MAP_") attribute saving behavior,
     * both saving and restoring, notably for "request-redirect*" request responses,
     * <p>
     * The default behavior is represented by {@link RedirectAttrPolicy.Always} to permit both saving and restoring.
     */
    public interface RedirectAttrPolicy extends AttrPolicy {

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session upon any controller
         * redirect.
         */
        public interface SavePolicy extends RedirectAttrPolicy, AttrEventPolicy {
            static class Invoker extends SaveAttrPolicyInvoker<SavePolicy> {
                // SCIPIO: 2018-07-10: Do not include multiPartMap, to prevent storing uploaded files in session;
                // NOTE: This is a heuristic based on most common usage of multiPartMap - may be tweaked in future.
                private static final Set<String> ATTR_NAME_EXCLUDES = UtilMisc.unmodifiableHashSet("multiPartMap", "requestBodyMap");
                protected Invoker(HttpServletRequest request) {
                    super(request);
                }
                @Override
                public Class<SavePolicy> getPolicyClass() {
                    return SavePolicy.class;
                }
                @Override
                public Object doSaveAttr(SavePolicy policy, HttpServletRequest request, String attrName,
                        Map<String, Object> saveAttrMap) {
                    return policy.doRedirectSaveAttr(request, attrName, saveAttrMap);
                }
                @Override
                public Set<String> getExtraAttrNameExcludes() {
                    return ATTR_NAME_EXCLUDES;
                }

                @Override
                public boolean isAttrNameExcluded(Map<String, Object> saveAttrMap, String attrName, Object value) {
                    // SCIPIO: 2020-01: Always exclude java servlet attributes, because transferring them across a redirect
                    // is a violation of their behaviors according to java servlet spec.
                    return isFilterAttrNames() && (getAttrNameExcludes().contains(attrName) || attrName.startsWith("javax.servlet."));
                }
            };

            public static SaveAttrPolicyInvoker<SavePolicy> getInvoker(HttpServletRequest request) {
                return new Invoker(request);
            }

            /**
             * Invoked just before this object is about to be saved from request attributes to session upon any controller redirect;
             * returns the object that should be saved in session.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was saved as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> to save, {@link AttrPolicy#VALUE_IGNORE} to prevent or a replacement value
             */
            Object doRedirectSaveAttr(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap);
        }

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session upon any controller
         * redirect; unless overridden, by default, saves <code>this</code>.
         */
        public interface Saveable extends SavePolicy {
            /**
             * Invoked just before this object is about to be saved from request attributes to session upon any controller redirect;
             * returns the object that should be saved in session, by default <code>this</code>.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was saved as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> (default) to save, {@link AttrPolicy#VALUE_IGNORE} to prevent or a replacement value
             */
            @Override
            default Object doRedirectSaveAttr(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return this;
            }
        }

        /**
         * Callback interface invoked just before this object is about to be saved from request attributes to session upon any controller
         * redirect; unless overridden, by default, prevents saving <code>this</code>.
         */
        public interface NotSaveable extends SavePolicy {
            /**
             * Invoked just before this object is about to be saved from request attributes to session upon any controller redirect;
             * returns the object that should be saved in session, by default {@link AttrPolicy#VALUE_IGNORE}.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was saved as
             * @param saveAttrMap The map of attributes being saved (into session)
             * @return <code>this</code> to save, {@link AttrPolicy#VALUE_IGNORE} (default) to prevent or a replacement value
             */
            @Override
            default Object doRedirectSaveAttr(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return AttrPolicy.VALUE_IGNORE;
            }
        }

        /**
         * Callback interface invoked just before this object is about to be restored from session attributes to request after a controller
         * redirect.
         */
        public interface RestorePolicy extends RedirectAttrPolicy, AttrEventPolicy {
            static class Invoker extends RestoreAttrPolicyInvoker<RestorePolicy> {
                protected Invoker(HttpServletRequest request) {
                    super(request);
                }
                @Override
                public Class<RestorePolicy> getPolicyClass() {
                    return RestorePolicy.class;
                }
                @Override
                public Object doRestoreAttr(RestorePolicy policy, HttpServletRequest request, String attrName,
                        Map<String, Object> saveAttrMap) {
                    return policy.doRedirectAttrRestore(request, attrName, saveAttrMap);
                }
            };

            public static RestoreAttrPolicyInvoker<RestorePolicy> getInvoker(HttpServletRequest request) {
                return new Invoker(request);
            }

            /**
             * Invoked just before this object is about to be restored from session attributes to request upon any controller redirect;
             * returns the object that should be saved in session.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was saved as
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
             * returns the object that should be saved in session, by default <code>this</code>.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was saved as
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
             * returns the object that should be saved in session, by default {@link AttrPolicy#VALUE_IGNORE}.
             * @param request The request
             * @param attrName The name of the request attribute that <code>this</code> object was saved as
             * @param saveAttrMap The map of saved attributes being restored (from session)
             * @return <code>this</code> to restore, {@link AttrPolicy#VALUE_IGNORE} (default) to prevent or a replacement value
             */
            @Override
            default Object doRedirectAttrRestore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
                return AttrPolicy.VALUE_IGNORE;
            }
        }

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request save OR restore callbacks for controller redirect logic;
         * unless overridden, by default this returns/saves/restores <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the save AND restore callbacks related to saving into the session attributes maps "_REQ_ATTR_MAP_"
         * used to implement controller redirects including the "request-redirect*" responses.
         * <p>
         * NOTE: This reflects the default system behavior.
         */
        public interface Always extends Saveable, Restorable {}

        /**
         * Handler for any {@link RequestHandler}-related session attribute to request save OR restore callback  for view-last logic;
         * unless overridden, by default this prevents saving/restoring <code>this</code> for all callbacks.
         * <p>
         * Callbacks include the save AND restore callbacks related to saving into the session attributes maps "_LAST_VIEW_PARAMS_",
         * "_SAVED_VIEW_PARAMS_" and "_HOME_VIEW_PARAMS_" used to implement the "view-last"/"view-saved"/"view-home"
         * controller request response.
         */
        public interface Never extends NotSaveable, NotRestorable {}
    }

    /*
     * ******************************************************************************
     * Policy INVOKER
     * ******************************************************************************
     * These can be run using the supplied INVOKER variables in interfaces above.
     *
     * DEV NOTE: Originally SaveRestorePolicyInvoker
     */

    /**
     * Main invoker for the attribute event implementations of request-to-session attribute saving;
     * new instance must be created at each attribute event (outside loops).
     * <p>
     * NOT thread-safe!
     */
    public static abstract class SaveRestoreAttrPolicyInvoker<T extends AttrPolicy> {
        private final HttpServletRequest request;
        private boolean filterAttrNames = true;
        //private RequestSavingAttrNamePolicy attrNamePolicy; // Optimization: not currently needed
        private Set<String> attrNameExcludes;

        protected SaveRestoreAttrPolicyInvoker(HttpServletRequest request) {
            this.request = request;
        }

        protected HttpServletRequest getRequest() {
            return request;
        }

        public boolean isFilterAttrNames() {
            return filterAttrNames;
        }

        /**
         * Sets whether attribute names - and not only values/types - should get filtered
         * by the main method calls; can be disabled in some cases for faster code.
         */
        public void setFilterAttrNames(boolean filterAttrNames) {
            this.filterAttrNames = filterAttrNames;
        }

        protected RequestAttrNamePolicy getAttrNamePolicy() {
            /* Optimization: not needed currently
            if (attrNamePolicy == null) {
                attrNamePolicy = RequestSavingAttrNamePolicy.fromOrDefault(request);
            }
            return attrNamePolicy;
            */
            return RequestAttrNamePolicy.fromOrDefault(request);
        }

        protected Set<String> getAttrNameExcludes() {
            if (attrNameExcludes == null) {
                attrNameExcludes = getAttrNamePolicy().getAllExcludes(getRequest(), getPolicyClass(),
                        getExtraAttrNameExcludes());
            }
            return attrNameExcludes;
        }

        public Set<String> getExtraAttrNameExcludes() {
            return null;
        }

        public abstract Class<T> getPolicyClass();

        public boolean attrValueTypeApplies(String attrName, Object value, Map<String, Object> saveAttrMap) {
            return (value != null) && getPolicyClass().isAssignableFrom(value.getClass()); // (value instanceof Xxx)
        }

        public boolean isAttrNameExcluded(Map<String, Object> saveAttrMap, String attrName, Object value) {
            return isFilterAttrNames() && getAttrNameExcludes().contains(attrName);
        }
    }

    public static abstract class SaveAttrPolicyInvoker<T extends AttrPolicy> extends SaveRestoreAttrPolicyInvoker<T> {
        protected SaveAttrPolicyInvoker(HttpServletRequest request) {
            super(request);
        }

        /**
         * Main attribute value-/type-based filter - this invokes the main attribute callbacks on attribute value
         * objects that implement AttrPolicy subclasses.
         */
        public abstract Object doSaveAttr(T poli, HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap);

        /**
         * Filters an individual attribute for value/type/name policy.
         * <p>
         * NOTE: The name check here is suboptimal, but probably won't matter.
         */
        public boolean filterSaveAttrToMap(Map<String, Object> saveAttrMap, String attrName, Object value) {
            if (isAttrNameExcluded(saveAttrMap, attrName, value)) {
                return false;
            }
            if (attrValueTypeApplies(attrName, value, saveAttrMap)) {
                @SuppressWarnings("unchecked")
                Object filteredValue = doSaveAttr((T) value, getRequest(), attrName, saveAttrMap);
                return RequestAttrPolicy.putMapValue(saveAttrMap, attrName, filteredValue);
            }
            saveAttrMap.put(attrName, value);
            return true;
        }

        /**
         * Filters an individual attribute for value/type/name policy.
         * <p>
         * NOTE: The name check here is suboptimal, but probably won't matter.
         */
        public boolean filterSaveAttrToMap(Map<String, Object> saveAttrMap, Map.Entry<String, Object> attrEntry) {
            return filterSaveAttrToMap( saveAttrMap, MapEntryAdapter.getKey(attrEntry), MapEntryAdapter.getValue(attrEntry));
        }

        /**
         * Bulk in-place attribute map filter/update.
         */
        public void filterMapAttr(Map<String, Object> saveAttrMap) {
            if (isFilterAttrNames()) {
                filterMapAttrNames(saveAttrMap);
            }
            filterMapAttrValues(saveAttrMap);
        }

        /**
         * Bulk in-place dedicated attribute names map filter/update.
         */
        public void filterMapAttrNames(Map<String, Object> saveAttrMap) {
            for(String exclude : getAttrNameExcludes()) {
                saveAttrMap.remove(exclude);
            }
        }

        /**
         * Bulk in-place dedicated attribute values map filter/update.
         */
        public void filterMapAttrValues(Map<String, Object> saveAttrMap) {
            for(Map.Entry<String, Object> attrEntry : saveAttrMap.entrySet()) {
                if (attrValueTypeApplies(attrEntry.getKey(), attrEntry.getValue(), saveAttrMap)) {
                    @SuppressWarnings("unchecked")
                    Object filteredValue = doSaveAttr((T) attrEntry.getValue(), getRequest(), attrEntry.getKey(), saveAttrMap);
                    RequestAttrPolicy.setEntryValue(attrEntry, filteredValue);
                }
            }
        }
    }

    public static abstract class RestoreAttrPolicyInvoker<T extends AttrPolicy> extends SaveRestoreAttrPolicyInvoker<T> {
        protected RestoreAttrPolicyInvoker(HttpServletRequest request) {
            super(request);
        }

        /**
         * Main attribute value-/type-based filter - this invokes the main attribute callbacks on attribute value
         * objects that implement AttrPolicy subclasses.
         */
        public abstract Object doRestoreAttr(T poli, HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap);

        /**
         * Filters an individual attribute for value/type/policy.
         * <p>
         * NOTE: The name check here is suboptimal, but probably won't matter.
         */
        public boolean filterRestoreAttrToRequest(String attrName, Object value, Map<String, Object> saveAttrMap) {
            if (isAttrNameExcluded(saveAttrMap, attrName, value)) {
                return false;
            }
            if (attrValueTypeApplies(attrName, value, saveAttrMap)) {
                @SuppressWarnings("unchecked")
                Object filteredValue = doRestoreAttr((T) value, getRequest(), attrName, saveAttrMap);
                return RequestAttrPolicy.setServletAttribute(getRequest(), attrName, filteredValue);
            }
            setServletAttribute(getRequest(), attrName, value);
            return true;
        }

        /**
         * Filters an individual attribute for value/type/policy.
         * <p>
         * NOTE: The name check here is suboptimal, but probably won't matter.
         */
        public boolean filterRestoreAttrToRequest(Map.Entry<String, Object> attrEntry, Map<String, Object> saveAttrMap) {
            return filterRestoreAttrToRequest(MapEntryAdapter.getKey(attrEntry), MapEntryAdapter.getValue(attrEntry), saveAttrMap);
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
