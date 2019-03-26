package org.ofbiz.content.content;

import java.io.Serializable;
import java.util.Locale;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContainer;

/**
 * SCIPIO: New base implementation for all common ContentWrappers.
 * <p>
 * This implements Serializable as general workaround for some cases, although such cases should be avoided.
 * <p>
 * NEW: This is now thread-safe (despite the local-worker pattern), as a workaround for cases where
 * instances may end up in session attributes.
 * HOWEVER, client code should AVOID exposing this to thread-unsafe situations, because in the
 * future, this could be changed...
 * <p>
 * Added 2017-11-25.
 */
@SuppressWarnings("serial")
public abstract class CommonContentWrapper implements ContentWrapper, Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private transient Delegator delegator;
    private final String delegatorName;
    private transient LocalDispatcher dispatcher;
    private final String dispatcherName;
    private final GenericValue entityValue;
    private final Locale locale;
    private final String mimeTypeId;
    /**
     * SCIPIO: Toggles whether the wrapper UtilCache should be used for this instance
     * for the get calls that have no override for this.
     */
    private final boolean useCache;

    protected CommonContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale,
            String mimeTypeId, boolean useCache) {
        this.dispatcher = dispatcher;
        this.dispatcherName = this.dispatcher.getName();
        this.entityValue = entityValue;
        this.delegator = extractDelegator(entityValue, dispatcher, null);
        this.delegatorName = delegator.getDelegatorName();
        this.locale = locale;
        this.mimeTypeId = mimeTypeId;
        this.useCache = useCache;
    }

    protected CommonContentWrapper(LocalDispatcher dispatcher, GenericValue entityValue, Locale locale,
            String mimeTypeId) {
        this(dispatcher, entityValue, locale, mimeTypeId, true);
    }

    protected CommonContentWrapper(GenericValue entityValue, HttpServletRequest request, boolean useCache) {
        this.dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        this.dispatcherName = this.dispatcher.getName();
        this.entityValue = entityValue;
        this.delegator = extractDelegator(entityValue, this.dispatcher, request);
        this.delegatorName = delegator.getDelegatorName();
        this.locale = UtilHttp.getLocale(request);
        this.mimeTypeId = getDefaultMimeTypeId(this.delegator);
        this.useCache = useCache;
    }

    protected CommonContentWrapper(GenericValue entityValue, HttpServletRequest request) {
        this(entityValue, request, true);
    }

    /**
     * Extracts the most specific delegator possible from the constructor args.
     */
    protected static Delegator extractDelegator(GenericValue entityValue, LocalDispatcher dispatcher, ServletRequest request) {
        if (entityValue != null) {
            return entityValue.getDelegator();
        } else {
            // NOTE: if you actually need a wrapper that always returns null, you could try passing a temporary
            // empty new GenericValue... here will always print warning because this was never a normal case,
            // almost sure to be an error.
            Debug.logWarning("ContentWrapper was created with null entity value - wrapper will only return null values", module);
            Delegator delegator = null;
            if (request != null) {
                delegator = (Delegator) request.getAttribute("delegator");
                if (delegator != null) return delegator;
            }
            if (dispatcher != null) {
                delegator = dispatcher.getDelegator();
                if (delegator != null) return delegator;
            } else {
                Debug.logError("ContentWrapper was created with no LocalDispatcher available - crash likely", module);
            }
            return DelegatorFactory.getDelegator("default");
        }
    }

    // REMOVED to support thread-safety
//    /**
//     * SCIPIO: Allows to disable the wrapper UtilCache for this wrapper.
//     * By default, wrapper cache is enabled for new instances.
//     */
//    public CommonContentWrapper setUseCache(boolean useCache) {
//        this.useCache = useCache;
//        return this;
//    }

    /**
     * SCIPIO: Content lookup by content type ID (available options depend on sub-class) with
     * explicit useCache (for individual call).
     */
    public String get(String contentTypeId, boolean useCache, String contentLang) {
        if (entityValue == null) return null;
        return getImpl(contentTypeId, useCache, contentLang);
    }

    /**
     * SCIPIO: Content lookup by content type ID (available options depend on sub-class) with
     * explicit useCache (for individual call), using "raw"/no encoding.
     */
    public String get(String contentTypeId, boolean useCache) {
        if (entityValue == null) return null;
        return getImpl(contentTypeId, useCache, "raw");
    }

    @Override
    public String get(String contentTypeId, String contentLang) {
        if (entityValue == null) return null;
        return getImpl(contentTypeId, this.useCache, contentLang);
    }

    @Override
    public String get(String contentTypeId) {
        if (entityValue == null) return null;
        return getImpl(contentTypeId, this.useCache, "raw");
    }

    /**
     * SCIPIO: Content lookup by content type ID (available options depend on sub-class) implementation.
     */
    protected abstract String getImpl(String contentTypeId, boolean useCache, String contentLang);

    public Delegator getDelegator() {
        Delegator delegator = this.delegator;
        if (delegator == null) { // handles serializable
            delegator = DelegatorFactory.getDelegator(delegatorName);
            this.delegator = delegator;
        }
        return delegator;
    }

    public GenericValue getEntityValue() {
        return entityValue;
    }

    public LocalDispatcher getDispatcher() {
        LocalDispatcher dispatcher = this.dispatcher;
        if (dispatcher == null) {
            dispatcher = ServiceContainer.getLocalDispatcher(dispatcherName, this.getDelegator());
            this.dispatcher = dispatcher;
        }
        return dispatcher;
    }

    public Locale getLocale() {
        return locale;
    }

    public String getMimeTypeId() {
        return mimeTypeId;
    }

    // pointless - pass at construction
//    public String getDefaultMimeTypeId() {
//        return getDefaultMimeTypeId(getDelegator());
//    }

    /**
     * Gets default mimeTypeId for content wrappers.
     * FIXME: unhardcode, stuck with old ofbiz code... always returns text/html
     */
    public static String getDefaultMimeTypeId(Delegator delegator) {
        return "text/html";
    }

    /**
     * Returns the default isUseCache on the instance, though individual calls may override.
     * For new instances that didn't specify explicit, this is always true.
     */
    public boolean isUseCache() {
        return useCache;
    }

}
