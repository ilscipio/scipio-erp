package org.ofbiz.service;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import javax.xml.ws.Dispatch;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

/**
 * Helper service context accessor for common operations on {@link DispatchContext} and service contexts (SCIPIO).
 * Map interface methods affect the wrapped service context.
 * NOTE: May be reused as base class for common implementations, but more methods may be added, so not recommended
 * for client code.
 */
public class ServiceContext implements Map<String, Object> {

    private final DispatchContext dctx;
    private final Map<String, Object> context;

    protected ServiceContext(DispatchContext dctx, Map<String, Object> context) {
        this.dctx = dctx;
        this.context = context;
    }

    public static ServiceContext from(DispatchContext dctx, Map<String, ?> context) {
        // NOTE: ? should always be Object, this is for simplicity/compatibility, could be misused but never happens
        return new ServiceContext(dctx, UtilGenerics.cast(context));
    }

    // TODO
    //public static ServiceContext from(ScipioContext scipioContext) {
    //}

    // TODO
    //public ScipioContext toScipioCtx() {
    //}

    public DispatchContext getDctx() {
        return dctx;
    }

    public Delegator getDelegator() {
        return getDctx().getDelegator();
    }

    public LocalDispatcher getDispatcher() {
        return getDctx().getDispatcher();
    }

    public Map<String, Object> getContext() {
        return context;
    }

    public GenericValue getUserLogin() {
        return (GenericValue) getContext().get("userLogin");
    }

    public Locale getLocale() {
        return (Locale) getContext().get("locale");
    }

    public TimeZone getTimeZone() {
        return (TimeZone) getContext().get("timeZone");
    }

    @Override
    public int size() {
        return getContext().size();
    }

    @Override
    public boolean isEmpty() {
        return getContext().isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return getContext().containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return getContext().containsValue(value);
    }

    @Override
    public Object get(Object key) {
        return getContext().get(key);
    }

    public <T> T getAttr(Object key) {
        return UtilGenerics.cast(get(key));
    }

    public <T> T getAttr(Object key, T defaultValue) {
        T value = getAttr(key);
        return (value != null) ? value : defaultValue;
    }

    @Override
    public Object put(String key, Object value) {
        return getContext().put(key, value);
    }

    @Override
    public Object remove(Object key) {
        return getContext().remove(key);
    }

    @Override
    public void putAll(Map<? extends String, ?> m) {
        getContext().putAll(m);
    }

    @Override
    public void clear() {
        getContext().clear();
    }

    @Override
    public Set<String> keySet() {
        return getContext().keySet();
    }

    @Override
    public Collection<Object> values() {
        return getContext().values();
    }

    @Override
    public Set<Entry<String, Object>> entrySet() {
        return getContext().entrySet();
    }

    @Override
    public boolean remove(Object key, Object value) {
        return getContext().remove(key, value);
    }
}
