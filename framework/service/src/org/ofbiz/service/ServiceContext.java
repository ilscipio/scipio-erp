package org.ofbiz.service;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;

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

    public DispatchContext dctx() {
        return dctx;
    }

    public Map<String, Object> context() {
        return context;
    }

    public Delegator delegator() {
        return dctx().getDelegator();
    }

    public LocalDispatcher dispatcher() {
        return dctx().getDispatcher();
    }

    public Security security() {
        return dctx().getSecurity();
    }

    public GenericValue userLogin() {
        return (GenericValue) context().get("userLogin");
    }

    public Locale locale() {
        return (Locale) context().get("locale");
    }

    public TimeZone timeZone() {
        return (TimeZone) context().get("timeZone");
    }

    @SuppressWarnings("unchecked")
    public <T> T attr(Object key) {
        return (T) get(key);
    }

    public <T> T attr(Object key, T defaultValue) {
        T value = attr(key);
        return (value != null) ? value : defaultValue;
    }

    @Override
    public int size() {
        return context().size();
    }

    @Override
    public boolean isEmpty() {
        return context().isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return context().containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return context().containsValue(value);
    }

    @Override
    public Object get(Object key) {
        return context().get(key);
    }

    @Override
    public Object put(String key, Object value) {
        return context().put(key, value);
    }

    @Override
    public Object remove(Object key) {
        return context().remove(key);
    }

    @Override
    public void putAll(Map<? extends String, ?> m) {
        context().putAll(m);
    }

    @Override
    public void clear() {
        context().clear();
    }

    @Override
    public Set<String> keySet() {
        return context().keySet();
    }

    @Override
    public Collection<Object> values() {
        return context().values();
    }

    @Override
    public Set<Entry<String, Object>> entrySet() {
        return context().entrySet();
    }

    @Override
    public boolean remove(Object key, Object value) {
        return context().remove(key, value);
    }

    public Map<String, Object> makeValidContext(String serviceName, String mode, Map<String, ?> context) throws GenericServiceException {
        return dctx().makeValidContext(serviceName, mode, context);
    }

    public ModelService getModelService(String serviceName) throws GenericServiceException {
        return dctx().getModelService(serviceName);
    }

    public ModelService getModelServiceAlways(String serviceName) throws IllegalArgumentException {
        return dctx().getModelServiceAlways(serviceName);
    }

    public ModelService getModelServiceOrNull(String serviceName) {
        return dctx().getModelServiceOrNull(serviceName);
    }

    /**
     * Returns the model of the last invoked service, or null if no service executing (SCIPIO).
     * @return the current service model, or null if no service executing
     */
    public ModelService getModelService() {
        return dctx().getModelService();
    }

    public boolean isService(String serviceName) {
        return dctx().isService(serviceName);
    }
}
