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
import java.util.function.Supplier;

/**
 * Helper service context accessor for common operations on {@link DispatchContext} and the service context map (SCIPIO).
 * <p>Map interface methods affect the wrapped service context.</p>
 * <p>NOTE: May be reused as base class for common implementations, but more methods may be added, so not recommended
 * for client code.</p>
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

    public static ServiceContext from(LocalDispatcher dispatcher, Map<String, ?> context) {
        return from(dispatcher.getDispatchContext(), context);
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

    /**
     * Returns a service attribute value from the service context map, or null.
     */
    @SuppressWarnings("unchecked")
    public <T> T attr(Object key) {
        return (T) get(key);
    }

    /**
     * Returns a service attribute value from the service context map, or if null, the given default value.
     */
    public <T> T attr(Object key, T defaultValue) {
        T value = attr(key);
        return (value != null) ? value : defaultValue;
    }

    /**
     * Returns a service attribute value from the service context map, or if null, the given default value supplied
     * by the given supplier callback or lambda function.
     */
    public <T> T attr(Object key, Supplier<T> defaultValueSupplier) {
        T value = attr(key);
        return (value != null) ? value : defaultValueSupplier.get();
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

    /**
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param serviceName The name of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT)
     * @param context The initial set of values to pull from
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidContext(String serviceName, String mode, Map<String, ?> context) throws GenericServiceException {
        return dctx().makeValidContext(serviceName, mode, context);
    }

    /**
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param model The ModelService object of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT)
     * @param context The initial set of values to pull from
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidContext(ModelService model, String mode, Map<String, ?> context) throws GenericServiceException {
        return DispatchContext.makeValidContext(model, mode, context);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN keys which are used in serviceName (SCIPIO)
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param serviceName The name of the service to obtain parameters for
     * @param context The initial set of values to pull from
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidInContext(String serviceName, Map<String, ?> context) throws GenericServiceException {
        return dctx().makeValidInContext(serviceName, context);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN keys which are used in serviceName (SCIPIO)
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param model The ModelService object of the service to obtain parameters for
     * @param context The initial set of values to pull from
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidInContext(ModelService model, Map<String, ?> context) throws GenericServiceException {
        return dctx().makeValidInContext(model, context);
    }

    /**
     * Uses an existing map of name value pairs and extracts the OUT keys which are used in serviceName (SCIPIO)
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param serviceName The name of the service to obtain parameters for
     * @param context The initial set of values to pull from
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidOutContext(String serviceName, Map<String, ?> context) throws GenericServiceException {
        return dctx().makeValidOutContext(serviceName, context);
    }

    /**
     * Uses an existing map of name value pairs and extracts the OUT keys which are used in serviceName (SCIPIO)
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param model The ModelService object of the service to obtain parameters for
     * @param context The initial set of values to pull from
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidOutContext(ModelService model, Map<String, ?> context) throws GenericServiceException {
        return dctx().makeValidOutContext(model, context);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN and OUT keys which are used in serviceName (SCIPIO)
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param serviceName The name of the service to obtain parameters for
     * @param context The initial set of values to pull from
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidInOutContext(String serviceName, Map<String, ?> context) throws GenericServiceException {
        return dctx().makeValidInOutContext(serviceName, context);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN and OUT keys which are used in serviceName (SCIPIO)
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param model The ModelService object of the service to obtain parameters for
     * @param context The initial set of values to pull from
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidInOutContext(ModelService model, Map<String, ?> context) throws GenericServiceException {
        return dctx().makeValidInOutContext(model, context);
    }

    /**
     * Gets the ModelService instance that corresponds to given the name
     * @param serviceName Name of the service
     * @return GenericServiceModel that corresponds to the serviceName
     */
    public ModelService getModelService(String serviceName) throws GenericServiceException {
        return dctx().getModelService(serviceName);
    }

    /**
     * Gets the ModelService instance that corresponds to given the name, throwing IllegalArgumentException if not found (SCIPIO).
     * @param serviceName Name of the service
     * @return GenericServiceModel that corresponds to the serviceName
     */
    public ModelService getModelServiceAlways(String serviceName) throws IllegalArgumentException {
        return dctx().getModelServiceAlways(serviceName);
    }

    /**
     * Gets the ModelService instance that corresponds to given the name, returning null and no logging if not found (SCIPIO).
     * @param serviceName Name of the service
     * @return GenericServiceModel that corresponds to the serviceName
     */
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

    /**
     * Gets the ModelService instance that corresponds to given the name, returning null and no logging if not found (SCIPIO).
     * @param serviceName Name of the service
     * @return GenericServiceModel that corresponds to the serviceName
     */
    public boolean isService(String serviceName) {
        return dctx().isService(serviceName);
    }
}
