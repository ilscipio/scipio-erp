package org.ofbiz.service;

import org.ofbiz.base.util.UtilGenerics;
import com.ilscipio.scipio.base.util.collections.AttrMap;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;

import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Helper service context accessor for common operations on {@link DispatchContext} and the service context map (SCIPIO).
 * <p>Map interface methods affect the wrapped service context.</p>
 * <p>NOTE: May be reused as base class for common implementations, but more methods may be added, so not recommended
 * for client code.</p>
 */
public class ServiceContext implements AttrMap<String, Object> {

    private final DispatchContext dctx;
    private final Map<String, Object> context;

    protected ServiceContext(DispatchContext dctx, Map<String, ?> context) {
        this.dctx = dctx;
        this.context = UtilGenerics.cast(context);
    }

    protected ServiceContext(ServiceContext other, DispatchContext dctx, Map<String, ?> context) {
        this.dctx = (dctx != null) ? dctx : other.dctx();
        this.context = (context != null) ? UtilGenerics.cast(context) : other.context();
    }

    protected ServiceContext(ServiceContext other, Map<String, ?> context) {
        this.dctx = other.dctx();
        this.context = UtilGenerics.cast(context);
    }

    /** Returns a new ServiceContext. */
    public static ServiceContext from(DispatchContext dctx, Map<String, ?> context) {
        // NOTE: ? should always be Object, this is for simplicity/compatibility, could be misused but never happens
        return new ServiceContext(dctx, context);
    }

    /** Returns a new ServiceContext. */
    public static ServiceContext from(LocalDispatcher dispatcher, Map<String, ?> context) {
        return from(dispatcher.getDispatchContext(), context);
    }

    /** Return this ServiceContext with a substitute context map, for delegation. */
    public ServiceContext from(Map<String, ?> newContext) {
        return new ServiceContext(this, newContext);
    }

    /** Returns a copy of this ServiceContext with a copy of its original context. */
    public ServiceContext copy() {
        return new ServiceContext(this, new HashMap<>(context()));
    }

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

    @Override
    public Object getOrDefault(Object key, Object defaultValue) {
        return context().getOrDefault(key, defaultValue);
    }

    @Override
    public void forEach(BiConsumer<? super String, ? super Object> action) {
        context().forEach(action);
    }

    @Override
    public void replaceAll(BiFunction<? super String, ? super Object, ?> function) {
        context().replaceAll(function);
    }

    @Override
    public Object putIfAbsent(String key, Object value) {
        return context().putIfAbsent(key, value);
    }

    @Override
    public boolean replace(String key, Object oldValue, Object newValue) {
        return context().replace(key, oldValue, newValue);
    }

    @Override
    public Object replace(String key, Object value) {
        return context().replace(key, value);
    }

    @Override
    public Object computeIfAbsent(String key, Function<? super String, ?> mappingFunction) {
        return context().computeIfAbsent(key, mappingFunction);
    }

    @Override
    public Object computeIfPresent(String key, BiFunction<? super String, ? super Object, ?> remappingFunction) {
        return context().computeIfPresent(key, remappingFunction);
    }

    @Override
    public Object compute(String key, BiFunction<? super String, ? super Object, ?> remappingFunction) {
        return context().compute(key, remappingFunction);
    }

    @Override
    public Object merge(String key, Object value, BiFunction<? super Object, ? super Object, ?> remappingFunction) {
        return context().merge(key, value, remappingFunction);
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
     * @param serviceName The name of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT)
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidContext(String serviceName, String mode) throws GenericServiceException {
        return makeValidContext(serviceName, mode, context());
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
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param model The ModelService object of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT)
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidContext(ModelService model, String mode) throws GenericServiceException {
        return makeValidContext(model, mode, context());
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
     * @param serviceName The name of the service to obtain parameters for
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidInContext(String serviceName) throws GenericServiceException {
        return makeValidInContext(serviceName, context());
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
     * Uses an existing map of name value pairs and extracts the IN keys which are used in serviceName (SCIPIO)
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param model The ModelService object of the service to obtain parameters for
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidInContext(ModelService model) throws GenericServiceException {
        return makeValidInContext(model, context());
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
     * @param serviceName The name of the service to obtain parameters for
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidOutContext(String serviceName) throws GenericServiceException {
        return makeValidOutContext(serviceName, context());
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
     * Uses an existing map of name value pairs and extracts the OUT keys which are used in serviceName (SCIPIO)
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param model The ModelService object of the service to obtain parameters for
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidOutContext(ModelService model) throws GenericServiceException {
        return makeValidOutContext(model, context());
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
     * @param serviceName The name of the service to obtain parameters for
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidInOutContext(String serviceName) throws GenericServiceException {
        return makeValidInOutContext(serviceName, context());
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
     * Uses an existing map of name value pairs and extracts the IN and OUT keys which are used in serviceName (SCIPIO)
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields
     * @param model The ModelService object of the service to obtain parameters for
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidInOutContext(ModelService model) throws GenericServiceException {
        return makeValidInOutContext(model, context());
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
