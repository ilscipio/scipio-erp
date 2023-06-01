package org.ofbiz.service;

import com.ilscipio.scipio.ce.util.collections.MapWrapper;
import com.ilscipio.scipio.ce.util.collections.ScipioMap;
import com.ilscipio.scipio.service.def.Service;
import org.ofbiz.base.util.Debug;
import com.ilscipio.scipio.ce.util.collections.AttrMap;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilObject;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;

import java.io.Serializable;
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
 * Service context wrapper for service-related {@link DispatchContext} and service parameter operations,
 * by convention "ctx" in most service code.
 *
 * <ul>
 *  <li>Serves as a drop-in replacement for many {@link DispatchContext} and context map methods and calls, especially
 *      when converting to modern service class format {@link ServiceHandler.LocalExec}.</li>
 *  <li>Implements per-execution modeling of service contexts so the model service, dispatch context and parameters are
 *      carried around together</li>
 *  <li>Acts as factory for {@link ServiceResult} (results are created for corresponding service - seamlessly).</li>
 *  <li>Implements Map so all its operations affect the wrapped service context map, {@link ServiceContext#context()}.</p>
 * </ul>
 * <p>
 *
 * <p>DEV NOTE: For simplicity this class is used as both the service input context and as base class for {@link ServiceResult},
 * so any methods added here will end up in that class as well (in the future could turn this into an interface implemented by
 * a ServiceContextIn class, but this is generally not needed as long as the {@link #as} factory methods are used and not
 * the constructors). This is also because the context acts as a factory for service results. This class should only
 * contain general-purpose service-related methods and delegate methods that
 * are appropriate or adapted for both input and output.</p>
 *
 * <p>SCIPIO: 3.0.0: Improved {@link #makeValidContext} to support IN-SYS, OUT-SYS, INOUT-SYS mode target parameters;
 *  deprecated makeValid(In|Out|InOut) variants (too many).</p>
 * <p>SCIPIO: 2.1.0: Added.</p>
 */
public class ServiceContext extends MapWrapper.Abstract<String, Object> implements AttrMap, ServiceResultFactory, Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // TODO: Factory configuration

    private final ModelService service;
    private final DispatchContext dctx;
    private final Map<String, Object> context;

    protected ServiceContext(ModelService service, DispatchContext dctx, Map<String, ?> context) {
        if (service == null) {
            Debug.logWarning("Missing service model for ServiceContext", module);
        }
        this.service = service;
        this.dctx = dctx;
        if (context == null) {
            context = new HashMap<>();
        }
        this.context = UtilGenerics.cast(context);
    }

    protected ServiceContext(ServiceContext other, ModelService service, DispatchContext dctx, Map<String, ?> context) {
       this((service != null) ? service : other.service(),
               (dctx != null) ? dctx : other.dctx(),
               (context != null) ? UtilGenerics.cast(context) : other.context());
    }

    protected ServiceContext copy(ModelService service, DispatchContext dctx, Map<String, ?> context) {
        return new ServiceContext(this, service, dctx, context);
    }

    /**
     * Returns a copy of this ServiceContext with a copy of its original context.
     */
    public ServiceContext copy() {
        return copy(null, null, new HashMap<>(context()));
    }

    /**
     * Returns a new ServiceContext.
     */
    public static ServiceContext from(ModelService service, DispatchContext dctx, Map<String, ?> context) {
        // NOTE: ? should always be Object, this is for simplicity/compatibility, could be misused but never happens
        return new ServiceContext(service, dctx, context);
    }

    /**
     * Returns a new ServiceContext.
     */
    public static ServiceContext from(ModelService service, LocalDispatcher dispatcher, Map<String, ?> context) {
        return from(service, dispatcher.getDispatchContext(), context);
    }

    /**
     * Returns a new ServiceContext.
     */
    public static ServiceContext from(String serviceName, DispatchContext dctx, Map<String, ?> context) throws GenericServiceException {
        // NOTE: ? should always be Object, this is for simplicity/compatibility, could be misused but never happens
        return from(dctx.getModelService(serviceName), dctx, context);
    }

    /**
     * Returns a new ServiceContext.
     */
    public static ServiceContext from(String serviceName, LocalDispatcher dispatcher, Map<String, ?> context) throws GenericServiceException {
        return from(serviceName, dispatcher.getDispatchContext(), context);
    }

    /**
     * Returns a new ServiceContext, using the currently-running service to determine the model service.
     *
     * <p>WARN: not always accurate; prefer {@link #from(ModelService, DispatchContext, Map)} or
     * {@link #from(String, DispatchContext, Map)}.</p>
     */
    public static ServiceContext from(DispatchContext dctx, Map<String, ?> context) {
        // NOTE: ? should always be Object, this is for simplicity/compatibility, could be misused but never happens
        return from(dctx.getModelService(), dctx, context);
    }

    /**
     * Returns a new ServiceContext, using the currently-running service to determine the model service.
     *
     * <p>WARN: not always accurate; prefer {@link #from(ModelService, LocalDispatcher, Map)} or
     * {@link #from(String, LocalDispatcher, Map)}.</p>
     */
    public static ServiceContext from(LocalDispatcher dispatcher, Map<String, ?> context) {
        return from(dispatcher.getDispatchContext(), context);
    }

    /**
     * Return this ServiceContext with a substitute service model and context map, for delegation.
     */
    public ServiceContext from(ModelService service, Map<String, ?> context) {
        if (context instanceof ServiceContext) {
            ServiceContext ctx = (ServiceContext) context;
            return copy(ctx.service(), ctx.dctx(), ctx.context());
        } else {
            return copy(null, null, context);
        }
    }

    /**
     * Return this ServiceContext with a substitute service model and context map, for delegation.
     */
    public ServiceContext from(String service, Map<String, ?> context) {
        if (context instanceof ServiceContext) {
            ServiceContext ctx = (ServiceContext) context;
            return copy(ctx.service(), ctx.dctx(), ctx.context());
        } else {
            return copy(null, null, context);
        }
    }

    /**
     * Return this ServiceContext with a substitute context map, for delegation.
     */
    public ServiceContext from(Map<String, ?> context) {
        return from((ModelService) null, context);
    }

    /**
     * Returns the model of the service this context was originally prepared for, or null if could not be determined.
     *
     * <p>By default, when a ServiceContext is created, if the caller use a {@link #as} overload lacking the
     * {@link ModelService} parameter, this value is automatically populated at the time of construction using
     * {@link DispatchContext#getModelService()}, which is the currently-running service at the time. In most contexts
     * one should be able to say in advance whether this should be null or not.</p>
     */
    public ModelService service() {
        return service;
    }

    /**
     * Returns the name of the service this context was originally prepared for, or null if could not be determined.
     *
     * <p>By default, when a ServiceContext is created, if the caller use a {@link #as} overload lacking the
     * {@link ModelService} parameter, this value is automatically populated at the time of construction using
     * {@link DispatchContext#getModelService()}, which is the currently-running service at the time. In most contexts
     * one should be able to say in advance whether this should be null or not.</p>
     */
    public String serviceName() {
        return (service != null) ? service.getName() : null;
    }

    public DispatchContext dctx() {
        return dctx;
    }

    public final Map<String, Object> context() {
        return context;
    }

    @Override
    public final Map<String, Object> wrapped() {
        // NOTE: If ever un-final, uncomment this version, but likely never
        //return context();
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
     * Gets the ModelService instance that corresponds to given the name.
     *
     * @param serviceName Name of the service
     * @return GenericServiceModel that corresponds to the serviceName
     */
    public ModelService getModelService(String serviceName) throws GenericServiceException {
        return dctx().getModelService(serviceName);
    }

    /**
     * Gets the ModelService instance that corresponds to given the name, throwing IllegalArgumentException if not found.
     *
     * @param serviceName Name of the service
     * @return GenericServiceModel that corresponds to the serviceName
     */
    public ModelService getModelServiceAlways(String serviceName) throws IllegalArgumentException {
        return dctx().getModelServiceAlways(serviceName);
    }

    /**
     * Gets the ModelService instance that corresponds to given the name, returning null and no logging if not found.
     *
     * @param serviceName Name of the service
     * @return GenericServiceModel that corresponds to the serviceName
     */
    public ModelService getModelServiceOrNull(String serviceName) {
        return dctx().getModelServiceOrNull(serviceName);
    }

    /**
     * Returns the model of the currently invoked service, or null if no service executing.
     *
     * <p>NOTE: After {@link LocalDispatcher#runSync} returns from a call,</p>
     *
     * @return the current service model, or null if no service executing
     * @see #service()
     */
    public ModelService getModelService() {
        return service();
    }

    /**
     * Gets the ModelService instance that corresponds to given the name, returning null and no logging if not found.
     *
     * @param serviceName Name of the service
     * @return GenericServiceModel that corresponds to the serviceName
     */
    public boolean isService(String serviceName) {
        return dctx().isService(serviceName);
    }

    /**
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName.
     *
     * <p>NOTE: This goes not guarantee the context will be 100% valid - there may be missing fields.</p>
     *
     * <p>NOTE: The best way to call {@link #makeValidContext} methods is with an inlined
     * "IN"/"OUT"/"INOUT"/"IN-SYS"/"OUT-SYS"/"INOUT-SYS" mode parameter because they are fixed and linking {@link ModelService}
     * adds needless verbosity and imports.</p>
     *
     * <p>SCIPIO: 3.0.0: Added options overload.</p>
     *
     * @param model The ModelService object of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT), according to {@link ModelService#PARAM_MODES}
     * @param context The initial set of values to pull from
     * @param options The options
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public <M extends ServiceContext> M makeValidContext(ModelService model, String mode, Map<String, ?> context, MakeValidOptions options) throws GenericServiceException {
        if (model == null) {
            model = service();
        }
        // Instead of a HashMap, make a ServiceContext
        ServiceContext targetCtx;
        if (options != null && options.targetContext() != null) {
            if (options.targetContext() instanceof ServiceContext) {
                targetCtx = (ServiceContext) options.targetContext();
            } else {
                targetCtx = fromServiceMode(model, mode, options.targetContext());
            }
        } else {
            targetCtx = fromServiceMode(model, mode, new HashMap<>());
        }
        options = (options != null) ? options.copy() : new MakeValidOptions();
        options.targetContext(targetCtx.context());
        dctx().makeValidContext(model, mode, context, options);
        return UtilGenerics.cast(targetCtx);
    }

    protected ServiceContext fromServiceMode(ModelService model, String mode, Map<String, Object> context) {
        String io = ModelService.getParamModeIO(mode);
        if (ModelService.OUT_PARAM.equals(io)) {
            return result(context);
        } else {
            return from(model, context);
        }
    }

    /**
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName.
     *
     * <p>NOTE: This goes not guarantee the context will be 100% valid - there may be missing fields.</p>
     *
     * <p>NOTE: This overload was present only as a static method <code>DispatchContext#makeValidContext(ModelService, String, Map)</code>.</p>
     *
     * <p>NOTE: The best way to call {@link #makeValidContext} methods is with an inlined
     * "IN"/"OUT"/"INOUT"/"IN-SYS"/"OUT-SYS"/"INOUT-SYS" mode parameter because they are fixed and linking {@link ModelService}
     * adds needless verbosity and imports.</p>
     *
     * @param model The ModelService object of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT), according to {@link ModelService#PARAM_MODES}
     * @param context The initial set of values to pull from
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public <M extends ServiceContext> M makeValidContext(ModelService model, String mode, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(model, mode, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName.
     *
     * <p>NOTE: This goes not guarantee the context will be 100% valid - there may be missing fields.</p>
     *
     * <p>NOTE: The best way to call {@link #makeValidContext} methods is with an inlined
     * "IN"/"OUT"/"INOUT"/"IN-SYS"/"OUT-SYS"/"INOUT-SYS" mode parameter because they are fixed and linking {@link ModelService}
     * adds needless verbosity and imports.</p>
     *
     * <p>SCIPIO: 3.0.0: Added options overload.</p>
     *
     * @param serviceName The name of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT), according to {@link ModelService#PARAM_MODES}
     * @param context The initial set of values to pull from
     * @param options The options
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public <M extends ServiceContext> M makeValidContext(String serviceName, String mode, Map<String, ?> context, MakeValidOptions options) throws GenericServiceException {
        return makeValidContext(getModelService(serviceName), mode, context, options);
    }

    /**
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName.
     *
     * <p>NOTE: This goes not guarantee the context will be 100% valid - there may be missing fields.</p>
     *
     * <p>NOTE: The best way to call {@link #makeValidContext} methods is with an inlined
     * "IN"/"OUT"/"INOUT"/"IN-SYS"/"OUT-SYS"/"INOUT-SYS" mode parameter because they are fixed and linking {@link ModelService}
     * adds needless verbosity and imports.</p>
     *
     * @param serviceName The name of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT), according to {@link ModelService#PARAM_MODES}
     * @param context The initial set of values to pull from
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public <M extends ServiceContext> M makeValidContext(String serviceName, String mode, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(serviceName, mode, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName from the fields of this service context.
     *
     * <p>NOTE: This goes not guarantee the context will be 100% valid - there may be missing fields.</p>
     *
     * @param serviceName The name of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT), according to {@link ModelService#PARAM_MODES}
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public <M extends ServiceContext> M makeValidContext(String serviceName, String mode) throws GenericServiceException {
        return makeValidContext(serviceName, mode, context(), null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName from the fields of this service context.
     *
     * <p>NOTE: This goes not guarantee the context will be 100% valid - there may be missing fields.</p>
     *
     * @param model The ModelService object of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT), according to {@link ModelService#PARAM_MODES}
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public <M extends ServiceContext> M makeValidContext(ModelService model, String mode) throws GenericServiceException {
        return makeValidContext(model, mode, context(), null);
    }


    /**
     * Uses an existing map of name value pairs and extracts the IN keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInContext(String serviceName, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(serviceName, ModelService.IN_PARAM, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInContext(ModelService model, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(model, ModelService.IN_PARAM, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidOutContext(String serviceName, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(serviceName, ModelService.OUT_PARAM, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidOutContext(ModelService model, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(model, ModelService.OUT_PARAM, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN and OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInOutContext(String serviceName, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(serviceName, ModelService.IN_OUT_PARAM, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN and OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInOutContext(ModelService model, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(model, ModelService.IN_OUT_PARAM, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInContext(String serviceName) throws GenericServiceException {
        return makeValidContext(serviceName, ModelService.IN_PARAM, context(), null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInContext(ModelService model) throws GenericServiceException {
        return makeValidContext(model, ModelService.IN_PARAM, context(), null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidOutContext(String serviceName) throws GenericServiceException {
        return makeValidContext(serviceName, ModelService.OUT_PARAM, context(), null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidOutContext(ModelService model) throws GenericServiceException {
        return makeValidContext(model, ModelService.OUT_PARAM, context(), null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN and OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInOutContext(String serviceName) throws GenericServiceException {
        return makeValidContext(serviceName, ModelService.IN_OUT_PARAM, context(), null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN and OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInOutContext(ModelService model) throws GenericServiceException {
        return makeValidContext(model, ModelService.IN_OUT_PARAM, context(), null);
    }
}
