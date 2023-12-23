package org.ofbiz.service;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Service implementation interface and abstract classes.
 *
 * <p>NOTE: For most cases, use {@link LocalService} as base service implementation, which is a facade provided over this one.</p>
 *
 * <p>Implicitly supports the {@link com.ilscipio.scipio.service.def.Service} annotation, but they can be used
 * completely independently.</p>
 *
 * <p>The recommended way to write new services is by extending {@link LocalService} ({@link ServiceHandler.LocalExec}).</p>
 *
 * <p>Typically an implementation will provided either a default constructor or a constructor taking a
 * {@link ServiceContext}; the default constructor causes a delayed call to {@link Local#init(ServiceContext)} whereas
 * the service context constructor does not.</p>
 *
 * <p>SCIPIO: 3.0.0: Added {@link LocalService}, optional {@link ServiceHandler.Exec}, {@link ServiceHandler.LocalExec} for standardization.</p>
 * <p>SCIPIO: 2.1.0: Added and integrated into service engine. See for example.</p>
 *
 * @see LocalService
 */
public interface ServiceHandler {

    // No method is explicitly defined because 3 overloads and various exception signatures are supported:
    //   exec(), exec(ServiceContext), exec(DispatchContext, Map)
    // The following is the preferred method name and overload:
    //Map<String, Object> exec(ServiceContext ctx) throws GeneralException;

    /**
     * Standard parameter-less service execution method.
     *
     * <p>Implementations of this rely on class members initialized through {@link Local#Local(ServiceContext)}
     * or {@link Local#init(ServiceContext)} (dynamically determined by reflection at runtime).</p>
     */
    interface Exec extends ServiceHandler {
        Map<String, Object> exec() throws GeneralException;
    }

    /**
     * Standard single-parameter {@link ServiceContext} service execution method.
     *
     * <p>Useful when translating old code to new class format or to avoid class members for whatever reason.</p>
     */
    interface ExecStandard extends ServiceHandler {
        Map<String, Object> exec(ServiceContext ctx) throws GeneralException;
    }

    /**
     * Legacy two-parameter {@link DispatchContext} service execution method.
     *
     * <p>Useful when translating old code to new class format.</p>
     */
    interface ExecLegacy extends ServiceHandler {
        Map<String, Object> exec(DispatchContext dctx, Map<String, Object> ctx) throws GeneralException;
    }

    /**
     * A service handler created once and reused for many service calls.
     *
     * <p>The service engine automatically creates singleton instances of these handlers.</p>
     */
    abstract class Shared implements ServiceHandler {

        /**
         * Standard static handler constructor; usually called once globally (except cache clears).
         */
        public Shared() {}

        // No method is explicitly defined because 3 overloads and various exception signatures are supported:
        //   exec(), exec(ServiceContext), exec(DispatchContext, Map)
        // The following is the preferred method name and overload:
        //public abstract Map<String, Object> exec(ServiceContext ctx) throws GeneralException;
    }

    /**
     * A service handler created and invoked at every service call.
     *
     * <p>Designed to hold a {@link ServiceContext} exposed as {@link #ctx} to service implementations, with
     * support for legacy {@link #dctx} and {@link #context} variables for legacy code.</p>
     *
     * <p>Typically an implementation will provided either a default constructor or a constructor taking a
     * {@link ServiceContext}; the default constructor causes a delayed call to {@link Local#init(ServiceContext)} whereas
     * the service context constructor does not.</p>
     */
    abstract class Local implements ServiceHandler {
        /** The ServiceContext for the service call; always non-null after {@link #Local(ServiceContext)} or {@link #init(ServiceContext)}. */
        protected ServiceContext ctx;
        /** DispatchContext for the service call, for easy access from legacy code. */
        protected DispatchContext dctx; // legacy code support
        /** Map context for the service call, for easy access from legacy code. */
        protected Map<String, Object> context; // legacy code support
        /**
         * A local field used to hold the leaf class service implementation's log, for use in service code templating for implementations.
         *
         * <p>Callers can initialize using {@link #initServiceLogNew} at the start of {@link #init(ServiceContext)}.
         * If a service code implementation or template makes use of this anywhere, it is responsible for initializing
         * it with its own default.</p>
         */
        protected Debug.OfbizLogger srvModule;

        /**
         * Standard delayed-init dynamic handler constructor that relies on {@link #init(ServiceContext)} to initialize fields.
         *
         * <p>This is often the preferred form as {@link #init(ServiceContext)} allows subclasses to completely
         * override parameter-setting behavior of extended classes if needed, and when no extra parameters are needed
         * both the constructor and init methods can be omitted.</p>
         *
         * <p>If this is the only constructor supplied by the subclass and no alternative constructor is supplied,
         * the {@link #init(ServiceContext)} will be called.</p>
         *
         * <p>This form allows implementation to avoid defining constructors if not necessary and thread safety is not
         * needed.</p>
         */
        public Local() {
        }

        /**
         * Standard dynamic handler constructor; sets {@link #ctx} and legacy {@link #dctx} and {@link #context}.
         *
         * <p>If this is the only constructor supplied by the subclass and no alternative accessor is supplied,
         * it will be invoked on every service call and {@link #init(ServiceContext)} will not be called.</p>
         */
        public Local(ServiceContext ctx) throws GeneralException {
            setServiceContext(ctx);
            initAttributes(ctx);
        }

        /**
         * Called by service engine invocations to set members from service parameters, for subclasses that rely on {@link #Local()}.
         *
         * <p>This is often the preferred form as {@link #init(ServiceContext)} allows subclasses to completely
         * override parameter-setting behavior of extended classes if needed.</p>
         *
         * <p>NOTE: super.init must be called to initialize the service context members.</p>
         */
        public void init(ServiceContext ctx) throws GeneralException {
            setServiceContext(ctx);
            initAttributes(ctx);
        }

        /**
         * Sets the value of {@link #srvModule} if not already set, for use in parametrizing {@link Debug} log operations.
         *
         * <p>This version can be called inside {@link #init(ServiceContext)} before any <code>super.init()</code> invocation to ensure
         * it is set even from within that method.</p>
         *
         * <p>Typically, this should be called from the {@link #init(ServiceContext)} method.</p>
         */
        protected ServiceContext initServiceLogNew(ServiceContext ctx, Debug.OfbizLogger localModule) {
            if (this.srvModule == null) {
                setServiceLog(localModule);
            }
            return ctx;
        }

        /**
         * Overrides the value of {@link #srvModule}, for use in parametrizing {@link Debug} log operations.
         *
         * <p>Typically, this should be called from the {@link #init(ServiceContext)} method, but it is often better to
         * call {@link #initServiceLogNew} before <code>super.init</code> instead, so that it is available from the super
         * init method itself.</p>
         */
        protected ServiceContext initServiceLog(ServiceContext ctx, Debug.OfbizLogger localModule) {
            setServiceLog(localModule);
            return ctx;
        }

        /**
         * Sets local module variable - avoid from main client/service code because this field was originally meant
         * to be on {@link ServiceContext}.
         *
         * <p>WARN: This should be avoided by calling code in favor of {@link #initServiceLogNew} or (if need be)
         * {@link #initServiceLog}.</p>
         */
        protected void setServiceLog(Debug.OfbizLogger srvModule) {
            this.srvModule = srvModule;
        }

        protected void setServiceContext(ServiceContext ctx) {
            this.ctx = ctx;
            this.dctx = ctx.dctx();
            this.context = ctx.context();
            // NOTE: There is no need to set srvModule here because currently it is set directly by initLocalModule[New]()
            //  by callers and service code templates/implementations that support it (otherwise left null, intentionally)
        }

        /**
         * Replaces {@link #ctx} and {@link #context} as applicable using given context.
         */
        protected void setContext(Map<String, ?> context) {
            if (this.ctx != null) {
                setServiceContext(ctx.from(context));
            } else {
                this.context = UtilGenerics.cast(context);
            }
        }

        /**
         * Replaces the context in {@link #ctx} and {@link #context} as applicable with a copy.
         * <p>Can be used instead of UtilMisc.makeMapWritable.</p>
         */
        protected void setContextCopy() {
            if (this.ctx != null) {
                setServiceContext(ctx.copy());
            } else {
                this.context = new HashMap<>(context);
            }
        }

        protected void initAttributes(ServiceContext ctx) {
            injectAttributes(ctx);
        }

        protected void injectAttributes(ServiceContext ctx) {
            ServiceHandler.injectAttributes(this, ctx.getModelService(), ctx);
        }

        // No method is explicitly defined because 3 overloads and various exception signatures are supported:
        //   exec(), exec(ServiceContext), exec(DispatchContext, Map)
        // The following is the preferred method name and overload:
        //public abstract Map<String, Object> exec() throws GeneralException;

        //public final Map<String, Object> exec(ServiceContext ctx) throws GeneralException {
        //    return exec();
        //}
    }

    static void injectAttributes(ServiceHandler serviceHandler, ModelService modelService, ServiceContext ctx) {
        if (modelService.getServiceClass() == null) {
            return;
        }
        List<ModelParam.ModelParamAndField> paramInjectFieldList = modelService.getParamInjectFieldList();
        if (paramInjectFieldList == null || paramInjectFieldList.isEmpty()) {
            return;
        }
        for (ModelParam.ModelParamAndField paramAndField : paramInjectFieldList) {
            String paramName = paramAndField.getParam().getName();
            Object paramValue = ctx.context().get(paramName);
            try {
                // TODO: IMPROVE: setAccessible clearly not ideal
                paramAndField.getField().setAccessible(true);
                paramAndField.getField().set(serviceHandler, paramValue);
            } catch (IllegalAccessException e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * A service handler created and invoked at every service call that implements {@link Exec}.
     *
     * <p>Designed to hold a {@link ServiceContext} exposed as {@link #ctx} to service implementations, with
     * support for legacy {@link #dctx} and {@link #context} variables for legacy code.</p>
     *
     * <p>Typically an implementation will provided either a default constructor or a constructor taking a
     * {@link ServiceContext}; the default constructor causes a delayed call to {@link Local#init(ServiceContext)} whereas
     * the service context constructor does not.</p>
     */
    abstract class LocalExec extends Local implements Exec {

        /**
         * Standard delayed-init dynamic handler constructor that relies on {@link #init(ServiceContext)} to initialize fields.
         *
         * <p>This is often the preferred form as {@link #init(ServiceContext)} allows subclasses to completely
         * override parameter-setting behavior of extended classes if needed, and when no extra parameters are needed
         * both the constructor and init methods can be omitted, and members can still be initialized from parameters from
         * {@link #exec()} for traditional code migration.</p>
         *
         * <p>If this is the only constructor supplied by the subclass and no alternative constructor is supplied,
         * the {@link #init(ServiceContext)} will be called.</p>
         *
         * <p>This form allows implementation to avoid defining constructors if not necessary and thread safety is not
         * needed.</p>
         */
        public LocalExec() {
        }

        /**
         * Standard dynamic handler constructor; sets {@link #ctx} and legacy {@link #dctx} and {@link #context}.
         *
         * <p>If this is the only constructor supplied by the subclass and no alternative accessor is supplied,
         * it will be invoked on every service call and {@link #init(ServiceContext)} will not be called.</p>
         */
        public LocalExec(ServiceContext ctx) throws GeneralException {
            super(ctx);
        }

    }

    interface Accessor {
        ServiceHandler getServiceHandler(ServiceContext ctx) throws GeneralException;

        class SingleAccessor implements Accessor {
            protected final ServiceHandler serviceHandler;

            public SingleAccessor(ServiceHandler serviceHandler) {
                this.serviceHandler = serviceHandler;
            }

            @Override
            public ServiceHandler getServiceHandler(ServiceContext ctx) throws GeneralException {
                return serviceHandler;
            }
        }
    }
}
