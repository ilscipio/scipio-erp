package org.ofbiz.service;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;

import java.util.HashMap;
import java.util.Map;

/**
 * Service implementation interface and abstract classes.
 *
 * <p>Implicitly supports the {@link com.ilscipio.scipio.service.def.Service} annotation, but they can be used
 * completely independently.</p>
 *
 * <p>The recommended way to write new services is by extending {@link ServiceHandler.LocalExec}.</p>
 *
 * <p>Typically an implementation will provided either a default constructor or a constructor taking a
 * {@link ServiceContext}; the default constructor causes a delayed call to {@link Local#init(ServiceContext)} whereas
 * the service context constructor does not.</p>
 *
 * <p>SCIPIO: 3.0.0: Added optional {@link ServiceHandler.Exec}, {@link ServiceHandler.LocalExec} for standardization.</p>
 * <p>SCIPIO: 2.1.0: Added and integrated into service engine. See for example.</p>
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
            this.ctx = ctx;
            this.dctx = ctx.dctx();
            this.context = ctx.context();
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
        }

        protected void setServiceContext(ServiceContext ctx) {
            this.ctx = ctx;
            this.dctx = ctx.dctx();
            this.context = ctx.context();
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

        // No method is explicitly defined because 3 overloads and various exception signatures are supported:
        //   exec(), exec(ServiceContext), exec(DispatchContext, Map)
        // The following is the preferred method name and overload:
        //public abstract Map<String, Object> exec() throws GeneralException;

        //public final Map<String, Object> exec(ServiceContext ctx) throws GeneralException {
        //    return exec();
        //}
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
