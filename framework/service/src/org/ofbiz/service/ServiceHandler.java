package org.ofbiz.service;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;

import java.util.HashMap;
import java.util.Map;

/**
 * Service implementation interface.
 * <p>Typically an implementation will provided either a default constructor or a constructor taking a
 * {@link ServiceContext}; the default constructor causes a delayed call to {@link Local#init(ServiceContext)} whereas
 * the service context constructor does not.</p>
 * <p>SCIPIO: 2.1.0: Added and integrated into service engine. See for example.</p>
 */
public interface ServiceHandler {

    // No method is explicitly defined because 3 overloads and various exception signatures are supported:
    //   exec(), exec(ServiceContext), exec(DispatchContext, Map)
    // The following is the preferred method name and overload:
    //Map<String, Object> exec(ServiceContext ctx) throws GeneralException;

    /**
     * A service handler created once and reused for many service calls.
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
     * <p>Designed to hold a {@link ServiceContext} exposed as {@link #ctx} to service implementations, with
     * support for legacy {@link #dctx} and {@link #context} variables for legacy code.</p>
     * <p>Typically an implementation will provided either a default constructor or a constructor taking a
     * {@link ServiceContext}; the default constructor causes a delayed call to {@link Dynamic#init(ServiceContext)} whereas
     * the service context constructor does not.</p>
     */
    abstract class Local implements ServiceHandler {
        /** The ServiceContext for the service call; always non-null after {@link #Dynamic(ServiceContext)} or {@link #init(ServiceContext)}. */
        protected ServiceContext ctx;
        /** DispatchContext for the service call, for easy access from legacy code. */
        protected DispatchContext dctx; // legacy code support
        /** Map context for the service call, for easy access from legacy code. */
        protected Map<String, Object> context; // legacy code support

        /**
         * Standard dynamic handler constructor; sets {@link #ctx} and legacy {@link #dctx} and {@link #context}.
         * <p>If this is the only constructor supplied by the subclass and no alternative accessor is supplied,
         * it will be invoked on every service call and {@link #init(ServiceContext)} will not be called.</p>
         */
        public Local(ServiceContext ctx) {
            this.ctx = ctx;
            this.dctx = ctx.dctx();
            this.context = ctx.context();
        }

        /**
         * Delayed-init dynamic handler constructor; leaves all fields null.
         * <p>If this is the only constructor supplied by the subclass and no alternative accessor is supplied,
         * the {@link #init(ServiceContext)} will be called.</p>
         * <p>This form allows implementation to avoid defining constructors if not necessary and thread safety is not
         * needed.</p>
         */
        public Local() {
        }

        /**
         * Called by service engine invocations for subclasses that rely on {@link #Dynamic()}.
         */
        public Local init(ServiceContext ctx) {
            setServiceContext(ctx);
            return this;
        }

        protected Local setServiceContext(ServiceContext ctx) {
            this.ctx = ctx;
            this.dctx = ctx.dctx();
            this.context = ctx.context();
            return this;
        }

        /**
         * Replaces {@link #ctx} and {@link #context} as applicable using given context.
         */
        protected Local setContext(Map<String, ?> context) {
            if (this.ctx != null) {
                setServiceContext(ctx.from(context));
            } else {
                this.context = UtilGenerics.cast(context);
            }
            return this;
        }

        /**
         * Replaces the context in {@link #ctx} and {@link #context} as applicable with a copy.
         * <p>Can be used instead of UtilMisc.makeMapWritable.</p>
         */
        protected Local setContextCopy() {
            if (this.ctx != null) {
                setServiceContext(ctx.copy());
            } else {
                this.context = new HashMap<>(context);
            }
            return this;
        }

        // No method is explicitly defined because 3 overloads and various exception signatures are supported:
        //   exec(), exec(ServiceContext), exec(DispatchContext, Map)
        // The following is the preferred method name and overload:
        //public abstract Map<String, Object> exec() throws GeneralException;

        //public final Map<String, Object> exec(ServiceContext ctx) throws GeneralException {
        //    return exec();
        //}
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
