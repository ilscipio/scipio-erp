package org.ofbiz.service;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;

import java.util.Map;

/**
 * Standard OOP service implementation facade class for new-style Scipio services, initialized at every service invocation.
 *
 * <p>In this style of service, one simply uses protected (non-final) class members such as
 * {@link LocalService#ctx} directly from the executing body method {@link LocalService#exec()}. They are
 * initialized by {@link #init(ServiceContext)} at every service invocation. This provides an efficient script-like
 * service writing style into which older code can easily be migrated (copy-pasted) with little to no overhead
 * compared to script languages.</p>
 *
 * <p>This promotes the use of default constructor with use of {@link #init(ServiceContext)} for initializing
 * members from service parameters. This way, further-extending implementations can replace member initialization
 * from parameters more easily than they can using a constructor, and when no extra parameters are needed both the
 * constructor and init methods can be omitted, and parameters can also be initialized from {@link #exec()} for
 * migrating and reusing traditional code.</p>
 *
 * <p>SCIPIO: 3.0.0: Added facade class with optimal usage pattern.</p>
 */
public abstract class LocalService extends ServiceHandler.LocalExec {

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
    public LocalService() {
    }

    /**
     * Called by service engine invocations to set members from service parameters, for subclasses that rely on {@link #Local()}.
     *
     * <p>This is often the preferred form as {@link #init(ServiceContext)} allows subclasses to completely
     * override parameter-setting behavior of extended classes if needed.</p>
     *
     * <p>NOTE: super.init must be called to initialize the service context members.</p>
     */
    @Override
    public void init(ServiceContext ctx) throws GeneralException {
        super.init(ctx);
    }

    /**
     * Standard service execution method.
     *
     * <p>To access context, simply refer to the member {@link #ctx} directly (in script-like fashion).
     * Also available are {@link #dctx} and {@link #context} which are superseded by {@link #ctx} but may be desirable
     * to more easily migrate older code.</p>
     *
     * <p>May return a {@link ServiceResult} or a standard Map containing a service result (often via {@link ServiceUtil}).</p>
     */
    @Override
    public abstract Map<String, Object> exec() throws GeneralException;

    /**
     * Sets the value of {@link #srvModule} if not already set, for use in parametrizing {@link Debug} log operations.
     *
     * <p>This version can be called inside {@link #init(ServiceContext)} before any <code>super.init()</code> invocation to ensure
     * it is set even from within that method.</p>
     *
     * <p>Typically, this should be called from the {@link #init(ServiceContext)} method.</p>
     */
    @Override
    protected ServiceContext initServiceLogNew(ServiceContext ctx, Debug.OfbizLogger localModule) {
        return super.initServiceLogNew(ctx, localModule);
    }

    /**
     * Overrides the value of {@link #srvModule}, for use in parametrizing {@link Debug} log operations.
     *
     * <p>Typically, this should be called from the {@link #init(ServiceContext)} method, but it is often better to
     * call {@link #initServiceLogNew} before <code>super.init</code> instead, so that it is available from the super
     * init method itself.</p>
     */
    @Override
    protected ServiceContext initServiceLog(ServiceContext ctx, Debug.OfbizLogger localModule) {
        return super.initServiceLog(ctx, localModule);
    }

}
