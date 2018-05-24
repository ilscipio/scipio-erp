package com.ilscipio.scipio.ce.base.container;

import java.util.List;

import org.ofbiz.base.start.Config;
import org.ofbiz.base.start.ExtendedStartupLoader;
import org.ofbiz.base.start.StartupException;
import org.ofbiz.base.start.StartupLoader;
import org.ofbiz.base.util.Debug;

import com.ilscipio.scipio.ce.base.event.ScipioEventDefs;

/**
 * A startup loader for non-data executions meant to run after all other framework
 * startup loaders (notably ContainerLoader).
 * <p>
 * In the start component start.properties and derivative files, this should be
 * chained after {@link org.ofbiz.base.container.ContainerLoader} and any other core
 * framework loaders.
 * <p>
 * NOTE: This does not (currently) include the solr rebuildSolrIndexAuto startup job;
 * see the SOLR_REBUILD_INIT job in applications/solr/data/SolrScheduledServiceData.xml.
 * <p>
 * DEV NOTE: For the official "FRAMEWORK IS LOADED" post-startup complete message,
 * see {@link org.ofbiz.base.start.Start#printStartupReadyMessage}.
 * These are intentionally separated.
 * <p>
 * Added 2018-05-22.
 */
public class ScipioFinishStartupLoader implements ExtendedStartupLoader {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String EVENTS_PROPPREFIX = "scipio.start.finish.loader";

    protected ScipioEventDefs<StartupLoader> loaderDefs;

    @Override
    public void load(Config config, String[] args) throws StartupException {
        if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup load events begin...", module);
        for(ScipioEventDefs.EventDef<StartupLoader> loaderDefs : getLoaderDefs()) {
            if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup load invoking sub-loader: " + loaderDefs, module);
            loaderDefs.getLoader().load(config, args);
        }
        if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup load events complete.", module);
    }

    @Override
    public void start() throws StartupException {
        if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup end events begin...", module);
        for(ScipioEventDefs.EventDef<StartupLoader> loaderDefs : getLoaderDefs()) {
            if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup end invoking sub-loader: " + loaderDefs, module);
            loaderDefs.getLoader().start();
        }
        if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup end events complete.", module);
    }

    @Override
    public void unload() throws StartupException {
        if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup unload events begin...", module);
        for(ScipioEventDefs.EventDef<StartupLoader> loaderDefs : getLoaderDefs()) {
            if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup unload invoking sub-loader: " + loaderDefs, module);
            loaderDefs.getLoader().unload();
        }
        if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup unload events complete.", module);
    }

    @Override
    public void execOnRunning() throws StartupException {
        if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup exec-on-running events begin...", module);
        for(ScipioEventDefs.EventDef<StartupLoader> loaderDefs : getLoaderDefs()) {
            if (loaderDefs.getLoader() instanceof ExtendedStartupLoader) {
                if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup exec-on-running invoking sub-loader: " + loaderDefs, module);
                ((ExtendedStartupLoader) loaderDefs.getLoader()).execOnRunning();
            }
        }
        if (Debug.verboseOn()) Debug.logVerbose("Scipio: Startup exec-on-running events complete.", module);
    }

    /**
     * Gets the finish loader defs from the start subloaders config.
     * <p>
     * NOTE: we reload the file every time because it's fast on most systems
     * and better than keeping useless reference in memory after startup in finished.
     */
    protected List<ScipioEventDefs.EventDef<StartupLoader>> getLoaderDefs() {
        ScipioEventDefs<StartupLoader> loaderDefs = this.loaderDefs;
        if (loaderDefs == null) {
            synchronized(this) {
                loaderDefs = this.loaderDefs;
                if (loaderDefs == null) {
                    loaderDefs = ScipioEventDefs.readEventDefs(EVENTS_PROPPREFIX, StartupLoader.class);
                    this.loaderDefs = loaderDefs;
                }
            }
        }
        return loaderDefs.getLoaderDefs();
    }

}
