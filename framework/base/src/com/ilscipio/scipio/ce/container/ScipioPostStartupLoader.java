package com.ilscipio.scipio.ce.container;

import org.ofbiz.base.start.Config;
import org.ofbiz.base.start.StartupException;
import org.ofbiz.base.start.StartupLoader;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.ScriptUtil;

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
public class ScipioPostStartupLoader implements StartupLoader {

    private static final String module = ScipioPostStartupLoader.class.getName();

    @Override
    public void load(Config config, String[] args) throws StartupException {
    }

    @Override
    public void start() throws StartupException {
        initPostStartup();
        initScriptEngine();
        initPostStartupComplete();
    }

    /**
     * showPostStartupIntroMsg.
     * <p>
     * NOTE: For the official "FRAMEWORK IS LOADED" post-startup complete message,
     * see {@link org.ofbiz.base.start.Start#printStartupReadyMessage}.
     */
    protected void initPostStartup() {
        Debug.logInfo("Scipio: Running post-startup actions...", module);
    }

    /**
     * showPostStartupIntroMsg.
     * <p>
     * NOTE: For the official "FRAMEWORK IS LOADED" post-startup complete message,
     * see {@link org.ofbiz.base.start.Start#printStartupReadyMessage}.
     */
    protected void initPostStartupComplete() {
        Debug.logInfo("Scipio: Post-startup complete.", module);
    }

    /**
     * Pre-initialize the script engine to prevent various issues with,
     * and so the log is less cluttered/confusing afterward.
     */
    protected void initScriptEngine() throws StartupException {
        ScriptUtil.getScriptEngineManager();
    }

    @Override
    public void unload() throws StartupException {
    }

}
