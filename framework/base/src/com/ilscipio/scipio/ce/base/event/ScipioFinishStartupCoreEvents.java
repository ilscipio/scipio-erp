package com.ilscipio.scipio.ce.base.event;

import org.ofbiz.base.start.Config;
import org.ofbiz.base.start.ExtendedStartupLoader;
import org.ofbiz.base.start.StartupException;
import org.ofbiz.base.util.ScriptUtil;

public class ScipioFinishStartupCoreEvents implements ExtendedStartupLoader {

    @Override
    public void load(Config config, String[] args) throws StartupException {
    }

    @Override
    public void start() throws StartupException {
        initScriptEngine();
    }

    @Override
    public void unload() throws StartupException {
    }

    @Override
    public void execOnRunning() throws StartupException {
    }

    /**
     * Pre-initialize the script engine to prevent various issues with,
     * and so the log is less cluttered/confusing afterward.
     */
    protected void initScriptEngine() throws StartupException {
        ScriptUtil.getScriptEngineManager();
    }
}
