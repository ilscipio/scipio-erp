package com.ilscipio.scipio.ce.base.event;

import org.ofbiz.base.start.Config;
import org.ofbiz.base.start.ExtendedStartupLoader;
import org.ofbiz.base.start.StartupException;
import org.ofbiz.base.util.Debug;

public class TestStartupEvents implements ExtendedStartupLoader {

    @Override
    public void load(Config config, String[] args) throws StartupException {
        Debug.logInfo("Scipio: TestStartupEvents: load", TestStartupEvents.class.getName());
    }

    @Override
    public void start() throws StartupException {
        Debug.logInfo("Scipio: TestStartupEvents: start", TestStartupEvents.class.getName());
    }

    @Override
    public void unload() throws StartupException {
        Debug.logInfo("Scipio: TestStartupEvents: unload", TestStartupEvents.class.getName());
    }

    @Override
    public void execOnRunning() throws StartupException {
        Debug.logInfo("Scipio: TestStartupEventsS: execOnRunning", TestStartupEvents.class.getName());
    }

}
