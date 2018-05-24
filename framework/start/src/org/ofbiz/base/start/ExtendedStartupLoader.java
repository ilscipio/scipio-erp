package org.ofbiz.base.start;

/**
 * SCIPIO: Extended startup loader with support for extra events
 * and callbacks.
 * <p>
 * Added 2018-05-23.
 */
public interface ExtendedStartupLoader extends StartupLoader {

    /**
     * Callback for post-startup events.
     * <p>
     * These execute once the server is in RUNNING state,
     * after the "FRAMEWORK IS LOADED" message has been printed.
     * <p>
     * In other words, it's functionally equivalent to adding
     * a startup service Job on eventId="SCH_EVENT_STARTUP",
     * but without need to modify data.
     *
     * @throws StartupException If an error was encountered.
     */
    public void execOnRunning() throws StartupException;

}
