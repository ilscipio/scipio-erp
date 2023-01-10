package com.ilscipio.scipio.ce.webapp.renderer;

/**
 * Renders controller view definitions with support and emphasis for renderer from static context
 * such as services, and supports a service interface.
 * <p>For screen widgets, this functions like a wrapper around ScreenRenderer to help support request emulation when needed
 * while reusing controller view definitions rather than binding the code to widget technology, similar to sendMailFromScreen.</p>
 * <p>SCIPIO: 2.1.0: Added for Hotwire/Turbo support (</p>
 */
public class ViewRenderer {

    private static final ViewRenderer DEFAULT = new ViewRenderer();

    public static ViewRenderer getDefault() {
        return DEFAULT;
    }




}
