package com.ilscipio.scipio.ce.webapp.renderer;

import java.io.Serializable;
import java.util.Map;

/**
 * Stores essential rendering information for a (future) ViewRenderer call.
 * <p>Needed to store webapp and store information </p>
 */
public class ViewContext implements Serializable {

    private final Map<String, Object> context;

    public ViewContext(Map<String, Object> context) {
        this.context = context;
    }

    public static ViewContext from(Map<String, Object> context) {
        return new ViewContext(context);
    }

    public Map<String, Object> getContext() {
        return context;
    }


}
