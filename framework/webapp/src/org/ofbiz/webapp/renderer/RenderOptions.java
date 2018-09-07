package org.ofbiz.webapp.renderer;

import java.io.Serializable;

/**
 * SCIPIO: Base render options class, stored in request attributes or context
 * and optionally in other objects.
 * <p>
 * NOT thread-safe.
 * <p>
 * NOTE: At this time, request and contexts are NOT required to contain this,
 * so callers must check for null or use specialized fetcher methods in the
 * subclasses. This could improve in the future.
 * <p>
 * See {@link org.ofbiz.widget.renderer.WidgetRenderOptions} for the real
 * implementation.
 */
@SuppressWarnings("serial")
public abstract class RenderOptions implements Serializable {

    public static final String FIELD_NAME = "scpRenderOpts";
    
    /**
     * Default constructor.
     */
    protected RenderOptions() {
    }

    /**
     * Copy constructor.
     */
    protected RenderOptions(RenderOptions other) {
    }

    public abstract RenderOptions copy();

    public abstract RenderOptions getReadOnly();
}
