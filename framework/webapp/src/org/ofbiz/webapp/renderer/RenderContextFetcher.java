package org.ofbiz.webapp.renderer;

import org.ofbiz.base.util.collections.MapStack;

/**
 * SCIPIO: abstracts out render context and writer fetching logic.
 */
public interface RenderContextFetcher {

    MapStack<String> getContext();

    Appendable getWriter();

}