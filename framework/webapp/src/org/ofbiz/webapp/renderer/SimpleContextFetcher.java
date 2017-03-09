package org.ofbiz.webapp.renderer;

import org.ofbiz.base.util.collections.MapStack;

/**
 * SCIPIO: Simple context fetcher that always returns the initial ones.
 * This is stock Ofbiz behavior (for ScreenRenderer).
 */
public class SimpleContextFetcher implements RenderContextFetcher {
    protected Appendable writer;
    protected MapStack<String> context;
    
    public SimpleContextFetcher(Appendable writer, MapStack<String> context) {
        this.writer = writer;
        this.context = context;
    }

    @Override
    public MapStack<String> getContext() {
        return context;
    }

    @Override
    public Appendable getWriter() {
        return writer;
    }
}