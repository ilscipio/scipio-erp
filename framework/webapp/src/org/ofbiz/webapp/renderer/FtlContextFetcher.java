package org.ofbiz.webapp.renderer;

import java.io.Writer;

import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;

/**
 * SCIPIO: Advanced context fetcher that fetches the Writer out of the Freemarker
 * environment if any is currently active; otherwise falls back on initial.
 */
public class FtlContextFetcher extends SimpleContextFetcher {
    public FtlContextFetcher(Appendable writer, MapStack<String> context) {
        super(writer, context);
    }

    @Override
    public Appendable getWriter() {
        Environment env = FreeMarkerWorker.getCurrentEnvironment();
        if (env != null) {
            Writer out = env.getOut();
            if (out != null) {
                return out;
            }
        }
        return writer;
    }
}