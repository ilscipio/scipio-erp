package org.ofbiz.webapp.renderer;

import java.io.Writer;

import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;
import freemarker.ext.util.WrapperTemplateModel;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

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
    
    /**
     * FTL context fetcher that only gets writer from Environment if it was null.
     */
    public static class FallbackFtlContextFetcher extends SimpleContextFetcher {

        public FallbackFtlContextFetcher(Appendable writer, MapStack<String> context) {
            super(writer, context);
        }
        
        @Override
        public Appendable getWriter() {
            if (writer != null) return writer;
            Environment env = FreeMarkerWorker.getCurrentEnvironment();
            if (env != null) {
                Writer out = env.getOut();
                if (out != null) {
                    return out;
                }
            }
            return null;
        }
    }
    
    /**
     * FTL context fetcher that only gets writer from Environment if it was null,
     * and also fetches the context from Environment if it was null.
     */
    public static class FullFallbackFtlContextFetcher extends FallbackFtlContextFetcher {

        public FullFallbackFtlContextFetcher(Appendable writer, MapStack<String> context) {
            super(writer, context);
        }

        @SuppressWarnings("unchecked")
        @Override
        public MapStack<String> getContext() {
            if (context != null) return context;
            Environment env = FreeMarkerWorker.getCurrentEnvironment();
            if (env != null) {
                try {
                    TemplateModel model = env.getGlobalVariable("context");
                    if (model != null && model instanceof WrapperTemplateModel) {
                        Object obj = ((WrapperTemplateModel) model).getWrappedObject();
                        if (obj instanceof MapStack) {
                            return (MapStack<String>) obj;
                        }
                    }
                } catch (TemplateModelException e) {
                    ;
                }
            }
            return null;
        }
    }

}