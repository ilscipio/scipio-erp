package org.ofbiz.webapp.renderer;

import java.io.Writer;

import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;
import freemarker.ext.util.WrapperTemplateModel;
import freemarker.template.TemplateModel;

/**
 * SCIPIO: Context fetchers that can get writers and contexts out of the Freemarker Environment.
 */
public abstract class FtlContextFetcher extends SimpleContextFetcher {
    protected FtlContextFetcher(Appendable writer, MapStack<String> context) {
        super(writer, context);
    }

    /**
     * SCIPIO: Advanced context fetcher that fetches the Writer out of the Freemarker
     * environment if any is currently active; otherwise falls back on initial.
     */
    public static class FtlWriterOnlyFetcher extends FtlContextFetcher {
        public FtlWriterOnlyFetcher(Appendable writer, MapStack<String> context) {
            super(writer, context);
        }
        
        public FtlWriterOnlyFetcher() {
            super(null, null);
        }
        
        @Override
        public Appendable getWriter() {
            Appendable writer = getWriterFromEnvironment(FreeMarkerWorker.getCurrentEnvironment());
            if (writer != null) return writer;
            return this.writer;
        }
    }

    /**
     * SCIPIO: Advanced context fetcher that fetches the Writer out of the Freemarker
     * environment if any is currently active; otherwise falls back on initial.
     * It does the same for the MapStack context.
     */
    public static class FtlFullFetcher extends FtlWriterOnlyFetcher {
        public FtlFullFetcher(Appendable writer, MapStack<String> context) {
            super(writer, context);
        }
        
        public FtlFullFetcher() {
            super(null, null);
        }
        
        @Override
        public MapStack<String> getContext() {
            MapStack<String> context = getContextFromEnvironment(FreeMarkerWorker.getCurrentEnvironment());
            if (context != null) return context;
            return this.context;
        }
    }

    /**
     * FTL context fetcher that only gets writer from Environment if it was null.
     */
    public static class FallbackFtlWriterOnlyFetcher extends FtlContextFetcher {
        public FallbackFtlWriterOnlyFetcher(Appendable writer, MapStack<String> context) {
            super(writer, context);
        }
        
        public FallbackFtlWriterOnlyFetcher() {
            super(null, null);
        }
        
        @Override
        public Appendable getWriter() {
            if (writer != null) return writer;
            return getWriterFromEnvironment(FreeMarkerWorker.getCurrentEnvironment());
        }
    }
    
    /**
     * FTL context fetcher that only gets writer from Environment if it was null,
     * and also fetches the context from Environment if it was null.
     */
    public static class FallbackFtlFullFetcher extends FallbackFtlWriterOnlyFetcher {
        public FallbackFtlFullFetcher(Appendable writer, MapStack<String> context) {
            super(writer, context);
        }
        
        public FallbackFtlFullFetcher() {
            super(null, null);
        }

        @Override
        public MapStack<String> getContext() {
            if (context != null) return context;
            return getContextFromEnvironment(FreeMarkerWorker.getCurrentEnvironment());
        }
    }

    public static Appendable getWriterFromEnvironment(Environment env) {
        if (env != null) {
            Writer out = env.getOut();
            if (out != null) {
                return out;
            }
        }
        return null;
    }
    
    @SuppressWarnings("unchecked")
    public static MapStack<String> getContextFromEnvironment(Environment env) {
        if (env != null) {
            try {
                TemplateModel model = env.getGlobalVariable("context");
                if (model != null && model instanceof WrapperTemplateModel) {
                    Object obj = ((WrapperTemplateModel) model).getWrappedObject();
                    if (obj instanceof MapStack) {
                        return (MapStack<String>) obj;
                    }
                }
            } catch (Exception e) {
                ;
            }
        }
        return null;
    }
}