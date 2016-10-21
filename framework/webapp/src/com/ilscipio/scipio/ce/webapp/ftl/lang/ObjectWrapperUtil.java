package com.ilscipio.scipio.ce.webapp.ftl.lang;

import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.webapp.ftl.ExtendedWrapper;

import freemarker.core.Environment;
import freemarker.template.ObjectWrapper;
import freemarker.template.TemplateModelException;

public abstract class ObjectWrapperUtil {

    private static CurrentObjectWrapperFetcher currentObjectWrapperFetcher = new CurrentObjectWrapperFetcher();
    private static RawCurrentObjectWrapperFetcher rawCurrentObjectWrapperFetcher = new RawCurrentObjectWrapperFetcher();
    private static ExtendedObjectWrapperFetcher extendedObjectWrapperFetcher = new ExtendedObjectWrapperFetcher(false);
    private static ExtendedObjectWrapperFetcher extendedSimpleMapObjectWrapperFetcher = new ExtendedObjectWrapperFetcher(true);

    private static final Map<String, ObjectWrapperFetcher> objectWrapperFetcherMap;
    static {
        Map<String, ObjectWrapperFetcher> map = new HashMap<>();

        // general
        map.put("current", currentObjectWrapperFetcher); // special resolution needed
        map.put("raw", rawCurrentObjectWrapperFetcher);

        // special, but still abstracted (all derived from the "current")
        // TODO: "*simple*" are not fully implemented
        map.put("simple-adapter", new SimpleMapObjectWrapperFetcher(false, false));
        map.put("simple-copy", new SimpleMapObjectWrapperFetcher(false, true));
        map.put("simple", new SimpleMapObjectWrapperFetcher(true, null));

        ObjectWrapperFetcher simpleTypeWrapperFetcher = new TrivialObjectWrapperFetcher(FreeMarkerWorker.getDefaultSimpleTypeWrapper());
        ObjectWrapperFetcher simpleTypeCopyingWrapperFetcher = new TrivialObjectWrapperFetcher(FreeMarkerWorker.getDefaultSimpleTypeCopyingWrapper());

        // TODO: these are not quite correctly implemented (the commented is the tentative implement), 
        // but we CAN use the default wrapper temporarily
        // to work these because it covers all known rendering cases just fine
        //map.put("raw-simple-adapter", new SimpleMapObjectWrapperFetcher(rawCurrentObjectWrapperFetcher, false, false));
        //map.put("raw-simple-copy", new SimpleMapObjectWrapperFetcher(rawCurrentObjectWrapperFetcher, false, true));
        //map.put("raw-simple", new SimpleMapObjectWrapperFetcher(rawCurrentObjectWrapperFetcher, true, null));
        map.put("raw-simple-adapter", simpleTypeWrapperFetcher);
        map.put("raw-simple-copy", simpleTypeCopyingWrapperFetcher);
        map.put("raw-simple", simpleTypeWrapperFetcher);
        
        // specific, advanced, non-abstracted
        map.put("complex-default", new TrivialObjectWrapperFetcher(FreeMarkerWorker.getDefaultOfbizWrapper()));
        map.put("complex-default-simplemap", new TrivialObjectWrapperFetcher(FreeMarkerWorker.getDefaultOfbizSimpleMapWrapper()));
        map.put("complex-extended", extendedObjectWrapperFetcher);
        map.put("complex-extended-simplemap", extendedSimpleMapObjectWrapperFetcher);

        map.put("basic-adapter", simpleTypeWrapperFetcher);
        map.put("basic-copy", simpleTypeCopyingWrapperFetcher);
        map.put("basic", simpleTypeWrapperFetcher);
        
        objectWrapperFetcherMap = map;
    }
    
    private static final ExtendedWrapper defaultHtmlExtendedWrapper = new ExtendedWrapper(FreeMarkerWorker.version, "html");
    private static final ExtendedWrapper defaultHtmlExtendedSimpleMapWrapper;
    static {
        ExtendedWrapper wrapper = new ExtendedWrapper(FreeMarkerWorker.version, "html");
        wrapper.setSimpleMapWrapper(true);
        defaultHtmlExtendedSimpleMapWrapper = wrapper;
    }

    public static interface ObjectWrapperFetcher {
        ObjectWrapper getWrapper(Environment env);
    }
    
    public static class TrivialObjectWrapperFetcher implements ObjectWrapperFetcher {
        protected final ObjectWrapper objectWrapper;

        public TrivialObjectWrapperFetcher(ObjectWrapper objectWrapper) {
            this.objectWrapper = objectWrapper;
        }
        
        @Override
        public ObjectWrapper getWrapper(Environment env) {
            return objectWrapper;
        }
    }
    
    public static class CurrentObjectWrapperFetcher implements ObjectWrapperFetcher {
        @Override
        public ObjectWrapper getWrapper(Environment env) {
            return env.getObjectWrapper();
        }
    }
    
    public static class RawCurrentObjectWrapperFetcher implements ObjectWrapperFetcher {
        @Override
        public ObjectWrapper getWrapper(Environment env) {
            ObjectWrapper curr = env.getObjectWrapper();
            if (curr instanceof ExtendedWrapper) {
                if (((ExtendedWrapper) curr).isSimpleMapWrapper()) {
                    return FreeMarkerWorker.getDefaultOfbizSimpleMapWrapper();
                } else {
                    return FreeMarkerWorker.getDefaultOfbizWrapper();
                }
            } else {
                return curr;
            }
        }
    }
    
    public static class ExtendedObjectWrapperFetcher implements ObjectWrapperFetcher {
        
        private final boolean simpleMap;
        
        protected ExtendedObjectWrapperFetcher(boolean simpleMap) {
            super();
            this.simpleMap = simpleMap;
        }

        @Override
        public ObjectWrapper getWrapper(Environment env) {
            ObjectWrapper wrapper = env.getObjectWrapper();
            if (wrapper instanceof ExtendedWrapper && ((ExtendedWrapper) wrapper).isSimpleMapWrapper() == simpleMap) {
                return wrapper;
            } else {
                // semi-reliable way to get the language
                Object simpleEncoder = null;
                try {
                    simpleEncoder = LangFtlUtil.unwrapOrNull(env.getGlobalVariable("simpleEncoder"));
                } catch (TemplateModelException e) {
                    ;
                }
                if (simpleEncoder != null) {
                    if (simpleEncoder instanceof UtilCodec.HtmlEncoder) { // heuristic
                        return simpleMap ? defaultHtmlExtendedWrapper : defaultHtmlExtendedSimpleMapWrapper;
                    } else {
                        return simpleMap ? FreeMarkerWorker.getDefaultOfbizWrapper() : FreeMarkerWorker.getDefaultOfbizSimpleMapWrapper();
                    }
                } else {
                    // FIXME: if we don't know the lang, we have to play it safe and use html
                    return simpleMap ? defaultHtmlExtendedWrapper : defaultHtmlExtendedSimpleMapWrapper;
                }
            }
        }
    }
    
    public static class SimpleMapObjectWrapperFetcher implements ObjectWrapperFetcher {
        
        protected final ObjectWrapperFetcher fetcher;
        protected final boolean preserving;
        protected final Boolean copying;

        protected SimpleMapObjectWrapperFetcher(ObjectWrapperFetcher fetcher, boolean preserving, Boolean copying) {
            this.fetcher = fetcher;
            this.preserving = preserving;
            this.copying = copying;
        }
        
        protected SimpleMapObjectWrapperFetcher(ObjectWrapper objectWrapper, boolean preserving, Boolean copying) {
            this.fetcher = new TrivialObjectWrapperFetcher(objectWrapper);
            this.preserving = preserving;
            this.copying = copying;
        }
        
        protected SimpleMapObjectWrapperFetcher(boolean preserving, Boolean copying) {
            this.fetcher = new CurrentObjectWrapperFetcher();
            this.preserving = preserving;
            this.copying = copying;
        }

        @Override
        public ObjectWrapper getWrapper(Environment env) {
            return SimpleRewrapObjectWrapper.getInstance(env, fetcher.getWrapper(env), preserving, copying);
        }
    }
    
    public static ObjectWrapper getObjectWrapperByName(String wrapper, Environment env) throws TemplateModelException {
        if (wrapper == null || wrapper.isEmpty()) {
            return env.getObjectWrapper();
        } else {
            ObjectWrapperFetcher fetcher = objectWrapperFetcherMap.get(wrapper);
            if (fetcher != null) {
                return fetcher.getWrapper(env);
            } else {
                return null;
            }
        }
    }
    
}
