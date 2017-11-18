package com.ilscipio.scipio.product.seo;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;

@SuppressWarnings("serial")
public class UrlProcessors implements Serializable {

    public static final String module = UrlProcessors.class.getName();
    
    private static final UrlProcessors DUMMY = new UrlProcessors(Collections.<UrlProcessor>emptyList());
    
    
    protected final List<UrlProcessor> processorList;
  
    protected UrlProcessors(Collection<UrlProcessor> processorList) {
        this.processorList = new ArrayList<UrlProcessor>(processorList);
    }
    
    public static UrlProcessors createProcessors(Collection<UrlProcessor> processorList) {
        return new UrlProcessors(processorList);
    }
    
    public static UrlProcessors getDummyProcessors() {
        return DUMMY;
    }

    public List<UrlProcessor> getProcessorList() {
        return processorList;
    }


    public String processUrl(String url) {
        for(UrlProcessor processor : processorList) {
            url = processor.processUrl(url);
        }
        return url;
    }

    public static abstract class UrlProcessor implements Serializable {
        protected UrlProcessor() {
        }
        
        public abstract String processUrl(String url);
    }
    
    public static class StaticMethodUrlProcessor extends UrlProcessor {
        private final Method method;
        
        public StaticMethodUrlProcessor(Method method) {
            this.method = method;
        }

        public static StaticMethodUrlProcessor getProcessor(String methodLocation) throws ClassNotFoundException, NoSuchMethodException, SecurityException {
            int sep = methodLocation.lastIndexOf('#');
            if (sep <= 0 || sep >= (methodLocation.length() - 1)) {
                throw new IllegalArgumentException("Invalid method location (format: 'my.package.MyClass#methodName'): " + methodLocation);
            }
            String classLocation = methodLocation.substring(0, sep);
            String methodName = methodLocation.substring(sep + 1);
            
            ClassLoader loader = Thread.currentThread().getContextClassLoader();
            Class<?> cls = loader.loadClass(classLocation);
            Method method = cls.getMethod(methodName, String.class);
            if (!String.class.isAssignableFrom(method.getReturnType())) {
                throw new IllegalArgumentException("Invalid method; does not return String: " + methodLocation + ": " + method.toString());
            }
            return new StaticMethodUrlProcessor(method);
        }

        @Override
        public String processUrl(String url) {
            try {
                return (String) method.invoke(null, url);
            } catch (InvocationTargetException e) {
                Throwable t = e.getCause();
                Debug.logError(t, "Seo: URL processor method threw an exception: " + method.toString() + ": " + t.getMessage(), module);
            } catch (Exception e) {
                Debug.logError(e, "Seo: Error invoking method: " + method.toString() + ": " + e.getMessage(), module);
            }
            return url;
        }
    }
    
    /**
     * Replaces the Map<String, String> which did not preserve order and other concerns.
     */
    public static class CharFilterUrlProcessor extends UrlProcessor {
        private final List<CharFilter> charFilters;

        public CharFilterUrlProcessor(Collection<CharFilter> charFilters) {
            this.charFilters = new ArrayList<>(charFilters);
        }
        
        public static CharFilterUrlProcessor getProcessor(Collection<CharFilter> charFilters) {
            return new CharFilterUrlProcessor(charFilters);
        }

        @Override
        public String processUrl(String url) {
            return CharFilter.applyCharFilters(url, charFilters);
        }
    }
    
    public static class DummyUrlProcessor extends UrlProcessor {
        private static final DummyUrlProcessor INSTANCE = new DummyUrlProcessor();
        
        public static DummyUrlProcessor getProcessor() { 
            return INSTANCE;
        }
        
        @Override
        public String processUrl(String url) {
            return url;
        }
    }
    
    public static class CharFilter implements Serializable {
        private final Pattern from; // pre-compiled = faster
        private final String to;

        public CharFilter(Pattern from, String to) {
            this.from = from;
            this.to = to;
        }
        
        public static CharFilter compile(String from, String to) {
            return new CharFilter(Pattern.compile(from), to);
        }
        
        public Pattern getFrom() {
            return from;
        }
        public String getTo() {
            return to;
        }
        
        public String apply(String url) {
            return from.matcher(url).replaceAll(to);
        }

        public static String applyCharFilters(String url, Collection<CharFilter> charFilters) {
            if (charFilters == null || url == null || url.isEmpty()) return url;
            for (CharFilter charFilter : charFilters) {
                url = charFilter.apply(url);
            }
            return url;
        }
    }
}
