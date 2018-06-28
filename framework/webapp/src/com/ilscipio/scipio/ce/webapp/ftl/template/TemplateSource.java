package com.ilscipio.scipio.ce.webapp.ftl.template;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModelException;

/**
 * TODO: currently does not support lazy compilation.
 * this is basically a placeholder to support it later.
 * <p>
 * FIXME: this implements a kind of "super-cache" due to massive limitations in availability
 * of variables. this means duplicate memory instances for now. there is no way around this
 * with the current classes.
 * this can be considered non-urgent because this class is not in widespread use,
 * and unlikely to 
 */
public abstract class TemplateSource {

    private static final long configTmplCacheExpireTime = UtilProperties.getPropertyAsLong("cache", "template.ftl.general.expireTime", 0);
    
    /**
     * SUPER-cache for all configurations.
     * WARN: first layer is implemented manually with double-locking idiot for thread safety,
     * to avoid nested UtilCache possible issues.
     */
    private static Map<Configuration, UtilCache<String, Template>> configTmplLocCaches = Collections.emptyMap();
    private static Map<Configuration, UtilCache<String, Template>> configTmplInlineSelfCaches = Collections.emptyMap();

    
    public abstract Template getTemplate() throws TemplateException, IOException;
    
    public static TemplateSource getForLocation(String templateLoc, UtilCache<String, Template> cache, Configuration config) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplate(templateLoc, cache, config));
    }
    
    public static TemplateSource getForLocationNoCache(String templateLoc, Configuration config) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplate(templateLoc, null, config)); // NOTE: null support is new in Scipio
    }
    
    /**
     * @deprecated use overloads with explicit cache and configuration
     */
    @Deprecated
    public static TemplateSource getForLocation(String templateLoc) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplate(templateLoc));
    }
    
    /**
     * Gets source for inline template, where the cache key is different from the template itself.
     */
    public static TemplateSource getForInline(String templateBody, String templateKey, String templateName, UtilCache<String, Template> cache, Configuration config) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplateFromString(templateBody, templateKey, templateName, cache, config));
    }
    
    /**
     * Gets source for inline template, where the cache key is the template string itself.
     * NOTE: this is probably not appropriate for large templates.
     */
    public static TemplateSource getForInlineSelfCache(String templateBody, String templateName, UtilCache<String, Template> cache, Configuration config) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplateFromString(templateBody, templateBody, templateName, cache, config));
    }
    
    /**
     * Gets source for inline template, where the cache key is the template string itself.
     * The template name is auto-decided.
     * NOTE: this is probably not appropriate for large templates.
     */
    public static TemplateSource getForInlineSelfCache(String templateBody, UtilCache<String, Template> cache, Configuration config) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplateFromString(templateBody, templateBody, makeTemplateNameForInline(templateBody), cache, config));
    }
    
    /**
     * Gets source for inline template, but with NO caching.
     */
    public static TemplateSource getForInlineNoCache(String templateBody, String templateName, Configuration config) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplateFromString(templateBody, null, templateName, null, config));
    }
    
    /**
     * Gets source for inline template, but with NO caching.
     * The template name is auto-decided.
     */
    public static TemplateSource getForInlineNoCache(String templateBody, Configuration config) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplateFromString(templateBody, null, makeTemplateNameForInline(templateBody), null, config));
    }
    
    public static String makeTemplateNameForInline(String templateBody) {
        // FIXME: using date hack for unique template name for now (what macro renderer does)
        return "inline_template_" + (new java.util.Date()).toString();
    }
    
    /**
     * Tries to get appropriate location-based UtilCache in use for templates being rendered.
     * <p>
     * FIXME: WARNING: massive limitations here, we do not have access to all the Configuration instances
     * we'd need to test to implement this, and a host of problems...
     * as a result I am creating a new two-layer cache system that first keys on the Configuration instances.
     */
    public static UtilCache<String, Template> getTemplateLocationCacheForConfig(Configuration config, Environment env) throws TemplateModelException {
        Map<Configuration, UtilCache<String, Template>> configTmplLocCaches = TemplateSource.configTmplLocCaches;
        
        UtilCache<String, Template> cache = configTmplLocCaches.get(config);
        if (cache == null) {
            // double-locking idiom, with configTmplLocCaches as unmodifiable
            synchronized(TemplateSource.class) {
                configTmplLocCaches = TemplateSource.configTmplLocCaches;
                cache = configTmplLocCaches.get(config);
                if (cache == null) {
                    Map<Configuration, UtilCache<String, Template>> newConfigCacheMap = new HashMap<>(configTmplLocCaches);
                    cache = UtilCache.createUtilCache("templatesource.ftl.location." + (configTmplLocCaches.size() + 1), 
                                0, configTmplCacheExpireTime, false);
                    newConfigCacheMap.put(config, cache);
                    
                    TemplateSource.configTmplLocCaches = newConfigCacheMap;
                }
            }
        }
        return cache;
    }
    
    /**
     * Heuristically tries to get the inline template UtilCache in use for templates being rendered.
     * Cache has the templates themselves as keys.
     * FIXME: serious limitations and memory implications
     * @see #getTemplateLocationCacheForConfig
     */
    public static UtilCache<String, Template> getTemplateInlineSelfCacheForConfig(Configuration config, Environment env) throws TemplateModelException {
        Map<Configuration, UtilCache<String, Template>> configTmplInlineSelfCaches = TemplateSource.configTmplInlineSelfCaches;
        
        UtilCache<String, Template> cache = configTmplInlineSelfCaches.get(config);
        if (cache == null) {
            // double-locking idiom, with configTmplInlineSelfCaches as unmodifiable
            synchronized(TemplateSource.class) {
                configTmplInlineSelfCaches = TemplateSource.configTmplInlineSelfCaches;
                cache = configTmplInlineSelfCaches.get(config);
                if (cache == null) {
                    Map<Configuration, UtilCache<String, Template>> newConfigCacheMap = new HashMap<>(configTmplInlineSelfCaches);
                    cache = UtilCache.createUtilCache("templatesource.ftl.inline.self." + (configTmplInlineSelfCaches.size() + 1), 
                                0, configTmplCacheExpireTime, false);
                    newConfigCacheMap.put(config, cache);
                    
                    TemplateSource.configTmplInlineSelfCaches = newConfigCacheMap;
                }
            }
        }
        return cache;
    }
    
    public static class DirectTemplateSource extends TemplateSource {
        protected final Template template;
        
        protected DirectTemplateSource(Template template) {
            this.template = template;
        }

        @Override
        public Template getTemplate() throws TemplateException, IOException {
            return template;
        }
    }
      
}
