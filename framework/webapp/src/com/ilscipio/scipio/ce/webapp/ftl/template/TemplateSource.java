package com.ilscipio.scipio.ce.webapp.ftl.template;

import java.io.IOException;

import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;

/**
 * TODO: currently does not support lazy compilation.
 * this is basically a placeholder to support it later.
 */
public abstract class TemplateSource {

    public abstract Template getTemplate() throws TemplateException, IOException;
    
    public static TemplateSource getForLocation(String templateLoc, UtilCache<String, Template> cache, Configuration config) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplate(templateLoc, cache, config));
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
    public static TemplateSource getForInline(String templateStr, String templateKey, String templateName, UtilCache<String, Template> cache, Configuration config) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplateFromString(templateStr, templateKey, templateName, cache, config));
    }
    
    /**
     * Gets source for inline template, where the cache key is the template string itself.
     * The template name is auto-decided.
     * NOTE: this is probably not appropriate for large templates.
     */
    public static TemplateSource getForInlineSelfCache(String templateStr, String templateName, UtilCache<String, Template> cache, Configuration config) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplateFromString(templateStr, templateStr, templateName, cache, config));
    }
    
    /**
     * Gets source for inline template, but with NO caching.
     */
    public static TemplateSource getForInline(String templateStr, String templateName, Configuration config) throws TemplateException, IOException {
        return new DirectTemplateSource(FreeMarkerWorker.getTemplateFromString(templateStr, templateName, config));
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
