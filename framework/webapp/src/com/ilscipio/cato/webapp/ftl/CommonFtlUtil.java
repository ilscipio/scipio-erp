package com.ilscipio.cato.webapp.ftl;

import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;
import freemarker.template.TemplateModelException;

/**
 * Cato: Generic Freemarker utility methods.
 * <p>
 * Most methods should go into feature-specific util classes.
 */
public abstract class CommonFtlUtil {

    public static final String module = CommonFtlUtil.class.getName();
    
    protected CommonFtlUtil() {
    }
    
    public static Environment getCurrentEnvironment() throws TemplateModelException {
        Environment env = FreeMarkerWorker.getCurrentEnvironment();
        if (env == null) {
            throw new TemplateModelException("Missing Freemarker environment");
        }
        return env;
    }
    
}
