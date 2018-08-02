package com.ilscipio.scipio.ce.webapp.ftl.context;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.renderer.RenderEnvType;

import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateModelException;

public abstract class UrlTransformUtil {

    protected UrlTransformUtil() {
    }

    /**
     * Tries to determine the fullPath Boolean value to use when generating the link,
     * and whether it should be forced or not, based on the rendering context.
     */
    public static Boolean determineFullPath(Boolean fullPathArg, RenderEnvType renderEnvType, Environment env) throws TemplateModelException {
        return renderEnvType.isStatic() ? Boolean.TRUE : fullPathArg;
    }

    /**
     * Determines what webSiteId arg should be used for link building.
     * <p>
     * @see org.ofbiz.common.email.NotificationServices#setBaseUrl
     */
    public static String determineWebSiteId(String webSiteIdArg, RenderEnvType renderEnvType, FullWebappInfo currentWebappInfo, Environment env) throws TemplateModelException {
        if (renderEnvType.isStatic() && UtilValidate.isEmpty(webSiteIdArg) && currentWebappInfo != null) {
            webSiteIdArg = currentWebappInfo.getWebSiteId();
        }
        return webSiteIdArg;
    }
    
    /**
     * Escapes a URL built by a transform such as ofbizUrl IF a language is specified.
     * <p>
     * WARN/FIXME?: 2016-10-19: THE STRICT BOOLEAN IS CURRENTLY IGNORED HERE BECAUSE THERE ARE TOO
     * MANY ESCAPED AMPERSANDS THROUGHOUT ALL OF OFBIZ AND TEMPLATES.
     */
    public static String escapeGeneratedUrl(String value, String lang, boolean strict, Environment env) throws TemplateModelException {
        //return TemplateFtlUtil.escapeFullUrl(value, lang, strict, env); // TODO/FIXME?
        return TemplateFtlUtil.escapeFullUrl(value, lang, null, env);
    }
}
