package com.ilscipio.scipio.cms.template.ftl;

import java.io.IOException;
import java.io.Serializable;
import java.io.Writer;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilRender.RenderExceptionMode;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.template.FtlTransformFactory;
import org.ofbiz.entity.Delegator;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.ftl.WebappUrlDirective;

import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.UrlTransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.OfbizFtlObjectType;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPage.PageWorker;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.template.CmsRenderUtil;

import freemarker.core.Environment;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * Page URL directive.
 * <p>
 * DEV NOTE: I changed the "page" arg to "name" so it is consistent with asset directive.
 * <pre>
 * {@code
 * <@pageUrl name="pageName" params="param1=value1&amp;param2=value2"/>
 * <@pageUrl id="10000" params={"param1":"value1", "param2":"value2"} escapeAs="html"/>
 * <@pageUrl name="pageName" webSiteId="realWebSiteId" lookupWebSiteId="webSiteIdPageIsStoredUnder" />
 * <@pageUrl name="pageName" webSiteId="webSiteId" webSiteLookupMode="any"/><!-- (any|default-only|mappings-only, default: any) -->
 * }
 * </pre>
 * <p>
 * 2017: In addition, the following args from <code>@pageUrl</code> are supported, with same meaning:
 * <ul>
 * <li>escapeAs
 * <li>rawParams
 * <li>strict
 * <li>extLoginKey
 * </ul>
 * As with <code>@pageUrl</code>, by default args may get auto-escaped when passed, and the resulting
 * URL is not html-escaped. The best way to handle all this is to use <code>escapeAs='html'</code>,
 * which prevents most issues and automatically prevents the escaping of the arguments, before finally
 * escaping the whole thing.
 * <p>
 * The default for <code>paramDelim</code> is <code>&amp;</code> (default) if rawParams resolves
 * to false, and <code>&</code> if it resolves to false. It can be overridden as explicit arg.
 * <p>
 * Thread-safe, immutable and global.
 */
public class CmsPageUrlDirective implements TemplateDirectiveModel, Serializable {

    private static final long serialVersionUID = 4165213097346590960L;

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    static final RenderExceptionMode urlLiveExceptionMode = CmsRenderUtil.getDirectiveLiveRenderExceptionMode(RenderExceptionMode.valueOfPermissive(UtilProperties.getPropertyValue("cms",
            "render.live.exception.directive.url.mode")));

    public static class Factory implements FtlTransformFactory { // SCIPIO
        private static final CmsPageUrlDirective DEFAULT = new CmsPageUrlDirective();

        @Override
        public TemplateModel getTransform(ClassLoader loader) {
            return DEFAULT;
        }
    }

    public static CmsPageUrlDirective getInstance() {
        return Factory.DEFAULT;
    }

    public static CmsPageUrlDirective create() { // SCIPIO: See CMS's PageUrlDirective
        return new CmsPageUrlDirective();
    }

    @SuppressWarnings("rawtypes")
    @Override
    public void execute(Environment env, Map argsUntyped, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        String output = makeLinkFromFtl(env, UtilGenerics.<String, TemplateModel>checkMap(argsUntyped), loopVars, body, null);
        Writer out = env.getOut();
        if (output != null) {
            out.write(output);
        }
    }

    static String makeLinkFromFtl(Environment env, Map<String, ? extends TemplateModel> args, TemplateModel[] loopVars, TemplateDirectiveBody body,
            TemplateModel nameModel) throws TemplateException, IOException {
        MapStack<String> context = CmsRenderUtil.getRenderContextAlways(env);
        CmsPageContext pageContext = CmsPageContext.getOrMakeFromContext(context);

        //HttpServletRequest request = pageContext.getRequest();
        // TODO: REVIEW: may be special for CMS vs other transforms
        //RenderEnvType renderEnvType = ContextFtlUtil.getRenderEnvType(env, request);

        final String escapeAs = TransformUtil.getStringArg(args, "escapeAs"); // SCIPIO: new
        boolean rawParamsDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we should get rawParams
        final boolean rawParams = TransformUtil.getBooleanArg(args, "rawParams", rawParamsDefault); // SCIPIO: new
        boolean strictDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we want strict handling
        final boolean strict = TransformUtil.getBooleanArg(args, "strict", strictDefault); // SCIPIO: new

        // The target webSiteId, whose webapp context path we want the link to contain (/shop, /website, etc.)
        String webSiteId = PageWorker.normalizeWebSiteId(TransformUtil.getBooleanOrStringArg(args, "webSiteId", null, false, rawParams));

        // The current/context webSiteId (may be null)
        String currentWebSiteId = pageContext.getWebSiteId(); // (no need normalize this one)
 
        // A lookupWebSiteId used only in the initial page lookup, may/will be needed for pages with multiple webSiteIds
        // NOTE: we don't include currentWebSiteId in this (it uses different logic i.e findByNameBest's preferredWebSiteId)
        String lookupWebSiteId = PageWorker.normalizeWebSiteId(TransformUtil.getBooleanOrStringArg(args, "lookupWebSiteId", null, false, rawParams));
        if (lookupWebSiteId == null) {
            lookupWebSiteId = webSiteId; 
        }

        // webSiteLookupMode influences which entities/fields get searched in findByNameBest
        String webSiteLookupMode = TransformUtil.getStringArg(args, "webSiteLookupMode");

        // interWebapp, automatically determined for CMS page URls, usually don't need to pass, special case only
        Boolean interWebapp = TransformUtil.getBooleanArg(args, "interWebapp");

        Boolean fullPath = TransformUtil.getBooleanArg(args, "fullPath");
        Boolean secure = TransformUtil.getBooleanArg(args, "secure");
        Boolean encode = TransformUtil.getBooleanArg(args, "encode");

        boolean extLoginKey = TransformUtil.getBooleanArg(args, "extLoginKey", false);
        // NOTE: the default for paramDelim is highly heuristic... for now just follow rawParams (even though it's not its exact meaning)
        final String paramDelimDefault = rawParams ? "&" : "&amp;";
        final String paramDelim = TransformUtil.getStringArg(args, "paramDelim", paramDelimDefault, true, true);

        boolean isFindById = false;
        String value = null;
        TemplateModel valueModel = nameModel;
        if (valueModel == null) {
            valueModel = args.get("id");
            if (valueModel != null) {
                isFindById = true;
            } else {
                valueModel = args.get("name");
                if (valueModel == null) {
                    handleError(env, null, "The id or name of linked page must be given as parameter \"id\" or \"name\" (was missing or null)");
                    return null;
                }
            }
        }
        value = TransformUtil.getStringArg(valueModel, rawParams);
        if (UtilValidate.isEmpty(value)) {
            handleError(env, null, "The id or name of linked page must be given as parameter \"id\" or \"name\" (was empty)");
        }

        String output = null;
        try {
            Delegator delegator = (Delegator) context.get("delegator");

            // this should have been done earlier, using non-static delegator
            //// we have to make sure the delegator is set so the
            //// RequestHandler can look up the WebSite
            //pageContext.getRequest().setAttribute("delegator", getDefaultDelegator());
            // get link

            // NOTE: 2016: for live renders we will use the cache here.
            final boolean useCache = !pageContext.isPreview();

            CmsPage page = null;
            if (isFindById) {
                page = CmsPage.getWorker().findById(delegator, value, useCache);
            } else {
                page = CmsPage.getWorker().findByNameBest(delegator, value, lookupWebSiteId, webSiteLookupMode, currentWebSiteId, useCache);
            }
            if (page == null) {
                throw new IllegalArgumentException("Could not locate page");
            }
            
            String paramStr = TransformUtil.getParamString(args, "params", paramDelim, rawParams);
            if (extLoginKey == Boolean.TRUE) {
                if (paramStr == null) {
                    paramStr = "";
                } else if (paramStr.length() > 0) {
                    paramStr = "?" + paramStr;
                }
                paramStr = RequestLinkUtil.checkAddExternalLoginKey(paramStr, pageContext.getRequest(), paramDelim);
            }

            // render link
            output = makeLinkAutoEx(pageContext, page, interWebapp, webSiteId, fullPath, secure, encode, paramStr);
            if (output != null) {
                output = UrlTransformUtil.escapeGeneratedUrl(output, escapeAs, strict, env);
            }
        } catch (Exception e) {
            handleError(env, e, "Could not build link to CMS page using: " + 
                    makeLookupFieldInfo(isFindById, value, webSiteId, lookupWebSiteId, currentWebSiteId));
            return null;
        }
        return output;
    }

    // just for logging/error (don't do this before that, otherwise needless overhead)
    private static Map<String, Object> makeLookupFieldInfo(boolean isFindById, String value, String webSiteId,
            String lookupWebSiteId, String currentWebSiteId) {
        return UtilMisc.toMap(isFindById ? "pageId" : "pageName", value, "webSiteId", webSiteId,
                "lookupWebSiteId", lookupWebSiteId, "currentWebSiteId", currentWebSiteId);
    }
    
    /**
     * Builds the URL string for a page, by wrapping around and emulating
     * the standard RequestHandler.makeLinkAuto call.
     * <p>
     * NOTE: like RequestHandler.makeLinkAuto, this version currently does not throw exceptions.
     * <p>
     * NOTE: The webSiteId should already be normalized, so it may be either null, the string "null", or a real webSiteId.
     *
     * @param context the current page context (may be created from context for non-CMS renderers)
     * @param page the target page to link to
     * @return complete url with protocol scheme and port (if necessary)
     */
    public static String makeLinkAuto(CmsPageContext context, CmsPage page, Boolean interWebapp, String webSiteId,
            Boolean fullPath, Boolean secure, Boolean encode, String paramStr) {
        try {
            return makeLinkAutoEx(context, page, interWebapp, webSiteId, fullPath, secure, encode, paramStr);
        } catch (Exception e) {
            final String errMsg = "Could not create link to page '" + (page != null ? page.getId() : "null") + "'";
            Debug.logError(e, "Cms: " + errMsg, module);
            return null;
        }
    }

    /**
     * Builds the URL string for a page, specified by pageId, by wrapping around and emulating
     * the standard RequestHandler.makeLinkAuto call.
     * <p>
     * NOTE: like RequestHandler.makeLinkAuto, this version currently does not throw exceptions.
     * <p>
     * NOTE: The webSiteId should already be normalized, so it may be either null, the string "null", or a real webSiteId.
     *
     * @param context the current page context (may be created from context for non-CMS renderers)
     * @param pageId the target page to link to
     * @return complete url with protocol scheme and port (if necessary)
     */
    public static String makeLinkAuto(CmsPageContext context, String pageId, Boolean interWebapp, String webSiteId,
            Boolean fullPath, Boolean secure, Boolean encode, String paramStr) {
        try {
            final boolean useCache = !context.isPreview();
            CmsPage page = CmsPage.getWorker().findByIdAlways(context.getDelegator(), pageId, useCache);
            return makeLinkAutoEx(context, page, interWebapp, webSiteId, fullPath, secure, encode, paramStr);
        } catch (Exception e) {
            final String errMsg = "Could not create link to page '" + pageId + "': " + e.getMessage();
            Debug.logError(e, "Cms: " + errMsg, module);
            return null;
        }
    }

    /**
     * Builds the URL string for a page, specified by pageName and webSiteId, by wrapping around and emulating
     * the standard RequestHandler.makeLinkAuto call.
     * <p>
     * NOTE: like RequestHandler.makeLinkAuto, this version does not throw exceptions.
     * <p>
     * NOTE: The webSiteId should already be normalized, so it may be either null, the string "null", or a real webSiteId.
     * @param context the current page context (may be created from context for non-CMS renderers)
     * @return complete url with protocol scheme and port (if necessary)
     */
    public static String makeLinkAutoByName(CmsPageContext context, String pageName, Boolean interWebapp, String webSiteId,
            Boolean fullPath, Boolean secure, Boolean encode, String paramStr, String webSiteLookupMode) {
        try {
            final boolean useCache = !context.isPreview();
            CmsPage page = CmsPage.getWorker().findByNameBestAlways(context.getDelegator(), pageName, webSiteId, webSiteLookupMode,
                    context.getWebSiteId(), useCache);
            return makeLinkAutoEx(context, page, interWebapp, webSiteId, fullPath, secure, encode, paramStr);
        } catch (Exception e) {
            final String errMsg = "Could not create link to page with name '" + pageName + "' for webSiteId '" + webSiteId + "': " + e.getMessage();
            Debug.logError(e, "Cms: " + errMsg, module);
            return null;
        }
    }

    /**
     * makeLinkAutoEx.
     * <p>
     * NOTE: Unlike for controller request/webapp URLs, CMS page URLs can decide the <code>interWebapp</code> flag
     * automatically; there's usually no need to specify it.
     * <p>
     * NOTE: The webSiteId should already be normalized, so it may be either null, the string "null", or a real webSiteId.
     */
    public static String makeLinkAutoEx(CmsPageContext context, CmsPage page, Boolean interWebapp, String webSiteId,
            Boolean fullPath, Boolean secure, Boolean encode, String paramStr) {
        String targetWebSiteId = webSiteId;
        if (UtilValidate.isEmpty(targetWebSiteId)) {
            targetWebSiteId = context.getWebSiteId(); // currentWebSiteId
            if (UtilValidate.isEmpty(targetWebSiteId)) {
                // SCIPIO: 2019-01-29: webSiteId fallback: if user was too lazy to include a webSiteId and we're
                // in a different app (not current/same website), we can try to get the best webSiteId instead of crashing...
                targetWebSiteId = page.getPrimaryWebSiteId();
            } else {
                if (interWebapp == null) {
                    // NOTE: Unlike for controller request/webapp URLs, CMS page URLs can decide this flag automatically (though slower)
                    interWebapp = false;
                }
            }
        }
        targetWebSiteId = PageWorker.getEffectiveWebSiteId(targetWebSiteId);
        String path = page.getPrimaryPathExpanded(targetWebSiteId);
        if (path == null) {
            throw new IllegalArgumentException("Page '" + page.getId() + "' does not have a primary path "
                    + "for target webSiteId '" + targetWebSiteId + "'; cannot build link");
        }

        path = TemplateFtlUtil.appendParamString(path, paramStr);

        final boolean absUrl = false; // the page path (sourcePath) is never from server root; always relative to context root
        final boolean controller = false; // TODO?: no way to exploit controller currently, not sure how could...
        if (interWebapp == null) {
            // NOTE: Unlike for controller request/webapp URLs, CMS page URLs can decide this flag automatically
            interWebapp = !targetWebSiteId.equals(context.getWebSiteId());
        }

        // 2016: FIXME: this should be calling RequestHandler.makeLinkAutoEx, but no such method exists yet...
        // NOTE: Here we MUST pass effWebSiteId, not webSiteId, even if it's the same as the current webapp,
        // otherwise it may add the wrong mount-point!
        return RequestHandler.makeLinkAuto(context.getRequest(), context.getResponse(),
                path, absUrl, interWebapp, targetWebSiteId, controller, fullPath, secure, encode);
    }

    /**
     * Builds the URL string for a page using current request.
     * 2016: TODO: WARN: REVIEW: this maybe should not be considered public at this time.
     *
     * @param request
     *            the current request
     * @param page
     *            the page the link should point to
     * @return complete url with protocol scheme and port (if necessary)
     */
    public static String makeLinkSimple(HttpServletRequest request, CmsPage page, String webSiteId) {
        // reconstruct the start of the URL
        StringBuffer url = new StringBuffer();
        String scheme = request.getScheme();
        int port = request.getServerPort();

        url.append(scheme);
        url.append("://");
        url.append(request.getServerName());
        if ((scheme.equals("http") && port != 80) || (scheme.equals("https") && port != 443)) {
            url.append(':');
            url.append(request.getServerPort());
        }
        url.append(request.getContextPath());
        url.append(page.getPrimaryPathExpanded(webSiteId));

        return url.toString();
    }

    public static boolean handleError(Environment env, Throwable t, String errorMsg) throws CmsException, TemplateException {
        // DEV NOTE: you could theoretically do something like this, but it just makes things worse and inconsistent with scipio macros
//        if (CmsRenderUtil.getDirectiveRenderExceptionMode(env, urlLiveExceptionMode) == RenderExceptionMode.BLANK) {
//            try {
//                env.getOut().write("#");
//            } catch(Exception e) {
//                Debug.logError(e, module);
//            }
//        }
        return CmsRenderUtil.handleDirectiveError(env, "Page link failed", t, errorMsg, urlLiveExceptionMode, module);
    }

    /**
     * SCIPIO: CmsPageUrlMethod - <code>makeCmsPageUrl</code> function wrapper around <code>@cmsPageUrl</code>.
     * <p>
     * Two variants supported:
     * <ul>
     * <li><code>makeCmsPageUrl(pageName)</code> - single string page name arg, no other options.
     * <li><code>makeCmsPageUrl({"name":pageName, "escapeAs":"html", ...})</code> - single map arg,
     *     with each entry an option to <code>@cmsPageUrl</code>.
     * </ul>
     * Unlike the macro, by default, this uses <code>rawParams=true</code> and <code>strict=true</code> (NOTE: strict
     * may or may not be honored; see ofbizUrl docs).
     */
    public static class Method implements TemplateMethodModelEx {
        public static class Factory implements FtlTransformFactory { // SCIPIO
            private static final Method DEFAULT = new Method();
            @Override
            public TemplateModel getTransform(ClassLoader loader) {
                return DEFAULT;
            }
        }
        public static Method getInstance() {
            return Factory.DEFAULT;
        }
        public static Method create() { // SCIPIO: See CMS's PageUrlDirective
            return new Method();
        }

        @Override
        public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
            return exec(WebappUrlDirective.getArgsMapOrNull(args), args);
        }

        public Object exec(Map<String, ? extends TemplateModel> args, List<?> posArgs) throws TemplateModelException {
            Environment env = Environment.getCurrentEnvironment();
            TemplateModel nameModel = null;
            if (args == null) {
                args = Collections.emptyMap();
                nameModel = (TemplateModel) posArgs.get(0); // Positional parameter(s) (limited support)
            }
            try {
                String output = CmsPageUrlDirective.makeLinkFromFtl(env, args, null, null, nameModel);
                return new SimpleScalar(output != null ? output : ""); // DO NOT return simple string (escaping)
            } catch (IOException | TemplateException e) {
                throw new TemplateModelException(e);
            }
        }
    }
}