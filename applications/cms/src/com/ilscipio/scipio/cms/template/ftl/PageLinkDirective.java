package com.ilscipio.scipio.cms.template.ftl;

import java.io.IOException;
import java.io.Serializable;
import java.io.Writer;
import java.util.HashMap;
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
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestLinkUtil;

import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.UrlTransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.OfbizFtlObjectType;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.template.CmsRenderUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateModel;

/**
 * Page Link/URL directive.
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
 * 2017: In addition, the following args from <code>@ofbizUrl</code> are supported, with same meaning:
 * <ul>
 * <li>escapeAs
 * <li>rawParams
 * <li>strict
 * <li>extLoginKey
 * </ul>
 * As with <code>@ofbizUrl</code>, by default args may get auto-escaped when passed, and the resulting
 * URL is not html-escaped. The best way to handle all this is to use <code>escapeAs='html'</code>,
 * which prevents most issues and automatically prevents the escaping of the arguments, before finally
 * escaping the whole thing.  
 * <p>
 * The default for <code>paramDelim</code> is <code>&amp;</code> (default) if rawParams resolves
 * to false, and <code>&</code> if it resolves to false. It can be overridden as explicit arg.
 * <p>
 * webSiteId can be set to boolean false to search by name only (special cases).
 * it can also be set to explicit string NULL to search for explicit no-website page.
 * <p>
 * Thread-safe, immutable and global.
 */
public class PageLinkDirective implements TemplateDirectiveModel, Serializable {

    private static final long serialVersionUID = 4165213097346590960L;
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private static final PageLinkDirective instance = new PageLinkDirective();
    
    static final RenderExceptionMode urlLiveExceptionMode = CmsRenderUtil.getDirectiveLiveRenderExceptionMode(RenderExceptionMode.valueOfPermissive(UtilProperties.getPropertyValue("cms", 
            "render.live.exception.directive.url.mode")));   
    
    public PageLinkDirective() {
        super();
    }

    public static PageLinkDirective getInstance() {
        return instance;
    }
    
    /**
     * @see freemarker.template.TemplateDirectiveModel#execute(freemarker.core.Environment,
     *      java.util.Map, freemarker.template.TemplateModel[],
     *      freemarker.template.TemplateDirectiveBody)
     */
    @SuppressWarnings("rawtypes")
    @Override
    public void execute(Environment env, Map argsUntyped, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        String output = makeLinkFromFtl(env, UtilGenerics.<String, TemplateModel>checkMap(argsUntyped), loopVars, body);
        Writer out = env.getOut();
        if (output != null) out.write(output);
    }
    
    static String makeLinkFromFtl(Environment env, Map<String, TemplateModel> args, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
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
        
        Object webSiteIdObj = TransformUtil.getBooleanOrStringArg(args, "webSiteId", null, false, rawParams);
        Object lookupWebSiteIdObj = TransformUtil.getBooleanOrStringArg(args, "lookupWebSiteId", null, false, rawParams);
        
        String webSiteId = (webSiteIdObj instanceof String) ? (String) webSiteIdObj : null;
        String lookupWebSiteId = (lookupWebSiteIdObj instanceof String) ? (String) lookupWebSiteIdObj : null;
        boolean useWebSiteIdLookup = !Boolean.FALSE.equals(lookupWebSiteIdObj); // true default
        
        String webSiteLookupMode = TransformUtil.getStringArg(args, "webSiteLookupMode"); // SCIPIO: new
        
        // NOTE: although this appears similar to our @ofbizUrl, there is small difference:
        // here we bind the logical "interWebapp" boolean to the presence of "webSiteId".
        // the difference from ofbizXxxUrl is that CMS pages always need a webSiteId one way
        // or another so common use will prevail...
        // user can still override explicit interWebapp if need.
        // TODO?: could have a pageLinkInterWebapp alias instead...
        Boolean interWebapp = TransformUtil.getBooleanArg(args, "interWebapp");
        if (interWebapp == null) {
            interWebapp = UtilValidate.isNotEmpty(webSiteId);
        }
        
        // TODO: REVIEW: may be special for CMS vs other transforms
        //Boolean fullPath = TransformUrlUtil.determineFullPath(TransformUtil.getBooleanArg(args, "fullPath"), renderEnvType, env);
        Boolean fullPath = TransformUtil.getBooleanArg(args, "fullPath");
        Boolean secure = TransformUtil.getBooleanArg(args, "secure");
        Boolean encode = TransformUtil.getBooleanArg(args, "encode");

        boolean extLoginKey = TransformUtil.getBooleanArg(args, "extLoginKey", false);
        // NOTE: the default for paramDelim is highly heuristic... for now just follow rawParams (even though it's not its exact meaning)
        final String paramDelimDefault = rawParams ? "&" : "&amp;";
        final String paramDelim = TransformUtil.getStringArg(args, "paramDelim", paramDelimDefault, true, true);
        TemplateModel paramsModel = args.get("params");

        // get final exec webSiteId
        webSiteId = UtilValidate.isNotEmpty(webSiteId) ? webSiteId : pageContext.getWebSiteId();
        if (UtilValidate.isEmpty(lookupWebSiteId)) {
            lookupWebSiteId = webSiteId;
        }
        
        // TODO: REVIEW: may be special for CMS vs other transforms
        //// for email context
        //webSiteId = TransformUrlUtil.determineWebSiteId(webSiteId, renderEnvType, env);
        
        String output = null;
        Map<String, Object> lookupFields = new HashMap<>();
        String value = null;
        boolean isFindById = false;
        // first, check if page name was given
        if (args.containsKey("id")) {
            value = TransformUtil.getStringArg(args, "id", rawParams);
            lookupFields.put("pageId", value);
            isFindById = true;
        } else if (args.containsKey("name")) {
            value = TransformUtil.getStringArg(args, "name", rawParams);
            lookupFields.put("pageName", value);
            if (useWebSiteIdLookup) {
                lookupFields.put("webSiteId", "NULL".equalsIgnoreCase(lookupWebSiteId) ? null : lookupWebSiteId);
            }
        } else {
            handleError(env, null, "The id or name of linked page must be given as parameter \"id\" or \"name\"");
            return null;
        }
        // the name parameter is there, so let's try to find the link
        if (UtilValidate.isNotEmpty(value)) {
            // get page name from directive
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
                    page = getPageByName(delegator, lookupFields, webSiteLookupMode, useCache);
                }
                if (page == null) {
                    throw new IllegalArgumentException("Could not locate page using fields: " + lookupFields.toString());
                }
                
                String paramStr;
                if (paramsModel == null) {
                    paramStr = null;
                } else if (OfbizFtlObjectType.STRING.isObjectType(paramsModel)) {
                    paramStr = TransformUtil.getStringArg(paramsModel, rawParams);
                } else if (OfbizFtlObjectType.MAP.isObjectType(paramsModel)) {
                    // SPECIAL: we're forced to duplicate the params map for the case where
                    // rawParams=false, to trigger the html auto-escaping
                    TemplateHashModelEx paramsHashModel = (TemplateHashModelEx) paramsModel;
                    paramStr = TemplateFtlUtil.makeParamString(paramsHashModel, paramDelim, rawParams);
                } else {
                    throw new IllegalArgumentException("pageUrl params: expect string or map, but instead got: " + paramsModel.getClass());
                }
                
                if (extLoginKey == Boolean.TRUE) {
                    // FIXME: kludge
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
                handleError(env, e, "Could not build link to CMS page with: " + lookupFields.toString());
                return null;
            }
        }
        return output;
    }
    
    /**
     * Gets page by name and webSiteId.
     */
    private static CmsPage getPageByName(Delegator delegator, Map<String, Object> lookupFields, String webSiteLookupMode, boolean useCache) throws GenericEntityException {
        boolean webSiteIdLegacyLookup = true;
        boolean webSiteIdMappingsLookup = true;
        if (UtilValidate.isNotEmpty(webSiteLookupMode)) {
            if ("default-only".equals(webSiteLookupMode)) {
                webSiteIdMappingsLookup = false;
            } else if ("mappings-only".equals(webSiteLookupMode)) {
                webSiteIdLegacyLookup = false;
            }
        }
        
        // 2016: double-lookup:
        // do a manual fast cached DB lookup to find the pageId, and afterward re-query
        // the CmsPage using using the findById call - 
        // this is probably the best way to reuse the memory instances (fewer duplicates)
        GenericValue pageValue = null;
        if (webSiteIdLegacyLookup) {
            List<GenericValue> pageValues = delegator.findByAnd("CmsPage", lookupFields, null, useCache);
            pageValue = EntityUtil.getFirst(pageValues);
            if (pageValues.size() > 1) {
                Debug.logWarning("Cms: Page link: more than one CmsPage found for lookup fields " + lookupFields.toString()
                        + "; using first only (id: " + pageValue.getPkShortValueString() + ")", module);
            }
        }
        if (pageValue == null && webSiteIdMappingsLookup && lookupFields.containsKey("webSiteId")) {
            Map<String, Object> mappingsFields = new HashMap<>();
            mappingsFields.put("pageName", lookupFields.get("pageName"));
            mappingsFields.put("sourceWebSiteId", lookupFields.get("webSiteId"));
            List<GenericValue> pageValues = delegator.findByAnd("CmsPageAndPrimaryProcessMapping", mappingsFields, null, useCache);
            pageValue = EntityUtil.getFirst(pageValues);
            if (pageValues.size() > 1) {
                Debug.logWarning("Cms: Page link: more than one CmsPageAndPrimaryProcessMapping found for lookup fields " + lookupFields.toString()
                        + "; using first only (id: " + pageValue.getPkShortValueString() + ")", module);
            }
        }
        CmsPage page = null;
        if (pageValue != null) {
            page = CmsPage.getWorker().findById(delegator, pageValue.getString("pageId"), useCache);
        }
        return page;
    } 
    
    private static CmsPage getPageByNameAlways(Delegator delegator, Map<String, Object> lookupFields, String webSiteLookupMode, boolean useCache) throws GenericEntityException {
        CmsPage page = getPageByName(delegator, lookupFields, webSiteLookupMode, useCache);
        if (page == null) {
            throw new IllegalArgumentException("Could not locate page using fields: " + lookupFields.toString());
        }
        return page;
    }

    /**
     * Builds the URL string for a page, by wrapping around and emulating
     * the standard RequestHandler.makeLinkAuto call.
     * <p>
     * NOTE: like RequestHandler.makeLinkAuto, this version currently does not throw exceptions.
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
     * NOTE: like RequestHandler.makeLinkAuto, this version currently does not throw exceptions.
     * <p>
     * 2016: FIXME: the webSiteId portion of this lookup may be considered incomplete because this does not
     * check the primary process mappings for the page, only the legacy CmsPage.webSiteId field.
     *
     * @param context the current page context (may be created from context for non-CMS renderers)
     * @param pageId the target page to link to
     * @return complete url with protocol scheme and port (if necessary)
     */
    public static String makeLinkAutoByName(CmsPageContext context, String pageName, Boolean interWebapp, String webSiteId,
            Boolean fullPath, Boolean secure, Boolean encode, String paramStr, String webSiteLookupMode) {
        try {  
            final boolean useCache = !context.isPreview();
            CmsPage page = getPageByNameAlways(context.getDelegator(), UtilMisc.<String, Object> toMap("pageName", pageName, "webSiteId", webSiteId), webSiteLookupMode, useCache);
            return makeLinkAutoEx(context, page, interWebapp, webSiteId, fullPath, secure, encode, paramStr);
        } catch (Exception e) {
            final String errMsg = "Could not create link to page with name '" + pageName + "' for webSiteId '" + webSiteId + "': " + e.getMessage();
            Debug.logError(e, "Cms: " + errMsg, module);
            return null;
        }
    }
    
    public static String makeLinkAutoEx(CmsPageContext context, CmsPage page, Boolean interWebapp, String webSiteId,
            Boolean fullPath, Boolean secure, Boolean encode, String paramStr) {
        String path = page.getPrimaryPathExpanded(UtilValidate.isNotEmpty(webSiteId) ? webSiteId : context.getWebSiteId());
        if (path == null) {
            throw new IllegalArgumentException("Page '" + page.getId() + "' does not have a primary path "
                    + "for webSiteId '" + webSiteId + "'; cannot build link");
        }
        
        if (paramStr != null && !paramStr.isEmpty()) {
            if (paramStr.startsWith("?")) {
                path += paramStr;
            } else {
                path += "?" + paramStr;
            }
        }
        
        final boolean absUrl = false; // the page path (sourcePath) is never from server root; always relative to context root
        final boolean controller = false; // TODO?: no way to exploit controller currently, not sure how could...
        
        // 2016: FIXME: this should be calling RequestHandler.makeLinkAutoEx, but no such method exists yet...
        return RequestHandler.makeLinkAuto(context.getRequest(), context.getResponse(),
                path, absUrl, interWebapp, webSiteId, controller, fullPath, secure, encode);
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
}