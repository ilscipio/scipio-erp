package com.ilscipio.scipio.ce.webapp.filter.urlrewrite;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.webapp.FullWebappInfo;

import com.ilscipio.scipio.ce.webapp.filter.urlrewrite.local.LocalUrlRewriter;
import com.ilscipio.scipio.ce.webapp.filter.urlrewrite.reqwrap.WrappedRequestUrlRewriter;

public abstract class ScipioUrlRewriter {

    public static final ScipioUrlRewriter DUMMY = new DummyUrlRewriter();

    // TODO: REVIEW: 2018-08-08: inter-webapp request factory has been tentatively switched to a wrapper factory
    // many implications...

    private static final UrlRewriterFactory requestFactory = new WrappedRequestUrlRewriter.WrappedRequestFactory(); // TODO?: unhardcode in future
    private static final UrlRewriterFactory contextFactory = new LocalUrlRewriter.LocalFactory(); // TODO?: unhardcode in future

    protected ScipioUrlRewriter() {
    }

    public static ScipioUrlRewriter getForRequest(FullWebappInfo webappInfo,
            HttpServletRequest request, HttpServletResponse response, boolean useCache) throws IOException {
        ScipioUrlRewriter rewriter = null;
        Cache cache = null;
        if (useCache) {
            cache = (Cache) request.getAttribute(Cache.CACHE_FIELD);
            if (cache != null) {
                rewriter = cache.getUrlRewriter(webappInfo);
                if (rewriter != null) {
                    return rewriter;
                }
            } else {
                cache = new Cache();
                request.setAttribute(Cache.CACHE_FIELD, cache);
            }
        }
        if (webappInfo.getUrlRewriteRealConfPath() == null) {
            rewriter = DUMMY;
        } else {
            rewriter = getFactory(webappInfo, null, request).loadForRequest(webappInfo, null, request, response);
        }
        if (useCache) {
            cache.addUrlRewriter(webappInfo, rewriter);
        }
        return rewriter;
    }

    /**
     * Loads URL rewriter for webapp using its own urlrewrite file,
     * or dummy rewriter if it has no such file, and caches the rewriter in the passed context if requested (context modified).
     * The context is used to fetch delegator, dispatcher, and other ofbiz context fields.
     * <p>
     * NOTE: Currently (2018-08) this assumes all contexts passed are some kind of
     * static render context (see {@link org.ofbiz.webapp.renderer.RenderEnvType}),
     * and prefers storing the cache in globalContext.
     */
    public static ScipioUrlRewriter getForContext(FullWebappInfo webappInfo,
            Map<String, Object> context, boolean useCache) throws IOException {
        ScipioUrlRewriter rewriter = null;
        Map<String, Object> srcCtx = null;
        Cache cache = null;
        if (useCache) {
            // TODO?: this could try to store cache in "request" attributes as well...
            @SuppressWarnings("unchecked")
            Map<String, Object> globalCtx = (Map<String, Object>) context.get("globalContext");
            srcCtx = globalCtx;
            if (srcCtx == null) {
                srcCtx = context;
            }
            cache = (Cache) srcCtx.get(Cache.CACHE_FIELD);
            if (cache != null) {
                rewriter = cache.getUrlRewriter(webappInfo);
                if (rewriter != null) {
                    return rewriter;
                }
            } else {
                cache = new Cache();
                srcCtx.put(Cache.CACHE_FIELD, cache);
            }
        }
        if (webappInfo.getUrlRewriteRealConfPath() == null) {
            rewriter = DUMMY;
        } else {
            rewriter = getFactory(webappInfo, null, context).loadForContext(webappInfo, null, context);
        }
        if (useCache) {
            cache.addUrlRewriter(webappInfo, rewriter);
        }
        return rewriter;
    }

    /**
     * Loads URL rewriter for webapp using specified urlrewrite file,
     * or dummy rewriter if path is null.
     * The context is used to fetch delegator, dispatcher, and other ofbiz context fields.
     * The context is NOT modified by this overload.
     */
    public static ScipioUrlRewriter getForContext(FullWebappInfo webappInfo,
            String urlConfPath, Map<String, Object> context) throws IOException {
        if (urlConfPath == null || urlConfPath.isEmpty()) {
            return DUMMY;
        }
        return getFactory(webappInfo, urlConfPath, context).loadForContext(webappInfo, urlConfPath, context);
    }

    protected static UrlRewriterFactory getFactory(FullWebappInfo webappInfo,
            String urlConfPath, HttpServletRequest request) {
        return requestFactory;
    }

    protected static UrlRewriterFactory getFactory(FullWebappInfo webappInfo,
            String urlConfPath, Map<String, Object> context) {
        return contextFactory;
    }

    // REMOVED: the caller should make sure to re-pass the context or request he used
    // to make the instance, otherwise we lose flexibility
    //public abstract String processOutboundUrl(String url);

    /**
     * Processes the URL through urlrewrite outbound-rules, derived from a webapp request.
     * Intended mainly for inter-webapp links.
     * <p>
     * urlWebappInfo can be optionally specified as a hint to disambiguate which webapp the link
     * was built for.
     */
    public abstract String processOutboundUrl(String url, FullWebappInfo urlWebappInfo, HttpServletRequest request, HttpServletResponse response);

    /**
     * Processes the URL through urlrewrite outbound-rules, derived from a render context.
     * Intended mainly for emails, sitemap, etc.
     * <p>
     * urlWebappInfo can be optionally specified as a hint to disambiguate which webapp the link
     * was built for.
     */
    public abstract String processOutboundUrl(String url, FullWebappInfo urlWebappInfo, Map<String, Object> context);

    public boolean isPresent() {
        return (this != DUMMY);
    }


    public interface UrlRewriterFactory {
        /**
         * Loads URL rewriter for webapp using specified urlrewrite file.
         * The request is used to fetch delegator, dispatcher, and other ofbiz context fields.
         */
        public ScipioUrlRewriter loadForRequest(FullWebappInfo webappInfo,
                String urlConfPath, HttpServletRequest request, HttpServletResponse response) throws IOException;
        /**
         * Loads URL rewriter for webapp using specified urlrewrite file.
         * The context is used to fetch delegator, dispatcher, and other ofbiz context fields.
         */
        public ScipioUrlRewriter loadForContext(FullWebappInfo webappInfo,
                String urlConfPath, Map<String, Object> context) throws IOException;
    }

    public static class Cache {
        private Map<String, ScipioUrlRewriter> contextPathCache = new HashMap<>();

        public static final String CACHE_FIELD = "scpUrlRewriterCache";

        public ScipioUrlRewriter getUrlRewriter(FullWebappInfo webappInfo) {
            return contextPathCache.get(webappInfo.getContextPath());
        }

        void addUrlRewriter(FullWebappInfo webappInfo, ScipioUrlRewriter rewriter) {
            contextPathCache.put(webappInfo.getContextPath(), rewriter);
        }

    }

    public static class DummyUrlRewriter extends ScipioUrlRewriter {
        private DummyUrlRewriter() {

        }
        //@Override
        //public String processOutboundUrl(String url) {
        //    return url;
        //}
        @Override
        public String processOutboundUrl(String url, FullWebappInfo urlWebappInfo, Map<String, Object> context) {
            return url;
        }
        @Override
        public String processOutboundUrl(String url, FullWebappInfo urlWebappInfo, HttpServletRequest request, HttpServletResponse response) {
            return url;
        }
    }
}
