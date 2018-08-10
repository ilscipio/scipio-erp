package com.ilscipio.scipio.ce.webapp.filter.urlrewrite.reqwrap;

import java.io.IOException;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.webapp.FullWebappInfo;
import org.tuckey.web.filters.urlrewrite.Conf;
import org.tuckey.web.filters.urlrewrite.UrlRewriteWrappedResponse;
import org.tuckey.web.filters.urlrewrite.UrlRewriter;

import com.ilscipio.scipio.ce.webapp.filter.UrlFilterHelper;
import com.ilscipio.scipio.ce.webapp.filter.urlrewrite.ScipioUrlRewriter;
import com.ilscipio.scipio.ce.webapp.filter.urlrewrite.UrlConfUtil;

/**
 * A very basic rewriter that simply wraps the HttpServletRequest for
 * an inter-webapp render attempt.
 */
public class WrappedRequestUrlRewriter extends ScipioUrlRewriter {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final String urlConfPath;
    protected final Conf conf;
    
    // tuckey objects
    protected final UrlRewriter tuckRewriter;
    //protected final UrlRewriteWrappedResponse tuckResponse;
    
    protected final FullWebappInfo webappInfo;
    
    public WrappedRequestUrlRewriter(String urlConfPath, Conf conf, 
            FullWebappInfo webappInfo) throws IOException {
        this.urlConfPath = urlConfPath;
        this.conf = conf;
        this.tuckRewriter = new UrlRewriter(conf);
        //this.tuckResponse = new UrlRewriteWrappedResponse(this.container.getResponse(),
        //        this.container.getRequest(), this.tuckRewriter);
        this.webappInfo = webappInfo;
    }
    
    public static class WrappedRequestFactory implements ScipioUrlRewriter.UrlRewriterFactory {

        @Override
        public ScipioUrlRewriter loadForRequest(FullWebappInfo webappInfo, String urlConfPath,
                HttpServletRequest request, HttpServletResponse response) throws IOException {
            Conf conf = UrlConfUtil.getConfFromLocationOrWebapp(urlConfPath, webappInfo.getExtWebappInfo());

            return new WrappedRequestUrlRewriter(urlConfPath, conf, webappInfo);
        }

        @Override
        public ScipioUrlRewriter loadForContext(FullWebappInfo webappInfo, String urlConfPath,
                Map<String, Object> context) throws IOException {
            throw new UnsupportedOperationException();
        }
        
    }

    @Override
    public String processOutboundUrl(String url, FullWebappInfo urlWebappInfo, Map<String, Object> context) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String processOutboundUrl(String url, FullWebappInfo urlWebappInfo, HttpServletRequest request, HttpServletResponse response) {
        if (Debug.verboseOn()) {
            Debug.logVerbose("urlrewrite: processing outbound url for " + url, module);
        }
        try {
            HttpServletRequest wrappedRequest = new WrappedHttpServletRequest(request);
            
            // NOTE: because attribs are buffered, don't need to unset this in finally block
            wrappedRequest.setAttribute(UrlFilterHelper.URLREWRITE_CONF_WEBAPP, webappInfo);
            wrappedRequest.setAttribute(UrlFilterHelper.OUT_URL_WEBAPP, urlWebappInfo);
            
            return new UrlRewriteWrappedResponse(response, 
                    wrappedRequest, 
                    tuckRewriter).encodeURL(url);
        } catch(Exception e) {
            Debug.logError(e, "Error encoding url '"
                    + url + "' using outbound-rules from " + urlConfPath + ": " + e.getMessage(), module);
            return url;
        }
    }

}
