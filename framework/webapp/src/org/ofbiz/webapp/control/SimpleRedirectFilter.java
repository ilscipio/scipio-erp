package org.ofbiz.webapp.control;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * SCIPIO: SimpleRedirectFilter - Redirects all incoming requests match an incoming
 * path to a target.
 */
public class SimpleRedirectFilter implements Filter {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected FilterConfig config = null;

    protected Pattern matchPattern = null;
    protected String replacement = null;

    protected boolean usePermanentRedirect = true;
    protected boolean matchFullPath = true;


    /**
     * @see javax.servlet.Filter#init(javax.servlet.FilterConfig)
     */
    public void init(FilterConfig config) throws ServletException {
        this.config = config;

        String matchPatternStr = config.getInitParameter("matchPattern");
        if (matchPatternStr != null && !matchPatternStr.isEmpty()) {
            matchPattern = Pattern.compile(matchPatternStr);
        }

        replacement = config.getInitParameter("replacement");

        usePermanentRedirect = !"false".equals(config.getInitParameter("usePermanentRedirect"));

        matchFullPath = "true".equals(config.getInitParameter("matchFullPath"));
    }

    /**
     * @see javax.servlet.Filter#doFilter(javax.servlet.ServletRequest,
     *      javax.servlet.ServletResponse, javax.servlet.FilterChain)
     */
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;

        if (matchPattern != null && replacement != null) {
            String matchPath;
            // FIXME: These values are all still URL-encoded!
            if (matchFullPath) {
                matchPath = httpRequest.getRequestURI();
                if (httpRequest.getQueryString() != null) {
                    matchPath += "?" + httpRequest.getQueryString();
                }
            } else {
                matchPath = httpRequest.getContextPath();
            }

            Matcher m = matchPattern.matcher(matchPath);
            if (m.find()) {
                StringBuffer fullTargetSb = getFullHost(httpRequest);
                m.appendReplacement(fullTargetSb, replacement);
                m.appendTail(fullTargetSb);
                if (!matchFullPath) {
                    fullTargetSb.append(getFullPostContextPath(httpRequest).toString());
                }

                // SCIPIO: NOTE: It's possible this should be encodeURL + strip jsessionid instead (even if redirect)...
                String fullTarget = httpResponse.encodeRedirectURL(fullTargetSb.toString());

                if (usePermanentRedirect) {
                    httpResponse.setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY);
                    httpResponse.setHeader("Location", fullTarget);
                    return;
                } else {
                    httpResponse.sendRedirect(fullTarget);
                    return;
                }
            }
        }

        // we're done checking; continue on
        chain.doFilter(request, response);
    }

    /**
     * @see javax.servlet.Filter#destroy()
     */
    public void destroy() {
        config = null;
    }

    public static StringBuffer getFullPostContextPath(HttpServletRequest request) {
        String contextPath = request.getContextPath();
        String contextPathWithDelim = contextPath;
        if (contextPathWithDelim.length() > 0 && !contextPathWithDelim.endsWith("/")) {
            contextPathWithDelim += "/"; // needed for startswith check
        }
        boolean contextPathIsRoot = (contextPath.length() == 0 || "/".equals(contextPath)); // "/" is just in case, shouldn't happen

        String requestUri = request.getRequestURI();

        // SANITY CHECK: if request URI doesn't start with contextPath, just return nothing
        if (!contextPathIsRoot && !requestUri.equals(contextPath) && !requestUri.startsWith(contextPathWithDelim)) {
            return new StringBuffer();
        }

        StringBuffer postContextPath;
        if (!contextPathIsRoot) {
            postContextPath = new StringBuffer(requestUri.substring(contextPath.length()));
        } else {
            postContextPath = new StringBuffer(requestUri);
        }

        if (request.getQueryString() != null) {
            postContextPath.append("?");
            postContextPath.append(request.getQueryString());
        }
        return postContextPath;
    }

    public static StringBuffer getFullHost(HttpServletRequest request) {
        StringBuffer url = new StringBuffer();
        String scheme = request.getScheme();
        int port = request.getServerPort();
        if (port < 0) {
            port = 80;
        }

        url.append(scheme);
        url.append("://");
        url.append(request.getServerName());
        if ((scheme.equals("http") && (port != 80))
            || (scheme.equals("https") && (port != 443))) {
            url.append(':');
            url.append(port);
        }

        return url;
    }

}
