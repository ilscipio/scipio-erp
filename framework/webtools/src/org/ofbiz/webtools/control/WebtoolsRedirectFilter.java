package org.ofbiz.webtools.control;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilValidate;

/**
 * WebtoolsRedirectFilter - Redirects all incoming requests form the original
 * 'webtools' context path to the context defined in redirectContextPath filter
 * initParameter
 */
public class WebtoolsRedirectFilter implements Filter {

    public static final String module = WebtoolsRedirectFilter.class.getName();

    protected FilterConfig config = null;

    /**
     * @see javax.servlet.Filter#init(javax.servlet.FilterConfig)
     */
    public void init(FilterConfig config) throws ServletException {
        this.config = config;
    }

    /**
     * @see javax.servlet.Filter#doFilter(javax.servlet.ServletRequest,
     *      javax.servlet.ServletResponse, javax.servlet.FilterChain)
     */
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;
        if (UtilValidate.isNotEmpty(httpRequest.getContextPath()) && httpRequest.getContextPath().equals("/webtools")) {
            if (UtilValidate.isNotEmpty(config.getInitParameter("redirectContextPath"))) {
                httpResponse.sendRedirect(config.getInitParameter("redirectContextPath") + httpRequest.getPathInfo());
            }
        }
        // we're done checking; continue on
        chain.doFilter(request, httpResponse);
    }

    /**
     * @see javax.servlet.Filter#destroy()
     */
    public void destroy() {
        config = null;
    }

}
