package com.ilscipio.scipio.product.seo;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.product.category.CatalogUrlServlet;
import org.ofbiz.webapp.control.ContextFilter;

/**
 * SCIPIO: 2017: Seo catalog URL filter.
 * <p>
 * FIXME: this should not extend ContextFilter, is an ofbiz design issue
 * compat workaround, for now ensures delegator is available.
 * WARN: base class will be changed to Filter interface in future!
 */
public class SeoCatalogUrlFilter extends ContextFilter {

    public static final String module = SeoCatalogUrlFilter.class.getName();
    
    public static final String CONTROL_MOUNT_POINT = CatalogUrlServlet.CONTROL_MOUNT_POINT;
    public static final String PRODUCT_REQUEST = CatalogUrlServlet.PRODUCT_REQUEST;
    public static final String CATEGORY_REQUEST = CatalogUrlServlet.CATEGORY_REQUEST;
    public static final String CATALOG_URL_MOUNT_POINT = CatalogUrlServlet.CATALOG_URL_MOUNT_POINT;
    
    public static final String FORWARDED_ATTR = "_SCPSEPCATURLFLTR_FWD_";
    
    protected String defaultLocaleString = null;
    protected String redirectUrl = null;
    protected String controlPrefix = null;

    @Override
    public void init(FilterConfig config) throws ServletException {
        super.init(config);
        
        String initDefaultLocalesString = config.getInitParameter("defaultLocaleString");
        String initRedirectUrl = config.getInitParameter("redirectUrl");
        defaultLocaleString = UtilValidate.isNotEmpty(initDefaultLocalesString) ? initDefaultLocalesString : "";
        redirectUrl = UtilValidate.isNotEmpty(initRedirectUrl) ? initRedirectUrl : "";
        
        if (UtilValidate.isNotEmpty(CONTROL_MOUNT_POINT)) {
            controlPrefix = "/" + controlPrefix;
        } else {
            controlPrefix = "";
        }
        
        SeoConfigUtil.init();
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        doFilter((HttpServletRequest) request, (HttpServletResponse) response, chain);
    }
    
    protected void doFilter(HttpServletRequest request, HttpServletResponse response, FilterChain chain) throws IOException, ServletException {
        Delegator delegator = getDelegatorForControl(request, request.getServletContext());

        boolean alreadyRun = Boolean.TRUE.equals(request.getAttribute(FORWARDED_ATTR));
        
        boolean handleRequest = false && !alreadyRun;
        if (handleRequest) {
            
            // TODO
            
            /*
            // OLD CODE: (no good anymore)
            String pathInfo = request.getPathInfo();
            List<String> pathElements = StringUtil.split(pathInfo, "/");
            if (pathElements == null) {
                pathElements = new ArrayList<String>();
            }
            
            // look for productId
            String productId = null;
            try {
                if (pathElements.size() > 0) {
                    String lastPathElement = pathElements.get(pathElements.size() - 1);
                    if (lastPathElement.startsWith("p_") || delegator.findOne("Product", UtilMisc.toMap("productId", lastPathElement), true) != null) {
                        if (lastPathElement.startsWith("p_")) {
                            productId = lastPathElement.substring(2);
                        } else {
                            productId = lastPathElement;
                        }
                        pathElements.remove(pathElements.size() - 1);
                    }
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, "Error looking up product info for ProductUrl with path info [" + pathInfo + "]: " + e.toString(), module);
            }
    
            // get category info going with the IDs that remain
            String categoryId = null;
            if (pathElements.size() == 1) {
                CategoryWorker.setTrail(request, pathElements.get(0), null);
                categoryId = pathElements.get(0);
            } else if (pathElements.size() == 2) {
                CategoryWorker.setTrail(request, pathElements.get(1), pathElements.get(0));
                categoryId = pathElements.get(1);
            } else if (pathElements.size() > 2) {
                List<String> trail = CategoryWorker.getTrail(request);
                if (trail == null) {
                    trail = new ArrayList<String>();
                }
    
                if (trail.contains(pathElements.get(0))) {
                    // first category is in the trail, so remove it everything after that and fill it in with the list from the pathInfo
                    int firstElementIndex = trail.indexOf(pathElements.get(0));
                    while (trail.size() > firstElementIndex) {
                        trail.remove(firstElementIndex);
                    }
                    trail.addAll(pathElements);
                } else {
                    // first category is NOT in the trail, so clear out the trail and use the pathElements list
                    trail.clear();
                    trail.addAll(pathElements);
                }
                CategoryWorker.setTrail(request, trail);
                categoryId = pathElements.get(pathElements.size() - 1);
            }
            if (categoryId != null) {
                request.setAttribute("productCategoryId", categoryId);
            }
    
            String rootCategoryId = null;
            if (pathElements.size() >= 1) {
                rootCategoryId = pathElements.get(0);
            }
            if (rootCategoryId != null) {
                request.setAttribute("rootCategoryId", rootCategoryId);
            }
    
            if (productId != null) {
                request.setAttribute("product_id", productId);
                request.setAttribute("productId", productId);
            }
    
            request.setAttribute(FORWARDED_ATTR, Boolean.TRUE);
            RequestDispatcher rd = request.getRequestDispatcher(controlPrefix
                    + (productId != null ? PRODUCT_REQUEST : CATEGORY_REQUEST));
            rd.forward(request, response);
            return;
            */
        }
        
        chain.doFilter(request, response);
    }
    
    public static class SeoCatalogUrlResponseWrapper extends HttpServletResponseWrapper {
        private final HttpServletRequest request;
        
        public SeoCatalogUrlResponseWrapper(HttpServletRequest request, HttpServletResponse response) {
            super(response);
            this.request = request;
        }
        
        @Override
        public String encodeURL(String url) {
            return super.encodeURL(url);
        }

        @Override
        public String encodeRedirectURL(String url) {
            return super.encodeRedirectURL(url);
        }

        @Override
        public void sendRedirect(String location) throws IOException {
            super.sendRedirect(location);
        }
    }
    
    
    /**
     * Special delegator lookup for filters which may run early in a chain; in this case,
     * request.getAttribute("delegator") may return nothing because 
     * ControlFilter/ControlServlet/LoginWorker not yet run.
     * <p>
     * FIXME?: This may be one request late for tenant delegator switches.
     * <p>
     * DEV NOTE: this is copied from CMS. TODO: move to a common util
     * 
     * @param request
     * @param servletContext
     */
    protected static Delegator getDelegatorForControl(HttpServletRequest request, ServletContext servletContext) {
        Delegator delegator = null;
        
        // Check request attribs
        delegator = (Delegator) request.getAttribute("delegator");
        if (delegator != null) {
            return delegator;
        }
        
        // Check session attribs (mainly for tenant delegator) - but don't create session if none yet!
        HttpSession session = request.getSession(false);
        if (session != null) {
            String delegatorName = (String) session.getAttribute("delegatorName");
            if (UtilValidate.isNotEmpty(delegatorName)) {
                delegator = DelegatorFactory.getDelegator(delegatorName);
                if (delegator != null) {
                    return delegator;
                } else {
                    Debug.logWarning("SCIPIO: SEO: ERROR: could not get session delegator for control/filter; " +
                            "delegator factory returned null for session delegatorName \"" + 
                            delegatorName + "\"; defaulting to servlet or default delegator", module);
                }
            }
        }
        
        // Check servlet context
        delegator = (Delegator) servletContext.getAttribute("delegator");
        if (delegator != null) {
            return delegator;
        }
        
        // Last resort: default delegator
        delegator = DelegatorFactory.getDelegator("default");
        
        if (delegator == null) {
            Debug.logError("SCIPIO: SEO: ERROR: could not get any delegator for control/filter!", module);
        }
        return delegator;
    }
}
