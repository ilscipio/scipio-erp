package com.ilscipio.scipio.solr;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

import org.ofbiz.base.util.Debug;

/**
 * Ofbiz Solr info/helper servlet.
 */
@SuppressWarnings("serial")
public class OfbizSolrInfoServlet extends HttpServlet {

    public static final String module = OfbizSolrInfoServlet.class.getName();
    
    /**
     * Used to deduce whether the solr webapp initialization step was reached
     * during Ofbiz initialization.
     */
    private static volatile boolean servletInitStatusReached = false;
    
    public OfbizSolrInfoServlet() {
        super();
    }
    
    /**
     * @see javax.servlet.Servlet#init(javax.servlet.ServletConfig)
     */
    @Override
    public void init(ServletConfig config) throws ServletException {
        super.init(config);
        boolean firstInit = setServletInitStatusReached();
        if (!firstInit) {
            Debug.logInfo("Solr: OfbizSolrInfoServlet: (Non-first) servlet init executed", module);
        }
    }
    
    private static boolean setServletInitStatusReached() {
        if (!isServletInitStatusReached()) {
            Debug.logInfo("Solr: OfbizSolrInfoServlet: First servlet init executed", module);
            servletInitStatusReached = true;
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * Checks if the init method was called for any instance of this servlet.
     * <p>
     * This can be used as a workaround to detect the approximate point at which the webapp was loaded.
     */
    public static boolean isServletInitStatusReached() {
        return servletInitStatusReached;
    }
    
}
