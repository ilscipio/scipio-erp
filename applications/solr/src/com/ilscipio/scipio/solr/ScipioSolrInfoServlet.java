package com.ilscipio.scipio.solr;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

import org.ofbiz.base.util.Debug;

/**
 * Scipio Solr info/helper servlet.
 * <p>
 * WARN: 2018-05-22: This should no longer be used to check Solr initialization state;
 * use {@link com.ilscipio.scipio.solr.SolrUtil#isSystemInitialized()} instead.
 */
@SuppressWarnings("serial")
public class ScipioSolrInfoServlet extends HttpServlet {

    public static final String module = ScipioSolrInfoServlet.class.getName();
    
    /**
     * Used to deduce whether the solr webapp initialization step was reached
     * during Ofbiz initialization.
     */
    private static volatile boolean servletInitStatusReached = false;
    
    @Override
    public void init(ServletConfig config) throws ServletException {
        super.init(config);
        boolean firstInit = setServletInitStatusReached();
        if (!firstInit) {
            Debug.logInfo("Solr: ScipioSolrInfoServlet: (Non-first) servlet init executed", module);
        }
    }
    
    private static boolean setServletInitStatusReached() {
        if (!isServletInitStatusReached()) {
            synchronized(ScipioSolrInfoServlet.class) {
                if (!isServletInitStatusReached()) {
                    Debug.logInfo("Solr: ScipioSolrInfoServlet: First servlet init executed", module);
                    servletInitStatusReached = true;
                    return true;
                }
            }
        } 
        return false;
    }
    
    /**
     * Checks if the init method was called for any instance of this servlet.
     * <p>
     * WARN: 2018-05-22: This should no longer be used to check Solr initialization state;
     * use {@link com.ilscipio.scipio.solr.SolrUtil#isSystemInitialized()} instead.
     */
    public static boolean isServletInitStatusReached() {
        return servletInitStatusReached;
    }
    
}
