package org.ofbiz.webapp;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;

/**
 * Cato: New class to handle pagination stuff like defaults and overrides.
 * <p>
 * This may concern UI elements, entity lookups, and more, because view sizes and such
 * tend to cross all boundaries.
 */
public class PaginateUtil {

    /**
     * Cato: Static default view size. Do not use this; call {@link #getViewSizeDefault()} instead.
     */
    public static final int DEFAULT_VIEW_SIZE_DEFAULT = 10;   
    
    /**
     * Cato: Static default view size. Do not use this; call {@link #getViewSizeOverride()} instead.
     */
    public static final int DEFAULT_VIEW_SIZE_OVERRIDE = 10;
     
    public static int getViewSizeDefault(HttpServletRequest request, GenericValue userLogin) {
        return getViewSizeDefault();
    }
    
    public static int getViewSizeDefault(Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin) {
        return getViewSizeDefault();
    }
    
    /**
     * Gets global view size default. NOTE: prefer calling version with more parameters.
     */
    public static int getViewSizeDefault() {
        return UtilProperties.getPropertyAsInteger("general.properties", "paginate.viewSize.default", DEFAULT_VIEW_SIZE_DEFAULT);
    }
    
    
    public static int getViewSizeDefault(HttpServletRequest request, GenericValue userLogin, 
            String resource, String property, int defaultVal) {
        return getViewSizeDefault(resource, property, defaultVal);
    }
    
    public static int getViewSizeDefault(Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin, 
            String resource, String property, int defaultVal) {
        return getViewSizeDefault(resource, property, defaultVal);
    }
    
    /**
     * Gets a specific property view size default. NOTE: prefer calling version with more parameters.
     */
    public static int getViewSizeDefault(String resource, String property, int defaultVal) {
        return UtilProperties.getPropertyAsInteger(resource, property, defaultVal);
    } 
    
}
