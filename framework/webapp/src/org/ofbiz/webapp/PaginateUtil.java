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
    
    /**
     * Cato: Gets global default view size.
     */
    public static int getViewSizeDefault() {
        return UtilProperties.getPropertyAsInteger("general.properties", "paginate.viewSize.default", DEFAULT_VIEW_SIZE_DEFAULT);
    }
    
    /**
     * Cato: Gets global override view size.
     */
    public static int getViewSizeOverride() {
        return UtilProperties.getPropertyAsInteger("general.properties", "paginate.viewSize.override", DEFAULT_VIEW_SIZE_OVERRIDE);
    }
    
    
    /**
     * Cato: Checks the passed view size and if not valid, returns a global default or override.
     * 
     * @param viewSize the tentative view size determined by caller
     * @param prioViewSize caller may request his view size to have priority (default false)
     * @param defaultViewSize optional default view size; if null or invalid, uses system default
     */
    public static int checkGetViewSize(HttpServletRequest request, GenericValue userLogin, Integer viewSize, Boolean prioViewSize, Integer defaultViewSize) {
        // TODO?: a default could be stored in session or per-user
        return checkGetViewSize(viewSize, prioViewSize, defaultViewSize);
    }
    
    public static int checkGetViewSize(Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin, Integer viewSize, Boolean prioViewSize, Integer defaultViewSize) {
        // TODO?: a default could be stored per-user
        return checkGetViewSize(viewSize, prioViewSize, defaultViewSize);
    }    
    
    /**
     * <strong>AVOID CALLING THIS METHOD</strong>; pass Delegator or Request to other methods instead.
     */
    public static int checkGetViewSize(Integer viewSize, Boolean prioViewSize, Integer defaultViewSize) {
        if (viewSize == null || viewSize <= 0) {
            if (defaultViewSize != null && defaultViewSize > 0) {
                viewSize = defaultViewSize;
            }
            else {
                viewSize = getViewSizeDefault();
            }
        }
        return processViewSizeOverride(viewSize, prioViewSize);
    }        
    
    
    
    /**
     * Cato: Decides what the final view size should be after all other selections have been
     * made based on override.
     * 
     * @param viewSize the tentative view size determined by caller
     * @param prioViewSize caller may request his view size to have priority (default false)
     */
    public static int processViewSizeOverride(Integer viewSize, Boolean prioViewSize) {
        if (viewSize == null) {
            viewSize = -1;
        }
        if (prioViewSize == null) {
            prioViewSize = false;
        }
        String sysDefaultMode = UtilProperties.getPropertyValue("general.properties", "paginate.viewSize.override.mode", "disabled");
        if ("force".equals(sysDefaultMode)) {
            // here, disregard prioViewSize, always use sys default
            return getViewSizeOverride();
        }
        else if ("prefer".equals(sysDefaultMode)) {
            // here we honor prioViewSize, but if not requested, we prefer the default
            if (prioViewSize) {
                if (viewSize > 0) {
                    return viewSize;
                }
                else {
                    return getViewSizeOverride();
                }
            }
            else {
                return getViewSizeOverride();
            }
        }
        else if ("fallback".equals(sysDefaultMode)) {
            if (viewSize > 0) {
                return viewSize;
            }
            else {
                return getViewSizeOverride();
            }
        }
        else {
            return viewSize;
        }
    }
    
}
