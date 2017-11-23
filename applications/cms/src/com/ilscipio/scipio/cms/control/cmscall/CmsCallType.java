package com.ilscipio.scipio.cms.control.cmscall;

public enum CmsCallType {
    // PAGE_INFO,
    
    OFBIZ_RENDER,
    OFBIZ_PREVIEW, // new 2016
    CMS_ADMIN_APP,
    CMS_STATIC_RENDER;
    
    public boolean cachingAllowed() {
        // 2016: MUST NOT CACHE PREVIEWS!
        return this != OFBIZ_PREVIEW;
    }
    
    public boolean isPreview() {
        return this == OFBIZ_PREVIEW;
    }
    
    public boolean representCaller() {
        if (this == OFBIZ_RENDER || this == OFBIZ_PREVIEW) {
            return true;
        }
        else {
            return false;
        }
    }
    
    public boolean requiresServerRequest() {
        if (this == OFBIZ_RENDER || this == OFBIZ_PREVIEW || this == CMS_STATIC_RENDER) {
            return true;
        }
        else {
            return false;
        }
    }
    
    public boolean supportsServerRequest() {
        if (this == OFBIZ_RENDER || this == OFBIZ_PREVIEW || this == CMS_STATIC_RENDER) {
            return true;
        }
        else {
            return false;
        }
    }
    
    public boolean requiresCallParams() {
        if (this == OFBIZ_RENDER || this == OFBIZ_PREVIEW) {
            return true;
        }
        else {
            return false;
        }
    }
}
