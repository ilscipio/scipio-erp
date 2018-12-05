package org.ofbiz.webapp.website;

import org.ofbiz.entity.GenericEntityException;

public class WebSiteEntityNotFoundException extends GenericEntityException {

    private static final long serialVersionUID = -5601192509173423208L;

    private final String webSiteId;

    public WebSiteEntityNotFoundException(String webSiteId) {
        this.webSiteId = webSiteId;
    }

    public WebSiteEntityNotFoundException(String message, String webSiteId) {
        super(message);
        this.webSiteId = webSiteId;
    }

    public WebSiteEntityNotFoundException(String message, Throwable cause, String webSiteId) {
        super(message, cause);
        this.webSiteId = webSiteId;
    }

    public WebSiteEntityNotFoundException(Throwable cause, String webSiteId) {
        super(cause);
        this.webSiteId = webSiteId;
    }

    public String getWebSiteId() {
        return webSiteId;
    }

}
