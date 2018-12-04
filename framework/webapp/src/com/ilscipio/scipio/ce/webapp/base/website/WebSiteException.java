package com.ilscipio.scipio.ce.webapp.base.website;

public class WebSiteException extends Exception {

    private static final long serialVersionUID = -5601192509173423208L;

    private final String webSiteId;

    public WebSiteException(String webSiteId) {
        this.webSiteId = webSiteId;
    }

    public WebSiteException(String message, String webSiteId) {
        super(message);
        this.webSiteId = webSiteId;
    }

    public WebSiteException(String message, Throwable cause, String webSiteId) {
        super(message, cause);
        this.webSiteId = webSiteId;
    }

    public WebSiteException(Throwable cause, String webSiteId) {
        super(cause);
        this.webSiteId = webSiteId;
    }

    public String getWebSiteId() {
        return webSiteId;
    }

}
