package com.ilscipio.scipio.cms.content;

/**
 * Bean-like in-memory representation of a Cms page.
 *
 * @see CmsPage
 */
public class CmsPageInfo {

    //private String webSiteId;
    private String pageId;

    public CmsPageInfo(CmsPage mapping) {
        //this.webSiteId = mapping.getWebSiteId();
        this.pageId = mapping.getId();
    }

    public CmsPageInfo(String pageId) {
        super();
        this.pageId = pageId;
    }

    public String getPageId() {
        return pageId;
    }

//    public String getWebSiteId() {
//        return webSiteId;
//    }

    public String getLogIdRepr() {
        return CmsPage.getLogIdRepr(pageId, null);
    }

    public String getLogIdReprTargetPage() {
        return CmsPage.getLogIdRepr(pageId, null);
    }

}
