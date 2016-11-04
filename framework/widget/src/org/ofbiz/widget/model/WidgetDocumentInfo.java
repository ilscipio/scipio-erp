package org.ofbiz.widget.model;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * SCIPIO: Stores extra information as user data in Document instances for
 * reference during model building.
 */
public class WidgetDocumentInfo {
    
    public static final String DOCUMENT_USER_DATA_KEY = "widgetDocInfo";
    
    private String resourceLocation = null; // original resource location in component:// format

    public static WidgetDocumentInfo retrieve(Document document) {
        return (WidgetDocumentInfo) document.getUserData(DOCUMENT_USER_DATA_KEY);
    }
    
    public static WidgetDocumentInfo retrieveAlways(Document document) {
        WidgetDocumentInfo docInfo = retrieve(document);
        if (docInfo == null) {
            docInfo = new WidgetDocumentInfo();
            document.setUserData(DOCUMENT_USER_DATA_KEY, docInfo, null);
        }
        return docInfo;
    }
    
    public static WidgetDocumentInfo retrieveAlways(Element element) {
        return retrieveAlways(element.getOwnerDocument());
    }
    
    public String getResourceLocation() {
        return resourceLocation;
    }

    public void setResourceLocation(String resourceLocation) {
        this.resourceLocation = resourceLocation;
    }

}