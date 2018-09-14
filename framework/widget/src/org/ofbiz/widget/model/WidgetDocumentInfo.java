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

    public static WidgetDocumentInfo retrieve(Element element) {
        return retrieve(element.getOwnerDocument());
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

    public static String getResourceLocation(Element element) {
        WidgetDocumentInfo docInfo = retrieve(element);
        if (docInfo != null) {
            return docInfo.getResourceLocation();
        }
        return null;
    }

    /**
     * Makes description for an element, including tag name, location, and column/line,
     * for log/error messages.
     * FIXME: seems redundant, there is another utility somewhere that does this?
     */
    public static String getElementDescriptor(Element element) {
        String desc = "element: " + element.getTagName();
        String location = getResourceLocation(element);
        if (location != null) {
            desc += ", location: " + location;
        }
        Integer startLine = (Integer) element.getUserData("startLine");
        if (startLine != null) {
            desc += ", line: " + startLine;
        }
        Integer startColumn = (Integer) element.getUserData("startColumn");
        if (startColumn != null) {
            desc += ", column: " + startColumn;
        }
        return desc;
    }
}