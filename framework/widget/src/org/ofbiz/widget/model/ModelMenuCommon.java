package org.ofbiz.widget.model;

import org.ofbiz.widget.model.ModelMenuNode.ModelMenuItemGroupNode;
import org.w3c.dom.Element;

/**
 * SCIPIO: Implementation common to ModelMenu and ModelSubMenu.
 * <p>
 * TODO: the common impl of ModelMenu and ModelSubMenu should be factored out into here
 * as time goes on...
 */
@SuppressWarnings("serial")
public abstract class ModelMenuCommon extends ModelWidget implements ModelMenuItemGroupNode {

    public ModelMenuCommon(String name) {
        super(name);
    }

    public ModelMenuCommon(Element widgetElement, String name) {
        super(widgetElement, name);
    }

    public ModelMenuCommon(Element widgetElement) {
        super(widgetElement);
    }

}
