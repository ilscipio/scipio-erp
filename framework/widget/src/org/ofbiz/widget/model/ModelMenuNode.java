package org.ofbiz.widget.model;

import java.io.Serializable;
import java.util.List;

import org.ofbiz.base.util.string.FlexibleStringExpander;

/**
 * SCIPIO: Represents any node within model menu.
 */
public interface ModelMenuNode extends Serializable {

    ModelMenuNode getParentNode();

    List<? extends ModelMenuNode> getChildrenNodes();

    // These methods return MANUAL per-item controls; they have nothing to do
    // with the context-selected item.
    FlexibleStringExpander getSelected();
    FlexibleStringExpander getDisabled();
    FlexibleStringExpander getExpanded();

    /**
     * SCIPIO: Menu item node.
     */
    public interface ModelMenuItemNode extends ModelMenuNode {

        @Override
        ModelMenuItemGroupNode getParentNode();

        @Override
        List<? extends ModelMenuItemGroupNode> getChildrenNodes();

    }

    /**
     * SCIPIO: Either top menu or sub-menu.
     */
    public interface ModelMenuItemGroupNode extends ModelMenuNode {

        @Override
        ModelMenuItemNode getParentNode();

        @Override
        List<? extends ModelMenuItemNode> getChildrenNodes();

    }

}