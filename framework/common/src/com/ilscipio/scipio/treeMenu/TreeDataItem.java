package com.ilscipio.scipio.treeMenu;

/**
 * SCIPIO: An interface representing a tree item. All tree menu libraries
 * integration must implement this as a representation of a data item in order
 * to use the existing services and events available to populate and render tree
 * menus.
 *
 * @author jsoto
 *
 */
public interface TreeDataItem {

    /**
     * Gets the id of the tree data item (all third party libs surely require an
     * id)
     *
     * @return
     */
    public String getId();

    /**
     * Sets the id of the tree data item
     *
     * @param id
     */
    public void setId(String id);

    /**
     *
     * @return
     */
//    public List<TreeDataItem> getChildren();
//
//    /**
//     *
//     * @param children
//     */
//    public void setChildren(List<TreeDataItem> children);

//    /**
//     * The type of the item. If the third party library doesn't support types
//     * for their tree items, override it but do nothing there.
//     *
//     * @param type
//     */
//    public void setType(String type);

//    /**
//     * Since a tree menu may contain multiple occurrences of the same item, mark
//     * it so
//     */
//    public void setMultipleOccurrences(boolean multipleOccurrences);

}