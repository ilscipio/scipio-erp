/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
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