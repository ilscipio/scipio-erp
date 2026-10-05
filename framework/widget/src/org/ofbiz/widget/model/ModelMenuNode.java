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