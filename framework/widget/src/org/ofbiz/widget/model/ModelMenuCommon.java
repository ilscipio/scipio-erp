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

import org.ofbiz.widget.model.ModelMenuNode.ModelMenuItemGroupNode;
import org.w3c.dom.Element;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

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

    public abstract Map<String, ModelMenuItem.ModelMenuItemAlias> getMenuItemAliasMap();

    public abstract Map<String, String> getMenuItemNameAliasMap();

    public List<String> getItemNamesAliasedTo(String forName) {
        List<String> names = new ArrayList<>();
        for (ModelMenuItem.ModelMenuItemAlias itemAlias : getMenuItemAliasMap().values()) {
            if (Objects.equals(forName, itemAlias.getForName())) {
                names.add(itemAlias.getName());
            }
        }
        return names;
    }

    /**
     * SCIPIO: NOTE: only valid if the sub-menus were part of the same ModelMenu instance.
     */
    public boolean isSame(ModelMenuCommon menu) {
        return (this == menu); // SCIPIO: NOTE: this works because we know how the ModelMenu was built
    }

}
