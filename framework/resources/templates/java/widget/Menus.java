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
package @component-package@.@component-name@.widget;

import com.ilscipio.scipio.widget.def.menu.*;

/**
 * Menu definitions for the @component-resource-name@ component.
 *
 * <p>SCIPIO: 4.0.0: Added by createComponent template.</p>
 */
public class @component-resource-name@Menus {

    @Menu(
        name = "MainAppBar",
        location = "component://@component-name@/widget/@component-resource-name@Menus.xml",
        title = "${uiLabelMap.@component-resource-name@Application}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "main", title = "${uiLabelMap.CommonMain}", link = @MenuLink(target = "main"))
        }
    )
    public interface MainAppBar {}

}
