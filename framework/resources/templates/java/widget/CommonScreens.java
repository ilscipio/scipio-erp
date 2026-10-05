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

import com.ilscipio.scipio.widget.def.screen.Action;
import com.ilscipio.scipio.widget.def.screen.ActionType;
import com.ilscipio.scipio.widget.def.screen.Screen;

/**
 * Common screens of the @component-resource-name@ webapp.
 *
 * <p>The screen renderer includes {@code component://@component-name@/widget/CommonScreens.xml#webapp-common-actions}
 * before every screen of this webapp (render-init). Put actions that every screen needs here: labels,
 * menu configuration, layout settings.</p>
 */
public class CommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://@component-name@/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "@component-resource-name@UiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", value = "@component-resource-name@", global = true)
    public interface webapp_common_actions {}
}
