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
/**
 * SCIPIO: Gets the visual theme resources into layoutSettings global using default logic.
 */

// The stock behavior is this:
// <set field="visualThemeId" from-field="userPreferences.VISUAL_THEME" global="true"/>
// <service service-name="getVisualThemeResources">
//     <field-map field-name="visualThemeId"/>
//     <field-map field-name="themeResources" from-field="layoutSettings"/>
// </service>
// <set field="layoutSettings" from-field="themeResources" default-value="${layoutSettings}" global="true"/>
//
// In Scipio, the theme should have been already
// looked up into rendererVisualThemeResources by the renderer, which uses more 
// complex lookup logic than the stock screen userPrefs-based lookup.
// See ScreenRenderer.populateXxx.
 
themeResources = (globalContext.layoutSettings != null) ? globalContext.layoutSettings : [:];
visualThemeId = null;

if (context.rendererVisualThemeResources) {
    // this squashes any VT_XXX set in screens (though usually meant to be avoided)
    //themeResources.putAll(context.rendererVisualThemeResources);
    context.rendererVisualThemeResources.each { k, v ->
        existingVals = themeResources[k];
        if (existingVals != null) {
            existingVals.addAll(v);
        } else {
            newVals = []; // copy so no issues editing
            newVals.addAll(v);
            themeResources[k] = newVals;
        }
    }
    visualThemeId = themeResources.VT_ID[0];
}

context.themeResources = themeResources;
globalContext.visualThemeId = visualThemeId;
globalContext.layoutSettings = themeResources;
