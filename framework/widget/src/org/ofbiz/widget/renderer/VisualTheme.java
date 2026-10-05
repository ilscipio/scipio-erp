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
package org.ofbiz.widget.renderer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.widget.model.ModelTheme;
import org.w3c.dom.Element;

/**
 * Widget Theme Library - VisualTheme class
 */
@SuppressWarnings("serial")
public final class VisualTheme implements Serializable {

    public static final String module = VisualTheme.class.getName();
    private ModelTheme modelTheme;
    private final String visualThemeId;
    private final List<String> screenshots;
    private final FlexibleStringExpander displayName;
    private final FlexibleStringExpander description;

    public String getVisualThemeId() {
        return visualThemeId;
    }

    public List<String> getScreenshots() {
        return screenshots;
    }

    public String getDisplayName(Map<String, Object> context) {
        return displayName.expandString(context);
    }

    public String getDescription(Map<String, Object> context) {
        return description.expandString(context);
    }

    /**
     * Only constructor to initialize a visualTheme from xml definition
     * @param modelTheme
     * @param visualThemeElement
     */
    public VisualTheme(ModelTheme modelTheme, Element visualThemeElement) {
        this.modelTheme = modelTheme;
        this.visualThemeId = visualThemeElement.getAttribute("id");
        this.displayName = FlexibleStringExpander.getInstance(visualThemeElement.getAttribute("display-name"));
        this.description = FlexibleStringExpander.getInstance(UtilXml.elementValue(UtilXml.firstChildElement(visualThemeElement, "description")));
        List<String> initScreenshots = new ArrayList<>();
        for (Element screenshotElement : UtilXml.childElementList(visualThemeElement, "screenshot")) {
            initScreenshots.add(screenshotElement.getAttribute("location"));
        }
        this.screenshots = Collections.unmodifiableList(initScreenshots);
    }

    public ModelTheme getModelTheme() {
        return modelTheme;
    }

    public String toString() {
        StringBuilder toString = new StringBuilder("visual-theme-id:").append(visualThemeId)
                .append(", display-name: ").append(this.displayName)
                .append(", description: ").append(description)
                .append(", screenshots: ").append(screenshots);
        return toString.toString();
    }
}
