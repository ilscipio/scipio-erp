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
package com.ilscipio.scipio.treeMenu.jsTree;

import java.util.HashMap;

import org.ofbiz.base.util.UtilValidate;

/**
 *
 * @author jsoto
 *
 */
@SuppressWarnings("serial")
public class JsTreeCore extends HashMap<String, Object> {

    public JsTreeCore(boolean multiple, HashMap<String, String> strings, Integer animation) {
        setMultiple(multiple);
        setStrings(strings);
        setAnimation(animation);
    }

    public void setMultiple(Boolean multiple) {
        put("multiple", multiple);
    }

    public void setStrings(HashMap<String, String> strings) {
        put("strings", strings);
    }

    public void setAnimation(Integer animation) {
        if (UtilValidate.isNotEmpty(animation))
            put("animation", animation);
    }

    public void setExpandSelectedOnload(Boolean expandSelectedOnload) {
        put("expand_selected_onload", expandSelectedOnload);
    }

    public void setWorker(Boolean worker) {
        put("worker", worker);
    }

    public void setForceText(Boolean forceText) {
        put("force_text", forceText);
    }

    public void setDblClickToggle(Boolean dblClickToggle) {
        put("dblclick_toggle", dblClickToggle);
    }

    public void setThemes(JsTreeTheme themes) {
        put("themes", themes);
    }

    public static class JsTreeTheme extends HashMap<String, Object> {
        private static final long serialVersionUID = -4086721432593844943L;

        public JsTreeTheme(String themeName, String themeUrl, String themeDir, boolean themeDots, boolean themeIcons, boolean themeStripes, String themeVariant,
                boolean themeResponsive) {

            setThemeName(themeName);
            setThemeUrl(themeUrl);
            setThemeDir(themeDir);
            setThemeDots(themeDots);
            setThemeIcons(themeIcons);
            setThemeStripes(themeStripes);
            setThemeVariant(themeVariant);
            setThemeResponsive(themeResponsive);

        }

        public JsTreeTheme(String themeName, String themeUrl, String themeDir) {
            setThemeName(themeName);
            setThemeUrl(themeUrl);
            setThemeDir(themeDir);
        }

        public void setThemeName(String themeName) {
            if (UtilValidate.isEmpty(themeName))
                themeName = String.valueOf(Boolean.FALSE);
            put("name", themeName);
        }

        public void setThemeUrl(String themeUrl) {
            if (UtilValidate.isEmpty(themeUrl))
                themeUrl = String.valueOf(Boolean.FALSE);
            put("url", themeUrl);
        }

        public void setThemeDir(String themeDir) {
            put("dir", themeDir);
        }

        public void setThemeDots(boolean themeDots) {
            put("dots", themeDots);
        }

        public void setThemeIcons(boolean themeIcons) {
            put("icons", themeIcons);
        }

        public void setThemeStripes(boolean themeStripes) {
            put("stripes", themeStripes);
        }

        public void setThemeVariant(String themeVariant) {
            put("variant", themeVariant);
        }

        public void setThemeResponsive(boolean themeResponsive) {
            put("responsive", themeResponsive);
        }

    }

}
