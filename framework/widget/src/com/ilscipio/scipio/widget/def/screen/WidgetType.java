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
package com.ilscipio.scipio.widget.def.screen;

/**
 * Enumeration of widget types for the unified {@link Widget} annotation.
 *
 * <p>Each type corresponds to an XML widget element type in screen definitions.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for unified widget annotation support.</p>
 */
public enum WidgetType {
    /**
     * Include screen widget - equivalent to &lt;include-screen&gt; element.
     */
    INCLUDE_SCREEN,

    /**
     * Include form widget - equivalent to &lt;include-form&gt; element.
     */
    INCLUDE_FORM,

    /**
     * Include menu widget - equivalent to &lt;include-menu&gt; element.
     */
    INCLUDE_MENU,

    /**
     * Include grid widget - equivalent to &lt;include-grid&gt; element.
     */
    INCLUDE_GRID,

    /**
     * Include tree widget - equivalent to &lt;include-tree&gt; element.
     */
    INCLUDE_TREE,

    /**
     * Label widget - equivalent to &lt;label&gt; element.
     */
    LABEL,

    /**
     * Screenlet widget - equivalent to &lt;screenlet&gt; element.
     */
    SCREENLET,

    /**
     * Container widget - equivalent to &lt;container&gt; element.
     */
    CONTAINER,

    /**
     * HTML template widget - equivalent to &lt;platform-specific&gt;&lt;html&gt;&lt;html-template&gt; element.
     */
    HTML_TEMPLATE,

    /**
     * Image widget - equivalent to &lt;image&gt; element.
     */
    IMAGE,

    /**
     * Horizontal separator widget - equivalent to &lt;horizontal-separator&gt; element.
     */
    HORIZONTAL_SEPARATOR,

    /**
     * Content widget - equivalent to &lt;content&gt; element.
     */
    CONTENT,

    /**
     * Sub-content widget - equivalent to &lt;sub-content&gt; element.
     */
    SUB_CONTENT,

    /**
     * Decorator section include - equivalent to &lt;decorator-section-include&gt; element.
     */
    DECORATOR_SECTION_INCLUDE,

    /**
     * Screen link widget - equivalent to &lt;link&gt; element.
     */
    LINK,

    /**
     * Column container widget - equivalent to &lt;column-container&gt; element.
     */
    COLUMN_CONTAINER,

    /**
     * Iterate section widget - equivalent to &lt;iterate-section&gt; element.
     */
    ITERATE_SECTION,

    /**
     * Include portal page - equivalent to &lt;include-portal-page&gt; element.
     * <p>SCIPIO: 4.0.0: the converter dropped these, so every portal page screen was empty.</p>
     */
    INCLUDE_PORTAL_PAGE,

    EMAIL_TEMPLATE
}
