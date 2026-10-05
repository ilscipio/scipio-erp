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
 * Enumeration of action types for the unified {@link Action} annotation.
 *
 * <p>Each type corresponds to an XML action element type in widget definitions.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for unified action annotation support.</p>
 */
public enum ActionType {
    /**
     * Set field action - equivalent to &lt;set&gt; element.
     */
    SET,

    /**
     * Clear field action - equivalent to &lt;clear-field&gt; element.
     */
    CLEAR_FIELD,

    /**
     * Service invocation action - equivalent to &lt;service&gt; element.
     */
    SERVICE,

    /**
     * Entity one lookup action - equivalent to &lt;entity-one&gt; element.
     */
    ENTITY_ONE,

    /**
     * Entity and lookup action - equivalent to &lt;entity-and&gt; element.
     */
    ENTITY_AND,

    /**
     * Entity condition lookup action - equivalent to &lt;entity-condition&gt; element.
     */
    ENTITY_CONDITION,

    /**
     * Get related one action - equivalent to &lt;get-related-one&gt; element.
     */
    GET_RELATED_ONE,

    /**
     * Get related action - equivalent to &lt;get-related&gt; element.
     */
    GET_RELATED,

    /**
     * Script action - equivalent to &lt;script&gt; element.
     */
    SCRIPT,

    /**
     * Property to field action - equivalent to &lt;property-to-field&gt; element.
     */
    PROPERTY_TO_FIELD,

    /**
     * Property map action - equivalent to &lt;property-map&gt; element.
     */
    PROPERTY_MAP,

    /**
     * Include screen actions - equivalent to &lt;include-screen-actions&gt; element.
     */
    INCLUDE_SCREEN_ACTIONS,

    /**
     * Include form actions - equivalent to &lt;include-form-actions&gt; element.
     */
    INCLUDE_FORM_ACTIONS,

    /**
     * Include form row actions - equivalent to &lt;include-form-row-actions&gt; element.
     */
    INCLUDE_FORM_ROW_ACTIONS,

    /**
     * Include menu actions - equivalent to &lt;include-menu-actions&gt; element.
     */
    INCLUDE_MENU_ACTIONS,

    /**
     * Include tree actions - equivalent to &lt;include-tree-actions&gt; element.
     */
    INCLUDE_TREE_ACTIONS,

    /**
     * Condition to field action - equivalent to &lt;condition-to-field&gt; element.
     */
    CONDITION_TO_FIELD,

    /**
     * Close object action - equivalent to &lt;close-object&gt; element.
     */
    CLOSE_OBJECT,

    /**
     * Throw exception action - equivalent to &lt;throw-exception&gt; element.
     */
    THROW_EXCEPTION
}
