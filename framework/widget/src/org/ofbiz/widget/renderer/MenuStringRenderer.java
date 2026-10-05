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

import java.io.IOException;
import java.util.Map;

import org.ofbiz.widget.model.CommonWidgetModels.Image;
import org.ofbiz.widget.model.ModelMenu;
import org.ofbiz.widget.model.ModelMenuItem;
import org.ofbiz.widget.model.ModelSubMenu;


/**
 * Widget Library - Form String Renderer interface
 */
public interface MenuStringRenderer extends StringRenderer { // SCIPIO: StringRenderer
    public void renderMenuItem(Appendable writer, Map<String, Object> context, ModelMenuItem menuItem, Boolean enabled) throws IOException ;
    public void renderMenuOpen(Appendable writer, Map<String, Object> context, ModelMenu menu) throws IOException ;
    public void renderMenuClose(Appendable writer, Map<String, Object> context, ModelMenu menu) throws IOException ;
    public void renderFormatSimpleWrapperOpen(Appendable writer, Map<String, Object> context, ModelMenu menu) throws IOException ;
    public void renderFormatSimpleWrapperClose(Appendable writer, Map<String, Object> context, ModelMenu menu) throws IOException ;
    public void renderFormatSimpleWrapperRows(Appendable writer, Map<String, Object> context, Object menu) throws IOException ;
    public void renderLink(Appendable writer, Map<String, Object> context, ModelMenuItem.MenuLink link, Boolean enabled) throws IOException ;
    public void renderImage(Appendable writer, Map<String, Object> context, Image image) throws IOException ;

    /**
     * SCIPIO: Render sub menu open.
     */
    public void renderSubMenuOpen(Appendable writer, Map<String, Object> context, ModelSubMenu subMenu) throws IOException ;
    /**
     * SCIPIO: Render sub menu close.
     */
    public void renderSubMenuClose(Appendable writer, Map<String, Object> context, ModelSubMenu subMenu) throws IOException ;

}
