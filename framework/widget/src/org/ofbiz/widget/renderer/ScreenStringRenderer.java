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

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.widget.model.ModelScreenWidget;

/**
 * Widget Library - Screen String Renderer interface.
 */
public interface ScreenStringRenderer extends StringRenderer { // SCIPIO: StringRenderer
    public void renderScreenBegin(Appendable writer, Map<String, Object> context) throws IOException;
    public void renderScreenEnd(Appendable writer, Map<String, Object> context) throws IOException;
    public void renderSectionBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.Section section) throws IOException;
    public void renderSectionEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.Section section) throws IOException;
    public void renderColumnContainer(Appendable writer, Map<String, Object> context, ModelScreenWidget.ColumnContainer columnContainer) throws IOException;
    public void renderContainerBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.Container container) throws IOException;
    public void renderContainerEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.Container container) throws IOException;
    public void renderContentBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.Content content) throws IOException;
    public void renderContentBody(Appendable writer, Map<String, Object> context, ModelScreenWidget.Content content) throws IOException;
    public void renderContentEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.Content content) throws IOException;
    public void renderSubContentBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.SubContent content) throws IOException;
    public void renderSubContentBody(Appendable writer, Map<String, Object> context, ModelScreenWidget.SubContent content) throws IOException;
    public void renderSubContentEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.SubContent content) throws IOException;

    public void renderHorizontalSeparator(Appendable writer, Map<String, Object> context, ModelScreenWidget.HorizontalSeparator separator) throws IOException;
    public void renderLabel(Appendable writer, Map<String, Object> context, ModelScreenWidget.Label label) throws IOException;
    public void renderLink(Appendable writer, Map<String, Object> context, ModelScreenWidget.ScreenLink link) throws IOException;
    public void renderImage(Appendable writer, Map<String, Object> context, ModelScreenWidget.ScreenImage image) throws IOException;

    public void renderContentFrame(Appendable writer, Map<String, Object> context, ModelScreenWidget.Content content) throws IOException;
    public void renderScreenletBegin(Appendable writer, Map<String, Object> context, boolean collapsed, ModelScreenWidget.Screenlet screenlet) throws IOException;
    public void renderScreenletSubWidget(Appendable writer, Map<String, Object> context, ModelScreenWidget subWidget, ModelScreenWidget.Screenlet screenlet) throws GeneralException, IOException;
    public void renderScreenletEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.Screenlet screenlet) throws IOException;

    public void renderPortalPageBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage) throws GeneralException, IOException;
    public void renderPortalPageEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage) throws GeneralException, IOException;
    public void renderPortalPageColumnBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage, GenericValue portalPageColumn) throws GeneralException, IOException;
    public void renderPortalPageColumnEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage, GenericValue portalPageColumn) throws GeneralException, IOException;
    public void renderPortalPagePortletBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage, GenericValue portalPortlet) throws GeneralException, IOException;
    public void renderPortalPagePortletBody(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage, GenericValue portalPortlet) throws GeneralException, IOException;
    public void renderPortalPagePortletEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage, GenericValue portalPortlet) throws GeneralException, IOException;
}



