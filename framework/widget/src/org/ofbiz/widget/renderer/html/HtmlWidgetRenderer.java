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
package org.ofbiz.widget.renderer.html;

import java.io.IOException;

import org.ofbiz.widget.WidgetWorker;
import org.ofbiz.widget.model.ModelWidget;

/**
 * Widget Library - HTML Widget Renderer implementation. HtmlWidgetRenderer
 * is a base class that is extended by other widget HTML rendering classes.
 * <p>
 * SCIPIO: NOTE: 2016-08-30: This class when used as a renderer base class is deprecated.
 * Some static utilities are preserved, for backward-compatibility, but use is discouraged.
 */
public class HtmlWidgetRenderer {
    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Characters that are appended to the end of each rendered element. Currently set to
     * CR/LF.
     * @deprecated SCIPIO: whitespace should be left to Freemarker macro template files.
     */
    @Deprecated
    public static final String whiteSpace = "\r\n";

    @Deprecated
    protected boolean widgetCommentsEnabled = false;

    /**
     * Helper method used to append whitespace characters to the end of each rendered element.
     * @deprecated SCIPIO: This class is no longer intended for use as a renderer superclass.
     * @param writer The writer to write to
     */
    @Deprecated
    public void appendWhitespace(Appendable writer) throws IOException {
        writer.append(whiteSpace);
    }

    /**
     * Helper method used to build the boundary comment string.
     * @deprecated SCIPIO: This class is no longer intended for use as a renderer base class.
     * @param boundaryType The boundary type: "Begin" or "End"
     * @param widgetType The widget type: "Screen Widget", "Form Widget", etc.
     * @param widgetName The widget name
     */
    @Deprecated
    public String buildBoundaryComment(String boundaryType, String widgetType, String widgetName) {
        return formatBoundaryComment(boundaryType, widgetType, widgetName);
    }

    /**
     * Helper method used to build the boundary comment string.
     * <p>
     * SCIPIO: NOTE: Use discouraged, it is better to leave this to the Freemarker
     * macro templates through Macro*Renderer.
     */
    public static String formatBoundaryComment(String boundaryType, String widgetType, String widgetName) {
        return "<!-- " + boundaryType + " " + widgetType + " " + widgetName + " -->" + whiteSpace;
    }

    /**
     * Renders the beginning boundary comment string.
     * @deprecated SCIPIO: This class is no longer intended for use as a renderer base class.
     * @param writer The writer to write to
     * @param widgetType The widget type: "Screen Widget", "Form Widget", etc.
     * @param modelWidget The widget
     */
    @Deprecated
    public void renderBeginningBoundaryComment(Appendable writer, String widgetType, ModelWidget modelWidget) throws IOException {
        if (this.widgetCommentsEnabled) {
            writer.append(this.buildBoundaryComment("Begin", widgetType, modelWidget.getBoundaryCommentName()));
        }
    }

    /**
     * Renders the ending boundary comment string.
     * @deprecated SCIPIO: This class is no longer intended for use as a renderer base class.
     * @param writer The writer to write to
     * @param widgetType The widget type: "Screen Widget", "Form Widget", etc.
     * @param modelWidget The widget
     */
    @Deprecated
    public void renderEndingBoundaryComment(Appendable writer, String widgetType, ModelWidget modelWidget) throws IOException {
        if (this.widgetCommentsEnabled) {
            writer.append(this.buildBoundaryComment("End", widgetType, modelWidget.getBoundaryCommentName()));
        }
    }

    /** Extracts parameters from a target URL string, prepares them for an Ajax
     * JavaScript call. This method is currently set to return a parameter string
     * suitable for the Prototype.js library.
     * @deprecated SCIPIO: 2018-09-07: use {@link org.ofbiz.widget.WidgetWorker#getAjaxParamsFromTarget(String)}.
     * @param target Target URL string
     * @return Parameter string
     */
    @Deprecated
    public static String getAjaxParamsFromTarget(String target) {
        return WidgetWorker.getAjaxParamsFromTarget(target);
    }
}
