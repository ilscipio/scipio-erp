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
package org.ofbiz.webapp.renderer;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;

/**
 * SCIPIO: Provides information about the current render.
 * <p>
 * This interface is essential in order to make information
 * available to webapp component from widget package that we can't reference.
 * <p>
 * 2018-08-16: Currently this info is provided by the
 * {@link org.ofbiz.widget.renderer.ScreenRenderer} class, because
 * it's the only thing available everywhere. But this is
 * Subject to change in the future! The implementations
 * of the fromRequest and fromContext methods below may change.
 */
public interface RendererInfo {

    /**
     * Returns the renderer name; usually "html", "xsl-fo",
     * "xml", "csv", etc. (as used on right side of xxxx.name= in widget.properties).
     */
    String getRendererName();

    @SuppressWarnings("unchecked")
    public static <T extends RendererInfo> T fromRequest(HttpServletRequest request) {
        return (T) request.getAttribute("screens"); // SUBJECT TO CHANGE
    }

    @SuppressWarnings("unchecked")
    public static <T extends RendererInfo> T fromContext(Map<String, Object> context, RenderEnvType renderEnvType) {
        return (T) context.get("screens"); // SUBJECT TO CHANGE
    }

    public static <T extends RendererInfo> T fromContext(Map<String, Object> context) {
        return fromContext(context, null);
    }

    public static <T extends RendererInfo> T fromRequestOrContext(HttpServletRequest request, Map<String, Object> context) {
        return (request != null) ? fromRequest(request) : (context != null) ? fromContext(context) : null;
    }
}
