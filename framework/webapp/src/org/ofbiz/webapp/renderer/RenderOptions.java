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

import java.io.Serializable;

/**
 * SCIPIO: Base render options class, stored in request attributes or context
 * and optionally in other objects.
 * <p>
 * NOT thread-safe.
 * <p>
 * NOTE: At this time, request and contexts are NOT required to contain this,
 * so callers must check for null or use specialized fetcher methods in the
 * subclasses. This could improve in the future.
 * <p>
 * See {@link org.ofbiz.widget.renderer.WidgetRenderOptions} for the real
 * implementation.
 */
@SuppressWarnings("serial")
public abstract class RenderOptions implements Serializable {

    public static final String FIELD_NAME = "scpRenderOpts";

    /**
     * Default constructor.
     */
    protected RenderOptions() {
    }

    /**
     * Copy constructor.
     */
    protected RenderOptions(RenderOptions other) {
    }

    public abstract RenderOptions copy();

    public abstract RenderOptions getReadOnly();
}
